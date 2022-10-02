#' Build xG model using xgboost
#'
#' @description The model is built on a past 3 years of data as xG change over time. For example, the model for 2021 is built with 2018, 2019, 2020 seasons. 2012 and 2013 are built with one and two seasons only, respectively. Model for 2011 is built and tested on 2011 data only.
#'
#' `r lifecycle::badge('experimental')`
#'
#' @param season The season for which to build the model. Must be 2011 or later.
#' @param save_model Whether to write the model to RDS file to be used later. File saved to default data directory, then `.../model/[season]_model.RDS`
#'
#' @return invisibly a list of object containing the model and the metrics against the training set. The model output can be used directly with `predict()`
#' @export
build_xgb_model<-function(season, save_model=TRUE){
  stopifnot(is.numeric(season))
  stopifnot(season >= 2011)
  stopifnot(season <= as.numeric(strftime(Sys.Date(), '%Y')))
  fenwick_events <- c('Shot', 'Missed Shot', 'Miss', 'Goal')

  if(season == 2011){
    warning("Season 2011 will be built using 2011-2012 data (i.e. no out-of-season data.")
  }

  if(season >= 2014){
    pbp<-dplyr::bind_rows(load_prepped_data(season-1), load_prepped_data(season-2), load_prepped_data(season-3))
  } else if (season == 2013){
    pbp<-dplyr::bind_rows(load_prepped_data(2011), load_prepped_data(2012))
  } else if (season <= 2012) {
    pbp<-load_prepped_data(2011)
  }

  pbp<-pbp %>%
    dplyr::filter(.data$event %in% fenwick_events) %>%
    dplyr::filter(.data$shootout != 1, .data$penalty_shot != 1, .data$target_net_empty != 1) %>%
    dplyr::select(c('result', 'adjusted_distance', 'shot_angle', 'shot_type', 'is_rebound', 'is_rush', 'speed', 'shooter_benefit', 'goalie_benefit',
                    'goal_differential', 'shooter_position', 'is_shooter_strong_side', 'is_goalie_glove_side', 'cross_ice', 'shooter_net_empty',
                    'shooter_pp_time', 'shooter_pp_time_remaining', 'goalie_pp_time', 'goalie_pp_time_remaining', 'is_even_strength',
                    'ordinal_goal_differential', 'x_coord', 'y_coord', 'event_id', 'previous_event'))

  if(file.exists(file.path(getOption("BulsinkBxG.data.path"), paste0(season, "_data.rds"))) & season > 2011){
    tdata<-readRDS(file.path(getOption("BulsinkBxG.data.path"), paste0(season, "_data.rds")))
    tdata<-tdata %>%
      dplyr::filter(.data$event %in% fenwick_events) %>%
      dplyr::filter(.data$shootout != 1, .data$penalty_shot != 1, .data$target_net_empty != 1)%>%
      dplyr::select(c('result', 'adjusted_distance', 'shot_angle', 'shot_type', 'is_rebound', 'is_rush', 'speed', 'shooter_benefit', 'goalie_benefit',
                      'goal_differential', 'shooter_position', 'is_shooter_strong_side', 'is_goalie_glove_side', 'cross_ice', 'shooter_net_empty',
                      'shooter_pp_time', 'shooter_pp_time_remaining', 'goalie_pp_time', 'goalie_pp_time_remaining', 'is_even_strength',
                      'ordinal_goal_differential', 'x_coord', 'y_coord', 'event_id', 'previous_event'))

    tt<-dplyr::bind_rows(pbp, tdata)
    train_test_split <- rsample::initial_time_split(data = tt, prop = nrow(pbp)/nrow(tt))
    train_data<-train_test_split %>% rsample::training()
    test_data<-train_test_split %>% rsample::testing()
    rm(tdata, tt, pbp)
  } else {
    train_test_split <- rsample::initial_split(data = pbp, strata='result')
    train_data<-train_test_split %>% rsample::training()
    test_data<-train_test_split %>% rsample::testing()
    rm(pbp)
  }

  message('massaging data')
  v_folds <- rsample::vfold_cv(train_data, strata = 'result')

  model_recipe <-
    recipes::recipe(result ~ ., data = train_data) %>%
    #themis::step_downsample(is_goal, under_ratio = 5) %>%
    recipes::update_role(.data$event_id, new_role = "ID") %>%
    recipes::step_novel(recipes::all_predictors(),-recipes::all_numeric()) %>%
    recipes::step_naomit(.data$adjusted_distance, .data$shot_angle, .data$x_coord, .data$y_coord, skip = TRUE) %>%
    recipes::step_dummy(recipes::all_predictors(),-recipes::all_numeric()) #%>%
  #recipes::prep() ## turns out feeding in new data is harder if the recipe is prepped? whatever that means.

  message('defining model parameters')

  xgb_spec <- parsnip::boost_tree(
    trees = 1000,
    tree_depth = tune::tune(),
    min_n = tune::tune(),
    loss_reduction = tune::tune(),                     ## first three: model complexity
    sample_size = tune::tune(),
    mtry = tune::tune(),                               ## randomness
    learn_rate = tune::tune(),                         ## step size

  )

  if(getOption("BulsinkBxG.xgboost.gpu", default = FALSE)){
    xgb_spec <- xgb_spec %>%
      parsnip::set_engine("xgboost", scale_pos_weight = tune::tune(), max_delta_step = 1, tree_method = 'gpu_hist')
  } else {
    xgb_spec <- xgb_spec %>%
      parsnip::set_engine("xgboost", scale_pos_weight = tune::tune(), max_delta_step = 1)
  }

  xgb_spec <- xgb_spec %>%
    parsnip::set_engine("xgboost", scale_pos_weight = tune::tune(), max_delta_step = 1)

  xgb_spec <- xgb_spec %>%
    parsnip::set_mode("classification")

  xgb_grid <- dials::grid_latin_hypercube(
    dials::tree_depth(),
    dials::min_n(),
    dials::loss_reduction(),
    sample_size = dials::sample_prop(),
    dials::finalize(dials::mtry(), train_data),
    dials::learn_rate(),
    dials::scale_pos_weight(range = c(8,10)),
    size = 30
  )

  xgb_wf <- workflows::workflow() %>%
    workflows::add_recipe(model_recipe) %>%#, blueprint = hardhat::default_recipe_blueprint(allow_novel_levels = TRUE)) %>%
    #workflows::add_formula(is_goal ~ .) %>%
    workflows::add_model(xgb_spec)

  if(!getOption("BulsinkBxG.xgboost.gpu", default = FALSE) & requireNamespace("doParallel")){
    doParallel::registerDoParallel(cores = parallel::detectCores())
  }

  message('tuning model')
  set.seed(1234)
  xgb_res <- tune::tune_grid(
    xgb_wf,
    resamples = v_folds,
    grid = xgb_grid,
    control = tune::control_grid(save_pred = TRUE),
    metrics = yardstick::metric_set(tes,  yardstick::roc_auc, yardstick::mn_log_loss)
  )

  if(!getOption("BulsinkBxG.xgboost.gpu", default = FALSE) & requireNamespace("doParallel")){
    doParallel::stopImplicitCluster()
  }
  #tune::show_best(xgb_res, "tes")

  best_model <- tune::select_best(xgb_res, "tes")

  message('finalizing model')
  final_xgb <- xgb_wf %>%
    tune::finalize_workflow(best_model) %>%
    parsnip::fit(data = train_data)

  #final_xgb
  message('testing model')
  final_res <- tune::last_fit(final_xgb,
                              train_test_split,
                              metrics = yardstick::metric_set(tes, yardstick::roc_auc, yardstick::mn_log_loss))

  #xgb_wf_model <- final_xgb$.workflow[[1]]

  if(save_model){
    #Current tests show:
    # a) the object to save is the final_xgb$.workflow[[1]]
    # b) further significant reductions in file size achieved by:
    #    i. butcher::axe_data()
    #   ii. butcher::axe_env()
    #  iii. butcher::axe_call()
    #
    # Note: butcher::butcher removes too much, the model won't work after that.
    # Note: butcher::axe_ctrl also fails?
    # See: https://github.com/tidymodels/butcher/issues/147 for updates on butcher performance

    if(!dir.exists(file.path(getOption("BulsinkBxG.data.path"), 'models'))){
      dir.create(file.path(getOption("BulsinkBxG.data.path"), 'models'))
    }
    if(requireNamespace('butcher')){
      savedmodel<-final_xgb$.workflow[[1]] %>%
        butcher::axe_data() %>%
        butcher::axe_env() %>%
        butcher::axe_call()
    } else {
      savedmodel<-final_xgb$.workflow[[1]]
    }
    saveRDS(savedmodel, file = file.path(getOption("BulsinkBxG.data.path"), "models", paste0(season,"_xgb_model.RDS")))
  }

  return(list(model=final_xgb, metrics=tune::collect_metrics(final_res)))
}
