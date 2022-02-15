#' Build xG model
#'
#' @description The model is built on a past 3 years of data as xG change over time. For example, the model for 2021 is built with 2018, 2019, 2020 seasons. 2012 and 2013 are built with one and two seasons only, respectively. Model for 2011 is built and tested on 2011 data only.
#'
#' `r lifecycle::badge('experimental')`
#'
#' @param season The season for which to build the model. Must be 2012 or later.
#' @param save_model Whether to write the model to RDS file to be used later. File saved to default data directory, then `.../model/[season]_model.RDS`
#'
#' @return invisibly a list of object containing the model and the metrics against the training set. The model output can be used directly with `predict()`
#' @export
build_logistic_model<-function(season, save_model=TRUE){
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

  lr_recipe <-
    recipes::recipe(result ~ ., data = train_data) %>%
    #themis::step_downsample(result) %>%
    recipes::update_role(.data$event_id, new_role = "ID") %>%
    recipes::step_novel(recipes::all_predictors(),-recipes::all_numeric()) %>%
    recipes::step_naomit(.data$adjusted_distance, .data$shot_angle, .data$x_coord, .data$y_coord, skip = TRUE) %>%
    recipes::step_dummy(recipes::all_nominal(), -recipes::all_outcomes()) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(recipes::all_predictors())

  #recipes::prep() ## turns out feeding in new data is harder if the recipe is prepped? whatever that means.

  message('defining model parameters')
  lr_mod<-parsnip::logistic_reg(penalty=tune::tune(), mixture = 1) %>%  #using a fully lasso version
    parsnip::set_engine('glmnet')

  lr_workflow <-
    workflows::workflow() %>%
    workflows::add_model(lr_mod) %>%
    workflows::add_recipe(lr_recipe)

  lr_grid<-tibble::tibble(penalty = c(seq(0,10^-1, length.out = 30)))

  doParallel::registerDoParallel(cores = parallel::detectCores(logical = FALSE)) # approximately halves the number of cores used, to help with ram management.

  message('tuning model')
  set.seed(1234)
  lr_res <- lr_workflow %>%
    tune::tune_grid(resamples = v_folds,
                    grid = lr_grid,
                    control = tune::control_grid(save_pred = TRUE),
                    metrics = yardstick::metric_set(tes,  yardstick::roc_auc, yardstick::mn_log_loss))

  best_model <- tune::select_by_one_std_err(lr_res, metric = "roc_auc", desc(penalty))

  message('finalizing model')
  final_lr <- lr_workflow %>%
    tune::finalize_workflow(best_model) %>%
    parsnip::fit(data = train_data)

  message('testing model')
  final_res <- tune::last_fit(final_lr,
                              train_test_split,
                              metrics = yardstick::metric_set(tes, yardstick::roc_auc, yardstick::mn_log_loss))

  gc(verbose = FALSE)

  if(save_model){
    message("saving model")
    if(!dir.exists(file.path(getOption("BulsinkBxG.data.path"), 'models'))){
      dir.create(file.path(getOption("BulsinkBxG.data.path"), 'models'))
    }
    savedmodel<-butcher::butcher(final_lr)

    saveRDS(savedmodel, file = file.path(getOption("BulsinkBxG.data.path"), "models", paste0(season,"_logistic_model.RDS")))
  }

  doParallel::stopImplicitCluster()

  suppressMessages(gc())

  return(list(model=final_lr, metrics=tune::collect_metrics(final_res)))
}
