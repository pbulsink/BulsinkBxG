#' Build xG model
#'
#' @description The model is built on a past 3 years of data as xG change over time. For example, the model for 2021 is built with 2018, 2019, 2020 seasons. 2012 and 2013 are built with one and two seasons only, respectively. Model for 2011 is built and tested on 2011 data only.
#'
#' `r lifecycle::badge('experimental')`
#'
#' @param season The season for which to build the model. Must be 2012 or later.
#' @param save_model Whether to write the model to RDS file to be used later. File saved to default data directory, then `.../model/[season]_model.RDS`
#' @param num_seasons_include how many seasons to include, default 3
#' @param weighted Whether to use weighted data (older seasons less important) or have all wights 1
#'
#' @return invisibly a list of object containing the model and the metrics against the training set. The model output can be used directly with `predict()`
#' @export
build_logistic_model<-function(season, num_seasons_include=3, weighted=TRUE, save_model=TRUE){
  set.seed(123)
  stopifnot(is.numeric(season))
  stopifnot(season >= 2011)
  stopifnot(season <= as.numeric(strftime(Sys.Date(), '%Y')))
  stopifnot(num_seasons_include > 0)
  stopifnot(season-num_seasons_include >= 2011)

  if(season == 2011){
    message("Season 2011 will be built using 2011-2012 data (i.e. no out-of-season data).")
  }

  if (season <= 2012) {
    pbp<-load_prepped_data(2011)
    split11<-rsample::initial_split(data = pbp, strata='result', prop = .25)
    pbp<-split11 %>% rsample::training()
    test_data_split<-split11 %>% rsample::testing() %>% split_clean_data()
    rm(split11)
  } else {
    s = season-1
    pbp<-NULL
    while (s >= 2011 & s >= season-num_seasons_include) {
      pbp<-dplyr::bind_rows(pbp, load_prepped_data(s))
      s<-s-1
    }
  }

  # Add weights:
  if (weighted){
    pbp$season <- as.integer(substr(as.character(pbp$game_id), 1,4))
    pbp$case_weight <- 1/((season-pbp$season)^2)
    pbp$case_weight <- hardhat::importance_weights(pbp$case_weight)
  } else {
    pbp$case_weight <- hardhat::importance_weights(1)
  }


  pbp_split<-pbp %>%
    split_clean_data()

  if(file.exists(file.path(getOption("BulsinkBxG.data.path"), paste0(season, "_data.rds"))) & season > 2011){
    test_data<-readRDS(file.path(getOption("BulsinkBxG.data.path"), paste0(season, "_data.rds")))
    test_data$case_weight <- hardhat::importance_weights(1)
    test_data_split<-test_data %>%
      split_clean_data()
    train_data_split<-pbp_split
  } else {
    split_ev <- rsample::initial_split(data = pbp_split$ev, strata='result', prop = .25)
    split_pp <- rsample::initial_split(data = pbp_split$pp, strata='result', prop = .25)
    split_pk <- rsample::initial_split(data = pbp_split$pk, strata='result', prop = .25)
    split_en <- rsample::initial_split(data = pbp_split$en, strata='result', prop = .25)
    split_sh <- rsample::initial_split(data = pbp_split$sh, strata='result', prop = .25)

    test_data_split<-list()
    test_data_split$ev<-split_ev %>% rsample::testing()
    test_data_split$pp<-split_pp %>% rsample::testing()
    test_data_split$pk<-split_pk %>% rsample::testing()
    test_data_split$en<-split_en %>% rsample::testing()
    test_data_split$sh<-split_sh %>% rsample::testing()
    #test_data<-dplyr::bind_rows(test_data_split)

    train_data_split<-list()
    train_data_split$ev<-split_ev %>% rsample::training()
    train_data_split$pp<-split_pp %>% rsample::training()
    train_data_split$pk<-split_pk %>% rsample::training()
    train_data_split$en<-split_en %>% rsample::training()
    train_data_split$sh<-split_sh %>% rsample::training()

    rm(split_ev, split_pp, split_pk, split_en, split_sh)
  }

  rm(pbp_split)

  message('Training Even Strength Model')
  ev_submodel<-build_logistic_submodel(train_data = train_data_split$ev, test_data = test_data_split$ev)
  ev_model<-ev_submodel$model
  ev_metrics<-ev_submodel$metrics
  message('Training Result: AUC: ', round(ev_metrics$roc_auc, 3), ", TES: ", round(ev_metrics$tes, 2), ", LogLoss: ", round(ev_metrics$log_loss, 3))

  message('Training Power Play Model')
  pp_submodel<-build_logistic_submodel(train_data = train_data_split$pp, test_data = test_data_split$pp)
  pp_model<-pp_submodel$model
  pp_metrics<-pp_submodel$metrics
  message('Training Result: AUC: ', round(pp_metrics$roc_auc, 3), ", TES: ", round(pp_metrics$tes, 2), ", LogLoss: ", round(pp_metrics$log_loss, 3))

  message('Training Penalty Kill Model')
  pk_submodel<-build_logistic_submodel(train_data = train_data_split$pk, test_data = test_data_split$pk)
  pk_model<-pk_submodel$model
  pk_metrics<- pk_submodel$metrics
  message('Training Result: AUC: ', round(pk_metrics$roc_auc, 3), ", TES: ", round(pk_metrics$tes, 2), ", LogLoss: ", round(pk_metrics$log_loss, 3))

  message('Training Empty Net Model')
  en_submodel<-build_logistic_submodel(train_data = train_data_split$en, test_data = test_data_split$en)
  en_model<-en_submodel$model
  en_metrics<-en_submodel$metrics
  message('Training Result: AUC: ', round(en_metrics$roc_auc, 3), ", TES: ", round(en_metrics$tes, 2), ", LogLoss: ", round(en_metrics$log_loss, 3))

  message('Training Shootout and Penalty Shot Model')
  sh_submodel<-build_logistic_submodel(train_data = train_data_split$sh, test_data = test_data_split$sh)
  sh_model<-sh_submodel$model
  sh_metrics<-sh_submodel$metrics
  message('Training Result: AUC: ', round(sh_metrics$roc_auc, 3), ", TES: ", round(sh_metrics$tes, 2), ", LogLoss: ", round(sh_metrics$log_loss, 3))

  if(requireNamespace('butcher')){
    model_collection<-list(
      "ev" = butcher::butcher(ev_model),
      "pp" = butcher::butcher(pp_model),
      "pk" = butcher::butcher(pk_model),
      "en" = butcher::butcher(en_model),
      "sh" = butcher::butcher(sh_model)
    )
  } else {
    model_collection<-list(
      "ev" = ev_model,
      "pp" = pp_model,
      "pk" = pk_model,
      "en" = en_model,
      "sh" = sh_model
    )
  }
  message('Testing Model Collection')

  test_results<-get_xg(model_collection = model_collection, data = test_data_split, metrics = TRUE)

  message('Test Result: AUC: ', round(test_results$auc, 3), ", TES: ", round(test_results$tes, 2), ", LogLoss: ", round(test_results$log_loss, 3))

  if(save_model){
    message("saving model")
    if(!dir.exists(file.path(getOption("BulsinkBxG.data.path"), 'models'))){
      dir.create(file.path(getOption("BulsinkBxG.data.path"), 'models'))
    }

    saveRDS(model_collection, file = file.path(getOption("BulsinkBxG.data.path"), "models", paste0(season, ifelse(weighted, "_weighted", ""), "_logistic_model.RDS")))
  }

  invisible(list(model=model_collection, metrics=test_results))
}

build_logistic_submodel<-function(train_data, test_data){

  v_folds <- rsample::vfold_cv(train_data, strata = 'result')

  model_recipe <-
    recipes::recipe(result ~ ., data = train_data) %>%
    recipes::update_role(.data$event_id, new_role = "ID") %>%
    recipes::step_novel(recipes::all_predictors(),-recipes::all_numeric()) %>%
    recipes::step_naomit(.data$adjusted_distance, .data$shot_angle, skip = TRUE) %>%
    recipes::step_dummy(recipes::all_predictors(),-recipes::all_numeric()) #%>%

  model_spec <- parsnip::logistic_reg(
    mode = "classification",
    engine = "glmnet",
    penalty = tune::tune(),
    mixture = 0.5
  )

  model_workflow <- workflows::workflow() %>%
    workflows::add_recipe(model_recipe) %>%
    workflows::add_model(model_spec)

  doParallel::registerDoParallel(cores = parallel::detectCores(logical = FALSE))

  pen_vals <- 10^seq(-6, 0, length.out = 19)
  grid <- dials::grid_latin_hypercube(
    dials::penalty(),
    dials::mixture(),
    size = 30
  )

  model_res<-tune::tune_grid(model_workflow,
                             resamples = v_folds,
                             grid = 25,
                             control = tune::control_grid(save_pred = T),
                             metrics = yardstick::metric_set(yardstick::roc_auc, yardstick::mn_log_loss, tes))

  best_model<-tune::select_best(model_res, 'roc_auc')

  final_model<-model_workflow %>%
    tune::finalize_workflow(best_model) %>%
    parsnip::fit(data = train_data)

  test_fit <- stats::predict(final_model, test_data, type = 'prob')

  metrics<-list(
    'roc_auc' = yardstick::roc_auc_vec(truth = test_data$result, estimate = test_fit$.pred_goal),
    'log_loss'= yardstick::mn_log_loss_vec(truth = test_data$result, estimate = test_fit$.pred_goal),
    'tes' = tes_vec(truth = test_data$result, estimate = test_fit$.pred_goal)
    )

  doParallel::stopImplicitCluster()

  return(list(model=final_model, metrics = list(tes=metrics$tes, roc_auc = metrics$roc_auc, log_loss = metrics$log_loss)))

}

split_clean_data<-function(data){
  if('event' %in% colnames(data)){
    fenwick_events <- c("Shot", "Missed Shot", "Miss", "Goal")
    data<-data %>%
      dplyr::filter(.data$event %in% fenwick_events)
  }
  data<-data %>%
    dplyr::select(c('result', 'adjusted_distance', 'shot_angle', 'shot_type', 'is_rebound', 'is_rush', 'speed', 'shooter_benefit', 'goalie_benefit',
                    'goal_differential', 'shooter_position', 'is_shooter_strong_side', 'is_goalie_glove_side', 'cross_ice', 'shooter_net_empty',
                    'shooter_pp_time', 'shooter_pp_time_remaining', 'goalie_pp_time', 'goalie_pp_time_remaining', 'is_even_strength',
                    'ordinal_goal_differential', 'previous_event', 'shootout', 'penalty_shot', 'target_net_empty', 'event', 'event_id'))

  ev<-data %>%
    dplyr::filter(.data$shootout != 1, .data$penalty_shot != 1, .data$target_net_empty != 1, .data$shooter_benefit == 0, .data$goalie_benefit == 0) %>%
    dplyr::select(-c('shootout', 'penalty_shot', 'target_net_empty', 'shooter_pp_time', 'shooter_pp_time_remaining', 'goalie_pp_time',
                     'goalie_pp_time_remaining', 'shooter_benefit', 'goalie_benefit', 'event'))

  pp<-data %>%
    dplyr::filter(.data$shootout != 1, .data$penalty_shot != 1, .data$target_net_empty != 1, .data$shooter_benefit > 0) %>%
    dplyr::select(-c('shootout', 'penalty_shot', 'target_net_empty', 'event'))

  pk<-data %>%
    dplyr::filter(.data$shootout != 1, .data$penalty_shot != 1, .data$target_net_empty != 1, .data$goalie_benefit > 0) %>%
    dplyr::select(-c('shootout', 'penalty_shot', 'target_net_empty', 'event'))

  en<-data %>%
    dplyr::filter(.data$target_net_empty == 1) %>%
    dplyr::select(-c('shootout', 'penalty_shot', 'target_net_empty', 'event'))

  sh<-data %>%
    dplyr::filter(.data$shootout == 1 | .data$penalty_shot == 1) %>%
    dplyr::select(-c('shootout', 'penalty_shot', 'event'))

  return(list("ev" = ev, "pp" = pp, "pk" = pk, "en" = en, "sh" = sh))
}


#' Get xG
#'
#' @param data The data to get an xG value from
#' @param model_collection a model collection
#' @param metrics T/F on if you want metrics returned instead of xG values
#'
#' @return either a xG value for each row of data and the data in a list,  or metrics on the data
#' @export
get_xg<-function(data, model_collection, metrics = FALSE){
  if(!all(c('ev', 'pp', 'pk', 'en', 'sh') %in% names(data))){
    data<-data %>% split_clean_data()
  }
  xg_ev <- xg_pp <- xg_pk <- xg_en <- xg_sh <- NULL

  if(sum(stats::complete.cases(data$ev)) > 0){
    #data$ev$xG<-workflows:::predict.workflow(model_collection$ev, data$ev, type = 'prob')$.pred_goal
    data$ev$xG<-stats::predict(model_collection$ev, data$ev, type = 'prob')$.pred_goal
    xg_ev<-sum(data$ev$xG, na.rm=TRUE)
  }
  if(sum(stats::complete.cases(data$pp)) > 0){
    #data$pp$xG<-workflows:::predict.workflow(model_collection$pp, data$pp, type = 'prob')$.pred_goal
    data$pp$xG<-stats::predict(model_collection$pp, data$pp, type = 'prob')$.pred_goal
    xg_pp<-sum(data$pp$xG, na.rm=TRUE)
  }
  if(sum(stats::complete.cases(data$pk)) > 0){
    #data$pk$xG<-workflows:::predict.workflow(model_collection$pk, data$pk, type = 'prob')$.pred_goal
    data$pk$xG<-stats::predict(model_collection$pk, data$pk, type = 'prob')$.pred_goal
    xg_pk<-sum(data$pk$xG, na.rm=TRUE)
  }
  if(sum(stats::complete.cases(data$en)) > 0){
    #data$en$xG<-workflows:::predict.workflow(model_collection$en, data$en, type = 'prob')$.pred_goal
    data$en$xG<-stats::predict(model_collection$en, data$en, type = 'prob')$.pred_goal
    xg_en<-sum(data$en$xG, na.rm=TRUE)
  }
  if(sum(stats::complete.cases(data$sh)) > 0){
    #data$sh$xG<-workflows:::predict.workflow(model_collection$sh, data$sh, type = 'prob')$.pred_goal
    data$sh$xG<-stats::predict(model_collection$sh, data$sh, type = 'prob')$.pred_goal
    xg_sh<-sum(data$sh$xG, na.rm=TRUE)
  }

  xg <- sum(xg_ev,xg_pp,xg_pk,xg_en,xg_sh, na.rm = TRUE)
  if(xg == 0){
    return(NULL)
  }


  if(metrics){
    truth<-c(data$ev$result, data$pp$result, data$pk$result, data$en$result, data$sh$result)
    estimate<-c(data$ev$xG, data$pp$xG, data$pk$xG, data$en$xG, data$sh$xG)
    estimate[is.na(estimate)]<-0
    return(list(tes = tes_vec(truth=truth, estimate = estimate),
                log_loss = yardstick::mn_log_loss_vec(truth = truth, estimate = estimate),
                auc = yardstick::roc_auc_vec(truth = truth, estimate = estimate)))
  } else {
    data<-dplyr::bind_rows(data) %>%
      dplyr::select('event_id', 'xG') %>%
      dplyr::arrange('event_id')
    return(list('data' = data, 'xg' = sum(xg_ev,xg_pp,xg_pk,xg_en,xg_sh)))
  }
}
