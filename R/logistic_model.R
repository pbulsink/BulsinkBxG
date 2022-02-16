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
    dplyr::select(c('result', 'adjusted_distance', 'shot_angle', 'shot_type', 'is_rebound', 'is_rush', 'speed', 'shooter_benefit', 'goalie_benefit',
                    'goal_differential', 'shooter_position', 'is_shooter_strong_side', 'is_goalie_glove_side', 'cross_ice', 'shooter_net_empty',
                    'shooter_pp_time', 'shooter_pp_time_remaining', 'goalie_pp_time', 'goalie_pp_time_remaining', 'is_even_strength',
                    'ordinal_goal_differential', 'previous_event', 'shootout', 'penalty_shot', 'target_net_empty', 'event'))
  pbp_ev<-pbp %>%
    dplyr::filter(.data$shootout != 1, .data$penalty_shot != 1, .data$target_net_empty != 1, .data$shooter_benefit == 0, .data$goalie_benefit == 0) %>%
    dplyr::select(-c('shootout', 'penalty_shot', 'target_net_empty', 'shooter_pp_time', 'shooter_pp_time_remaining', 'goalie_pp_time',
                     'goalie_pp_time_remaining', 'shooter_benefit', 'goalie_benefit', 'event'))

  pbp_pp<-pbp %>%
    dplyr::filter(.data$shootout != 1, .data$penalty_shot != 1, .data$target_net_empty != 1, .data$shooter_benefit > 0) %>%
    dplyr::select(-c('shootout', 'penalty_shot', 'target_net_empty', 'event'))

  pbp_pk<-pbp %>%
    dplyr::filter(.data$shootout != 1, .data$penalty_shot != 1, .data$target_net_empty != 1, .data$goalie_benefit > 0) %>%
    dplyr::select(-c('shootout', 'penalty_shot', 'target_net_empty', 'event'))

  pbp_en<-pbp %>%
    dplyr::filter(.data$target_net_empty == 1) %>%
    dplyr::select(-c('shootout', 'penalty_shot', 'target_net_empty', 'event'))

  pbp_sh<-pbp %>%
    dplyr::filter(.data$shootout == 1 | .data$penalty_shot == 1) %>%
    dplyr::select(-c('shootout', 'penalty_shot', 'event'))

  if(file.exists(file.path(getOption("BulsinkBxG.data.path"), paste0(season, "_data.rds"))) & season > 2011){
    test_data<-readRDS(file.path(getOption("BulsinkBxG.data.path"), paste0(season, "_data.rds")))
    test_data<-test_data %>%
      dplyr::filter(.data$event %in% fenwick_events) %>%
      dplyr::select(c('result', 'adjusted_distance', 'shot_angle', 'shot_type', 'is_rebound', 'is_rush', 'speed', 'shooter_benefit', 'goalie_benefit',
                      'goal_differential', 'shooter_position', 'is_shooter_strong_side', 'is_goalie_glove_side', 'cross_ice', 'shooter_net_empty',
                      'shooter_pp_time', 'shooter_pp_time_remaining', 'goalie_pp_time', 'goalie_pp_time_remaining', 'is_even_strength',
                      'ordinal_goal_differential', 'previous_event', 'shootout', 'penalty_shot', 'target_net_empty', 'event'))

    test_ev<-test_data %>%
      dplyr::filter(.data$shootout != 1, .data$penalty_shot != 1, .data$target_net_empty != 1, .data$shooter_benefit == 0, .data$goalie_benefit == 0) %>%
      dplyr::select(-c('shootout', 'penalty_shot', 'target_net_empty', 'shooter_pp_time', 'shooter_pp_time_remaining', 'goalie_pp_time',
                       'goalie_pp_time_remaining', 'shooter_benefit', 'goalie_benefit', 'event'))

    test_pp<-test_data %>%
      dplyr::filter(.data$shootout != 1, .data$penalty_shot != 1, .data$target_net_empty != 1, .data$shooter_benefit > 0) %>%
      dplyr::select(-c('shootout', 'penalty_shot', 'target_net_empty', 'event'))

    test_pk<-test_data %>%
      dplyr::filter(.data$shootout != 1, .data$penalty_shot != 1, .data$target_net_empty != 1, .data$goalie_benefit > 0) %>%
      dplyr::select(-c('shootout', 'penalty_shot', 'target_net_empty', 'event'))

    test_en<-test_data %>%
      dplyr::filter(.data$target_net_empty == 1) %>%
      dplyr::select(-c('shootout', 'penalty_shot', 'target_net_empty', 'event'))

    test_sh<-test_data %>%
      dplyr::filter(.data$shootout == 1 | .data$penalty_shot == 1) %>%
      dplyr::select(-c('shootout', 'penalty_shot', 'event'))

    train_ev<-pbp_ev
    train_pp<-pbp_pp
    train_pk<-pbp_pk
    train_en<-pbp_en
    train_sh<-pbp_sh

  } else {
    split_ev <- rsample::initial_split(data = pbp_ev, strata='result', prop = .25)
    split_pp <- rsample::initial_split(data = pbp_pp, strata='result', prop = .25)
    split_pk <- rsample::initial_split(data = pbp_pk, strata='result', prop = .25)
    split_en <- rsample::initial_split(data = pbp_en, strata='result', prop = .25)
    split_sh <- rsample::initial_split(data = pbp_sh, strata='result', prop = .25)

    test_ev<-split_ev %>% rsample::testing()
    test_pp<-split_pp %>% rsample::testing()
    test_pk<-split_pk %>% rsample::testing()
    test_en<-split_en %>% rsample::testing()
    test_sh<-split_sh %>% rsample::testing()

    train_ev<-split_ev %>% rsample::training()
    train_pp<-split_pp %>% rsample::training()
    train_pk<-split_pk %>% rsample::training()
    train_en<-split_en %>% rsample::training()
    train_sh<-split_sh %>% rsample::training()

    rm(split_ev, split_pp, split_pk, split_en, split_sh)
  }

  rm(pbp_ev, pbp_pp, pbp_pk, pbp_en, pbp_sh)

  message('Training Even Strength Model')
  ev_submodel<-build_logistic_submodel(train_data = train_ev, test_data = test_ev)
  ev_model<-ev_submodel$model
  ev_metrics<-ev_submodel$metrics
  rm(train_ev, test_ev)
  message('Training Result: AUC: ', ev_metrics$roc_auc, ", TES: ", ev_metrics$tes, ", LogLoss: ", ev_metrics$log_loss)

  message('Training Power Play Model')
  pp_submodel<-build_logistic_submodel(train_data = train_pp, test_data = test_pp)
  pp_model<-pp_submodel$model
  pp_metrics<-pp_submodel$metrics
  rm(train_pp, test_pp)
  message('Training Result: AUC: ', pp_metrics$roc_auc, ", TES: ", pp_metrics$tes, ", LogLoss: ", pp_metrics$log_loss)

  message('Training Penalty Kill Model')
  pk_submodel<-build_logistic_submodel(train_data = train_pk, test_data = test_pk)
  pk_model<-pk_submodel$model
  pk_metrics<- pk_submodel$metrics
  rm(train_pk, test_pk)
  message('Training Result: AUC: ', pk_metrics$roc_auc, ", TES: ", pk_metrics$tes, ", LogLoss: ", pk_metrics$log_loss)

  message('Training Empty Net Model')
  en_submodel<-build_logistic_submodel(train_data = train_en, test_data = test_en)
  en_model<-en_submodel$model
  en_metrics<-en_submodel$metrics
  rm(train_en, test_en)
  message('Training Result: AUC: ', en_metrics$roc_auc, ", TES: ", en_metrics$tes, ", LogLoss: ", en_metrics$log_loss)

  message('Training Shootout and Penalty Shot Model')
  sh_submodel<-build_logistic_submodel(train_data = train_sh, test_data = test_sh)
  sh_model<-sh_submodel$model
  sh_metrics<-sh_submodel$metrics
  rm(train_sh, test_sh)
  message('Training Result: AUC: ', sh_metrics$roc_auc, ", TES: ", sh_metrics$tes, ", LogLoss: ", sh_metrics$log_loss)

  model_collection<-list(
    "EV" = ev_model, #butcher::butcher(ev_model)...,
    "PP" = pp_model,
    "PK" = pk_model,
    "EN" = en_model,
    "SH" = sh_model
  )
  message('Testing Model Collection')

  test_results<-get_xg(model_collection = model_collection, data = test_data, metrics = TRUE)

  preds<- predict(cv.lasso, newx = x.test, s = "lambda.1se", type = 'response')

  test_tes<-tes_vec(truth = factor(y.test), estimate = preds, event_level = 'second')
  test_roc<-yardstick::roc_auc_vec(truth = factor(y.test), estimate = preds, event_level = 'second')
  test_ll<-yardstick::mn_log_loss_vec(truth = factor(y.test), estimate = preds, event_level = 'second')

  gc(verbose = FALSE)

  if(save_model){
    message("saving model")
    if(!dir.exists(file.path(getOption("BulsinkBxG.data.path"), 'models'))){
      dir.create(file.path(getOption("BulsinkBxG.data.path"), 'models'))
    }
    savedmodel<-butcher::butcher(cv.lasso)

    saveRDS(savedmodel, file = file.path(getOption("BulsinkBxG.data.path"), "models", paste0(season,"_logistic_model.RDS")))
  }

  doParallel::stopImplicitCluster()

  suppressMessages(gc())

  return(list(model=final_lr, metrics=list(tes = test_tes, roc_auc=test_roc, log_loss = test_ll)))
}

build_logistic_submodel<-function(train_data, test_data){

  #message('massaging data')
  x<-stats::model.matrix(result ~ ., train_data)
  x_test<-stats::model.matrix(result ~., test_data)
  y<-ifelse(train_data[complete.cases(train_data),]$result == 'goal', 1, 0)
  y_test<-ifelse(test_data[complete.cases(test_data),]$result == 'goal', 1, 0)
  y_test<-factor(y_test, levels = c(1,0))

  doParallel::registerDoParallel(cores = parallel::detectCores(logical = FALSE)) # approximately halves the number of cores used, to help with ram management.

  #message('tuning model')
  set.seed(1234)

  mod<-glmnet::cv.glmnet(x, y, alpha = 1, family = "binomial", parallel = TRUE)

  #message('testing model')
  # final_res <- tune::last_fit(final_lr,
  #                             train_test_split,
  #                             metrics = yardstick::metric_set(tes, yardstick::roc_auc, yardstick::mn_log_loss))

  preds<-as.vector(predict(mod, newx = x_test, s = "lambda.1se", type = 'response'))

  doParallel::stopImplicitCluster()

  test_tes<-tes_vec(truth = y_test, estimate = preds)
  test_roc<-yardstick::roc_auc_vec(truth = y_test, estimate = preds)
  test_ll<-yardstick::mn_log_loss_vec(truth = y_test, estimate = preds)

  return(list(model=mod, metrics = list(tes=test_tes, roc_auc = test_roc, log_loss = test_ll)))
}

get_xg<-function(data, model_collection, metrics = FALSE){
  fenwick_events <- c("Shot", "Missed Shot", "Miss", "Goal")

  data<-data %>%
    dplyr::filter(.data$event %in% fenwick_events) %>%
    dplyr::select(c('result', 'adjusted_distance', 'shot_angle', 'shot_type', 'is_rebound', 'is_rush', 'speed', 'shooter_benefit', 'goalie_benefit',
                    'goal_differential', 'shooter_position', 'is_shooter_strong_side', 'is_goalie_glove_side', 'cross_ice', 'shooter_net_empty',
                    'shooter_pp_time', 'shooter_pp_time_remaining', 'goalie_pp_time', 'goalie_pp_time_remaining', 'is_even_strength',
                    'ordinal_goal_differential', 'previous_event', 'shootout', 'penalty_shot', 'target_net_empty'))

  stopifnot(nrow(data) > 0)

  data_ev<-data %>%
    dplyr::filter(.data$shootout != 1, .data$penalty_shot != 1, .data$target_net_empty != 1, .data$shooter_benefit == 0, .data$goalie_benefit == 0) %>%
    dplyr::select(-c('shootout', 'penalty_shot', 'target_net_empty', 'shooter_pp_time', 'shooter_pp_time_remaining', 'goalie_pp_time',
                     'goalie_pp_time_remaining', 'shooter_benefit', 'goalie_benefit'))

  data_pp<-data %>%
    dplyr::filter(.data$shootout != 1, .data$penalty_shot != 1, .data$target_net_empty != 1, .data$shooter_benefit > 0) %>%
    dplyr::select(-c('shootout', 'penalty_shot', 'target_net_empty'))

  data_pk<-data %>%
    dplyr::filter(.data$shootout != 1, .data$penalty_shot != 1, .data$target_net_empty != 1, .data$goalie_benefit > 0) %>%
    dplyr::select(-c('shootout', 'penalty_shot', 'target_net_empty'))

  data_en<-data %>%
    dplyr::filter(.data$target_net_empty == 1) %>%
    dplyr::select(-c('shootout', 'penalty_shot', 'target_net_empty'))

  data_sh<-data %>%
    dplyr::filter(.data$shootout == 1 | .data$penalty_shot == 1) %>%
    dplyr::select(-c('shootout', 'penalty_shot'))

  truth_ev <- truth_pp <- truth_pk <- truth_en <- truth_sh <- NULL
  xg_ev <- xg_pp <- xg_pk <- xg_en <- xg_sh <- NULL

  if(sum(complete.cases(data_ev)) > 0){
    truth_ev<-factor(ifelse(data_ev[complete.cases(data_ev),]$result == 'goal', 1, 0), levels = c(1,0))
    data_ev<-stats::model.matrix(result ~ ., data_ev)
    xg_ev<-as.vector(predict(model_collection$EV, newx = data_ev, s = "lambda.1se", type = 'response'))
  }
  if(sum(complete.cases(data_pp)) > 0){
    truth_pp<-factor(ifelse(data_pp[complete.cases(data_pp),]$result == 'goal', 1, 0), levels = c(1,0))
    data_pp<-stats::model.matrix(result ~ ., data_pp)
    xg_pp<-as.vector(predict(model_collection$PP, newx = data_pp, s = "lambda.1se", type = 'response'))
  }
  if(sum(complete.cases(data_pk)) > 0){
    truth_pk<-factor(ifelse(data_pk[complete.cases(data_pk),]$result == 'goal', 1, 0), levels = c(1,0))
    data_pk<-stats::model.matrix(result ~ ., data_pk)
    xg_pk<-as.vector(predict(model_collection$PK, newx = data_pk, s = "lambda.1se", type = 'response'))
  }
  if(sum(complete.cases(data_en)) > 0){
    truth_en<-factor(ifelse(data_en[complete.cases(data_en),]$result == 'goal', 1, 0), levels = c(1,0))
    data_en<-stats::model.matrix(result ~ ., data_en)
    xg_en<-as.vector(predict(model_collection$EN, newx = data_en, s = "lambda.1se", type = 'response'))
  }
  if(sum(complete.cases(data_sh)) > 0){
    truth_sh<-factor(ifelse(data_sh[complete.cases(data_sh),]$result == 'goal', 1, 0), levels = c(1,0))
    data_sh<-stats::model.matrix(result ~ ., data_sh)
    xg_sh<-as.vector(predict(model_collection$SH, newx = data_sh, s = "lambda.1se", type = 'response'))
  }

  if(all(is.null(truth_ev), is.null(truth_pp), is.null(truth_pk), is.null(truth_en), is.null(truth_sh))){
    return(NULL)
  }

  if(metrics){
    truth<-c(truth_ev, truth_pp, truth_pk, truth_en, truth_sh)
    estimate<-c(xg_ev,xg_pp,xg_pk,xg_en,xg_sh)
    return(list(tes = tes_vec(truth=truth, estimate = estimate),
                log_loss = yardstick::mn_log_loss_vec(truth = truth, estimate = estimate),
                auc = yardstick::roc_auc_vec(truth = truth, estimate = estimate)))
  } else {
    return(sum(xg_ev,xg_pp,xg_pk,xg_en,xg_sh))
  }
}
