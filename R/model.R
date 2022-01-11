prep_xg_model_data<-function(pbp){
  fenwick_events <- c('Shot', 'Missed Shot', 'Miss', 'Goal')
  corsi_events <- c('Shot', 'Missed Shot', 'Miss', 'Goal', 'Blocked Shot', 'Block')

  data <- pbp %>%
    dplyr::filter(.data$event %in% corsi_events | dplyr::lag(.data$event %in% corsi_events)) %>%  # Use Fenwicks because blocks are incorrectly located
    dplyr::mutate(is_home = dplyr::if_else(.data$team_tri_code == .data$home_team, 1, 0),  # binary home team event factor
           is_goal = dplyr::if_else(.data$event == "Goal", 1, 0))%>%  # binary is goal factor
    dplyr::group_by(.data$game_id, .data$about_period, .data$team_tri_code) %>%
    dplyr::mutate(
      x_correct = dplyr::if_else(stats::median(.data$x_coord[.data$event %in% c('shot', 'Goal') & abs(.data$x_coord) > 25]) < 0, -1L, 1L, 1L),  # have to put all the shots on the right side of the rink.
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      x_coord = .data$x_coord * .data$x_correct,
      y_coord = .data$y_coord * .data$x_correct
    ) %>%
    #dplyr::select(-.data$x_correct) %>%
    # dplyr::ungroup(.data$about_period, .data$team_tri_code) %>%
    #        # absolute x coordinate - put home and away on same side of ice
    #        x_coord = dplyr::if_else(.data$team_tri_code == .data$away_team & .data$time <= 1200, .data$x_coord, .data$x_coord*-1L),
    #        x_coord = dplyr::if_else(.data$team_tri_code == .data$away_team & .data$time >= 2400 & .data$time <= 3600, .data$x_coord, .data$x_coord*-1L),
    #        x_coord = dplyr::if_else(.data$team_tri_code == .data$home_team & .data$time >= 1200 & .data$time <= 2400, .data$x_coord, .data$x_coord*-1L),
    #        y_coord = dplyr::if_else(.data$team_tri_code == .data$away_team & .data$time <= 1200, .data$y_coord, .data$y_coord*-1L),
    #        y_coord = dplyr::if_else(.data$team_tri_code == .data$away_team & .data$time >= 2400 & .data$time <= 3600, .data$y_coord, .data$y_coord*-1L),
    #        y_coord = dplyr::if_else(.data$team_tri_code == .data$home_team & .data$time >= 1200 & .data$time <= 2400, .data$y_coord, .data$y_coord*-1L),
    #        # if overtime and playoffs then change all overtime results
    #        x_coord = dplyr::if_else(substr(.data$game_id, 6,6) == "3" & .data$time > 3600 & .data$team_tri_code == .data$away_team & floor(.data$time/1200) %% 2 == 0, .data$x_coord, .data$x_coord*-1L),
    #        x_coord = dplyr::if_else(substr(.data$game_id, 6,6) == "3" & .data$time > 3600 & .data$team_tri_code == .data$home_team & floor(.data$time/1200) %% 2 == 1, .data$x_coord, .data$x_coord*-1L),
    #        y_coord = dplyr::if_else(substr(.data$game_id, 6,6) == "3" & .data$time > 3600 & .data$team_tri_code == .data$away_team & floor(.data$time/1200) %% 2 == 0, .data$y_coord, .data$y_coord*-1L),
    #        y_coord = dplyr::if_else(substr(.data$game_id, 6,6) == "3" & .data$time > 3600 & .data$team_tri_code == .data$home_team & floor(.data$time/1200) %% 2 == 1, .data$y_coord, .data$y_coord*-1L)) %>%
    #
    #calculate time_diff but only use the game, don't revert to previous game in data stack
    dplyr::group_by(.data$game_id) %>%
    # #arrange(event_index, .by_group = TRUE) %>%
    dplyr::mutate(time_diff = .data$time - dplyr::lag(.data$time)) %>%  # calculate time from previous event
    dplyr::mutate(previous_event = dplyr::lag(.data$event)) %>%
    dplyr::mutate(previous_event = dplyr::if_else(is.na(.data$previous_event), "Unk", .data$previous_event)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(time_diff = dplyr::if_else(is.na(.data$time_diff), 1000, .data$time_diff),  # fix any NA results - 100 s will have such low impact on values.
           time_diff = dplyr::if_else(.data$time_diff < 0, 1000, .data$time_diff)) %>%  # <0 time diff makes no sense, bump it to the psudo na value of 1000 s

    dplyr::mutate(is_rebound = dplyr::if_else(.data$time_diff < 3 & .data$event %in% fenwick_events & dplyr::lag(.data$event) %in% corsi_events & .data$team_tri_code == dplyr::lag(.data$team_tri_code), 1, 0, missing = 0),  # is this a rebound shot?
                   is_rush = dplyr::if_else(.data$time_diff < 5 & dplyr::lag(.data$x_coord) < 25, 1, 0, missing = 0),  # does this look like a shot on a rush?
                   speed = dplyr::if_else(!is.na(.data$time_diff), sqrt((.data$x_coord-dplyr::lag(.data$x_coord))^2 + (.data$y_coord - dplyr::lag(.data$y_coord))^2)/.data$time_diff, 0, missing = 0),  # speed from previous event to shot location in ft/s
                   shot_side = dplyr::if_else(.data$y_coord < 0, 'R', 'L', missing = "Unk"),  # from which side (goalie view) the fenwick comes from
                   cross_ice = dplyr::if_else(.data$shot_side == dplyr::lag(.data$shot_side), 1, 0, missing = 0), # if shot comes from opposite side of previous event
                   delta_angle = dplyr::if_else(.data$time_diff < 5, abs(atan((.data$y_coord) / (89 - abs(.data$x_coord))) * (180 / pi)) - abs(atan(dplyr::lag(.data$y_coord) / (89 - abs(dplyr::lag(.data$x_coord)))) * (180 / pi)), 0), missing = 0) %>%
    #We've done our manipulations using the lagged data, go to fenwick only data now
    #dplyr::filter(.data$event %in% fenwick_events & .data$about_period_type != "SHOOTOUT") %>%
    dplyr::mutate(shooter_id = .data$player_id_1,
                  goalie_id = dplyr::if_else(.data$is_home == 1, .data$away_goalie, .data$home_goalie)) %>%
    #dplyr::filter(!grepl("PS - ", .data$shot_type)) %>% # drop Penalty Shots from the main model
    dplyr::mutate(shot_side = dplyr::if_else(abs(.data$y_coord) <= 3, "Mid", .data$shot_side)) %>%
    dplyr::mutate(hand_position = get_player_stat(.data$shooter_id, stat=c('shoots_catches','primary_position_abbreviation'))) %>%
    tidyr::separate(.data$hand_position, c("shooter_hand", "shooter_position"), sep = ",", remove=TRUE) %>%
    dplyr::mutate(shooter_position = dplyr::if_else(is.na(.data$shooter_position), "Unk", .data$shooter_position, "Unk")) %>%
    dplyr::mutate(goalie_glove = get_player_stat(.data$goalie_id, stat='shoots_catches')) %>%
    dplyr::mutate(is_shooter_strong_side = dplyr::case_when(.data$shooter_hand =="L" & .data$shot_side == "R" ~ 1,
                                                         .data$shooter_hand =="R" & .data$shot_side == "L" ~ 1,
                                                         .data$shot_side == "Mid" ~ 1, #shots from the middle are strong for both hands.
                                                         TRUE ~ 0),
                  is_goalie_glove_side = dplyr::if_else(.data$goalie_glove == .data$shot_side, 1, 0, 0)) %>%  # shots from middle aren't glove side.
    dplyr::mutate(shot_angle = dplyr::if_else(abs(.data$y_coord) < 3, 0, dplyr::if_else(.data$y_coord > 0,
                                                              abs(atan((.data$y_coord - 3) / (89 - abs(.data$x_coord))) * (180 / pi)),
                                                              abs(atan((.data$y_coord + 3) / (89 - abs(.data$x_coord))) * (180 / pi)))),# angle to the closest post if outside

                   shot_distance = sqrt((89 - .data$x_coord)^2 + .data$y_coord^2)#, # distance to the center of the goal mouth TODO: change to nearest post?
                   ) %>%
    dplyr::mutate(shot_angle = dplyr::if_else(.data$x_coord >= 89, 90 + .data$shot_angle,.data$shot_angle)) %>% # fix angles for behind-net shots
    dplyr::mutate(shooter_state = dplyr::if_else(.data$is_home == 1, substr(.data$strength, 1,1), substr(.data$strength, 3,3), missing = as.character(ifelse(.data$is_home==1, 5-.data$home_in_box, 5-.data$away_in_box))),
                  defender_state = dplyr::if_else(.data$is_home == 1, substr(.data$strength, 3,3), substr(.data$strength, 1,1), missing = as.character(ifelse(.data$is_home==0, 5-.data$home_in_box, 5-.data$away_in_box)))) %>%
    dplyr::mutate(shooter_benefit = dplyr::case_when(.data$shooter_state == "E" ~ 1,
                                                     .data$defender_state == "E" ~ 0,
                                                     as.numeric(.data$shooter_state)>as.numeric(.data$defender_state) ~ 1,
                                                     TRUE ~ 0),
                  goalie_benefit = dplyr::case_when(.data$shooter_state == "E" ~ 0,
                                                    .data$defender_state == "E" ~ 1,
                                                    as.numeric(.data$shooter_state)<as.numeric(.data$defender_state) ~ 1,
                                                    TRUE ~ 0),
                  shooter_net_empty = dplyr::if_else(.data$shooter_state == "E", 1, 0,0),
                  target_net_empty = dplyr::if_else(.data$defender_state == "E", 1, 0,0),
                  penalty_shot = dplyr::if_else(grepl("PS - ", .data$shot_type),1,0,0),
                  shootout = dplyr::if_else(.data$about_period_type == "SHOOTOUT", 1, 0, 0),
                  is_even_strength = dplyr::if_else(.data$shooter_state == .data$defender_state, 1, 0)) %>%
    dplyr::mutate(goal_differential = dplyr::if_else(.data$is_home == 1, .data$home_goals - .data$away_goals, .data$away_goals - .data$home_goals))%>%
    dplyr::mutate(goal_differential = dplyr::if_else(.data$event == "Goal", as.integer(.data$goal_differential - 1), as.integer(.data$goal_differential))) %>%  # Goals is counted on a goal event, but the shot was befoer the goal was in.
    dplyr::mutate(ordinal_goal_differential = dplyr::case_when(.data$goal_differential > 0 ~ "Leading",
                                                               .data$goal_differential < 0 ~ "Trailing",
                                                               TRUE ~ "Tied")) %>%
    dplyr::mutate(shooter_pp_time = dplyr::if_else(.data$is_home == 1, .data$away_time_on_pk, .data$home_time_on_pk, 0),
                  shooter_pp_time_remaining = dplyr::if_else(.data$is_home == 1, .data$away_time_to_full_strength, .data$home_time_to_full_strength, 0),
                  goalie_pp_time = dplyr::if_else(.data$is_home == 1, .data$home_time_on_pk, .data$away_time_on_pk, 0),
                  goalie_pp_time_remaining = dplyr::if_else(.data$is_home == 1, .data$home_time_to_full_strength, .data$away_time_to_full_strength, 0)) %>%
    dplyr::mutate(shot_type = dplyr::if_else(is.na(.data$shot_type), "Unk", .data$shot_type, "Unk"),
                  shot_side = dplyr::if_else(is.na(.data$shot_type), "Mid", .data$shot_side, "Mid"),
                  speed = dplyr::if_else(is.na(.data$speed) | is.infinite(.data$speed), 0, .data$speed, 0)) %>% # sometimes thing break and we get infinite speed - that's no good.
    dplyr::mutate(shot_type = forcats::as_factor(.data$shot_type),
                  shot_side = forcats::as_factor(.data$shot_side),
                  ordinal_goal_differential = forcats::as_factor(.data$ordinal_goal_differential),
                  shooter_position = forcats::as_factor(.data$shooter_position),
                  previous_event = forcats::as_factor(.data$previous_event)) %>%
    dplyr::mutate(shooter_position = forcats::fct_relevel(.data$shooter_position, 'Unk'),  # This and following relevel the unknown/tied/etc. to first element, so that recipe::dummy doesn't put a column for that variable.
                  shot_type = forcats::fct_relevel(.data$shot_type, 'Unk'),
                  ordinal_goal_differential = forcats::fct_relevel(.data$ordinal_goal_differential, 'Tied'),
                  shot_side = forcats::fct_relevel(.data$shot_side, 'Mid'),
                  previous_event = forcats::fct_relevel(.data$previous_event, 'Unk')) %>%
    dplyr::mutate(shot_type = forcats::fct_drop(.data$shot_type)) %>%
    dplyr::mutate(result = factor(as.character(.data$is_goal), levels = c('0', '1'), labels = c('no_goal','goal'), ordered = TRUE))

    #dplyr::order_by(c("game_id", "time", "about_event_id"))
  #select(season, game_id, game_period, event_type, event_detail, team_tri_code, event_player_1, coords_x, coords_y, home_team, away_team, home_skaters, away_skaters, home_score, away_score, is_home, is_goal, x_coord, y_coord, time_diff, is_rebound, is_rush, speed, shot_side, cross_ice, is_cluster, pbp_distance, event_zone, shot_angle, shot_distance, attacker_benefit, shooter_net_empty, target_net_empty, shooter_goal_differential)
  gc()
  return(data)
}

#' Load Prepped data
#'
#' @param season Season's prepped data to load.
#'
#' @return a data frame
#' @export
load_prepped_data<-function(season){
  if(!file.exists(file.path(getOption("BulsinkBxG.data.path"), paste0(season, "_data.rds")))){
    message("Processing Data")
    data<-prep_xg_model_data(load_season_pbp(season))
    message('adjusting for rink bias')
    data<-adjust_for_rink_bias(data, season = season)
    saveRDS(data, file.path(getOption("BulsinkBxG.data.path"), paste0(season, "_data.rds")))
    invisible(data)
  } else {
    invisible(readRDS(file.path(getOption("BulsinkBxG.data.path"), paste0(season, "_data.rds"))))
  }
}

determine_rink_bias<-function(data){
  #subfunction for purr (data, team)
  venue_adj <- function(venue, data){
    data<-data %>%
      dplyr::filter(.data$penalty_shot == 0, .data$shootout==0, .data$event %in% c("Shot", "Goal", "Miss", "Missed Shot"))
    n_teams<-length(unique(c(data$home_team[!is.na(data$home_team)], data$away_team[!is.na(data$away_team)])))
    homedata <- data %>% dplyr::filter(.data$home_team == venue)

    away_dist <- mean(homedata[homedata$team_tri_code != venue, ]$shot_distance, na.rm = TRUE)
    home_dist <- mean(homedata[homedata$team_tri_code == venue, ]$shot_distance, na.rm = TRUE)
    venue_dist<-mean(homedata$shot_distance, na.rm = TRUE)
    venue_angle<-mean(homedata$shot_angle, na.rm = TRUE)
    rm(homedata)
    league_average_dist<-mean(data$shot_distance, na.rm=TRUE)
    league_dist<-mean(data %>% dplyr::filter(.data$home_team != venue) %>% dplyr::pull(.data$shot_distance), na.rm = TRUE)
    league_angle<-mean(data %>% dplyr::filter(.data$home_team != venue) %>% dplyr::pull(.data$shot_angle), na.rm = TRUE)
    return(list('venue'=venue, 'angle_bias' = venue_angle/league_angle, 'distance_bias' = venue_dist/league_dist, 'home_dist'= home_dist, 'away_dist' = away_dist, 'league_average_dist' = league_average_dist))
  }

  venue_adjustments<-purrr::map_dfr(unique(c(data$home_team[!is.na(data$home_team)], data$away_team[!is.na(data$away_team)])),
                                    function(x) venue_adj(x, data))

  return(venue_adjustments)
}

calculate_all_rink_biases<-function(){
  yearlist<-list()
  for(y in 2011:2020){
    data<-load_prepped_data(y)
    rinkb<-determine_rink_bias(data)
    rinkb$season<-y
    yearlist[[as.character(y)]] <- rinkb
    rm(data)
    gc(verbose = FALSE)
  }

  rinkbiases<-dplyr::bind_rows(yearlist)
  return(rinkbiases)
}

adjust_for_rink_bias <- function(data, season, backchecking = FALSE){
  if(season %in% rink_biases$season & !backchecking){
    data <- data %>%
      dplyr::rowwise() %>%
      dplyr::mutate(adjusted_distance = dplyr::if_else(.data$is_home == 1,
                                                       .data$shot_distance * (rink_biases[rink_biases$venue == .data$team_tri_code & rink_biases$season == season, ]$league_average_dist / rink_biases[rink_biases$venue == .data$team_tri_code & rink_biases$season == season, ]$home_dist),
                                                       .data$shot_distance * (rink_biases[rink_biases$venue == .data$team_tri_code & rink_biases$season == season, ]$league_average_dist / rink_biases[rink_biases$venue == .data$team_tri_code & rink_biases$season == season, ]$away_dist),
                                                       .data$shot_distance))
  } else {
    #assume that season is current or backchecking
    if(!any(rink_biases$season) %in% c((season-3):(season-1))) {
      message("No historical data to do adjustment, adjusted_distance = shot_distance")
      data$adjusted_distance = data$shot_distance
    }
    data <- data %>%
      dplyr::rowwise() %>%
      dplyr::mutate(adjusted_distance = dplyr::if_else(.data$is_home == 1,
                                                       .data$shot_distance * (mean(rink_biases[rink_biases$venue == .data$team_tri_code & rink_biases$season %in% c((season-3):(season-1)), ]$league_average_dist) / mean(rink_biases[rink_biases$venue == .data$team_tri_code & rink_biases$season %in% c((season-3):(season-1)), ]$home_dist)),
                                                       .data$shot_distance * (mean(rink_biases[rink_biases$venue == .data$team_tri_code & rink_biases$season %in% c((season-3):(season-1)), ]$league_average_dist) / mean(rink_biases[rink_biases$venue == .data$team_tri_code & rink_biases$season %in% c((season-3):(season-1)), ]$away_dist)),
                                                       .data$shot_distance))
  }
  return(data)
}


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
build_model<-function(season, save_model=TRUE){
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

  ) %>%
    parsnip::set_engine("xgboost", scale_pos_weight = tune::tune()) %>%
    parsnip::set_mode("classification")

  xgb_grid <- dials::grid_latin_hypercube(
    dials::tree_depth(),
    dials::min_n(),
    dials::loss_reduction(),
    sample_size = dials::sample_prop(),
    dials::finalize(dials::mtry(), train_data),
    dials::learn_rate(),
    dials::scale_pos_weight(),
    size = 30
  )

  xgb_wf <- workflows::workflow() %>%
    workflows::add_recipe(model_recipe) %>%#, blueprint = hardhat::default_recipe_blueprint(allow_novel_levels = TRUE)) %>%
    #workflows::add_formula(is_goal ~ .) %>%
    workflows::add_model(xgb_spec)

  doParallel::registerDoParallel(cores = parallel::detectCores())

  message('tuning model')
  set.seed(1234)
  xgb_res <- tune::tune_grid(
    xgb_wf,
    resamples = v_folds,
    grid = xgb_grid,
    control = tune::control_grid(save_pred = TRUE),
    metrics = yardstick::metric_set(yardstick::accuracy, yardstick::roc_auc, yardstick::mn_log_loss, tes)
  )

  #tune::show_best(xgb_res, "roc_auc")

  best_model <- tune::select_best(xgb_res, "roc_auc")

  message('finalizing model')
  final_xgb <- xgb_wf %>%
    tune::finalize_workflow(best_model) %>%
    parsnip::fit(data = train_data)

  #final_xgb
  message('testing model')
  final_res <- tune::last_fit(final_xgb,
                                   train_test_split,
                                   metrics = yardstick::metric_set(yardstick::accuracy, yardstick::roc_auc, yardstick::mn_log_loss, tes))

  #xgb_wf_model <- final_xgb$.workflow[[1]]

  if(save_model){
    if(!dir.exists(file.path(getOption("BulsinkBxG.data.path"), 'models'))){
      dir.create(file.path(getOption("BulsinkBxG.data.path"), 'models'))
    }
    saveRDS(final_xgb, file = file.path(getOption("BulsinkBxG.data.path"), "models", paste0(season,"_model.RDS")))
  }

  return(list(model=final_xgb, metrics=tune::collect_metrics(final_res)))
}


build_save_past_models <- function(){
  purrr::walk(2012:2021, function(x) build_model(x))
}


test_model <- function(model, test_data, metrics = NULL){
  if (is.null(metrics)){
    metrics <- yardstick::metric_set(yardstick::accuracy, yardstick::roc_auc, yardstick::mn_log_loss, tes)
  }

  test_data$.pred <- stats::predict(model, new_data = test_data)$.pred_goal

  m <- yardstick::metrics(data = test_data, metrics = metrics)

  return(m)
}


#' Load Season Model
#'
#' @description This function will load the season model from a file. If it does not exist, it will attempt to download from GitHub. Thus, if you plan to make your own xG model based on this code, please use `build_model[BulsinkBxG::build_model]` instead.
#' @param season The season's xG model to load
#'
#' @return a xG model.
#' @export
load_season_model<-function(season){
  stopifnot(season >= 2011)

  if(!file.exists(file.path(getOption("BulsinkBxG.data.path"), 'models', paste0(season, "_model.RDS")))) {

    message("Downloading season model from GitHub")

    git_url<-paste0("https://raw.githubusercontent.com/pbulsink/BulsinkBxG/main/data-raw/", season, "_model.RDS")
    if(!dir.exists(file.path(getOption("BulsinkBxG.data.path"), 'models'))){
      dir.create(file.path(getOption("BulsinkBxG.data.path"), 'models'))
    }
    utils::download.file(git_url, file.path(getOption("BulsinkBxG.data.path"), 'models', paste0(season, "_model.RDS")), method="curl")
  }
  if(file.exists(file.path(getOption("BulsinkBxG.data.path"), 'models', paste0(season, "_model.RDS")))){
    model<-readRDS(file.path(getOption("BulsinkBxG.data.path"), "models", paste0(season,"_model.RDS")))
  } else {
    stop("File was not found and could not be loaded")
  }

  return(model)
}