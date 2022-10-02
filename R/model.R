prep_xg_model_data<-function(pbp){
  fenwick_events <- c('Shot', 'Missed Shot', 'Miss', 'Goal')
  corsi_events <- c('Shot', 'Missed Shot', 'Miss', 'Goal', 'Blocked Shot', 'Block')

  data <- pbp %>%
    dplyr::filter(.data$event %in% corsi_events | dplyr::lag(.data$event %in% corsi_events)) %>%  # Use Fenwicks because blocks are incorrectly located
    dplyr::mutate(is_home = dplyr::if_else(.data$team_tri_code == .data$home_team, 1, 0),  # binary home team event factor
           is_goal = dplyr::if_else(.data$event == "Goal", 1, 0))%>%  # binary is goal factor
    dplyr::mutate(shot_type = dplyr::if_else(.data$shot_type %in% c('Unk', 'Tip-In', 'Snap Shot', 'Wrist Shot', 'Slap Shot', 'Backhand', 'Deflected', 'Wrap-around'),
                                          .data$shot_type, 'Unk', 'Unk')) %>%
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
                  is_cluster = dplyr::if_else(.data$time_diff < 5 & .data$event %in% fenwick_events & dplyr::lag(.data$event) %in% corsi_events & .data$team_tri_code == dplyr::lag(.data$team_tri_code), 1, 0, missing = 0),
                   is_rush = dplyr::if_else(.data$time_diff < 5 & dplyr::lag(.data$x_coord) < 25, 1, 0, missing = 0),  # does this look like a shot on a rush?
                   speed = dplyr::if_else(!is.na(.data$time_diff), sqrt((.data$x_coord-dplyr::lag(.data$x_coord))^2 + (.data$y_coord - dplyr::lag(.data$y_coord))^2)/.data$time_diff, 0, missing = 0),  # speed from previous event to shot location in ft/s
                   shot_side = dplyr::if_else(.data$y_coord < 0, 'R', 'L', missing = "Unk"),  # from which side (goalie view) the fenwick comes from
                   cross_ice = dplyr::if_else(.data$shot_side == dplyr::lag(.data$shot_side), 1, 0, missing = 0), # if shot comes from opposite side of previous event
                   delta_angle = dplyr::if_else(.data$time_diff < 5, abs(atan((.data$y_coord) / (89 - abs(.data$x_coord))) * (180 / pi)) - abs(atan(dplyr::lag(.data$y_coord) / (89 - abs(dplyr::lag(.data$x_coord)))) * (180 / pi)), 0), missing = 0) %>%
    #We've done our manipulations using the lagged data, go to fenwick only data now
    dplyr::filter(.data$event %in% fenwick_events) %>% # & .data$about_period_type != "SHOOTOUT") %>%
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
    dplyr::mutate(result = factor(as.character(.data$is_goal), levels = c('1', '0'), labels = c('goal','no_goal'), ordered = TRUE))

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
  for(y in 2011:2021){
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
  season<-as.integer(season)
  if(season == 2011 && !backchecking){
    message("Must use 2011 data to adjust 2011 shot distances - no previous seasons available")
    backchecking <- FALSE
  }
  if(season %in% rink_biases$season && !backchecking){
    data <- data %>%
      dplyr::rowwise() %>%
      dplyr::mutate(adjusted_distance = dplyr::if_else(.data$is_home == 1,
                                                       .data$shot_distance * (rink_biases[rink_biases$venue == .data$team_tri_code & rink_biases$season == season, ]$league_average_dist / rink_biases[rink_biases$venue == .data$team_tri_code & rink_biases$season == season, ]$home_dist),
                                                       .data$shot_distance * (rink_biases[rink_biases$venue == .data$team_tri_code & rink_biases$season == season, ]$league_average_dist / rink_biases[rink_biases$venue == .data$team_tri_code & rink_biases$season == season, ]$away_dist),
                                                       .data$shot_distance)) %>%
      dplyr::mutate(adjusted_distance = dplyr::if_else(is.na(.data$adjusted_distance) | is.nan(.data$adjusted_distance), .data$shot_distance, .data$adjusted_distance))
  } else {
    #assume that season is current or backchecking
    if(!any(rink_biases$season %in% c((season-3):(season-1)))) {
      message("No historical data to do adjustment, adjusted_distance = shot_distance")
      data$adjusted_distance = data$shot_distance
    }
    data <- data %>%
      dplyr::rowwise() %>%
      dplyr::mutate(adjusted_distance = dplyr::if_else(.data$is_home == 1,
                                                       .data$shot_distance * (mean(rink_biases[rink_biases$venue == .data$team_tri_code & rink_biases$season %in% c((season-3):(season-1)), ]$league_average_dist) / mean(rink_biases[rink_biases$venue == .data$team_tri_code & rink_biases$season %in% c((season-3):(season-1)), ]$home_dist)),
                                                       .data$shot_distance * (mean(rink_biases[rink_biases$venue == .data$team_tri_code & rink_biases$season %in% c((season-3):(season-1)), ]$league_average_dist) / mean(rink_biases[rink_biases$venue == .data$team_tri_code & rink_biases$season %in% c((season-3):(season-1)), ]$away_dist)),
                                                       .data$shot_distance)) %>%
      dplyr::mutate(adjusted_distance = dplyr::if_else(is.na(.data$adjusted_distance) | is.nan(.data$adjusted_distance), .data$shot_distance, .data$adjusted_distance))
  }
  return(data)
}

build_model<-function(season, save_model = TRUE, weighted = TRUE, model='logistic'){
  if(model == 'logistic'){
    build_logistic_model(season=season, save_model = save_model, weighted=weighted)
  } else if (model == 'xgb') {
    build_xgb_model(season=season, save_model = save_model)
  } else {
    stop("Must select either 'xgb' or 'logistic' model type")
  }
}

build_save_past_models <- function(){
  purrr::walk(2011:2021, function(x) build_model(x))
}


#' Test a Model's performance
#'
#' @param model The model to test
#' @param test_data The data to test agains
#'
#' @return a named list with four metrics
#' @export
test_model <- function(model, test_data){
  test_data <- test_data %>%
    dplyr::filter(.data$event %in% c('Shot', 'Missed Shot', 'Miss', 'Goal'))

  if('.workflow' %in% names(model)){
    model<-model$.workflow[[1]]
  }
  test_data$.pred <- stats::predict(model, new_data = test_data, type = 'prob')$.pred_goal

  return(list('auc' = yardstick::roc_auc_vec(truth = test_data$result, estimate = test_data$.pred),
              'log_loss' = yardstick::mn_log_loss_vec(truth=test_data$result, estimate = test_data$.pred),
              'tes' = (sum(test_data$is_goal)-sum(test_data$.pred))^2,  # TODO: Use tes.vec
              'accuracy' = sum(test_data$.pred > 0.5)/sum(test_data$is_goal == 1)))
}


#' Load Season Model
#'
#' @description This function will load the season model from a file. If it does not exist, it will attempt to download from GitHub. Thus, if you plan to make your own xG model based on this code, please use `build_model[BulsinkBxG::build_model]` instead.
#' @param season The season's xG model to load
#' @param model What type of model to load - default is logistic, the default model type. also accepts xgb
#' @param weighted whether to load the regular or weighted model
#'
#' @return a xG model.
#' @export
load_season_model<-function(season, model = 'logistic', weighted=TRUE){
  stopifnot(season >= 2011)
  stopifnot(model %in% c('logistic', 'xgb'))


  if(!file.exists(file.path(getOption("BulsinkBxG.data.path"), 'models', paste0(season, ifelse(weighted, "_weighted", ""), "_", model, "_model.RDS")))) {

    message("Downloading season model from GitHub")

    git_url<-paste0("https://raw.githubusercontent.com/pbulsink/BulsinkBxG/main/data-raw/", season, ifelse(weighted, "_weighted", ""), "_", model, "_model.RDS")
    if(!dir.exists(file.path(getOption("BulsinkBxG.data.path"), 'models'))){
      dir.create(file.path(getOption("BulsinkBxG.data.path"), 'models'))
    }
    utils::download.file(git_url, file.path(getOption("BulsinkBxG.data.path"), 'models', paste0(season, ifelse(weighted, "_weighted", ""), "_model.RDS")), method="curl")
  }
  if(file.exists(file.path(getOption("BulsinkBxG.data.path"), 'models', paste0(season, ifelse(weighted, "_weighted", ""), "_", model, "_model.RDS")))){
    model<-readRDS(file.path(getOption("BulsinkBxG.data.path"), "models", paste0(season, ifelse(weighted, "_weighted", ""), "_", model, "_model.RDS")))
  } else {
    stop("File was not found and could not be loaded")
  }

  return(model)
}
