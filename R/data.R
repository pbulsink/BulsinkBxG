
#' Load season PBP from File
#'
#' @param season Season file to load
#'
#' @return pbp of whole season
#' @export
load_season_pbp<-function(season){
  if(!file.exists(file.path(getOption("BulsinkBxG.data.path"), paste0(season, "_pbp.rds")))){
    return(compile_season(season = season))
  } else {
    return(readRDS(file.path(getOption("BulsinkBxG.data.path"), paste0(season, "_pbp.rds"))))
  }
}

#' Compile PBP to single season file
#'
#' @param season Season to compile to file
#' @param progress Whether to show a progress bar. Default true, but requires the `progress` package
#'
#' @return pbp of whole season
#' @export
compile_season<-function(season, progress = TRUE){
  #get gameIDs to load
  gameIds<-get_game_ids(season=season)
  gameIds<-gameIds[is_valid_gameId(gameIds)]

  pbp<-data.frame()
  games_to_retry_scraping<-c()

  if(!requireNamespace("progress")){
    progress <- FALSE
  }

  if(progress){
    pb <- progress::progress_bar$new(
      format = "Compiling game :gid [:bar] :percent eta: :eta",
      total = length(gameIds)
    )
  }
  pbplist<-list()

  for(i in seq_along(gameIds)){
    gameId<-gameIds[[i]]
    if(progress){
      pb$tick(tokens=list(gid = gameId))
    }
    if(file.exists(file.path(getOption("BulsinkBxG.data.path"), season, paste0(gameId, "_pbp.RDS")))){
      pbp_g<-readRDS(file.path(getOption("BulsinkBxG.data.path"), season, paste0(gameId, "_pbp.RDS")))
      pbplist[[i]]<-pbp_g  # subset(pbp_g, select = -players)
    } else {
      games_to_retry_scraping<-c(games_to_retry_scraping, gameId)
    }
  }

  tryCatch({pbp<-dplyr::bind_rows(pbplist)},
           error = function(e){
             message("error in combining files. Retrying")
             if(progress){
               pb <- progress::progress_bar$new(
                 format = "Compiling game :gid [:bar] :percent eta: :eta",
                 total = length(gameIds)
               )
             }

             for(i in seq_along(gameIds)){
               gameId<-gameIds[[i]]
               if(progress){
                 pb$tick(tokens=list(gid = gameId))
               }
               if(file.exists(file.path(getOption("BulsinkBxG.data.path"), season, paste0(gameId, "_pbp.RDS")))){
                 pbp_g<-pbplist[[i]]
                 pbp<-dplyr::bind_rows(pbp, pbp_g)  # subset(pbp_g, select = -players)
               } else {
                 games_to_retry_scraping<-c(games_to_retry_scraping, gameId)
               }
             }
           })



  if(length(games_to_retry_scraping) > 0){
    if(progress){
      pb <- progress::progress_bar$new(
        format = "Re-scraping game :gid [:bar] :percent eta: :eta",
        total = length(games_to_retry_scraping)
      )
    }

    for(gameId in games_to_retry_scraping){
      if(progress){
        pb$tick(tokens=list(gid = gameId))
      }
      pbp_g<-data.frame()
      pbp_g<-process_game_pbp(gameId)
      if(nrow(pbp_g) > 0 | is.na(nrow(pbp_g))){
        pbp<-dplyr::bind_rows(pbp, pbp_g)
      }
    }
  }

  if(progress){
    pb$terminate()
  }
  saveRDS(pbp, file.path(getOption("BulsinkBxG.data.path"), paste0(season, "_pbp.rds")))

  invisible(pbp)
}


#' Get all active players and goalies at any point in time
#'
#' @param time Time to query, in seconds
#' @param shifts Shifts data frame
#'
#' @return a data.frame of players on the ice at the provided time
get_players_at_time<-function(time, shifts){
  # active_players<-shifts[shifts$startTime <= time & shifts$endTime >= time, ]
  # active_players$shiftAgeAtTime<-time-active_players$startTime
  # active_players<-unique(active_players)
  # return(active_players)

  on<-shifts[shifts$game_seconds <= time, c("player_on_id_1", "player_on_id_2", "player_on_id_3", "player_on_id_4", "player_on_id_5", "player_on_id_6", "player_on_id_7")]
  on<-unlist(on, use.names = FALSE)
  off<-shifts[shifts$game_seconds <= time, c("player_off_id_1", "player_off_id_2", "player_off_id_3", "player_off_id_4", "player_off_id_5", "player_off_id_6", "player_off_id_7")]
  off<-unlist(off, use.names=FALSE)

  players<-unique(c(on, off))
  players<-players[sapply(players, function(x) (sum(on==x)- sum(off=x))>0)]
  return(players)
}


#' Get the goalies for the provided team from the feed
#'
#' @param feed pbp feed data frame
#' @param team Team to get goalies for
#'
#' @return a data frame containting goalie names and player.id
get_goalies <- function(feed, team){
  shots_against<-feed[feed$result_event=="Shot" & feed$team_tri_code != team, ]
  goalies<-unique(shots_against[shots_against$player_type_2 == 'Goalie', ]$player_id_2)
  return(goalies)
}


#' Process a Season's PBPs
#'
#' @param season the season's pbp files to process
#' @param progress Whether to show a progress bar. Default true, but requires the `progress` package
#'
#' @export
process_season_pbp<-function(season, progress=TRUE){
  gameIds<-get_game_ids(season=season)
  gameIds<-gameIds[is_valid_gameId(gameIds)]
  if(!requireNamespace("progress")){
    progress <- FALSE
  }
  if(progress){
    pb <- progress::progress_bar$new(
      format = "Processing game :gid [:bar] :percent eta: :eta",
      total = length(gameIds)
    )
  }

  for(gameId in gameIds){
    if(progress){
      pb$tick(tokens=list(gid = gameId))
    }
    tryCatch(process_game_pbp(gameId = gameId),
             error = function(e) message("GameID: ",gameId, ", Error: ", e)
    )
  }
  if(progress){
    pb$terminate()
  }
}


#' Process PBP
#'
#' @description Given a GameId, try to load and process the feed and shifts file to one pbp. Saves a file back in the default data directory named `[gameId]_pbp.RDS`
#'
#' @param gameId the ID of the game you wish to process
#'
#' @return a pbp data frame or invisible.
#' @export
process_game_pbp<-function(gameId){
  stopifnot(is_valid_gameId(gameId))

  season<-substr(gameId, 1,4)
  #Ensure file exists before we try load it. If not, try download. Else stop
  if(!file.exists(file.path(getOption("BulsinkBxG.data.path"), season, paste0(gameId, "_feed.RDS")))){
    scrape_and_save(gameId, overwrite_downloads = TRUE)
  }
  stopifnot(file.exists(file.path(getOption("BulsinkBxG.data.path"), season, paste0(gameId, "_feed.RDS"))))

  feed<-readRDS(file.path(getOption("BulsinkBxG.data.path"), season, paste0(gameId, "_feed.RDS")))
  shifts<-tryCatch(unique(readRDS(file.path(getOption("BulsinkBxG.data.path"), season, paste0(gameId, "_shifts.RDS")))),
                   error = function(e){message("Couldn't load shifts data for gameId ", gameId, "... Continuing"); return(data.frame())})

  homeTeam<-substr(feed$result_event_code[1], 1, 3)
  awayTeam<-unique(feed$team_tri_code)[which(unique(feed$team_tri_code) != homeTeam)]

  feed$home_team <- homeTeam
  feed$away_team <- awayTeam

  if(!all(shifts$team_abbrev %in% c(homeTeam, awayTeam))){
    #something is wrong with shifts data - NULL out to avoid issues.
    shifts<-data.frame()
  }

  feed$time<-((as.integer(substr(feed$about_period_time, 1,2))+(20*(feed$about_period-1)))*60) + as.integer(substr(feed$about_period_time, 4,5))
  homeGoalies<-get_goalies(feed, homeTeam)
  awayGoalies<-get_goalies(feed, awayTeam)

  feed<-feed[!is.na(feed$coordinates_x),]
  feed$game_id <- gameId

  if(nrow(feed) == 0){
    message(paste0('Feed for game ', gameId, ' not available. Returning NULL.'))
    return(NULL)
  }
  stopifnot(nrow(feed) > 0)

  if(nrow(shifts)>0){
    homePlayers<-get_feed_players(feed, shifts, homeTeam, homeGoalies)
    awayPlayers<-get_feed_players(feed, shifts, awayTeam, awayGoalies)

    colnames(homePlayers) <- c("home_onice_1", "home_onice_2", "home_onice_3", "home_onice_4", "home_onice_5", "home_onice_6", "home_goalie")

    colnames(awayPlayers) <- c("away_onice_1", "away_onice_2", "away_onice_3", "away_onice_4", "away_onice_5", "away_onice_6", "away_goalie")

    feed<-dplyr::bind_cols(feed, homePlayers, awayPlayers)

  } else {  # Shifts doesn't exist.
    feed$home_onice_1 <- feed$home_onice_2 <- feed$home_onice_3 <- feed$home_onice_4 <- feed$home_onice_5 <- feed$home_onice_6 <- feed$home_goalie <- NA_integer_
    feed$away_onice_1 <- feed$away_onice_2 <- feed$away_onice_3 <- feed$away_onice_4 <- feed$away_onice_5 <- feed$away_onice_6 <- feed$away_goalie <- NA_integer_

    feed[feed$team_tri_code == awayTeam, ]$home_goalie <- purrr::map_int(feed[feed$team_tri_code == awayTeam, ]$about_event_idx, function(x) ifelse(feed[feed$about_event_idx == x, ]$player_type_1 == 'Goalie', feed[feed$about_event_idx == x, ]$player_id_1, ifelse(feed[feed$about_event_idx == x, ]$player_type_2 == 'Goalie', feed[feed$about_event_idx == x, ]$player_id_2, ifelse(feed[feed$about_event_idx == x, ]$player_type_3 == 'Goalie', feed[feed$about_event_idx == x, ]$player_id_3, NA))))
    feed[feed$team_tri_code == homeTeam, ]$away_goalie <- purrr::map_int(feed[feed$team_tri_code == homeTeam, ]$about_event_idx, function(x) ifelse(feed[feed$about_event_idx == x, ]$player_type_1 == 'Goalie', feed[feed$about_event_idx == x, ]$player_id_1, ifelse(feed[feed$about_event_idx == x, ]$player_type_2 == 'Goalie', feed[feed$about_event_idx == x, ]$player_id_2, ifelse(feed[feed$about_event_idx == x, ]$player_type_3 == 'Goalie', feed[feed$about_event_idx == x, ]$player_id_3, NA))))
  }

  feed<-process_penalties(feed)

  feed$result_event_code <- feed$result_event_type_id <- feed$result_game_winning_goal <- feed$about_event_id <- NULL
  feed$about_period_time <- feed$about_ordinal_num <- feed$about_period_time <- feed$about_period_time_remaining <-  NULL
  feed$about_date_time <- feed$team_id <- feed$team_name <- feed$team_link <- NULL

  feed <- dplyr::rename(feed, "event" = "result_event", "shot_type" = "result_secondary_type", "home_goals" = "about_goals_home", "away_goals" = "about_goals_away", "x_coord" = "coordinates_x", "y_coord" = "coordinates_y")

  if(nrow(shifts)>0){
    #Game State (5v5, 4v5, Ev5, etc)
    feed$home_strength<-ifelse(
      is.na(feed$home_goalie), "E",
        ifelse(is.na(feed$home_onice_4), "3",
          ifelse(is.na(feed$home_onice_5), "4",
           ifelse(is.na(feed$home_onice_6), "5", "6"))))
    feed$away_strength<-ifelse(
      is.na(feed$away_goalie), "E",
            ifelse(is.na(feed$away_onice_4), "3",
                   ifelse(is.na(feed$away_onice_5), "4",
                          ifelse(is.na(feed$away_onice_6), "5", "6"))))
  } else {
    feed$home_strength<-as.character(5-feed$home_in_box)
    feed$away_strength<-as.character(5-feed$away_in_box)
    if(any(as.logical(feed$result_empty_net), na.rm = TRUE)){
      feidx<-feed[feed$result_empty_net, ]$about_event_idx
      feidx<-feidx[!is.na(feidx)]
      for(f in feidx){
        if(feed[feed$about_event_idx == f, ]$team_tri_code == homeTeam){
          feed[feed$about_event_idx == f, ]$away_strength = "E"
        } else {
          feed[feed$about_event_idx == f, ]$home_strength == "E"
        }
      }
    }

  }
  feed$strength <- paste(feed$home_strength, feed$away_strength, sep = "v")
  feed$home_strength<-feed$away_strength<-NULL

  feed$event_id <- as.numeric(paste0(feed$game_id, sprintf("%04d", feed$about_event_idx)))

  saveRDS(feed, file=file.path(getOption("BulsinkBxG.data.path"), season, paste0(gameId, "_pbp.RDS")))

  invisible(feed)
}


get_feed_players<-function(feed, shifts, team, goalies){
  g_p<-function(time, eventType, shifts, team, goalies){
    shifts<-shifts[shifts$team_abbrev == team, ]
    if(eventType %in% c("FACEOFF","Faceoff")){
      # faceoff events happen at a break. This can be the only time that a player is starting their shift at the event or endeing their shift at the event time (i.e. the event preceeding the faceoff)
      on<-shifts[shifts$game_seconds <= time, c("player_on_id_1", "player_on_id_2", "player_on_id_3", "player_on_id_4", "player_on_id_5", "player_on_id_6", "player_on_id_7")]
      off<-shifts[shifts$game_seconds <= time, c("player_off_id_1", "player_off_id_2", "player_off_id_3", "player_off_id_4", "player_off_id_5", "player_off_id_6", "player_off_id_7")]
    } else {
      #difference is that we don't accept people who just started the shift or ended their shift 'that second' on a non-faceoff event.
      on<-shifts[shifts$game_seconds < time, c("player_on_id_1", "player_on_id_2", "player_on_id_3", "player_on_id_4", "player_on_id_5", "player_on_id_6", "player_on_id_7")]
      off<-shifts[shifts$game_seconds < time, c("player_off_id_1", "player_off_id_2", "player_off_id_3", "player_off_id_4", "player_off_id_5", "player_off_id_6", "player_off_id_7")]
    }
    on<-unlist(on, use.names = FALSE)
    off<-unlist(off, use.names=FALSE)
    players<-unique(c(on, off))
    players<-players[!is.na(players)]
    if(length(players) > 6){
      players<-players[sapply(players, function(x) (sum(on==x, na.rm = TRUE) - sum(off==x, na.rm = TRUE))>0)]
    }
    goalie <- players[players %in% goalies]
    if(length(goalie) < 1){
      goalie<-NA
    }
    players<-players[!(players %in% goalies)]
    if(length(players) > 6){
      message("Game ", feed$game_id[1], " has more than 6 players on")
      players<-players[1:6]
    }
    players<-c(players, rep(NA, 6-length(players)))

    players<-as.numeric(players)
    goalie<-as.numeric(goalie)

    pl_df<-data.frame("p1"=players[1], "p2"=players[2], "p3"=players[3], "p4"=players[4], "p5"=players[5], "p6"=players[6], "g"=goalie)
    return(pl_df[1,])
  }

  # g_p_v<-Vectorize(g_p, vectorize.args = c("time", "eventType"))

  players<-list()
  for(i in 1:nrow(feed)){
    players[[i]]<-g_p(feed$time[i], feed$result_event[i], shifts, team, goalies)
  }

  players<-dplyr::bind_rows(players)
  #players<-g_p_v(feed$time, feed$result.event, shifts, team, goalies)
  #players<-t(players)
  #players<-as.data.frame(players)

  return(players)
}


#' Get Player Stat
#'
#' @param playerId player ID (single or vector)
#' @param stat stat to query - e.g. shoots_catches, primary_position_abbreviation
#' @param player_db passed, or loaded from the file.
#'
#' @return a vector of `stat`
#' @export
get_player_stat<-function(playerId=numeric(), stat=NULL, player_db=NULL){
  #Useful stats are:
  # - primary_position_abbreviation
  # - shoots_catches

  if(is.null(player_db)){
    if(file.exists(file.path(getOption("BulsinkBxG.data.path"), "players.RDS"))){
      player_db<-readRDS(file.path(getOption("BulsinkBxG.data.path"), "players.RDS"))
    } else {
      player_db<-scrape_players(playerId)
      if(!is.null(player_db)){
        saveRDS(player_db, file.path(getOption("BulsinkBxG.data.path"), "players.RDS"))
      }
    }
  }

  if(length(playerId) == 0){
    return(player_db)
  }

  if(any(!(playerId %in% player_db$id))){
    player<-scrape_players(playerId)
    if(!is.null(player)){
      player_db<-dplyr::bind_rows(player_db, player)
      saveRDS(player_db, file.path(getOption("BulsinkBxG.data.path"), "players.RDS"))
    }
    if(all(!(playerId %in% player_db$id))){
      return(NULL)
    }
  }
  if(!is.null(stat) & all(stat %in% colnames(player_db))){
    if(length(stat)==1){
      return(purrr::map_chr(playerId, function(p) iferror(unique(player_db[player_db$id == p, stat])[1], 'Unk')))
    } else {
      return(purrr::map_chr(playerId, function(p) iferror(paste(unique(player_db[player_db$id == p, stat]), collapse=","), 'Unk')))
    }

  } else{
    invisible(player_db)
  }

}

process_penalties<-function(feed){
  #Penalties are feed$result.eventTypeId == "PENALTY"
  #Notes:
  #  result.penaltyMinutes %in% c(2, 5, 10)
  #  result.secondaryType are penalties
  #  result.penaltySeverity %in% c("Minor", "Bench Minor", "Major", "Misconduct)
  # offsetting minors can go from 5v5 to 4v4. But if 4v4, not go to 3v3.
  # Fighting majors do not go to 4v4, fighting instigators go to 5v4 etc.
  # So, some penalties are non-impactful and will not be tracked.
  penalty_frame<-data.frame(
    "start_time"=feed[feed$result_event_type_id == "PENALTY",]$time,
    "length"=feed[feed$result_event_type_id == "PENALTY",]$result_penalty_minutes * 60,
    "type"=feed[feed$result_event_type_id == "PENALTY",]$result_secondary_type,
    "team"=feed[feed$result_event_type_id == "PENALTY",]$team_tri_code,
    "severity"=feed[feed$result_event_type_id == "PENALTY",]$result_penalty_severity
  )
  feed$home_in_box<-feed$away_in_box<-0
  feed$home_time_to_full_strength<-feed$away_time_to_full_strength<-0
  feed$home_time_on_pk<-feed$away_time_on_pk<-0
  if(nrow(penalty_frame) == 0){
    #no penalties
    return(feed)
  }
  homeTeam<-feed$home_team[[1]]

  penalty_frame$offsetting <- FALSE
  penalty_frame<-dplyr::add_count(penalty_frame, .data$start_time, name="penalties_at_time")
  if(any(penalty_frame$penalties_at_time > 1)){
    penalty_frame[penalty_frame$penalties_at_time > 1,]$offsetting <- TRUE
    if(any(penalty_frame$type %in% c("Fighting", "Misconduct"))){
      penalty_frame[penalty_frame$type %in% c("Fighting", "Misconduct"),]$offsetting <- FALSE
    }
  }

  if(all(penalty_frame$offsetting)){
    #effectively no penalties - at least, no powerplays
    return(feed)
  }
  penalty_frame<-dplyr::add_count(penalty_frame, .data$start_time, .data$offsetting)
  penalty_frame[!penalty_frame$offsetting,]$n <- 1
  if(nrow(penalty_frame[penalty_frame$offsetting & penalty_frame$n==1, ]) > 0){
    penalty_frame[penalty_frame$offsetting & penalty_frame$n==1, ]$offsetting <- FALSE
  }

  for(penalty in 1:nrow(penalty_frame)){
    if(penalty_frame[penalty, 'offsetting']){
      next
    }

    for(f in feed[(feed$time >= penalty_frame[penalty,]$start_time & feed$time <= (penalty_frame[penalty,]$start_time+penalty_frame[penalty,]$length)), ]$about_event_id){
      if(feed[feed$about_event_id == f, ]$result_event_type_id == "PENALTY"){
        next
      }
      if(penalty_frame[penalty, 'team'] == homeTeam){
        if(feed[feed$about_event_id == f,]$result_event == 'Goal' & feed[feed$about_event_id == f,]$team_tri_code != homeTeam){
          #Away Team scored, end penalty
          break
        }
        feed[feed$about_event_id == f, ]$home_in_box <- feed[feed$about_event_id == f, ]$home_in_box + 1
        feed[feed$about_event_id == f, ]$home_time_to_full_strength <- max(feed[feed$about_event_id == f,]$home_time_to_full_strength, ((penalty_frame[penalty,]$start_time + penalty_frame[penalty,]$length)-feed[feed$about_event_id == f,]$time))
        feed[feed$about_event_id == f, ]$home_time_on_pk <- max(feed[feed$about_event_id==f,]$home_time_on_pk, (feed[feed$about_event_id == f,]$time - penalty_frame[penalty,]$start_time))
      } else {
        if(feed[feed$about_event_id == f,]$result_event == 'Goal' & feed[feed$about_event_id == f,]$team_tri_code == homeTeam){
          #Home Team scored, end penalty
          break
        }
        feed[feed$about_event_id == f, ]$away_in_box <- feed[feed$about_event_id == f,]$away_in_box + 1
        feed[feed$about_event_id == f, ]$away_time_to_full_strength <- max(feed[feed$about_event_id == f,]$away_time_to_full_strength, ((penalty_frame[penalty,]$start_time + penalty_frame[penalty,]$length)-feed[feed$about_event_id == f,]$time))
        feed[feed$about_event_id == f, ]$away_time_on_pk <- max(feed[feed$about_event_id == f,]$away_time_on_pk, (feed[feed$about_event_id == f,]$time - penalty_frame[penalty,]$start_time))
      }
    }
  }
  return(feed)
}


determine_rink_bias<-function(data){
  #subfunction for purr (data, team)
  venue_adj <- function(venue, data){
    data<-data %>%
      dplyr::filter(.data$penalty_shot == 0, .data$shootout==0)
    n_teams<-length(unique(c(data$home_team[!is.na(data$home_team)], data$away_team[!is.na(data$away_team)])))
    homedata<-dplyr::bind_rows(
      data %>% dplyr::filter(.data$home_team == venue, .data$team_tri_code != venue),
      data %>% dplyr::filter(.data$home_Team == venue, .data$team_tri_code == venue) %>% dplyr::slice_sample(prop = 1/n_teams)
    )

    home_dist<-mean(homedata$shot_distance)
    home_angle<-mean(homedata$shot_angle)
    rm(homedata)
    league_dist<-mean(data %>% dplyr::filter(.data$home_team != venue) %>% dplyr::pull(.data$shot_distance))
    league_angle<-mean(data %>% dplyr::filter(.data$home_team != venue) %>% dplyr::pull(.data$shot_angle))
    return(list('venue'=venue, 'angle_bias' = home_angle/league_angle, 'distance_bias' = home_dist/league_dist))
  }

  venue_adjustments<-purrr::map_dfr(unique(c(data$home_team[!is.na(data$home_team)], data$away_team[!is.na(data$away_team)])),
                                    function(x) venue_adj(x, data))

  return(venue_adjustments)
}

