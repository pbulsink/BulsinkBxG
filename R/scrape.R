#' Scrape and Save
#'
#' @description Scrape games and save the raw files in a directory
#'
#' @param gameIds game ids to scrape
#' @param overwrite_downloads If true, will re-download games. Else only try for missing games
#'
#' @export
scrape_and_save<-function(gameIds, overwrite_downloads = FALSE){
  #Ensure Data directory is available, if not, make it
  check_or_create_dir(season=unique(substr(gameIds, 1,4)))

  #weird files - shifts2015020497
  gameIds<-gameIds[is_valid_gameId(gameIds)]
  pb <- progress::progress_bar$new(
    format = "Scraping game :gid [:bar] :percent eta: :eta",
    total = length(gameIds)+1
  )
  pb$tick(0)
  for(gameId in gameIds){
    pb$tick(tokens=list(gid = gameId))
    if(!is_valid_gameId(gameId)){
      #pb$tick(tokens=list(gid = gameId))
      next
    }
    season<-substr(gameId, 1,4)
    if(!overwrite_downloads){
      if(file.exists(file.path(getOption("BulsinkBxG.data.path"), season, paste0(gameId, "_feed.RDS"))) & file.exists(file.path(getOption("BulsinkBxG.data.path"), paste0(gameId, "_shifts.RDS")))){
        #pb$tick(tokens=list(gid = gameId))
        next
      }
    }

    feed <- NULL
    shifts <- NULL

    #use trys for errors.
    tryCatch(
      {feed <- scrape_game_feed(gameId)},
      error = function(e){
        tryCatch({Sys.sleep(10); feed<-scrape_game_feed(gameId)},
                 error = function(e) message("Error feed GameID: ", gameId, ", Error Msg: ",e))
      }
    )

    tryCatch(
      {shifts<-scrape_game_shifts(gameId)},
      error = function(e){
        tryCatch({Sys.sleep(10); shifts<-scrape_game_shifts(gameId)},
                 error = function(e) message("Error Shifts GameID: ", gameId, ", Error Msg: ",e))
      }
    )

    if(!is.null(feed)){
      saveRDS(feed, file=file.path(getOption("BulsinkBxG.data.path"), season, paste0(gameId, "_feed.RDS")))
    }

    if(!is.null(shifts)){
      saveRDS(shifts, file=file.path(getOption("BulsinkBxG.data.path"), season, paste0(gameId, "_shifts.RDS")))
    }
  }

  pb$terminate()
}


s_s_all_seasons<-function(){
  for(year in c(2011:2021)){
    scrape_season(year)
  }
}


## With componets from fastRhockey
#' @importFrom rlang .data
scrape_game_feed<-function(game_id){
  stopifnot(is_valid_gameId(game_id))

  base_url <- "https://statsapi.web.nhl.com/api/v1/game/"

  full_url <- paste0(base_url,
                     game_id,
                     "/feed/live")
  res <- httr::RETRY("GET", full_url)

  # Check the result
  check_status(res)
  resp <- res %>% httr::content(as = "text", encoding = "UTF-8")

  tryCatch(
    expr = {
      #---Live Data----
      live_data_df <- jsonlite::fromJSON(resp)[["liveData"]]
      plays_df <- live_data_df$plays$allPlays
      plays_player <- jsonlite::fromJSON(jsonlite::toJSON(plays_df$players),flatten=TRUE)
      plays_df <- jsonlite::fromJSON(jsonlite::toJSON(plays_df),flatten=TRUE)
      if(length(plays_player)>0){
        plays_player <- purrr::map_dfr(1:length(plays_player), function(x){
          if("playerType" %in% colnames(plays_player[[x]]) && nrow(plays_player[[x]])>2){
            player <- data.frame(
              "playerType_1" = plays_player[[x]]$playerType[[1]],
              "player.id_1" = plays_player[[x]]$player.id[[1]],
              "player.fullName_1" = plays_player[[x]]$player.fullName[[1]],
              "player.link_1" = plays_player[[x]]$player.link[[1]],
              "playerType_2" = plays_player[[x]]$playerType[[2]],
              "player.id_2" = plays_player[[x]]$player.id[[2]],
              "player.fullName_2" = plays_player[[x]]$player.fullName[[2]],
              "player.link_2" = plays_player[[x]]$player.link[[2]],
              "playerType_3" = plays_player[[x]]$playerType[[3]],
              "player.id_3" = plays_player[[x]]$player.id[[3]],
              "player.fullName_3" = plays_player[[x]]$player.fullName[[3]],
              "player.link_3" = plays_player[[x]]$player.link[[3]])
            return(player)

          } else if("playerType" %in% colnames(plays_player[[x]]) && nrow(plays_player[[x]])==2){
            player <- data.frame(
              "playerType_1" = plays_player[[x]]$playerType[[1]],
              "player.id_1" = plays_player[[x]]$player.id[[1]],
              "player.fullName_1" = plays_player[[x]]$player.fullName[[1]],
              "player.link_1" = plays_player[[x]]$player.link[[1]],
              "playerType_2" = plays_player[[x]]$playerType[[2]],
              "player.id_2" = plays_player[[x]]$player.id[[2]],
              "player.fullName_2" = plays_player[[x]]$player.fullName[[2]],
              "player.link_2" = plays_player[[x]]$player.link[[2]],
              "playerType_3" = NA_character_,
              "player.id_3" = NA_integer_,
              "player.fullName_3" = NA_character_,
              "player.link_3" = NA_character_)
            return(player)
          } else if ("playerType" %in% colnames(plays_player[[x]]) && nrow(plays_player[[x]])==1){
            player <- data.frame(
              "playerType_1" = plays_player[[x]]$playerType[[1]],
              "player.id_1" = plays_player[[x]]$player.id[[1]],
              "player.fullName_1" = plays_player[[x]]$player.fullName[[1]],
              "player.link_1" = plays_player[[x]]$player.link[[1]],
              "playerType_2" = NA_character_,
              "player.id_2" = NA_integer_,
              "player.fullName_2" = NA_character_,
              "player.link_2" = NA_character_,
              "playerType_3" = NA_character_,
              "player.id_3" = NA_integer_,
              "player.fullName_3" = NA_character_,
              "player.link_3" = NA_character_)
            return(player)
          } else {
            player <- data.frame(
              "playerType_1" = NA_character_,
              "player.id_1" = NA_integer_,
              "player.fullName_1" = NA_character_,
              "player.link_1" = NA_character_,
              "playerType_2" = NA_character_,
              "player.id_2" = NA_integer_,
              "player.fullName_2" = NA_character_,
              "player.link_2" = NA_character_,
              "playerType_3" = NA_character_,
              "player.id_3" = NA_integer_,
              "player.fullName_3" = NA_character_,
              "player.link_3" = NA_character_)
            return(player)
          }
        })
      }
      if(length(plays_df)>0 && length(plays_player)>0){
        all_plays <- plays_df %>%
          dplyr::select(-.data$players) %>%
          dplyr::bind_cols(plays_player) %>%
          janitor::clean_names() %>%
          tibble::as_tibble()
      }else{
        all_plays = tibble::tibble()
      }
    },
    error = function(e) {
      message(e)
      message(Sys.time(), ": Invalid arguments or no game feed data for ", game_id, " available!")
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(all_plays)
}


## With components from fastRhockey
#' @importFrom rlang .data
scrape_game_shifts<-function(game_id){
  stopifnot(is_valid_gameId(game_id))
  base_url <- "https://api.nhle.com/stats/rest/en/shiftcharts?cayenneExp=gameId="
  full_url <- paste0(base_url, game_id)

  res <- httr::RETRY("GET", full_url)

  # Check the result
  check_status(res)

  resp <- res %>%
    httr::content(as = "text", encoding = "UTF-8")


  tryCatch(
    expr = {
      site <- jsonlite::fromJSON(resp)

      shifts_raw <- site$data %>%
        dplyr::tibble() %>%
        janitor::clean_names() %>%
        tidyr::unite("player_name", c(.data$first_name, .data$last_name), sep = " ") %>%
        dplyr::select(
          .data$game_id, .data$player_id, .data$player_name,
          .data$team_abbrev, .data$team_id, .data$team_name,
          .data$period, .data$start_time, .data$end_time, .data$duration) %>%
        dplyr::filter(!is.na(.data$duration)) %>%
        dplyr::mutate(
          start_time_ms = lubridate::ms(.data$start_time),
          start_seconds = lubridate::period_to_seconds(.data$start_time_ms),
          start_game_seconds = .data$start_seconds + (1200 * (.data$period-1)),
          end_time_ms = lubridate::ms(.data$end_time),
          end_seconds = lubridate::period_to_seconds(.data$end_time_ms),
          end_game_seconds = .data$end_seconds + (1200 * (.data$period-1)),
          duration = lubridate::ms(.data$duration),
          duration_seconds = lubridate::period_to_seconds(.data$duration))

      shifts_on <- shifts_raw %>%
        dplyr::group_by(
          .data$team_name, .data$period, .data$start_time, .data$team_abbrev,
          .data$start_seconds, .data$start_game_seconds) %>%
        dplyr::summarize(
          num_on = dplyr::n(),
          players_on_name = paste(.data$player_name, collapse = ", "),
          players_on_id = paste(.data$player_id, collapse = ", "),
          .groups = "drop") %>%
        tidyr::separate(.data$players_on_name,
                        c("player_on_name_1","player_on_name_2","player_on_name_3","player_on_name_4","player_on_name_5","player_on_name_6","player_on_name_7"),
                        sep = ", ",
                        remove = TRUE,
                        extra = "warn",
                        fill = "right") %>%
        tidyr::separate(.data$players_on_id,
                        c("player_on_id_1","player_on_id_2","player_on_id_3","player_on_id_4","player_on_id_5","player_on_id_6","player_on_id_7"),
                        sep = ", ",
                        remove = TRUE,
                        extra = "warn",
                        fill = "right") %>%
        dplyr::rename(
          period_time = .data$start_time,
          period_seconds = .data$start_seconds,
          game_seconds = .data$start_game_seconds)

      shifts_off <- shifts_raw %>%
        dplyr::group_by(
          .data$team_name, .data$period, .data$team_abbrev,
          .data$end_time, .data$end_seconds, .data$end_game_seconds) %>%
        dplyr::summarize(
          num_off = dplyr::n(),
          players_off_name = paste(.data$player_name, collapse = ", "),
          players_off_id = paste(.data$player_id, collapse = ", "),
          .groups = "drop") %>%
        tidyr::separate(.data$players_off_name,
                        c("player_off_name_1","player_off_name_2","player_off_name_3","player_off_name_4","player_off_name_5","player_off_name_6","player_off_name_7"),
                        sep = ", ",
                        remove = TRUE,
                        extra = "warn",
                        fill = "right") %>%
        tidyr::separate(.data$players_off_id,
                        c("player_off_id_1","player_off_id_2","player_off_id_3","player_off_id_4","player_off_id_5","player_off_id_6","player_off_id_7"),
                        sep = ", ",
                        remove = TRUE,
                        extra = "warn",
                        fill = "right") %>%
        dplyr::rename(
          period_time = .data$end_time,
          period_seconds = .data$end_seconds,
          game_seconds = .data$end_game_seconds)

      shifts <- dplyr::full_join(
        shifts_on, shifts_off,
        by = c("game_seconds", "team_name", "period", "period_time", "period_seconds", "team_abbrev")) %>%
        dplyr::arrange(.data$game_seconds) %>%
        dplyr::mutate(
          game_seconds_remaining = 3600 - .data$game_seconds) %>%
        dplyr::rename(event_team = .data$team_name) %>%
        janitor::clean_names() %>%
        tibble::as_tibble()
    },
    error = function(e) {
      message(Sys.time(), ": Invalid arguments or no game shift data for ", game_id, " available!")
  },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(shifts)
}


#' Scrape season
#'
#' @description Shortcut function to scrape a whole season's games
#'
#' @param season season 2011++ as numeric. Season as four digit year (start of season). E.g. 2018 for 20182019 season.
#' @param skipgames optional vector of games to skip
#' @param ... extra parameters to pass to scrape_and_save
#'
#' @export
scrape_season<-function(season, skipgames=NULL, ...){

  #handle ellipsis
  args<-list(...)
  if("overwrite_downloads" %in% names(args)){
    overwrite_downloads <- args$overwrite_downloads
  } else {
    overwrite_downloads <- FALSE
  }
  gameIds<-get_game_ids(season=season)

  scrape_and_save(gameIds, overwrite_downloads = overwrite_downloads)
}


#' Build Player DB
#'
#' @description Given a pbp, build or add to playerdb all the players in that pbp
#'
#' @param pbp a pbp file, which will be the source for all of the playerID to add to db
#'
#' @return invisible player db
#' @export
build_player_db<-function(pbp){
  unique_players<-unique(unlist(pbp[,c("home_onice_1", "home_onice_2", "home_onice_3", "home_onice_4", "home_onice_5", "home_onice_6",
                                       "away_onice_1", "away_onice_2", "away_onice_3", "away_onice_4", "away_onice_5", "away_onice_6",
                                       "home_goalie", "away_goalie")]))

  if(file.exists(file.path(getOption("BulsinkBxG.data.path"), "players.RDS"))){
    playerdb<-readRDS(file.path(getOption("BulsinkBxG.data.path"), "players.RDS"))
    unique_players<-unique_players[!(unique_players %in% playerdb$id)]
  } else {
    check_or_create_dir()
    playerdb<-NULL
  }

  unique_players<-unique_players[!is.na(unique_players)]
  if(length(unique_players) == 0){
    invisible(playerdb)
  }

  players<-scrape_players(unique_players)
  playerdb<-dplyr::bind_rows(playerdb, players)

  saveRDS(playerdb, file.path(getOption("BulsinkBxG.data.path"), "players.RDS"))
  invisible(playerdb)
}

scrape_players<-function(playerId){
  if(length(playerId) == 0){
    return(NULL)
  }
  unique_players<-unique(playerId)
  unique_players<-unique_players[!is.na(unique_players)]
  players<-nhlapi::nhl_players(playerIds = unique_players)
  players<-players %>%
    dplyr::select(dplyr::any_of(c("id", "fullName", "firstName", "lastName", "primaryNumber", "birthDate", "birthCity",
                      "birthStateProvince", "birthCountry", "nationality", "height", "weight", "alternateCaptain",
                      "captain", "rookie", "shootsCatches", "primaryPosition.code", "primaryPosition.name",
                      "primaryPosition.type", "primaryPosition.abbreviation")))
  players<-janitor::clean_names(players)
  if(nrow(players) == 0){
    return(NULL)
  } else {
    return(players)
  }
}
