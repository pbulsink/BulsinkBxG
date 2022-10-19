#' Get Game xG
#'
#' @param gameId GameID to get the xG values for
#' @param model Season model to use to predict xG, optional. Will load correct model for season if not provided.
#'
#' @return a named list with home_xg and away_xg
#' @export
#'
#' @examples
#' \dontrun{
#' get_game_xg(2011020001)
#' }
get_game_xg<-function(gameId, model=NULL){
  stopifnot(is_valid_gameId(gameId))

  season<-substr(gameId, 1, 4)

  stopifnot(as.numeric(season)>=2011)

  if(file.exists(file.path(getOption("BulsinkBxG.data.path"), "xG.csv"))){
    xg_files<-utils::read.csv(file.path(getOption("BulsinkBxG.data.path"), "xG.csv"))
    if(gameId %in% xg_files$GameId){
      return(list("home_xg" = xg_files[xg_files$GameId == gameId,]$home_xg, "away_xg" = xg_files[xg_files$GameId == gameId,]$away_xg))
    }
  } else {
    xg_files<-data.frame("GameId" = character(), "home_xg" = numeric(), "away_xg" = numeric())
    write.table(xg_files, file = file.path(getOption("BulsinkBxG.data.path"), "xG.csv"), col.names = TRUE, row.names = FALSE, sep = ",", append=FALSE)
  }

  pbp<-model_game_xg(gameId = gameId, model = model)

  xg_files<-data.frame("GameId" = gameId, "home_xg" = sum(pbp[pbp$is_home == 1 ,]$xG, na.rm = TRUE), "away_xg" = sum(pbp[pbp$is_home == 0 ,]$xG, na.rm = TRUE))
  utils::write.table(xg_files, file = file.path(getOption("BulsinkBxG.data.path"), "xG.csv"), append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",")
  invisible(list("home_xg" = sum(pbp[pbp$is_home == 1 ,]$xG), "away_xg" = sum(pbp[pbp$is_home == 0 ,]$xG)))
}

#' Game Report
#'
#' @description Get all xG for team and players for a gameId.
#'
#' @param gameId Game ID to build the report
#' @param model optional model for that season's games
#'
#' @return a list with home and away teams, their xg, and a data frame of all players and their xg for and against.
#' @export
build_game_report<-function(gameId, model = NULL){
  stopifnot(is_valid_gameId(gameId))

  season<-substr(gameId, 1, 4)

  stopifnot(as.numeric(season)>=2011)

  pbp<-model_game_xg(gameId = gameId, model = model)

  home_team<-utils::head(pbp$home_team, 1)
  away_team<-utils::head(pbp$away_team, 1)

  home_xg<-sum(pbp[pbp$is_home == 1, ]$xG)
  away_xg<-sum(pbp[pbp$is_home == 0, ]$xG)

  home_goals<-nrow(pbp[pbp$is_home == 1 & pbp$event == "Goal", ])
  away_goals<-nrow(pbp[pbp$is_home == 0 & pbp$event == "Goal", ])

  home_players<-unique(c(pbp$home_onice_1, pbp$home_onice_2, pbp$home_onice_3, pbp$home_onice_4, pbp$home_onice_5, pbp$home_onice_6, pbp$home_goalie))
  away_players<-unique(c(pbp$away_onice_1, pbp$away_onice_2, pbp$away_onice_3, pbp$away_onice_4, pbp$away_onice_5, pbp$away_onice_6, pbp$away_goalie))

  home_players<-home_players[!is.na(home_players)]
  away_players<-away_players[!is.na(away_players)]

  playerframe<-data.frame("PlayerId" = c(home_players, away_players),
                          "Team" = c(rep(home_team, length(home_players)), rep(away_team, length(away_players))),
                          "xG_for" = 0, "xG_against" = 0)

  for(p in playerframe$PlayerId){
    xgon<-pbp %>%
      dplyr::filter(
          .data$home_onice_1 == p | .data$home_onice_2 == p  | .data$home_onice_3 == p | .data$home_onice_4 == p | .data$home_onice_5 == p | .data$home_onice_6 == p | .data$home_goalie == p | .data$away_onice_1 == p | .data$away_onice_2 == p  | .data$away_onice_3 == p | .data$away_onice_4 == p | .data$away_onice_5 == p | .data$away_onice_6 == p | .data$away_goalie == p
      )

    playerframe[playerframe$PlayerId == p, ]$xG_for <- sum(xgon[playerframe[playerframe$PlayerId == p, ]$Team == xgon$team_tri_code, ]$xG)
    playerframe[playerframe$PlayerId == p, ]$xG_against <- sum(xgon[playerframe[playerframe$PlayerId == p, ]$Team != xgon$team_tri_code, ]$xG)
  }

  return(list("game" = gameId, "home_team" = home_team, "away_team" = away_team, "home_xg" = home_xg, "away_xg" = away_xg, "home_goals" = home_goals, "away_goals" = away_goals, "player_xg" = playerframe))
}

model_game_xg<-function(gameId, model=NULL){
  stopifnot(requireNamespace('workflows', quietly=TRUE))
  season<-substr(gameId, 1, 4)

  stopifnot(as.numeric(season)>=2011)

  if(is.null(model)){
    model<-'logistic'
  }

  model<-load_season_model(season = as.numeric(season), model=model)

  if(model=='logistic'){
    stopifnot(requireNamespace('glmnet', quietly = TRUE))
  } else if (model == 'xgboost'){
    stopifnot(requireNamespace('xgboost', quietly=TRUE))
  }

  if(file.exists(file.path(getOption("BulsinkBxG.data.path"), season, paste0(gameId, "_pbp.rds")))){
    game_pbp<-readRDS(file.path(getOption("BulsinkBxG.data.path"), season, paste0(gameId, "_pbp.rds")))
  } else {
    game_pbp<-process_game_pbp(gameId)
  }

  game_pbp<-suppressWarnings(prep_xg_model_data(game_pbp))
  game_pbp<-adjust_for_rink_bias(game_pbp, season=as.numeric(season))

  game_pbp<-game_pbp[game_pbp$event %in% c('Shot', 'Missed Shot', 'Miss', 'Goal'), ]

  xg_res<-get_xg(data = game_pbp, model_collection = model)$data

  game_pbp<-dplyr::left_join(game_pbp, xg_res, by = 'event_id')

  game_pbp$xG_pred <- game_pbp$xG

  game_pbp$xG <- ifelse(game_pbp$is_cluster == 0, game_pbp$xG_pred, game_pbp$xG_pred * (1-dplyr::lag(game_pbp$xG_pred)))

  return(game_pbp)
}


#' Get A Season's xG values
#'
#' @description This function returns forces calculation of GameID and Home & Away xG values for every game in a season. It also writes all games' results to
#' a file at getOption("BulsinkBxG.data.path")/xG.csv
#'
#' @param season Season in YYYYY format (for example 2011 for the 2011-2012 season)
#'
#' @return Nothing (invisibly TRUE)
#' @export
get_season_xGs<-function(season){
  gameIds<-get_game_ids(season=season)
  gameIds<-gameIds[is_valid_gameId(gameIds)]
  purrr::walk(gameIds, get_game_xg)
  invisible(TRUE)
}
