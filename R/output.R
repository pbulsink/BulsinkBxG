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

  if(file.exist(file.path(getOption("BulsinkBxG.data.path"), "xG.csv"))){
    xg_files<-read.csv(file.path(getOption("BulsinkBxG.data.path"), "xG.csv"))
    if(gameId %in% xg_files$GameId){
      return(list("home_xg" = xg_files[xg_files$GameId == gameId,]$home_xg, "away_xg" = xg_files[xg_files$GameId == gameId,]$away_xg))
    }
  } else {
    xg_files<-data.frame("GameId" = character(), "home_xg" = numeric(), "away_xg" = numeric())
    write.csv(xg_files, file = file.path(getOption("BulsinkBxG.data.path"), "xG.csv"), col.names = TRUE)
  }

  season<-substr(gameId, 1, 4)
  if(is.null(model)){
    model<-load_season_model(season)
  }

  if(file.exist(file.path(getOption("BulsinkBxG.data.path"), season, paste0(gameId, "_pbp.rds")))){
    game_pbp<-file.path(getOption("BulsinkBxG.data.path"), season, paste0(gameId, "_pbp.rds"))
  } else {
    game_pbp<-process_game_pbp(gameId)
  }

  game_pbp<-prep_xg_model_data(game_pbp)

  game_pbp<-game_pbp[game_pbp$event %in% c('Shot', 'Missed Shot', 'Miss', 'Goal'), ]

  game_pbp_home<-game_pbp[game_pbp$is_home == 1, ]
  game_pbp_away<-game_pbp[game_pbp$is_home == 0, ]

  game_pbp_home$xG_pred<-predict(model, new_data = game_pbp_home)
  game_pbp_away$xG_pred<-predict(model, new_data = game_pbp_away)

  game_pbp_home <- game_pbp_home %>%
    dplyr::mutate("xG" = .data$xG_pred) %>%
    dplyr::rowwise() %>%
    dplyr::mutate("xG" = dplyr::if_else(.data$is_cluster == 0, .data$xG, .data$xG * dplyr::lag(.data$xG))) %>%
    dplyr::select(-.data$xG_pred)

  game_pbp_away <- game_pbp_away %>%
    dplyr::mutate("xG" = .data$xG_pred) %>%
    dplyr::rowwise() %>%
    dplyr::mutate("xG" = dplyr::if_else(.data$is_cluster == 0, .data$xG, .data$xG * dplyr::lag(.data$xG))) %>%
    dplyr::select(-.data$xG_pred)

  xg_files<-data.frame("GameId" = gameId, "home_xg" = um(game_pbp_home$xG), "away_xg" = sum(game_pbp_away$xG))
  write.csv(xg_files, file = file.path(getOption("BulsinkBxG.data.path"), "xG.csv"), append = TRUE, col.names = FALSE)
  return(list("home_xg" = sum(game_pbp_home$xG), "away_xg" = sum(game_pbp_away$xG)))
}

