gId<-function(gameId){
  if(!is.numeric(gameId)){
    return(FALSE)
  } else if (!nchar(gameId) == 10) {
    return(FALSE)
  } else if (!grepl(pattern = "20[1,2][0-9]0[2,3][0-9]{4}", gameId)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}
is_valid_gameId<-Vectorize(gId)

#' getGameIDs
#'
#' @description Returns a vector of gameIds for the season provided. Must be 2011 to present (earlier seasons do not contain play by play data).
#'
#' @param season A season. Enter as a four digit number, e.g., 2018 for the 2018-2019 season
#'
#' @return a vector of gameIds in the format YYYY0XGGGG, where YYYY is the season, 0X is the game type (02 for regular season, 03 for playoffs) and GGGG is the four digit game number of the season, or the round, series and game number of playoff games
#' @export
get_game_ids<-function(season){
  #Find GameIds to download - TODO fake it if error
  stopifnot(is.numeric(season))
  stopifnot(nchar(season)==4)
  stopifnot(season>=2011)
  stopifnot(season<=format(Sys.Date(), "%Y"))
  sched<-nhlapi::nhl_schedule_seasons(season)

  gameIds<-c()
  for (y in 1:length(sched)){
    if(!(length(sched[[y]]$dates)>0)){
      next
    }
    for(d in 1:nrow(sched[[y]]$dates)){
      for (g in 1:sched[[y]]$dates$totalGames[[d]]){
        if(sched[[y]]$dates$games[[d]]$status.detailedState[[g]] != 'Final'){
          #Skip games that aren't final scores
          next
        }
        gameIds<-c(gameIds, sched[[y]]$dates$games[[d]]$gamePk[[g]])
      }
    }
  }
  gameIds<-gameIds[is_valid_gameId(gameIds)]
  return(gameIds)
}


check_or_create_dir<-function(season=NULL){
  if(!dir.exists(getOption("BulsinkBxG.data.path"))){
    dir.create(getOption("BulsinkBxG.data.path"))
  }
  if(!is.null(season)){
    for(s in season){
        dir.create(file.path(getOption("BulsinkBxG.data.path"), s))
    }
  }
}

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.BulsinkBxG <- list(
    BulsinkBxG.data.path = "~/NHLpbp"
  )
  toset <- !(names(op.BulsinkBxG) %in% names(op))
  if(any(toset)) options(op.BulsinkBxG[toset])

  invisible()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage('BulsinkB xG package loaded. Using ', getOption("BulsinkBxG.data.path"), ' as data path.\nTo change path, set option("BulsinkBxG.data.path" = [new path]).\nThis can be done interactively or using .RProfile to save your preference.')
}


## From fastRhockey
check_status <- function(res) {
  x = httr::status_code(res)
  if(x != 200) stop("The API returned an error", call. = FALSE)
}

iferror<-function(f, e){
  tryCatch({r<-eval(f)}, error={r<-e})
  if(length(r)==0){
    r<-e
  }
  return(r)
}

get_all_players_in_pbp<-function(pbp){
  players <- pbp %>%
    dplyr::select(c(dplyr::starts_with('away_onice'), dplyr::starts_with('home_onice'), dplyr::ends_with('_goalie'))) %>%
    unlist() %>%
    unique()
  return(players)
}
