#' Scrape and Save
#'
#' @description Scrape games with EW's scraper and save the files in a directory
#'
#' @param scrapegames game ids to scrape
#' @param data_dir directory in which to save games
#' @param overwrite_downloads If true, will re-download games. Else only try for missing games
#' @param ... additional parameters to pass to scraper
#'
#' @export
scrape_and_save<-function(scrapegames, data_dir="~/NHLpbp", overwrite_downloads = FALSE, ...){
  ## Dependencies
  library(RCurl); library(xml2); library(rvest); library(jsonlite); library(foreach)
  library(lubridate)
  library(stringr); library(readr); library(tidyr); library(dplyr)

  ## Source scraper functions from GitHub
  devtools::source_url("https://raw.githubusercontent.com/evolvingwild/evolving-hockey/master/EH_scrape_functions.R")

  if(!dir.exists(data_dir)){
    dir.create(data_dir)
  }
  badgames<-c()
  summarygames<-c()
  for(game in scrapegames){
    if(!file.exists(file.path(data_dir, paste0(game, ".rds"))) || overwrite_downloads){
      pbp<-NULL

      pbp<-tryCatch(
        sc.scrape_pbp(games = as.character(game)),
        error = function(e){
          message("Attempted full scrape of game ", game, " threw error ", e)
          return(NA)
        }
      )
      if(is.na(pbp)){
        message('Trying to get events anyway')
        pbp<-tryCatch({
          sc.scrape_pbp(games = as.character(game), scrape_type = 'event_summary')
          summarygames<-c(summarygames, game)
          },
          error = function(e){
            message("Attempted Events Only scrape of game ", game, " threw error ", e)
            return(NA)
          }
        )
      }

      if(!is.na(pbp) & !is.null(pbp)){
        saveRDS(pbp, file=file.path(data_dir, paste0(game, ".rds")))
      } else {
        badgames<-c(badgames, game)
      }
      invisible(gc())
    }
  }
  if(length(badgames) > 0){
    message("Bad Games: c(", paste(badgames, collapse = ', '), ")")
  }
  if(length(summarygames) > 0){
    message("Summary games: c(", paste(summarygames, collapse = ', '), ")")
  }
}

#' Scrape season
#'
#' @description Shortcut function to scrape a whole season's games using EW's scraper.
#'
#' @param season season 2007++ as numeric
#' @param data_dir directory in which to save games
#' @param skipgames optional vector of games to skip
#' @param ...
#'
#' @export
scrape_season<-function(season, data_dir = "~/NHLpbp", skipgames=NULL, ...){
  ## Dependencies
  library(RCurl); library(xml2); library(rvest); library(jsonlite); library(foreach)
  library(lubridate)
  library(tidyverse) ## -- specifically: stringr, readr, tidyr, and dplyr

  ## Source scraper functions from GitHub
  devtools::source_url("https://raw.githubusercontent.com/evolvingwild/evolving-hockey/master/EH_scrape_functions.R")
  if(!(season %in% c(2019, 2020))){
    invisible(games<-as.numeric(sc.scrape_schedule(start_date=as.Date(paste0(season, "-09-01")), end_date = as.Date(paste0(season+1, "-08-01")))$game_id))
  } else {
    if (season == 2019){
      invisible(games<-as.numeric(sc.scrape_schedule(start_date = as.Date('2019-09-01'), end_date = as.Date("2020-10-15"))$game_id))
    } else {
      invisible(games<-as.numeric(sc.scrape_schedule(start_date = as.Date('2020-11-01'), end_date = as.Date("2021-08-01"))$game_id))
    }
  }
  games<-games[games > as.numeric(paste0(season, "020000"))]
  if(!is.null(skipgames)){
    games<-games[!(games %in% skipgames)]
  }
  scrape_and_save(games, data_dir=data_dir, ... = ...)
}
