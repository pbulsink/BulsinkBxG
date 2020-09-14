
#' Load season PBP from File
#'
#' @param season Season file to load
#' @param data_dir location of raw data
#'
#' @return pbp of whole season
#' @export
load_season_pbp<-function(season, data_dir = "~/NHLpbp"){
  filelist<-list.files(path = data_dir, recursive = FALSE)
}

#' Compile PBP to single season file
#'
#' @param season Season to compile to file
#' @param data_dir location of raw data
#'
#' @return pbp of whole season
#' @export
compile_season<-function(season, data_dir = "~/NHLpbp"){
  filelist<-list.files(path = data_dir, recursive = FALSE)
  filelist<-filelist[grepl(pattern = paste0(year, "[0-9]{6}.rds"), x=filelist, ignore.case = TRUE)]

  stopifnot(length(filelist) > 0)
  pbp<-NULL
  for(f in filelist){
    pbp_f<-readRDS(file.path(data_dir, f))$pbp_base
    if(isnull(pbp)){
      pbp<-pbp_f
    } else {
      pbp<-rbind(pbp, pbp_f)
    }
  }
  saveRDS(file.path(data_dir, paste0(season, ".rds")))
  return(pbp)
}
