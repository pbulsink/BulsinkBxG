
load_pbp_file_into_db<-function(pbp_file, db_file="~/NHLpbp/pbp.sqlite"){
  stopifnot(file.exists(pbp_file))
}


#' Load season PBP from File
#'
#' @param year Season file to load
#' @param data_dir location of raw data
#'
#' @return pbp of whole season
#' @export
load_season_pbp<-function(year, data_dir = "~/NHLpbp"){
  if(!file.exists(file.path(data_dir, paste0(season, ".rds")))){
    return(compile_season(season = season, data_dir = data_dir))
  } else {
    return(readRDS(file.path(data_dir, paste0(season, ".rds"))))
  }
}

#' Compile PBP to single season file
#'
#' @param year Season to compile to file
#' @param data_dir location of raw data
#'
#' @return pbp of whole season
#' @export
compile_season<-function(year, data_dir = "~/NHLpbp"){
  filelist<-list.files(path = data_dir, recursive = FALSE)
  filelist<-filelist[grepl(pattern = paste0(season, "[0-9]{6}.rds"), x=filelist, ignore.case = TRUE)]

  stopifnot(length(filelist) > 0)
  pbp<-NULL

  pb<-txtProgressBar(min=1, max = length(filelist), style = 3)
  i<-1
  for(f in filelist){
    pbp_f<-readRDS(file.path(data_dir, f))
    if('pbp_base' %in% names(pbp_f)){
      if(is.null(pbp)){
        pbp<-pbp_f$pbp_base
      } else {
        pbp<-rbind(pbp, pbp_f$pbp_base)
      }
    }

    i<-i+1
    setTxtProgressBar(pb, i)
  }
  saveRDS(pbp, file.path(data_dir, paste0(year, ".rds")))
  return(pbp)
}
