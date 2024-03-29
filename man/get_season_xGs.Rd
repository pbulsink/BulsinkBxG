% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/output.R
\name{get_season_xGs}
\alias{get_season_xGs}
\title{Get A Season's xG values}
\usage{
get_season_xGs(season)
}
\arguments{
\item{season}{Season in YYYYY format (for example 2011 for the 2011-2012 season)}
}
\value{
Nothing (invisibly TRUE)
}
\description{
This function returns forces calculation of GameID and Home & Away xG values for every game in a season. It also writes all games' results to
a file at getOption("BulsinkBxG.data.path")/xG.csv
}
