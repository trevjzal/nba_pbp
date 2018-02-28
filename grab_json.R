library(jsonlite)
library(tidyverse)
library(data.table)

startyear <- 2017


GetURL <- function(startyear = numeric(), gameid = numeric()){
  gameid <- as.character(gameid)
  while(nchar(gameid) < 4){
    gameid <- paste0("0", gameid)
  }
  
  year_abbrev <- startyear %% 100
  id_url <- paste0("002", year_abbrev, "0", gameid)
  
  url <- paste0("http://data.nba.com/data/v2015/json/mobile_teams/nba/",
                startyear,
                "/scores/pbp/", 
                id_url, 
                "_full_pbp.json")
}

testid <- 261
testurl <- GetURL(startyear, testid)

data_json = jsonlite::fromJSON(txt = testurl)
data <- data.table(data_json$g$pd)
setnames(data, "p", "quarter")

