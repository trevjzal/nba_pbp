library(jsonlite)
library(tidyverse)
library(data.table)
library(stringr)

startyear <- 2017


GetFullPBPURL <- function(startyear = numeric(), gameid = numeric()){
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

BindFullPBP <- function(dt){
  dtlist <- list()
  for(i in seq(nrow(dt))){
    dtlist[[i]] <- data.table(dt$pla[[i]])
    dtlist[[i]][, quarter := dt[i]$quarter]
  }

  dt2 <- rbindlist(dtlist)
}

CleanPBP <- function(dt){
  names(dt) <- c("event", "clock", "description", "locX", "locY", "opt1", "opt2",
                 "shot_type", "event_type", "opid", "teamid", "playerid",
                 "home_score", "visitor_score", "epid", "oftid", "order")
  
  return(TRUE)
}



GrabAndSaveGameData  <- function(season_year, game_id){
  # Save games by day in season subdirectory
  if(!dir.exists(season_year))
    dir.create(season_year)
  
  # Get URL
  url <- GetFullPBPURL(season_year, game_id)
  
  # Grab PBP data and clean
  data_json <- jsonlite::fromJSON(txt = url)
  data <- data.table(data_json$g$pd)
  setnames(data, "p", "quarter")
  dt <- BindFullPBP(data) %>% CleanPBP
  
  # Save
  splitup <- tstrsplit(data_json$g$gcode, "/")
  game_date <- splitup[[1]]
  filename <- paste0(splitup[[2]], ".rds")
  exportdir <- file.path(season_year, game_date)
  if(!dir.exists(exportdir))
    dir.create(exportdir)
  
  saveRDS(dt, file.path(exportdir, filename))
}











