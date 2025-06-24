# I wasn't able to perform web scraping from the original online 
# database that I was planning to use.
# However I found a source that publishes data on all official FIDE games weekly. 
# That database uses PGNs (Portable Game Notation) and I'm going to work with them.
# Therefore, I now have access to the most current data.


# This code is for me to create and update the .csv dataset

install.packages(c("bigchess", "dplyr", "lubridate"))
library(bigchess)  # to work with .PGN files
library(dplyr)
library(lubridate)

setwd("/Users/alibekmamyrbay/Desktop/PGNs")

pgn_files <- list.files(pattern = "\\.pgn$", full.names = TRUE)

all_games_list <- lapply(pgn_files, function(f) {
  lines <- readLines(f, warn = FALSE)
  # split into candidate game‐blocks at blank lines
  games <- split(lines, cumsum(lines == ""))
  # drop any blocks with no tags
  games <- Filter(function(g) any(grepl("^\\[", g)), games)
  
  # parse each real game
  do.call(rbind, lapply(games, function(g) {
    tags <- grep("^\\[", g, value = TRUE)
    kv   <- strcapture(
      '^\\[(\\w+) "(.*)"\\]$',
      tags,
      proto = list(key=character(), value=character())
    )
    tv   <- setNames(kv$value, kv$key)
    
    data.frame(
      Event    = tv["Event"],
      Date     = as.Date(tv["Date"], format="%Y.%m.%d"),
      White    = tv["White"],
      Black    = tv["Black"],
      Result   = tv["Result"],
      WhiteElo = as.integer(tv["WhiteElo"]),
      BlackElo = as.integer(tv["BlackElo"]),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }))
})

# bind all files together
all_headers_df <- do.call(rbind, all_games_list)
rownames(all_headers_df) <- NULL

write.csv(all_headers_df, "fide_games_weekly.csv", row.names = FALSE)
