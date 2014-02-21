setwd("/Users/t-rex-Box/Desktop/work/kaggle_march_madness/")

seaonRes <- read.csv("data/regular_season_results.csv", header = TRUE, stringsAsFactors = FALSE)
seasons <- read.csv("data/seasons.csv", header = TRUE, stringsAsFactors = FALSE)
teams <- read.csv("data/teams.csv", header = TRUE, stringsAsFactors = FALSE)
tourneyRes <- read.csv("data/tourney_results.csv", header = TRUE, stringsAsFactors = FALSE)
tourneySeeds.csv <- read.csv("data/tourney_seeds.csv", header = TRUE, stringsAsFactors = FALSE)
tournySlots <- read.csv("data/tourney_slots.csv", header = TRUE, stringsAsFactors = FALSE)


