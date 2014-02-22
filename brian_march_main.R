setwd("/Users/Brian_Liou/Documents/Startup/kaggle_march_madness/")

regSeason <- read.csv("data/regular_season_results.csv", header = TRUE, stringsAsFactors = FALSE)
seasons <- read.csv("data/seasons.csv", header = TRUE, stringsAsFactors = FALSE)
teams <- read.csv("data/teams.csv", header = TRUE, stringsAsFactors = FALSE)
tourneyRes <- read.csv("data/tourney_results.csv", header = TRUE, stringsAsFactors = FALSE)
tourneySeeds <- read.csv("data/tourney_seeds.csv", header = TRUE, stringsAsFactors = FALSE)
tourneySlots <- read.csv("data/tourney_slots.csv", header = TRUE, stringsAsFactors = FALSE)

##Creating data tables based on team statistics to calculate:
#  Home Wins
#  Games Played
#  Away Wins
#  Wins by margin <= 3
#  Wins by margin >= 7
#  Losses by margin <= 3
#  Losses by margin >= 7
#  Num of wins in last 3 weeks or % of wins in last 3 weeks
#  Num of wins in last 7 games
#  Wins against teams that are in the tournament? (there are 304 teams in season A and 64 get into tourney)

##Adds point differential column
regSeason$ptdiff <- regSeason$wscore - regSeason$lscore

##Starting with Season A
head(tourneySeeds)

playoff_teams <- sort(tourneySeeds$team[which(tourneySeeds$season == "A")])

season_a <- regSeason[which(regSeason$season == "A"), ]

win_freq_table <- as.data.frame(table(season_a$wteam))
wins_by_team <- win_freq_table[win_freq_table$Var1 %in% playoff_teams, ]

loss_freq_table <- as.data.frame(table(season_a$lteam))
loss_by_team <- loss_freq_table[loss_freq_table$Var1 %in% playoff_teams, ]

gamesPlayed_by_team <- wins_by_team$Freq + loss_by_team$Freq

away_wins <- season_a[which(season_a$wloc == "A"), ]
awayWins_table <- as.data.frame(table(away_wins$wteam))
awayWins_by_team <- awayWins_table[awayWins_table$Var1 %in% playoff_teams, ]

home_wins <- season_a[which(season_a$wloc == "H"), ]
homeWins_table <- as.data.frame(table(home_wins$wteam))
homeWins_by_team <- homeWins_table[homeWins_table$Var1 %in% playoff_teams, ]

neut_wins <- season_a[which(season_a$wloc == "N"), ]
neutWins_table <- as.data.frame(table(neut_wins$wteam))
neutWins_by_team <- neutWins_table[neutWins_table$Var1 %in% playoff_teams, ]
# SOME TEAMS had 0 Neutral games, namely teams: 539|551|625|721|701|662

games_by_lt3 <- season_a[which(season_a$ptdiff <= 3), ]
wins_lt3_table <- as.data.frame(table(games_by_lt3$wteam))
wins_lt3_by_team <- wins_lt3_table[wins_lt3_table$Var1 %in% playoff_teams, ]

loss_lt3_table <- as.data.frame(table(games_by_lt3$lteam))
loss_lt3_by_team <- loss_lt3_table[loss_lt3_table$Var1 %in% playoff_teams, ]

games_by_gt7 <- season_a[which(season_a$ptdiff >= 7), ]
wins_gt7_table <- as.data.frame(table(games_by_gt7$wteam))
wins_gt7_by_team <- wins_gt7_table[wins_gt7_table$Var1 %in% playoff_teams, ]

loss_gt7_table <- as.data.frame(table(games_by_gt7$wteam))
loss_gt7_by_team <- loss_gt7_table[loss_gt7_table$Var1 %in% playoff_teams, ]

##Create matchup data in the same order as the submission order of the permutations of the
##teams will be

## Should still be able to combine team data with matchup data to predict 0/1 for any matchup
