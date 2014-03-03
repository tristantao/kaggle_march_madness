setwd("/Users/Brian_Liou/Documents/Startup/kaggle_march_madness/")
source('utility.R')

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
#  Num of wins in last 4 weeks or % of wins in last 4 weeks
#  Num of wins in last 7 games
#  Wins/loss against teams that are in the tournament? (there are 304 teams in season A and 64 get into tourney)

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
neutWins_table <- as.data.frame(table(neut_wins$wteam),stringsAsFactors = FALSE)
neutWins_by_team <- neutWins_table[neutWins_table$Var1 %in% playoff_teams, ]
# SOME TEAMS had 0 Neutral games, namely teams: 539|551|625|721|701|662
datone <- c(539, 0)
dattwo <- c(551, 0)
dathree <- c(625, 0)
datfour <- c(721, 0)
datfive <- c(701, 0)
datsix <- c(662, 0)

neutWins_by_team <- missingPteam(neutWins_by_team)

# Check to see calculations are correct
wins_by_team$Freq #Should be the same as
awayWins_by_team$Freq + homeWins_by_team$Freq + neutWins_by_team$Freq

games_by_lt3 <- season_a[which(season_a$ptdiff <= 3), ]
wins_lt3_table <- as.data.frame(table(games_by_lt3$wteam), stringsAsFactors = FALSE)
wins_lt3_by_team <- wins_lt3_table[wins_lt3_table$Var1 %in% playoff_teams, ]
#Adding in missing teams
wins_lt3_by_team <- missingPteam(wins_lt3_by_team)

loss_lt3_table <- as.data.frame(table(games_by_lt3$lteam), stringsAsFactors = FALSE)
loss_lt3_by_team <- loss_lt3_table[loss_lt3_table$Var1 %in% playoff_teams, ]
#Adding in missing teams
loss_lt3_by_team <- missingPteam(loss_lt3_by_team)

games_by_gt7 <- season_a[which(season_a$ptdiff >= 7), ]
wins_gt7_table <- as.data.frame(table(games_by_gt7$wteam), stringsAsFactors = FALSE)
wins_gt7_by_team <- wins_gt7_table[wins_gt7_table$Var1 %in% playoff_teams, ]
#Adding in missing teams
wins_gt7_by_team <- missingPteam(wins_gt7_by_team)

loss_gt7_table <- as.data.frame(table(games_by_gt7$wteam), stringsAsFactors = FALSE)
loss_gt7_by_team <- loss_gt7_table[loss_gt7_table$Var1 %in% playoff_teams, ]
#Adding in missing teams
loss_gt7_by_team <- missingPteam(loss_gt7_by_team)

#Figure out which row is daynum = 104 (4 weeks before last day daynum = 132)
match(104, season_a$daynum)

#Last four weeks of season a
last_four_season_a <- season_a[match(104, season_a$daynum):nrow(season_a), ]

#Number of wins in last 4 weeks
last_four_win_table <- as.data.frame(table(last_four_season_a$wteam), stringsAsFactors = FALSE)
last_four_win_by_team <- last_four_win_table[last_four_win_table$Var1 %in% playoff_teams, ]

#Number of losses in last 4 weeks
last_four_loss_table <- as.data.frame(table(last_four_season_a$lteam), stringsAsFactors = FALSE)
last_four_loss_by_team <- last_four_loss_table[last_four_loss_table$Var1 %in% playoff_teams, ]
#Adding missing values
last_four_loss_by_team <- missingPteam(last_four_loss_by_team)

#Win percentage in last 4 weeks
last_four_winpct <- last_four_win_by_team$Freq / (last_four_loss_by_team$Freq + last_four_win_by_team$Freq)
last_four_winpct <- as.data.frame(cbind(as.integer(last_four_win_by_team$Var1), as.numeric(last_four_winpct)))
colnames(last_four_winpct) <- c("Var1", "WinPct")

#Wins against teams that are in the tournament
rankedwins <- data.frame()
colnames(rankedwins) <- c("Var1", "Wins")
for(i in playoff_teams) {
  individ_team_wins <- season_a[which(season_a$wteam == i), ]
  wins <- sum(individ_team_wins$lteam %in% playoff_teams)
  vector <- c(i, wins)
  rankedwins <- rbind(rankedwins, vector)
}

individ_team_wins <- season_a[which(season_a$wteam == 511), ]
wins <- sum(individ_team_wins$lteam %in% playoff_teams)

#Losses against teams that are in the tournament
rankedloss <- data.frame()
colnames(rankedloss) <- c("Var1", "Loss")
for(i in playoff_teams) {
  individ_team_loss <- season_a[which(season_a$lteam == i), ]
  loss <- sum(individ_team_loss$wteam %in% playoff_teams)
  vector <- c(i, loss)
  rankedloss <- rbind(rankedloss, vector)
}

individ_team_loss <- season_a[which(season_a$lteam == 577), ]
loss <- sum(individ_team_loss$wteam %in% playoff_teams)

##Create matchup data in the same order as the submission order of the permutations of the
##teams will be

## Should still be able to combine team data with matchup data to predict 0/1 for any matchup

## WHat woud trends by team over multiple seasons loook like?
