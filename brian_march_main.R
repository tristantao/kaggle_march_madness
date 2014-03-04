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
N_playoff_teams <- sort(tourneySeeds$team[which(tourneySeeds$season == "N")])

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

neutWins_by_team <- missingPteam(neutWins_by_team, playoff_teams)

# Check to see calculations are correct
wins_by_team$Freq #Should be the same as
awayWins_by_team$Freq + homeWins_by_team$Freq + neutWins_by_team$Freq

games_by_lt3 <- season_a[which(season_a$ptdiff <= 3), ]
wins_lt3_table <- as.data.frame(table(games_by_lt3$wteam), stringsAsFactors = FALSE)
wins_lt3_by_team <- wins_lt3_table[wins_lt3_table$Var1 %in% playoff_teams, ]
#Adding in missing teams
wins_lt3_by_team <- missingPteam(wins_lt3_by_team, playoff_teams)

loss_lt3_table <- as.data.frame(table(games_by_lt3$lteam), stringsAsFactors = FALSE)
loss_lt3_by_team <- loss_lt3_table[loss_lt3_table$Var1 %in% playoff_teams, ]
#Adding in missing teams
loss_lt3_by_team <- missingPteam(loss_lt3_by_team, playoff_teams)

games_by_gt7 <- season_a[which(season_a$ptdiff >= 7), ]
wins_gt7_table <- as.data.frame(table(games_by_gt7$wteam), stringsAsFactors = FALSE)
wins_gt7_by_team <- wins_gt7_table[wins_gt7_table$Var1 %in% playoff_teams, ]
#Adding in missing teams
wins_gt7_by_team <- missingPteam(wins_gt7_by_team, playoff_teams)

loss_gt7_table <- as.data.frame(table(games_by_gt7$wteam), stringsAsFactors = FALSE)
loss_gt7_by_team <- loss_gt7_table[loss_gt7_table$Var1 %in% playoff_teams, ]
#Adding in missing teams
loss_gt7_by_team <- missingPteam(loss_gt7_by_team, playoff_teams)

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
last_four_loss_by_team <- missingPteam(last_four_loss_by_team, playoff_teams)

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

##Creating submission file for season A
subfile <- submissionFile(64,playoff_teams, "A")

## Creating dataframe for response for season A ONLY
playoff_teams <- sort(tourneySeeds$team[which(tourneySeeds$season == "A")])

season_a_matches <- tourneyRes[which(tourneyRes$season == "A"), ]

team <- vector()
result <- vector()
for(i in c(1:nrow(season_a_matches))) {
  row <- season_a_matches[i, ]
  if(row$wteam < row$lteam) {
    vector <- paste("A","_",row$wteam,"_", row$lteam, sep ="")
    team <- c(team, vector)
    result <- c(result, 1)
  } else {
    oth <- paste("A", "_", row$lteam, "_", row$wteam, sep ="")
    team <- c(team, oth)
    result <- c(result, 0)
  }
}
tourneyWin <- data.frame("Matchup" = team, "Res" = result)

## Creating dataframe for response for all seasons
team <- vector()
result <- vector()
for(i in c(1:nrow(tourneyRes))) {
  row <- tourneyRes[i, ]
  if(row$wteam < row$lteam) {
    vector <- paste(row$season,"_",row$wteam,"_", row$lteam, sep ="")
    team <- c(team, vector)
    result <- c(result, 1)
  } else {
    oth <- paste(row$season, "_", row$lteam, "_", row$wteam, sep ="")
    team <- c(team, oth)
    result <- c(result, 0)
  }
}
matchupRes <- data.frame("Matchup" = team, "Res" = result)


## Combining all of the indidividual statistics into one table by TEAMID
team_stats <- data.frame()
team_stats <- cbind(homeWins_by_team, awayWins_by_team$Freq, neutWins_by_team$Freq, wins_lt3_by_team$Freq,
                    wins_gt7_by_team$Freq, loss_lt3_by_team$Freq, loss_gt7_by_team$Freq, last_four_win_by_team$Freq,
                    last_four_loss_by_team$Freq, last_four_winpct$WinPct, rankedwins$Wins,
                    rankedloss$Loss)
colnames(team_stats) <- c("TEAMID", "HW", "AW", "NW", "WLT3", "WGT7", "LLT3", "LGT7", "W4WEEK", "L4WEEK", "PCT4WEEK", "RANKWIN",
                          "RANKLOSS")
  
## Creating final grouped datafame to run models on
A_model_frame <- cbind(tourneyWin)
colnames(A_model_frame) <- c("Matchup", "Win")
# Copy of team_stats and renaming headers to _A for AWAY TEAM
team_stats_away <- team_stats
colnames(team_stats_away) <- c("TEAMID", "HW_A", "AW_A", "NW_A", "WLT3_A", "WGT7_A", "LLT3_A", "LGT7_A", "W4WEEK_A", "L4WEEK_A", 
                               "PCT4WEEK_A", "RANKWIN_A", "RANKLOSS_A")

# for 1 : nrows of model.frame match each rows team IDs with rows in team_stats$TEAMID and
# rbind them all together 

## Selecting by TeamID
pattern <- "[A-Z]_([0-9]{3})_([0-9]{3})"
teamIDs <- as.data.frame(str_match(A_model_frame$Matchup, pattern))
teamIDs <- teamIDs[ , c(2,3)]
colnames(teamIDs) <- c("HomeID", "AwayID")


A_model_frame <- cbind(tourneyWin, teamIDs)

## First rbind all of the homeIDs together in model.frame and then all of the awayIDs
## together and then cbind everything
home.frame <- data.frame()
for(i in A_model_frame$HomeID) {
  home.frame <- rbind(home.frame, team_stats[match(i, team_stats$TEAMID), ])
}
#Removing teamID column
home.frame <- home.frame[ , -1]

away.frame <- data.frame()
for(i in A_model_frame$AwayID) {
  away.frame <- rbind(away.frame, team_stats_away[match(i, team_stats_away$TEAMID), ])
}
away.frame <- away.frame[ , -1]

A_model_frame <- cbind(A_model_frame, home.frame, away.frame)


## TRIAL MODEL

test.glm <- glm(Res ~ AW + WLT3 + WGT7 + LLT3 + LGT7 + W4WEEK + RANKWIN + RANKLOSS +
                  AW_A + WLT3_A + WGT7_A + LLT3_A + LGT7_A + W4WEEK_A + RANKWIN_A + RANKLOSS_A,
                family = binomial, data = model.frame)

p.hats <- predict.glm(test.glm,newdata = model.frame, type = "response")

winvec <- vector()
for(i in 1:length(p.hats)) {
  if(p.hats[i] > .5) {
    winvec[i] <- 1
  } else {
    winvec[i] <- 0
  }
}

##Need to know create model.frame format for all seasons. How do we evaluate the glm

## Function needs to change by the playoff matchups that are determined by tourneyRes


