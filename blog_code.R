setwd("/Users/Brian_Liou/Documents/Startup/kaggle_march_madness/")
## THEY NEED TO SET OWN WD

regSeason <- read.csv("data/regular_season_results.csv", header = TRUE, stringsAsFactors = FALSE)
seasons <- read.csv("data/seasons.csv", header = TRUE, stringsAsFactors = FALSE)
teams <- read.csv("data/teams.csv", header = TRUE, stringsAsFactors = FALSE)
tourneyRes <- read.csv("data/tourney_results.csv", header = TRUE, stringsAsFactors = FALSE)
tourneySeeds <- read.csv("data/tourney_seeds.csv", header = TRUE, stringsAsFactors = FALSE)
tourneySlots <- read.csv("data/tourney_slots.csv", header = TRUE, stringsAsFactors = FALSE)

head(regSeason)
tail(regSeason)

head(tourneyRes)
tail(tourneyRes)

submissionFile <- function(season) {
  playoffTeams <- sort(tourneySeeds$team[which(tourneySeeds$season == season)])
  numTeams <- length(playoffTeams)
  matrix <- matrix(nrow =numTeams, ncol = numTeams)
  for(i in c(1:numTeams)) {
    for(j in c(1:numTeams)) {
      matrix[i,j] <- paste(season,"_",playoffTeams[i],"_", playoffTeams[j], sep ="")
    }
  }
  keep <- upper.tri(matrix, diag = F)
  idcol <- vector()
  for(i in c(1:numTeams)) {
    for(j in c(1:numTeams)) {
      if(keep[i,j] == T) {
        idcol <- c(idcol, matrix[i,j])
      }
    }
  }
  form <- data.frame("Matchup" = idcol, "Win" = NA)
  return(form)
}

sub_file <- data.frame()
for(i in LETTERS[14:18]) {
  sub_file <- rbind(sub_file, submissionFile(i))
}
colnames(sub_file) <- c("id", "pred")
sub_file$pred <- .5
write.csv(sub_file, file = "kaggle.csv", row.names = FALSE)

## Determining Your Response Values
season_matches <- tourneyRes[which(tourneyRes$season == "A"), ]
team <- vector()
result <- vector()
for(i in c(1:nrow(season_matches))) {
  row <- season_matches[i, ]
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
matchup_frame <- data.frame("Matchup" = team, "Win" = result)


## Creating Your Predictors

#Selecting and sorting the playoff teamIDs least to greatest for season A
playoff_teams <- sort(tourneySeeds$team[which(tourneySeeds$season == "A")])

#Selecting the seeds for season A
playoff_seeds <- tourneySeeds[which(tourneySeeds$season == "A"), ]

#Selecting the regular season statistics for season A
season <- regSeason[which(regSeason$season == "A"), ]

#Wins by team
win_freq_table <- as.data.frame(table(season$wteam))
wins_by_team <- win_freq_table[win_freq_table$Var1 %in% playoff_teams, ]

#Losses by team
loss_freq_table <- as.data.frame(table(season$lteam))
loss_by_team <- loss_freq_table[loss_freq_table$Var1 %in% playoff_teams, ]

#Total Win Percentage
gamesplayed <- as.vector(wins_by_team$Freq + loss_by_team$Freq)
total_winpct <- round(wins_by_team$Freq / gamesplayed, digits = 3)
total_winpct_by_team <- as.data.frame(cbind(as.vector(loss_by_team$Var1), total_winpct))
colnames(total_winpct_by_team) <- c("Var1", "Freq")

#Num of wins in last 6 games
wins_last_six_games_by_team <- data.frame()
for(i in playoff_teams) {
  games <- season[which(season$wteam == i | season$lteam == i), ]
  numwins <- sum(tail(games$wteam) == i)
  put <- c(i, numwins)
  wins_last_six_games_by_team <- rbind(wins_last_six_games_by_team, put)
}
colnames(wins_last_six_games_by_team) <- c("Var1", "Freq")

#Seed in tournament
pattern <- "[A-Z]([0-9][0-9])"
team_seeds <- as.data.frame(str_match(playoff_seeds$seed, pattern))
seeds <- as.numeric(team_seeds$V2)
playoff_seeds$seed  <- seeds
seed_col <- vector()
for(i in playoff_teams) {
  val <- match(i, playoff_seeds$team)
  seed_col <- c(seed_col, playoff_seeds$seed[val])
}
team_seed <- data.frame("Var1" = playoff_teams, "Freq" =seed_col)

#Combining columns together
team_metrics <- data.frame()
team_metrics <- cbind(total_winpct_by_team, wins_last_six_games_by_team$Freq, team_seed$Freq)
colnames(team_metrics) <- c("TEAMID", "A_TWPCT","A_WST6", "A_SEED")


team_metrics_copy <- team_metrics
colnames(team_metrics_copy) <- c("TEAMID", "B_TWPCT","B_WST6", "B_SEED")
pattern <- "[A-Z]_([0-9]{3})_([0-9]{3})"
teamIDs <- as.data.frame(str_match(matchup_frame$Matchup, pattern))[ ,c(2,3)]
colnames(teamIDs) <- c("A_ID", "B_ID")
train_data_frame <- cbind(matchup_frame, teamIDs)
A_frame <- data.frame()
for(i in train_data_frame$A_ID) {
  A_frame <- rbind(A_frame, team_metrics[match(i, team_metrics$TEAMID), ])
}
#Removing teamID column
A_frame <- A_frame[ , -1]

B_frame <- data.frame()
for(i in train_data_frame$B_ID) {
  B_frame <- rbind(B_frame, team_metrics_copy[match(i, team_metrics_copy$TEAMID), ])
}
B_frame <- B_frame[ , -1]

train_data_frame <- cbind(train_data_frame, A_frame, B_frame)
head(train_data_frame)


write.csv(matchup_frame[1:6, ], "matchup_frame.csv", row.names = FALSE)
write.csv(team_metrics[1:6, ], "team_metrics.csv", row.names = FALSE)
write.csv(team_metrics_copy[1:6, ], "team_metrics_copy.csv", row.names = FALSE)
write.csv(train_data_frame[1:6, ], "train_data_frame.csv", row.names = FALSE)






library('stringr')
data_frame_model <- function(seasonletter) {
  teamMetrics <- team_metrics_by_season(seasonletter)
  season_matches <- tourneyRes[which(tourneyRes$season == seasonletter), ]
  team <- vector()
  result <- vector()
  for(i in c(1:nrow(season_matches))) {
    row <- season_matches[i, ]
    if(row$wteam < row$lteam) {
      vector <- paste(seasonletter,"_",row$wteam,"_", row$lteam, sep ="")
      team <- c(team, vector)
      result <- c(result, 1)
    } else {
      oth <- paste(seasonletter, "_", row$lteam, "_", row$wteam, sep ="")
      team <- c(team, oth)
      result <- c(result, 0)
    }
  }
  model_data_frame <- data.frame("Matchup" = team, "Win" = result)
  teamMetrics_away <- teamMetrics
  colnames(teamMetrics_away) <- c("TEAMID", "TWPCT_A", "AWPCT_A", "WLT2_A", "LLT2_A", "WGT7_A",
                                  "LGT7_A", "PCT4WEEK_A", "RANKPCT_A", "WST6_A", "SEED_A", "TWIN_A")
  pattern <- "[A-Z]_([0-9]{3})_([0-9]{3})"
  teamIDs <- as.data.frame(str_match(model_data_frame$Matchup, pattern))
  teamIDs <- teamIDs[ , c(2,3)]
  colnames(teamIDs) <- c("HomeID", "AwayID")
  model_data_frame <- cbind(model_data_frame, teamIDs)
  home_frame <- data.frame()
  for(i in model_data_frame$HomeID) {
    home_frame <- rbind(home_frame, teamMetrics[match(i, teamMetrics$TEAMID), ])
  }
  #Removing teamID column
  home_frame <- home_frame[ , -1]
  
  away_frame <- data.frame()
  for(i in model_data_frame$AwayID) {
    away_frame <- rbind(away_frame, teamMetrics_away[match(i, teamMetrics_away$TEAMID), ])
  }
  away_frame <- away_frame[ , -1]
  
  model_data_frame <- cbind(model_data_frame, home_frame, away_frame)
  
  return(model_data_frame)
}































team_metrics_by_season <- function(seasonletter) {
  playoff_teams <- sort(tourneySeeds$team[which(tourneySeeds$season == seasonletter)])
  playoff_seeds <- tourneySeeds[which(tourneySeeds$season == seasonletter), ]
  season <- regSeason[which(regSeason$season == seasonletter), ]
  ##Each of these dataframes is labled "Var1" and "Freq" for TeamID and Statistic respectively
  #Wins (NOT A USABLEVAR, must scale)
  win_freq_table <- as.data.frame(table(season$wteam))
  wins_by_team <- win_freq_table[win_freq_table$Var1 %in% playoff_teams, ]
  #Losses (NOT A USABLEVAR, must scale)
  loss_freq_table <- as.data.frame(table(season$lteam))
  loss_by_team <- loss_freq_table[loss_freq_table$Var1 %in% playoff_teams, ]
  #Total Win Percentage
  gamesplayed <- as.vector(wins_by_team$Freq + loss_by_team$Freq)
  total_winpct <- round(wins_by_team$Freq / gamesplayed, digits = 3)
  total_winpct_by_team <- as.data.frame(cbind(as.vector(loss_by_team$Var1), total_winpct))
  colnames(total_winpct_by_team) <- c("Var1", "Freq")
  #Wins by margin of less than 2
  games_by_lt2 <- season[which(season$ptdiff <= 2), ]
  wins_lt2_table <- as.data.frame(table(games_by_lt2$wteam), stringsAsFactors = FALSE)
  wins_lt2_by_team <- wins_lt2_table[wins_lt2_table$Var1 %in% playoff_teams, ]
  wins_lt2_by_team <- missingPteam(wins_lt2_by_team, playoff_teams) #Missing Teams Check
  #Losses by margin of less than 2
  loss_lt2_table <- as.data.frame(table(games_by_lt2$lteam), stringsAsFactors = FALSE)
  loss_lt2_by_team <- loss_lt2_table[loss_lt2_table$Var1 %in% playoff_teams, ]
  loss_lt2_by_team <- missingPteam(loss_lt2_by_team, playoff_teams) #Missing Teams Check
  #Num of wins in last 6 games
  wins_last_six_games_by_team <- data.frame()
  for(i in playoff_teams) {
    games <- season[which(season$wteam == i | season$lteam == i), ]
    numwins <- sum(tail(games$wteam) == i)
    put <- c(i, numwins)
    wins_last_six_games_by_team <- rbind(wins_last_six_games_by_team, put)
  }
  colnames(wins_last_six_games_by_team) <- c("Var1", "Freq")
  #Adding Seed
  pattern <- "[A-Z]([0-9][0-9])"
  team_seeds <- as.data.frame(str_match(playoff_seeds$seed, pattern))
  seeds <- as.numeric(team_seeds$V2)
  playoff_seeds$seed  <- seeds
  seed_col <- vector()
  for(i in playoff_teams) {
    val <- match(i, playoff_seeds$team)
    seed_col <- c(seed_col, playoff_seeds$seed[val])
  }
  team_seed <- data.frame("Var1" = playoff_teams, "Freq" =seed_col)
  #Combining columns together
  team_metrics <- data.frame()
  team_metrics <- cbind(total_winpct_by_team, wins_lt2_by_team$Freq,loss_lt2_by_team$Freq,
                         wins_last_six_games_by_team$Freq, team_seed$Freq)
  
  colnames(team_metrics) <- c("TEAMID", "TWPCT", "WLT2","LLT2", "WST6", "SEED")
  return(team_metrics)
}


