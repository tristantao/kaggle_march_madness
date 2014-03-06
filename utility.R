##Need to write a utility function which adds back the missing playoff_team if they aren't
##included in the statistic. For ex. Neutral Wins by Team. Always run this utility function
##when calculating all of the statistics to make sure you have the data for all of the playoff teams
##basically just adding c(teamID, 0) for when playoff_team %in% ________ is FALSE when utility function
## != 64
# statistic = Data frame with colnames c("Var1", "Freq")
# playoffTeams = vector of team IDs in playoff
missingPteam <- function(statistic, playoffTeams) {
  vector <- playoffTeams %in% statistic$Var1
  missTeam <- data.frame()
  if (sum(vector) != length(playoffTeams)) {
    for(i in c(1:length(vector))) {
      if (vector[i] == F) {
        insert <- c(playoffTeams[i],0)
        missTeam <- rbind(missTeam, insert)
      }
    }
    colnames(missTeam) <- c("Var1", "Freq")
  }
  fullTeam <- rbind(statistic, missTeam)
  fullTeam <- fullTeam[order(fullTeam$Var1), ]
  return(fullTeam)
}

###################################################################################################
###################################################################################################

##Function which creates the submission file for any given season
# numTeams = number of teams in playoff season
# playoffTeams = vector of team IDs that are in the tournament
# season = String for which season, ex) "A"
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

###################################################################################################
###################################################################################################


## VERSION I
##Function creates data frame of all playoff teams individual team statistics
# seasonletter = String season Ex) "A"
team_metrics_by_season <- function(seasonletter) {
  playoff_teams <- sort(tourneySeeds$team[which(tourneySeeds$season == seasonletter)])
  season <- regSeason[which(regSeason$season == seasonletter), ]
  ##Each of these dataframes is labled "Var1" and "Freq" for TeamID and Statistic respectively
  #Wins
  win_freq_table <- as.data.frame(table(season$wteam))
  wins_by_team <- win_freq_table[win_freq_table$Var1 %in% playoff_teams, ]
  #Losses
  loss_freq_table <- as.data.frame(table(season$lteam))
  loss_by_team <- loss_freq_table[loss_freq_table$Var1 %in% playoff_teams, ]
  #Total Win Percentage
  gamesplayed <- as.vector(wins_by_team$Freq + loss_by_team$Freq)
  total_winpct <- round(wins_by_team$Freq / gamesplayed, digits = 3)
  total_winpct_by_team <- as.data.frame(cbind(as.vector(loss_by_team$Var1), total_winpct))
  colnames(total_winpct_by_team) <- c("Var1", "Freq")
  #Away Wins
  away_wins <- season[which(season$wloc == "A"), ]
  awayWins_table <- as.data.frame(table(away_wins$wteam))
  awayWins_by_team <- awayWins_table[awayWins_table$Var1 %in% playoff_teams, ]
  #Home Wins
  home_wins <- season[which(season$wloc == "H"), ]
  homeWins_table <- as.data.frame(table(home_wins$wteam))
  homeWins_by_team <- homeWins_table[homeWins_table$Var1 %in% playoff_teams, ]
  #Neutral Wins
  neut_wins <- season[which(season$wloc == "N"), ]
  neutWins_table <- as.data.frame(table(neut_wins$wteam),stringsAsFactors = FALSE)
  neutWins_by_team <- neutWins_table[neutWins_table$Var1 %in% playoff_teams, ]
  neutWins_by_team <- missingPteam(neutWins_by_team, playoff_teams) #Missing Teams Check
  #Wins by margin of less than 2
  games_by_lt2 <- season[which(season$ptdiff <= 2), ]
  wins_lt2_table <- as.data.frame(table(games_by_lt2$wteam), stringsAsFactors = FALSE)
  wins_lt2_by_team <- wins_lt2_table[wins_lt2_table$Var1 %in% playoff_teams, ]
  wins_lt2_by_team <- missingPteam(wins_lt2_by_team, playoff_teams) #Missing Teams Check
  #Losses by margin of less than 2
  loss_lt2_table <- as.data.frame(table(games_by_lt2$lteam), stringsAsFactors = FALSE)
  loss_lt2_by_team <- loss_lt2_table[loss_lt2_table$Var1 %in% playoff_teams, ]
  loss_lt2_by_team <- missingPteam(loss_lt2_by_team, playoff_teams) #Missing Teams Check
  #Wins by margin of greater than 7
  games_by_gt7 <- season[which(season$ptdiff >= 7), ]
  wins_gt7_table <- as.data.frame(table(games_by_gt7$wteam), stringsAsFactors = FALSE)
  wins_gt7_by_team <- wins_gt7_table[wins_gt7_table$Var1 %in% playoff_teams, ]
  wins_gt7_by_team <- missingPteam(wins_gt7_by_team, playoff_teams) #MTC
  #Losses by margin of greater than 7
  loss_gt7_table <- as.data.frame(table(games_by_gt7$lteam), stringsAsFactors = FALSE)
  loss_gt7_by_team <- loss_gt7_table[loss_gt7_table$Var1 %in% playoff_teams, ]
  loss_gt7_by_team <- missingPteam(loss_gt7_by_team, playoff_teams) #MTC
  #Number of wins in last 4 weeks
  last_four_season <- season[match(104, season$daynum):nrow(season), ]
  last_four_win_table <- as.data.frame(table(last_four_season$wteam), stringsAsFactors = FALSE)
  last_four_win_by_team <- last_four_win_table[last_four_win_table$Var1 %in% playoff_teams, ]
  last_four_win_by_team <- missingPteam(last_four_win_by_team, playoff_teams) #MTC
  #Number of losses in last 4 weeks
  last_four_loss_table <- as.data.frame(table(last_four_season$lteam), stringsAsFactors = FALSE)
  last_four_loss_by_team <- last_four_loss_table[last_four_loss_table$Var1 %in% playoff_teams, ]
  last_four_loss_by_team <- missingPteam(last_four_loss_by_team, playoff_teams)
  #Win percentage in last 4 weeks
  last_four_winpct <- last_four_win_by_team$Freq / (last_four_loss_by_team$Freq + last_four_win_by_team$Freq)
  last_four_winpct <- as.data.frame(cbind(as.integer(last_four_win_by_team$Var1), round(as.numeric(last_four_winpct), digits = 3)))
  colnames(last_four_winpct) <- c("Var1", "Freq")
  #Number of wins against teams in the tournament
  rankedwins <- data.frame()
  for(i in playoff_teams) {
    individ_team_wins <- season[which(season$wteam == i), ]
    wins <- sum(individ_team_wins$lteam %in% playoff_teams)
    vector <- c(i, wins)
    rankedwins <- rbind(rankedwins, vector)
  }
  colnames(rankedwins) <- c("Var1", "Freq")
  #Number of losses against teams in the tournament
  rankedloss <- data.frame()
  for(i in playoff_teams) {
    individ_team_loss <- season[which(season$lteam == i), ]
    loss <- sum(individ_team_loss$wteam %in% playoff_teams)
    vector <- c(i, loss)
    rankedloss <- rbind(rankedloss, vector)
  }
  colnames(rankedloss) <- c("Var1", "Freq")
  #Num of wins in last 6 games
  wins_last_six_games_by_team <- data.frame()
  for(i in playoff_teams) {
    games <- season[which(season$wteam == i | season$lteam == i), ]
    numwins <- sum(tail(games$wteam) == i)
    put <- c(i, numwins)
    wins_last_six_games_by_team <- rbind(wins_last_six_games_by_team, put)
  }
  colnames(wins_last_six_games_by_team) <- c("Var1", "Freq")
  #Combining columns together
  team_metrics <- data.frame()
  team_metrics <- cbind(wins_by_team, loss_by_team$Freq, total_winpct_by_team$Freq, awayWins_by_team$Freq,
                        homeWins_by_team$Freq, neutWins_by_team$Freq, wins_lt2_by_team$Freq,
                        loss_lt2_by_team$Freq, wins_gt7_by_team$Freq, loss_gt7_by_team$Freq,
                        last_four_win_by_team$Freq, last_four_loss_by_team$Freq, last_four_winpct$Freq,
                        rankedwins$Freq, rankedloss$Freq, wins_last_six_games_by_team$Freq)
  
  colnames(team_metrics) <- c("TEAMID", "W", "L","TWPCT", "AW", "HW", "NW", "WLT2", "LLT2", "WGT7",
                              "LGT7", "W4WEEK", "L4WEEK", "PCT4WEEK", "RANKWIN", "RANKLOSS", "WST6")
  return(team_metrics)
}

###################################################################################################
###################################################################################################

## VERSION II
##Function creates data frame of all playoff teams individual team statistics
# seasonletter = String season Ex) "A"
team_metrics_by_season_vtwo <- function(seasonletter) {
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
  # % of Wins away out of away games played
  away_gamesplayed <- vector()
  for(i in playoff_teams) {
    home <- season[which(season$wteam == i), ]
    away <- season[which(season$lteam == i), ]
    num <- nrow(home[which(home$wloc == "A"), ])
    num_two <- nrow(away[which(away$wloc == "H"), ])
    away_gamesplayed <- c(away_gamesplayed, num + num_two)
  }
  away_wins <- season[which(season$wloc == "A"), ]
  awayWins_table <- as.data.frame(table(away_wins$wteam))
  awayWins_by_team <- awayWins_table[awayWins_table$Var1 %in% playoff_teams, ]
  awayWins_by_team <- missingPteam(awayWins_by_team, playoff_teams) #Missing Teams Check
  awayWins_by_team$Freq <- round(awayWins_by_team$Freq / away_gamesplayed, digits = 3)
  #Wins by margin of less than 2
  games_by_lt2 <- season[which(season$ptdiff <= 2), ]
  wins_lt2_table <- as.data.frame(table(games_by_lt2$wteam), stringsAsFactors = FALSE)
  wins_lt2_by_team <- wins_lt2_table[wins_lt2_table$Var1 %in% playoff_teams, ]
  wins_lt2_by_team <- missingPteam(wins_lt2_by_team, playoff_teams) #Missing Teams Check
  #Losses by margin of less than 2
  loss_lt2_table <- as.data.frame(table(games_by_lt2$lteam), stringsAsFactors = FALSE)
  loss_lt2_by_team <- loss_lt2_table[loss_lt2_table$Var1 %in% playoff_teams, ]
  loss_lt2_by_team <- missingPteam(loss_lt2_by_team, playoff_teams) #Missing Teams Check
  #Wins by margin of greater than 7
  games_by_gt7 <- season[which(season$ptdiff >= 7), ]
  wins_gt7_table <- as.data.frame(table(games_by_gt7$wteam), stringsAsFactors = FALSE)
  wins_gt7_by_team <- wins_gt7_table[wins_gt7_table$Var1 %in% playoff_teams, ]
  wins_gt7_by_team <- missingPteam(wins_gt7_by_team, playoff_teams) #MTC
  #Losses by margin of greater than 7
  loss_gt7_table <- as.data.frame(table(games_by_gt7$lteam), stringsAsFactors = FALSE)
  loss_gt7_by_team <- loss_gt7_table[loss_gt7_table$Var1 %in% playoff_teams, ]
  loss_gt7_by_team <- missingPteam(loss_gt7_by_team, playoff_teams) #MTC
  #Number of wins in last 4 weeks (NOT A USABLE VAR, must scale)
  last_four_season <- season[match(104, season$daynum):nrow(season), ]
  last_four_win_table <- as.data.frame(table(last_four_season$wteam), stringsAsFactors = FALSE)
  last_four_win_by_team <- last_four_win_table[last_four_win_table$Var1 %in% playoff_teams, ]
  last_four_win_by_team <- missingPteam(last_four_win_by_team, playoff_teams) #MTC
  #Number of losses in last 4 weeks (NOT A USABLEVAR, must scale)
  last_four_loss_table <- as.data.frame(table(last_four_season$lteam), stringsAsFactors = FALSE)
  last_four_loss_by_team <- last_four_loss_table[last_four_loss_table$Var1 %in% playoff_teams, ]
  last_four_loss_by_team <- missingPteam(last_four_loss_by_team, playoff_teams)
  #Win percentage in last 4 weeks
  last_four_winpct <- last_four_win_by_team$Freq / (last_four_loss_by_team$Freq + last_four_win_by_team$Freq)
  last_four_winpct <- as.data.frame(cbind(as.integer(last_four_win_by_team$Var1), round(as.numeric(last_four_winpct), digits = 3)))
  colnames(last_four_winpct) <- c("Var1", "Freq")
  #Number of wins against teams in the tournament (NOT A USABLEVAR, must scale)
  rankedwins <- data.frame()
  for(i in playoff_teams) {
    individ_team_wins <- season[which(season$wteam == i), ]
    wins <- sum(individ_team_wins$lteam %in% playoff_teams)
    vector <- c(i, wins)
    rankedwins <- rbind(rankedwins, vector)
  }
  colnames(rankedwins) <- c("Var1", "Freq")
  #Number of losses against teams in the tournament (NOT A USABLEVAR, must scale)
  rankedloss <- data.frame()
  for(i in playoff_teams) {
    individ_team_loss <- season[which(season$lteam == i), ]
    loss <- sum(individ_team_loss$wteam %in% playoff_teams)
    vector <- c(i, loss)
    rankedloss <- rbind(rankedloss, vector)
  }
  colnames(rankedloss) <- c("Var1", "Freq")
  #Win percentage against teams in the tournament
  rankedpercent <- data.frame()
  col <- round(rankedwins$Freq / (rankedwins$Freq + rankedloss$Freq), digits = 3)
  for(i in c(1:length(col))) {
    if(is.na(col[i])) {
      col[i] <- 0
    }
  }
  rankedpercent <- as.data.frame(cbind(rankedwins$Var1, col))
  colnames(rankedpercent) <- c("Var1", "Freq")
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
  team_metrics <- cbind(total_winpct_by_team, awayWins_by_team$Freq, wins_lt2_by_team$Freq,
                        loss_lt2_by_team$Freq, wins_gt7_by_team$Freq, loss_gt7_by_team$Freq,
                        last_four_winpct$Freq, rankedpercent$Freq, wins_last_six_games_by_team$Freq,
                        team_seed$Freq)
  
  colnames(team_metrics) <- c("TEAMID", "TWPCT", "AWPCT", "WLT2", "LLT2", "WGT7", "LGT7",
                              "PCT4WEEK", "RANKPCT", "WST6", "SEED")
  return(team_metrics)
}

###################################################################################################
###################################################################################################

##Function creates data frames in model form by season for training the model
# seasonletter = String ex) "A"
# teamMetrics = team metrics by season aka the dataframe returned by team_metrics_by_season("A")
library('stringr')
data_frame_model <- function(seasonletter) {
  teamMetrics <- team_metrics_by_season_vtwo(seasonletter)  ##CHANGE TO REG OR _VTWO
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
                                  "LGT7_A", "PCT4WEEK_A", "RANKPCT_A", "WST6_A", "SEED_A")
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

###################################################################################################
###################################################################################################

##Function creates data frames in model form by season for prediction
pred_frame_model <- function(season) {
  model_data_frame <- submissionFile(season)
  teamMetrics <- team_metrics_by_season_vtwo(season)  
  teamMetrics_away <- teamMetrics
  colnames(teamMetrics_away) <- c("TEAMID", "TWPCT_A", "AWPCT_A", "WLT2_A", "LLT2_A", "WGT7_A",
                                  "LGT7_A", "PCT4WEEK_A", "RANKPCT_A", "WST6_A", "SEED_A")
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





