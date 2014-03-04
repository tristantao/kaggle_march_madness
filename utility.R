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
  if (sum(vector) != 64) {
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

##Function which creates the submission file for any given season
# numTeams = number of teams in playoff season
# playoffTeams = vector of team IDs that are in the tournament
# season = String for which season, ex) "A"
submissionFile <- function(numTeams, playoffTeams, season) {
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
  form <- data.frame("id" = idcol, "pred" = NA)
  return(form)
}