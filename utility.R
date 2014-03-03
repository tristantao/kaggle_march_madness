#Need to write a utility function which adds back the missing playoff_team if they aren't
#included in the statistic. For ex. Neutral Wins by Team. Always run this utility function
#when calculating all of the statistics to make sure you have the data for all of the playoff teams
#basically just adding c(teamID, 0) for when playoff_team %in% ________ is FALSE when utility function
# != 64

missingPteam <- function(statistic) {
  vector <- playoff_teams %in% statistic$Var1
  missTeam <- data.frame()
  if (sum(vector) != 64) {
    for(i in c(1:length(vector))) {
      if (vector[i] == F) {
        insert <- c(playoff_teams[i],0)
        missTeam <- rbind(missTeam, insert)
      }
    }
    colnames(missTeam) <- c("Var1", "Freq")
  }
  fullTeam <- rbind(statistic, missTeam)
  fullTeam <- fullTeam[order(fullTeam$Var1), ]
  return(fullTeam)
}