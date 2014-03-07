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
write.csv(sub_file, file = "subblog.csv", row.names = FALSE)
