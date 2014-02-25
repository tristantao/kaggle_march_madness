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
#  Num of wins in last 4 weeks or % of wins in last 4 weeks
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
neutWins_table <- as.data.frame(table(neut_wins$wteam),stringsAsFactors = FALSE)
neutWins_by_team <- neutWins_table[neutWins_table$Var1 %in% playoff_teams, ]
# SOME TEAMS had 0 Neutral games, namely teams: 539|551|625|721|701|662
datone <- c(539, 0)
dattwo <- c(551, 0)
dathree <- c(625, 0)
datfour <- c(721, 0)
datfive <- c(701, 0)
datsix <- c(662, 0)

# Need to add these teams to the neutral wins table
neutWins_by_team <- rbind(neutWins_by_team, datone,dattwo,dathree,datfour,datfive,datsix)
neutWins_by_team_cor <- neutWins_by_team[order(neutWins_by_team$Var1), ]

# Check to see calculations are correct
wins_by_team$Freq #Should be the same as
awayWins_by_team$Freq + homeWins_by_team$Freq + neutWins_by_team$Freq

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

#Figure out which row is daynum = 104 (4 weeks before last day daynum = 132)
match(104, season_a$daynum)

last_three_season_a <- season_a[match(104, season_a$daynum):nrow(season_a), ]
last_three_win_table <- as.data.frame(table(last_three_season_a$wteam))
last_three_win_by_team <- last_three_win_table[last_three_win_table$Var1 %in% playoff_teams, ]
last_three_loss_table <- as.data.frame(table(last_three_season_a$lteam))
last_three_loss_by_team <- last_three_loss_table[last_three_loss_table$Var1 %in% playoff_teams, ]

#Need to write a utility function which adds back the missing playoff_team if they aren't
#included in the statistic. For ex. Neutral Wins by Team. Always run this utility function
#when calculating all of the statistics to make sure you have the data for all of the playoff teams
#basically just adding c(teamID, 0) for when playoff_team %in% ________ is FALSE when utility function
# != 64


missingPteam <- function(statistic) {
  vector <- playoff_teams %in% statistic$Var1
  missTeam <- data.frame()
  if (sum(vector) != 64) {
    for(i in length(vector)) {
      if (vector[i] == F) {
        missTeam <- rbind(missTeam, c(playoff_teams[i], 0))
      }
    }
    colnames(missTeam) <- c("Var1", "Freq")
  }
  fullTeam <- rbind(statistic, missTeam)
  fullTeam <- fullTeam[order(fullTeam$Var1), ]
  return(fullTeam)
}
##Create matchup data in the same order as the submission order of the permutations of the
##teams will be

testfunct <- function(stat) {
  vector <- playoff_teams %in% stat$Var1
  print(vector)
}
## Should still be able to combine team data with matchup data to predict 0/1 for any matchup

## WHat woud trends by team over multiple seasons loook like?