setwd("/Users/Brian_Liou/Documents/Startup/kaggle_march_madness/")
source('utility.R')

regSeason <- read.csv("data/regular_season_results.csv", header = TRUE, stringsAsFactors = FALSE)
regSeason$ptdiff <- regSeason$wscore - regSeason$lscore
seasons <- read.csv("data/seasons.csv", header = TRUE, stringsAsFactors = FALSE)
teams <- read.csv("data/teams.csv", header = TRUE, stringsAsFactors = FALSE)
tourneyRes <- read.csv("data/tourney_results.csv", header = TRUE, stringsAsFactors = FALSE)
tourneySeeds <- read.csv("data/tourney_seeds.csv", header = TRUE, stringsAsFactors = FALSE)
tourneySlots <- read.csv("data/tourney_slots.csv", header = TRUE, stringsAsFactors = FALSE)
library('stringr')
library('randomForest')

## Visualizations for exploratory analysis
plot(season_a$SEED, season_a$AWPCT, xlim = c(0,17), ylim = c(0,1))
summary(season_a$AWPCT)
vector_TWPCT <- as.numeric(as.vector(season_a$TWPCT))
plot(season_a$SEED, vector_TWPCT, xlim = c(0,17), ylim = c(0,1))
summary(vector_TWPCT)

a_season <- team_metrics_by_season("A")
## GLM MODEL

##### STATISTICAL ANALYSIS

## Creating master train data with all seasons
train_data <- data.frame()
for(i in LETTERS[1:18]) {
  train_data <- rbind(train_data, data_frame_model(i))
}

## Fitting GLM
train.glm <- glm(Win ~ AWPCT + PCT4WEEK + RANKPCT + WST6 +
                   AWPCT_A + PCT4WEEK_A + RANKPCT_A + WST6_A,
                 family = binomial, data = train_data)
library('rpart')
## Classification Tree
train.rpart <- rpart(Win ~ AWPCT + PCT4WEEK + RANKPCT + WST6 + SEED +
                       AWPCT_A + PCT4WEEK_A + RANKPCT_A + WST6_A + SEED_A,
                     data = train_data, method = "class",
                     control=rpart.control(minsplit=30, cp=0.001))
## Random Forest
train.rf <- randomForest(Win ~ AWPCT + PCT4WEEK + RANKPCT + WST6 + SEED +
                           AWPCT_A + PCT4WEEK_A + RANKPCT_A + WST6_A + SEED_A,
                         data = train_data, importance = TRUE, ntree =3000)

## Creating test data with last 5 seasons
# N O P Q R

N_season <- pred_frame_model("N")
O_season <- pred_frame_model("O")
P_season <- pred_frame_model("P")
Q_season <- pred_frame_model("Q")
R_season <- pred_frame_model("R")

test_data <- rbind(N_season, O_season, P_season, Q_season, R_season)

#Predictions from GLM
p.hats <- predict.glm(train.glm, newdata = test_data, type = "response")

#Predictions from Classification Tree
p.hats.rpart <- predict(train.rpart, newdata = test_data, type = "prob")
p.hats.rpart <- p.hats.rpart[ , 1]

#Predictions from randomForest
p.hats.rf <- predict(train.rf, newdata= test_data)

wins <- vector()
for(i in 1:length(p.hats.rf)) {
  if(p.hats.rpart[i] > .5) {
    wins[i] <- 0
  } else {
    survival[i] <- 1
  }
}

for(i in 1:5) {
  print(i)
}

##Ensemble model predictions
ensemble.p.hats <- (p.hats.rpart + p.hats.rf) / 2

subfile <- data.frame("id" = test_data$Matchup, "pred"= p.hats.rf)

write.csv(subfile, file = "sub9.csv", row.names = FALSE)

#Sub2.csv
train.glm <- glm(Win ~ AWPCT + PCT4WEEK + RANKPCT + WST6 + SEED +
                   AWPCT_A + PCT4WEEK_A + RANKPCT_A + WST6_A + SEED_A,
                 family = binomial, data = train_data)
#Sub3.csv
train.glm <- glm(Win ~ AWPCT + PCT4WEEK + RANKPCT + WST6 +
                   AWPCT_A + PCT4WEEK_A + RANKPCT_A + WST6_A,
                 family = binomial, data = train_data)

#Sub4.csv KILLER ONE
train.rpart <- rpart(Win ~ AWPCT + PCT4WEEK + RANKPCT + WST6 + SEED +
                       AWPCT_A + PCT4WEEK_A + RANKPCT_A + WST6_A + SEED_A,
                     data = train_data, method = "class",
                     control=rpart.control(minsplit=30, cp=0.001))


## Remove wins that are not against top 64 teams
## Road and neutral wins are critical for selection committee

## Find the top 15 teams with "experienced coaches" and label them with an indicator variable
## Try an ensemble method where you average the results from Logistic/ClassificationTree/Rf