setwd("/Users/Brian_Liou/Documents/Startup/kaggle_march_madness")
source('utility.R')
library('e1071')

upsets <- read.csv("data/upsets.csv", header = T)
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
for(i in LETTERS[1:13]) {
  train_data <- rbind(train_data, data_frame_model(i))
}

## Creating test data with last 5 seasons
# N O P Q R

N_season <- pred_frame_model("N")
O_season <- pred_frame_model("O")
P_season <- pred_frame_model("P")
Q_season <- pred_frame_model("Q")
R_season <- pred_frame_model("R")

test_data <- rbind(N_season, O_season, P_season, Q_season, R_season)

## Fitting GLM
train.glm <- glm(Win ~ AWPCT + RANKPCT + WST6
                   AWPCT_A + RANKPCT_A,
                 family = binomial, data = train_data)
library('rpart')
## Classification Tree
train.rpart <- rpart(Win ~ AWPCT + PCT4WEEK + RANKPCT + WST6 + SEED +
                       AWPCT_A + PCT4WEEK_A + RANKPCT_A + WST6_A + SEED_A,
                     data = train_data, method = "class",
                     control=rpart.control(minsplit=30, cp=0.001))
## Random Forest
train.rf <- randomForest(Win ~ AWPCT + PCT4WEEK + RANKPCT + WST6 + SEED + UPSET +
                           AWPCT_A + PCT4WEEK_A + RANKPCT_A + WST6_A + SEED_A + UPSET_A,
                         data = train_data, importance = TRUE, ntree =1501)
## K-NN
train.knn <- knn(train_data, test_data, as.factor(train_data$Win), prob =T)


#Predictions from GLM
p.hats.glm <- predict.glm(train.glm, newdata = test_data, type = "response")

#Predictions from SVM
test_data$Win <- 0
train.svm <- svm(as.factor(Win) ~ AWPCT + PCT4WEEK + RANKPCT + WST6 + SEED +
                   AWPCT_A + PCT4WEEK_A + RANKPCT_A + WST6_A + SEED_A, data = train_data,
                 probability = TRUE)
#Predictions from Classification Tree
p.hats.rpart <- predict(train.rpart, newdata = test_data, type = "prob")
p.hats.rpart <- p.hats.rpart[ , 1]

#Predictions from randomForest
p.hats.rf <- predict(train.rf, newdata= test_data)

#Predictions from SVM
p.hats.svm <- predict(train.svm, newdata=test_data, probability = TRUE)
p.hats.svm <- attr(p.hats.svm,"prob")[ , 1]


wins <- vector()
for(i in 1:length(train.pred)) {
  if(train.pred[i] > .5) {
    wins[i] <- 1
  } else {
    wins[i] <- 0
  }
}

# 1112 1049 541 322 83

##Ensemble model predictions
ensemble.p.hats <- (p.hats.rf +p.hats.rpart) / 2

subfile <- data.frame("id" = test_data$Matchup, "pred"= p.hats.rf)

write.csv(subfile, file = "sub16.csv", row.names = FALSE)

#Sub2.csv
train.glm <- glm(Win ~ AWPCT + PCT4WEEK + RANKPCT + WST6 + SEED + TWIN +
                   AWPCT_A + PCT4WEEK_A + RANKPCT_A + WST6_A + SEED_A + TWIN_A,
                 family = binomial, data = train_data)
#Sub3.csv
train.glm <- glm(Win ~ AWPCT + PCT4WEEK + RANKPCT + WST6 +
                   AWPCT_A + PCT4WEEK_A + RANKPCT_A + WST6_A,
                 family = binomial, data = train_data)

#Sub4.csv KILLER ONE
train.rpart <- rpart(Win ~ AWPCT + PCT4WEEK + RANKPCT + WST6 + SEED + UPSET +
                       AWPCT_A + PCT4WEEK_A + RANKPCT_A + WST6_A + SEED_A + UPSET_A,
                     data = train_data, method = "class",
                     control=rpart.control(minsplit=30, cp=0.001))

#Sub10.csv scored .56 error
train.svm <- svm(as.factor(Win) ~ AWPCT + PCT4WEEK + RANKPCT + WST6 + SEED +
                   AWPCT_A + PCT4WEEK_A + RANKPCT_A + WST6_A + SEED_A, data = train_data,
                 probability = TRUE)

## Remove wins that are not against top 64 teams
## Road and neutral wins are critical for selection committee

## Find the top 15 teams with "experienced coaches" and label them with an indicator variable
## Try an ensemble method where you average the results from Logistic/ClassificationTree/Rf


### Upset analysis
upset_frame <- rbind(
train_data[which(train_data$Matchup == "D_708_842"), ],
train_data[which(train_data$Matchup == "F_610_629"), ],
train_data[which(train_data$Matchup == "B_560_769"), ],
train_data[which(train_data$Matchup == "C_826_674"), ],
train_data[which(train_data$Matchup == "A_736_810"), ],
train_data[which(train_data$Matchup == "K_716_628"), ],
train_data[which(train_data$Matchup == "J_786_828"), ],
train_data[which(train_data$Matchup == "J_533_636"), ],
train_data[which(train_data$Matchup == "K_559_602"), ],
train_data[which(train_data$Matchup == "M_766_827"), ],
train_data[which(train_data$Matchup == "M_568_603"), ]
)

## Add an indicator variable when WST6 is 4 or greater && AWPCT is > .550 && Seed >= 8

train_data$UPSET <- NA
train_data$UPSET_A <- NA
for(i in 1:nrow(train_data)) {
  if((train_data$AWPCT[i] > .550 & train_data$WST6[i] >= 4 & train_data$SEED[i] >= 8)) {
    train_data$UPSET[i] <- 1
    #print("HI")
  } else {
    train_data$UPSET[i] <- 0
  }
  if((train_data$AWPCT_[i] > .550 & train_data$WST6_A[i] >= 4 & train_data$SEED_A[i] >= 8)) {
    train_data$UPSET_A[i] <- 1
  } else {
    train_data$UPSET_A[i] <- 0
  }
}


## APPLYING variable to test_data
test_data$UPSET <- NA
test_data$UPSET_A <- NA
for(i in 1:nrow(test_data)) {
  if((test_data$AWPCT[i] > .550 & test_data$WST6[i] >= 4 & test_data$SEED[i] >= 8)) {
    test_data$UPSET[i] <- 1
    #print("HI")
  } else {
    test_data$UPSET[i] <- 0
  }
  if((test_data$AWPCT_[i] > .550 & test_data$WST6_A[i] >= 4 & test_data$SEED_A[i] >= 8)) {
    test_data$UPSET_A[i] <- 1
  } else {
    test_data$UPSET_A[i] <- 0
  }
}

upsetcheck <- data.frame()
for(i in 1:nrow(upsets)) {
  value <- paste(upsets$X[i], "_", upsets$ID[i], "_", upsets$ID.1[i], sep= "")
  valuee <- paste(upsets$X[i], "_", upsets$ID.1[i], "_", upsets$ID[i], sep= "")
  if (nrow(train_data[which(train_data$Matchup == value), ]) > 0) {
    upsetcheck <- rbind(upsetcheck, value)
    print("HI")
  } else {
    upsetcheck <- rbind(upsetcheck, valuee)
  }
}






