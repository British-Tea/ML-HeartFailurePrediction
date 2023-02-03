# Install needed packages-------------------------------------------------------
install.packages("dplyr")
install.packages("tidyverse")
install.packages("class")
library(dplyr)
library(tidyverse)
library(corrplot)
library(class)


# import heart.csv
heart <- read.csv("C:/Users/ash_w/OneDrive/Documents/#Work/Machine Learning/project/RStudio/Heart Failure Prediction Model/heart.csv")

# looking at the datatypes in data set
str(heart)


# Analyzing data ---------------------------------------------------------------
  
# Age analysis
boxplot(Age~HeartDisease,
        data=heart,
        xlab="Heart Disease", 
        ylab="Age")

ggplot(heart)+
  geom_violin(aes(HeartDisease, Age,
                  fill= HeartDisease== 1))

ggplot(heart)+
  geom_histogram(aes(Age))+
  facet_wrap(~ HeartDisease)

# Sex analysis
ggplot(heart)+
  geom_bar(aes(Sex,
               fill= HeartDisease == 1),
           position= "dodge")

# ChestPainType analysis
ggplot(heart)+
  geom_bar(aes(ChestPainType,
               fill= HeartDisease == 1),
           position= "dodge")


# FastingBS analysis
ggplot(heart)+
  geom_bar(aes(FastingBS == 1,
               fill= HeartDisease == 1),
           position= "dodge")

# RestingECG analysis
ggplot(heart)+
  geom_bar(aes(RestingECG,
               fill= HeartDisease == 1),
           position= "dodge")

# ExerciseAngina analysis
ggplot(heart)+
  geom_bar(aes(ExerciseAngina,
               fill= HeartDisease == 1),
           position= "dodge")

# RestingBP analysis
boxplot(RestingBP~HeartDisease,
        data=heart, 
        main="resting bp and heart disease",
        xlab="Heart disease", 
        ylab="resting bp")

ggplot(heart)+
  geom_violin(aes(HeartDisease, RestingBP,
                  fill= HeartDisease== 1))

ggplot(heart)+
  geom_histogram(aes(RestingBP))+
  facet_wrap(~ HeartDisease)

# Cholesterol analysis
boxplot(Cholesterol~HeartDisease,
        data=heart, 
        main="cholesterol and heart disease",
        xlab="Heart disease", 
        ylab="cholesterol")

ggplot(heart)+
  geom_violin(aes(HeartDisease, Cholesterol,
                  fill= HeartDisease== 1))

ggplot(heart)+
  geom_histogram(aes(Cholesterol))+
  facet_wrap(~ HeartDisease)

# MaxHR analysis
boxplot(MaxHR~HeartDisease,
        data=heart, 
        main="maxHR and heart disease",
        xlab="Heart disease", 
        ylab="maxHR")

ggplot(heart)+
  geom_violin(aes(HeartDisease, MaxHR,
                  fill= HeartDisease== 1))

ggplot(heart)+
  geom_histogram(aes(MaxHR))+
  facet_wrap(~ HeartDisease)

# Oldpeak analysis
boxplot(Oldpeak~HeartDisease,
        data=heart, 
        main="oldpeak and heart disease",
        xlab="Heart disease", 
        ylab="oldpeak")

ggplot(heart)+
  geom_violin(aes(HeartDisease, Oldpeak,
                  fill= HeartDisease== 1))

ggplot(heart)+
  geom_histogram(aes(Oldpeak))+
  facet_wrap(~ HeartDisease)

# STSlope analysis
ggplot(heart)+
  geom_bar(aes(ST_Slope,
               fill= HeartDisease == 1),
           position= "dodge")


# Cleaning data
heart$Sex <- as.numeric(as.factor(heart$Sex))
heart$ST_Slope <- as.numeric(as.factor(heart$ST_Slope))
heart$ChestPainType <- as.numeric(as.factor(heart$ChestPainType))
heart$ExerciseAngina <- as.numeric(as.factor(heart$ExerciseAngina))
heart$RestingECG <- as.numeric(as.factor(heart$RestingECG))

str(heart)
# Finding correlation in normalized data
cordat <- cor(heart)
corrplot(cordat, type = "lower", order = "original")




# Normalizing the data----------------------------------------------------------
normalize <- function(x) {
  return((x-min(x)) / (max(x) - min(x))) }

heart.norm <- as.data.frame(lapply(heart, normalize))


str(heart.norm)

# Splitting data into test and train--------------------------------------------
heart.sub <- subset(heart, select = c(Age, 
                                      Sex,
                                      ExerciseAngina, 
                                      Oldpeak,
                                      ST_Slope,
                                      ChestPainType,
                                      HeartDisease))


set.seed(123)
split <- sample(1:nrow(heart.norm),
                size=nrow(heart.norm)*0.8,replace = FALSE)

train.heart <- heart.sub[split,]

test.heart <- heart.sub[-split,]

# Separating heartFailure column
train.heart.failure <- heart.sub[split, 7]

test.heart.failure <- heart.sub[-split, 7]


# Training Model
knn.27 <- knn(train= train.heart,
              test = test.heart,
              cl   = train.heart.failure,
              k    = 27)

knn.28 <- knn(train= train.heart,
              test = test.heart,
              cl   = train.heart.failure,
              k    = 28)

# Testing The Model
ACC.27 <- 100 * sum(test.heart.failure == knn.27)/NROW(test.heart.failure)
ACC.28 <- 100 * sum(test.heart.failure == knn.28)/NROW(test.heart.failure)

ACC.27
ACC.28

# Repeat's training and testing for best accuracy
i=1
k.value=1
  for (i in 1:29){ 
    knn.mod <- knn(train=train.heart, test=test.heart, cl=train.heart.failure, k=i)
    k.value[i] <- 100 * sum(test.heart.failure == knn.mod)/NROW(test.heart.failure)
      cat(i," = ", k.value[i],"
")}
plot(k.value, type="b", xlab="K- Value",ylab="Accuracy level")

# Testing 3rd
knn.3 <- knn(train= train.heart,
              test = test.heart,
              cl   = train.heart.failure,
              k    = 3)
ACC.3 <- 100 * sum(test.heart.failure == knn.3)/NROW(test.heart.failure)
ACC.3


# Testing other values ---------------------------------------------------------

heart.sub2 <- subset(heart, select = c(ExerciseAngina, 
                                       Oldpeak,
                                       ST_Slope,
                                       ChestPainType,
                                       FastingBS,
                                       HeartDisease))

heart.sub2$Sex <- as.numeric(as.factor(heart.sub2$Sex))
heart.sub2$RestingECG <- as.numeric(as.factor(heart.sub2$RestingECG))
heart.sub2$ST_Slope <- as.numeric(as.factor(heart.sub2$ST_Slope))
heart.sub2$ChestPainType <- as.numeric(as.factor(heart.sub2$ChestPainType))
heart.sub2$ExerciseAngina <- as.numeric(as.factor(heart.sub2$ExerciseAngina))

heart.norm <- as.data.frame(lapply(heart.sub2, normalize))


# Splitting data into test and train
set.seed(123)
split <- sample(1:nrow(heart.norm),
                size=nrow(heart.norm)*0.8,replace = FALSE)

train.heart2 <- heart.sub2[split,]

test.heart2 <- heart.sub2[-split,]


# Separating heartFailure column
train.heart.failure2 <- heart.sub2[split, ncol(heart.norm)]

test.heart.failure2 <- heart.sub2[-split, ncol(heart.norm)]



i=1
k.value=1
for (i in 1:29){ 
  knn.mod <- knn(train=train.heart2, test=test.heart2, cl=train.heart.failure2, k=i)
  k.value[i] <- 100 * sum(test.heart.failure2 == knn.mod)/NROW(test.heart.failure2)
  cat(i," = ", k.value[i],"
")}
plot(k.value, type="b", xlab="K- Value",ylab="Accuracy level")

