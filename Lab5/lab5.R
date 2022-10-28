library(ISLR)
library(dplyr)
head(Hitters)
dim(Hitters)
is.na(Hitters)
HittersData <- na.omit(Hitters)
dim(HittersData)
glimpse(HittersData)
head(HittersData)
SalaryPredictModel1 <- lm(Salary ~., data = HittersData)
summary(SalaryPredictModel1)

cooksD <- cooks.distance(SalaryPredictModel1)
influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
influential

names_of_influential <- names(influential)
names_of_influential
outliers <- HittersData[names_of_influential,]
Hitters_Without_Outliers <- HittersData %>% anti_join(outliers)

SalaryPredictModel2 <- lm(Salary ~. , data = Hitters_Without_Outliers)
summary(SalaryPredictModel2)

#Validation set example with Auto dataset
library(ISLR)
data("Auto")
library(MASS)
library(boot)
set.seed(1)
??cv.glm

help("sample")
train = sample(392, 196)
lm.fit <- lm(mpg~horsepower, data=Auto, subset=train)
attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)

#Quadratic Regression Line
lm.fit2 <- lm(mpg~poly(horsepower, 2), data=Auto, subset=train) #Quadratic
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

#Cubic regression line
lm.fit3 <- lm(mpg~poly(horsepower, 3), data=Auto, subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

set.seed(2)
train = sample(392, 196)
lm.fit <- lm(mpg~horsepower, data=Auto, subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)

lm.fit2 <- lm(mpg~poly(horsepower, 2), data=Auto, subset=train) #Quadratic
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

lm.fit3 <- lm(mpg~poly(horsepower, 3), data=Auto, subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)



# K-Fold Example with Auto dataset
library(boot)
set.seed(17)
help("rep")
cv.error.10 = rep(0, 10)
for(i in 1:10){
  glm.fit = glm(mpg~poly(horsepower, i), data = Auto)
  cv.error.10[i] = cv.glm(Auto, glm.fit, K=10) $delta[1]
}
cv.error.10


