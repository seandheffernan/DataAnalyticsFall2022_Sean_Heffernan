sales <- read.csv("Documents/Fall_2023/Data_Analytics/DataAnalyticsFall2022_Sean_Heffernan/Data_Sets/NYC_Citywide_Annualized_Calendar_Sales_Update.csv")

# I will specifically be looking at Manhattan for this assignment
manhattan_sales <- sales[sales$BOROUGH == 1, ]
View(manhattan_sales)

#1a
# To conduct an effective data analysis, I should be looking at trends surrounding the sales of buildings in Manhattan by looking
# at various factors which might influence final sales price. This includes features like the zip code or latitude/longitude (location may indicate 
# higher or lower-end neighborhood), size of the property (in square feet) and building class. To that end,
# I will be investigating these features individually first, to see if I can come to any conclusions surrounding them.
# For example, to make these simple I will be looking at the sizes of the properties and the building class to see
# if there are any trends surrounding purchases made - are more buildings office buildings? Store buildings? Etc.
# How large are the purchase sizes (square feet-wise), and does that influence the total cost? I used both
# histograms and boxplots to visualize these data points as well as the summary method to view specific stats.
boxplot(manhattan_sales$YEAR.BUILT)
summary(manhattan_sales$YEAR.BUILT)
# The average year of building being built was 1745. Interestingly, some data points have NA as a year built while others have 0 - maybe the pre-processing for this data set was inconsistent?
hist(manhattan_sales$SALE.PRICE)
summary(manhattan_sales$SALE.PRICE)
# The most expensive property was a whopping 2 billion dollars on 75 9 Avenue. Doing some outside research, it appears
# the building was bought by Google for office space.

#1b
boxplot(manhattan_sales$SALE.PRICE)
summary(manhattan_sales$SALE.PRICE)
# IQR has been calculated by getting the 1st and 3rd quartiles via the summary() method and getting the difference
# between the two. 1860000 - 83240 = 1776760.
# Lower Inner Fence: 83240 - 1.5*1776760 = -2581900
# Upper Inner Fence: 1860000 + 1.5*1776760 = 4525140
# Outliers found by getting values outside the lower and upper inner fences. There are 7312 outliers
lower_fence <- manhattan_sales$SALE.PRICE[manhattan_sales$SALE.PRICE < -2581900]
upper_fence <- manhattan_sales$SALE.PRICE[manhattan_sales$SALE.PRICE > 4525140]

#1c
library(dplyr)
library(tidyr)
# Convert sales to numerics (since they were strings before)
manhattan_sales$LAND.SQUARE.FEET = as.numeric(gsub(",","", manhattan_sales$LAND.SQUARE.FEET))
manhattan_sales$GROSS.SQUARE.FEET = as.numeric(gsub(",","", manhattan_sales$GROSS))

# Remove NA and 0 values from land and gross square feet
manhattan_clean <- filter(manhattan_sales, LAND.SQUARE.FEET != 0 | GROSS.SQUARE.FEET != 0)
manhattan_clean <- manhattan_clean %>% drop_na(LAND.SQUARE.FEET, GROSS.SQUARE.FEET)
View(manhattan_clean)

# Create regression model 1
train = sample(15900, 7500)
model1 <- lm(SALE.PRICE ~ LAND.SQUARE.FEET + GROSS.SQUARE.FEET, data = manhattan_clean, subset=train)
summary(model1)
mean((manhattan_clean$SALE.PRICE-predict(model1,manhattan_clean))[-train]^2)

# Create regression model 2
train2 = sample(15900, 7500)
model2 <- lm(SALE.PRICE ~ LAND.SQUARE.FEET + GROSS.SQUARE.FEET, data = manhattan_clean, subset=train2)
summary(model2)
mean((manhattan_clean$SALE.PRICE-predict(model2,manhattan_clean))[-train2]^2)

# Create regression model 3
train3 = sample(15900, 7500)
model3 <- lm(SALE.PRICE ~ LAND.SQUARE.FEET + GROSS.SQUARE.FEET, data = manhattan_clean, subset=train3)
summary(model3)
mean((manhattan_clean$SALE.PRICE-predict(model3,manhattan_clean))[-train3]^2)

# MSE of each result were extraordinarily high and the multiple r squared values were extremely low (MSE was
# the quadrillions lol and R squared was below 0.4 indicating low correlation). What this tells me is that 
# the Land and Gross square feet of a property aren't nearly enough to predict what the potential price of 
# a property will be. This comes to no surprise considering the different locations of properties around Manhattan
# areas with higher cost of living compared to others. For each sample of data I took roughly half of the dataset
# and used it as training data. While there were some variations on the R squared value as well as the MSE,
# they remain consistently around the same values.

#1d
# Multiple steps needed to be taken in order to clean the data (many of which I did in 1c also). For starters,
# I needed to be conscious of data which *appeared* to be legible but in fact were being read incorrectly by R.
# For example, the Land and Gross square feet of properties were strings and not numerics, so couldn't be properly used
# as training features until being parsed out as numerics. Additionally, many of the columns have NA values or just 0
# as a value, which can lead to an inaccurate model. For that sake, it is necessary to clean out and remove those rows
# if the feature I wish to look at has 0 or NA. Finally, it would be good to remove the outliers for 
# the sale price, as these could negatively influence the model. As far as model fits go, Multivariate regression w/ Land and
# Gross square feet is not enough to create a fitting model for this data as proven in the previous question. Due to the accurate
# nature of Random Forest due to the many different regressions used, I will use that to model the same data
# as before and see if the same conclusions are met.

#2a
install.packages("randomForest")
library(randomForest)
set.seed(1)
train <- sample(nrow(manhattan_clean), 0.7*nrow(manhattan_clean), replace = FALSE)
TrainSet <- manhattan_clean[train,]
ValidSet <- manhattan_clean[-train,]
summary(TrainSet)
summary(ValidSet)

model <- randomForest(SALE.PRICE ~ LAND.SQUARE.FEET + GROSS.SQUARE.FEET, data=manhattan_clean, importance=TRUE)
predTrain <- predict(model, TrainSet, type = "class")
table(predTrain, TrainSet$SALE.PRICE)
predValid <- predict(model, ValidSet, type = "class")
print(mean((predValid-ValidSet$SALE.PRICE)^2))
table(predValid, ValidSet$SALE.PRICE)
importance(model)
varImpPlot(model)
# After printing out the mean squared error (not as high as multivariate regression but still very high
# in the hundred billions), we can see that using land square feet and gross square feet alone is still
# inaccurate to properly predict sale price. However, the random forest helps us determine that the gross
# square feet is more important compared to land square feet as far as an increase in MSE. By using varImpPlot()
# it is clear that the removal of GROSS SQUARE FEET leads to the largest percentage in increased MSE (around 26%)
# while the removal of LAND SQUARE FEET in the prediction only increases MSE by 7.36%

#2b
model
# As accomplished in the previous step, we can verify the fit of the model by looking at the MSE of the
# validation set. I calculated the MSE to be in the hundred billions (6.051824e+14 to be exact in my case),
# which is extremely high, indicating that these two variables are not enough to accurately predict SALE.PRICE.
# The r squared value supports this theory, with an R squared value of 29.74 (the % Var explained value).
# Overall, the MSE is much lower compared to the multivariate regression I performed in 1c, but the previous
# discovery that GROSS and LAND SQUARE FEET are not enough to properly predict sales price remains true.

#2c
# Overall I found it frustrating that the types of variables for each feature varied even when they probably shouldn't have
# (the prime example here is the strings for land and gross square feet vs the already numeric prices). Without proper
# discovery beforehand it can lead to hours wasted trying to debug when models when perhaps it's the data which was inaccurate.
# This just goes to show how important preprocessing and data cleaning is when it comes to large datasets. When first conducting
# the multivariate regression, I was very unsure about the results I was getting due to the obscenely high MSE values
# and low R squared values. However, after doing some more thinking (it is unlikely for two variables in a 30 column dataset toZ predict
# EVERYTHING) and the follow-up random forest model, I was more confident with my conclusions.

#3
# Of the two model types, random forest seems to outperform multivariate regression in this case. This 
# isn't that surprising considering one of random forest's strengths being good to create estimates
# for missing data (in our case we only used 2 features which made up for a small fraction of the data).
# Before resorting to random forest, I had also tried KMeans clustering for 2a to try and make predictions
# for sale price. However, there is no native predict() method that works with KMeans, so I couldn't use it 
# for regression as I had intended for. That being said, plotting out the KMeans was still useful for visualizing 
# the relationship between price and gross square feet and identifying some clustering - but again, it couldn't 
# exactly be used to predict the desired label (predicting prices for certain gross & land square feet). As an overall 
# conclusion, it's apparent that a fine balance needed to be struck between too many and too little features being used
# when creating an accurate regression model in efficient time. Too many features and the model creation might take too long,
# too few features and you risk an inaccurate model (like the one we put together during this assignment).






