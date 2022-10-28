sales <- read.csv("Documents/Fall_2023/Data_Analytics/DataAnalyticsFall2022_Sean_Heffernan/Data_Sets/NYC_Citywide_Annualized_Calendar_Sales_Update.csv")

# I will specifically be looking at Manhattan for this assignment
manhattan_sales <- sales[sales$BOROUGH == 1, ]
View(manhattan_sales)

#1a
boxplot(manhattan_sales$YEAR.BUILT)
hist(manhattan_sales$SALE.PRICE)


#1b

#1c

#1d

#2a

#2b

#2c

#3