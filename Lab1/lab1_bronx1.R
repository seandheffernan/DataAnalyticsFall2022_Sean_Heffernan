library(gdata) 
#faster xls reader but requires perl!
#bronx1<-read.xls(file.choose(),pattern="BOROUGH",stringsAsFactors=FALSE,sheet=1,perl="<SOMEWHERE>/perl/bin/perl.exe") 
#bronx1<-bronx1[which(bronx1$GROSS.SQUARE.FEET!="0" & bronx1$LAND.SQUARE.FEET!="0" & bronx1$SALE.PRICE!="$0"),]

#alternate
library(xlsx)
bronx1<-read.xlsx("Documents/Fall_2023/Data_Analytics/DataAnalyticsFall2022_Sean_Heffernan/Data_Sets/rollingsales_bronx.xls",pattern="BOROUGH",stringsAsFactors=FALSE,sheetIndex=1,startRow=5,header=TRUE)
View(bronx1)
#
attach(bronx1) # If you choose to attach, leave out the "data=." in lm regression
SALE.PRICE<-sub("\\$","",SALE.PRICE) 
SALE.PRICE<-as.numeric(gsub(",","", SALE.PRICE)) 
GROSS.SQUARE.FEET<-as.numeric(gsub(",","", GROSS.SQUARE.FEET)) 
LAND.SQUARE.FEET<-as.numeric(gsub(",","", LAND.SQUARE.FEET)) 
plot(log(GROSS.SQUARE.FEET), log(SALE.PRICE)) 
logGrossFeet <- log(GROSS.SQUARE.FEET)
logGrossFeet[which(!is.finite(logGrossFeet))] <- 0
logSalePrice <- log(SALE.PRICE)
logSalePrice[which(!is.finite(logSalePrice))] <- 0
m1<-lm(logSalePrice~logGrossFeet)
summary(m1)
abline(m1,col="red",lwd=2)
plot(resid(m1))

# Model 2

logSquareFeet <- log(bronx1$LAND.SQUARE.FEET)
logSquareFeet[which(!is.finite(logSquareFeet))] <- 0
m2<-lm(logSalePrice~logGrossFeet+logSquareFeet+factor(bronx1$NEIGHBORHOOD))
summary(m2)
plot(resid(m2))
# Suppress intercept - using "0+ ..."
m2a<-lm(logSalePrice~0+logGrossFeet+logSquareFeet+factor(bronx1$NEIGHBORHOOD))
summary(m2a)
plot(resid(m2a))

# Model 3
m3<-lm(logSalePrice~0+logGrossFeet+logSquareFeet+factor(bronx1$NEIGHBORHOOD)+factor(bronx1$BUILDING.CLASS.CATEGORY))
summary(m3)
plot(resid(m3))

# Model 4
m4<-lm(logSalePrice~0+logGrossFeet+logSquareFeet+factor(bronx1$NEIGHBORHOOD)*factor(bronx1$BUILDING.CLASS.CATEGORY))
summary(m4)
plot(resid(m4))
#

#Fixed linear models to remove NaN & Inf values
