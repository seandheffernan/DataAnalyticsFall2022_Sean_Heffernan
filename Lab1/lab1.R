# Getting familiar with R
days <- c('Mon', 'Tue', 'Wed', 'Thur', 'Fri', 'Sat', 'Sun')
temp <- c(28.0, 30.5, 32, 31.2, 29.3, 27.9, 26.4)
snowed <- c('T', 'T', 'F', 'F', 'T', 'T', 'F')
help("data.frame")
RPI_Weather_Week <- data.frame(days, temp, snowed)

RPI_Weather_Week
head(RPI_Weather_Week)

str(RPI_Weather_Week)
summary(RPI_Weather_Week)

RPI_Weather_Week[1,]
RPI_Weather_Week[,1]
RPI_Weather_Week[, 'snowed']
RPI_Weather_Week[,'days']
RPI_Weather_Week[, 'temp']
RPI_Weather_Week[1:5, c("days", "temp")]
RPI_Weather_Week$temp
subset(RPI_Weather_Week,subset=snowed==TRUE)

sorted.snowed <- order(RPI_Weather_Week[,'snowed'])
sorted.snowed
RPI_Weather_Week[sorted.snowed,]

# Reading EPI Data (Lab 1 Part 1)
EPI_data <- read.csv("Documents/Fall_2023/Data_Analytics/DataAnalyticsFall2022_Sean_Heffernan/Data_Sets/2010EPI_data.csv")
View(EPI_data)
attach(EPI_data) 	# sets the ‘default’ object
fix(EPI_data)
EPI
tf <- is.na(EPI)
E <- EPI[!tf]
summary(EPI) 	# stats
fivenum(EPI,na.rm=TRUE)
help(stem)
stem(EPI)		 # stem and leaf plot
help(hist)
hist(EPI)
hist(EPI, seq(30., 95., 1.0), prob=TRUE)
help(lines)
lines(density(EPI,na.rm=TRUE,bw=1.)) # or try bw=“SJ”
help(rug)
rug(EPI) 

# Linear basis and least-squares constraints
multivariate <- read.csv("Documents/Fall_2023/Data_Analytics/DataAnalyticsFall2022_Sean_Heffernan/Data_Sets/multivariate.csv")
attach(multivariate)
mm <- lm(Homeowners~Immigrant)
mm
