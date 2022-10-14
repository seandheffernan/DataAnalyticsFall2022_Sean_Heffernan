nyt4 <- read.csv("Documents/Fall_2023/Data_Analytics/DataAnalyticsFall2022_Sean_Heffernan/Data_Sets/nyt4.csv")
nyt8 <- read.csv("Documents/Fall_2023/Data_Analytics/DataAnalyticsFall2022_Sean_Heffernan/Data_Sets/nyt8.csv")
nyt9 <- read.csv("Documents/Fall_2023/Data_Analytics/DataAnalyticsFall2022_Sean_Heffernan/Data_Sets/nyt9.csv")
nyt12 <- read.csv("Documents/Fall_2023/Data_Analytics/DataAnalyticsFall2022_Sean_Heffernan/Data_Sets/nyt12.csv")
nyt13 <- read.csv("Documents/Fall_2023/Data_Analytics/DataAnalyticsFall2022_Sean_Heffernan/Data_Sets/nyt13.csv")
nyt22 <- read.csv("Documents/Fall_2023/Data_Analytics/DataAnalyticsFall2022_Sean_Heffernan/Data_Sets/nyt22.csv")
nyt23 <- read.csv("Documents/Fall_2023/Data_Analytics/DataAnalyticsFall2022_Sean_Heffernan/Data_Sets/nyt23.csv")

#1a
boxplot(nyt4$Age, nyt4$Clicks)
boxplot(nyt8$Age, nyt8$Clicks)
boxplot(nyt9$Age, nyt9$Clicks)
boxplot(nyt12$Age, nyt12$Clicks)
boxplot(nyt13$Age, nyt13$Clicks)
boxplot(nyt22$Age, nyt22$Clicks)
boxplot(nyt23$Age, nyt23$Clicks)
# As far as the distribution of age goes, the median seems to be around the 30 range for datasets nyt4-nyt13, and 20 for nyt22-nyt33
# The outliers for Age tend to be above the 100 range with the upper hinge ranging between 40 and 50.
# As for the Clicks, a since a large majority of users did not click, the median  seems to be 0, with all click counts above 0 being considered outliers.
# The maximum number of clicks in all datasets above seems to be 4.

#1b
hist(nyt4$Impressions, col='green')
hist(nyt4$Clicks, col='red', add=TRUE)
hist(nyt8$Impressions, col='green')
hist(nyt8$Clicks, col='red', add=TRUE)
hist(nyt9$Impressions, col='green')
hist(nyt9$Clicks, col='red', add=TRUE)
hist(nyt12$Impressions, col='green')
hist(nyt12$Clicks, col='red', add=TRUE)
hist(nyt13$Impressions, col='green')
hist(nyt13$Clicks, col='red', add=TRUE)
hist(nyt22$Impressions, col='green')
hist(nyt22$Clicks, col='red', add=TRUE)
hist(nyt23$Impressions, col='green')
hist(nyt23$Clicks, col='red', add=TRUE)
# The impressions of the datasets seems to follow a normal distribution with a slight skew to the right with a median around 5
# A slight skew to the right indicates a majority of users tend to have 5 or less impressions compared to 5 or more.
# The clicks have an extremely positive skewed distribution (as the top of the histogram is the left-most side of the histogram)
# The distribution of clicks is also far lower, with almost all clicks being either 0 or 1.

#1c
plot(ecdf(nyt4$Impressions), col='green')
plot(ecdf(nyt4$Clicks), col='red', add=TRUE)
qqplot(nyt4$Impressions, nyt4$Clicks)
plot(ecdf(nyt8$Impressions), col='green')
plot(ecdf(nyt8$Clicks), col='red', add=TRUE)
qqplot(nyt8$Impressions, nyt8$Clicks)
plot(ecdf(nyt9$Impressions), col='green')
plot(ecdf(nyt9$Clicks), col='red', add=TRUE)
qqplot(nyt9$Impressions, nyt9$Clicks)
plot(ecdf(nyt12$Impressions), col='green')
plot(ecdf(nyt12$Clicks), col='red', add=TRUE)
qqplot(nyt12$Impressions, nyt12$Clicks)
plot(ecdf(nyt13$Impressions), col='green')
plot(ecdf(nyt13$Clicks), col='red', add=TRUE)
qqplot(nyt13$Impressions, nyt13$Clicks)
plot(ecdf(nyt22$Impressions), col='green')
plot(ecdf(nyt22$Clicks), col='red', add=TRUE)
qqplot(nyt22$Impressions, nyt22$Clicks)
plot(ecdf(nyt23$Impressions), col='green')
plot(ecdf(nyt23$Clicks), col='red', add=TRUE)
qqplot(nyt23$Impressions, nyt23$Clicks)

# Most of these plots tell the same stories as the previous histogram/boxplots. As you can see, about 90% of all
# data points have clicks of 0, with about 99.98% of total clicks being beelow the 4 mark. As for impressions, 
# the distribution is a bit more normal, with 50% of data points having around 5 or less clicks.
# Looking at the qq plots, it looks as if again the impressions and clicks have somewhat of a right skew, with the
# points starting at 0 clicks as the impressions go up for some time before, jumping up as the impressions cross the 8 mark.

# Running shapiro test to test if Clicks/Impressions are normally distributed
shapiro.test(nyt4$Clicks[0:5000])
shapiro.test(nyt4$Impressions[0:5000])
# Reject the null hypothesis as both P-values are < 0.05, meaning Clicks and Impressions are not normally distributed.

# Running the Wilcoxon test to see if the two data samples are independent or not.
wilcox.test(nyt4$Clicks, nyt4$Impressions, data=nyt4)
# As the p-value is less than 0.05, we reject the null hypothesis, meaning the two data samples are independent.

#1e
# Generally, I noticed some of the features in the dataset being somewhat unhelpful, for example the Clicks feature.
# Given how little times people actually "Clicked" it was hard to discover any correlations or patterns

#2
# Will filter out data points where user isn't signed in
library(dplyr)
filterNyt4 <- filter(nyt4, Signed_In==1)
filterNyt12 <- filter(nyt12, Signed_In==1)
filterNyt13 <- filter(nyt13, Signed_In==1)
filterNyt22 <- filter(nyt22, Signed_In==1)
boxplot(filterNyt4)
boxplot(filterNyt12)
boxplot(filterNyt13)
boxplot(filterNyt22)

hist(nyt4$Impressions, col='green')
hist(nyt4$Clicks, col='red', add=TRUE)
hist(filterNyt4$Impressions, col='green')
hist(filterNyt4$Clicks, col='red', add=TRUE)
hist(filterNyt12$Impressions, col='green')
hist(filterNyt12$Clicks, col='red', add=TRUE)
hist(filterNyt13$Impressions, col='green')
hist(filterNyt13$Clicks, col='red', add=TRUE)
hist(filterNyt22$Impressions, col='green')
hist(filterNyt22$Clicks, col='red', add=TRUE)

plot(ecdf(filterNyt4$Impressions), col='green')
plot(ecdf(filterNyt4$Clicks), col='red', add=TRUE)
plot(ecdf(filterNyt12$Impressions), col='green')
plot(ecdf(filterNyt12$Clicks), col='red', add=TRUE)
plot(ecdf(filterNyt13$Impressions), col='green')
plot(ecdf(filterNyt13$Clicks), col='red', add=TRUE)
plot(ecdf(filterNyt22$Impressions), col='green')
plot(ecdf(filterNyt22$Clicks), col='red', add=TRUE)

shapiro.test(filterNyt4$Clicks[0:5000])
shapiro.test(filterNyt4$Impressions[0:5000])

wilcox.test(filterNyt4$Clicks, filterNyt4$Impressions, data=filterNyt4)

# With the distribution filter, the age range is no longer as broad, with ages past 90 being considered outliers (as can be seen from the boxplot)
# The histogram for clicks and impressions remains the same regardless of the filter.
# The same can be said for the ecdf and significance tests, which shows that regardless of whether 
# users are signed in or not, the behavior of impressions and clicks remains unchanged. The null hypothesis
# remains rejected, meaning Clicks and Impressions are not normally distributed with neither data sample being dependent.





