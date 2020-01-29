#####################
# load libraries
# set wd
# clear global .envir
#####################
# Farris Sabir Problem Set 1 QTM 200
# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}
library(ggplot2)

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set working directory
setwd("~/Documents/GitHub/QTM200Spring2020/problem_sets/PS1")


#####################
# Problem 1
#####################
## Dataset
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
## Calculate the mean, standard deviation, and sample size in order to calculate the lower and upper bounds of the confidence interval.
meanIQ <- sum(y)/length(y)
demeanedSum <- NULL
for(i in 1:length(y)){
  demeanedSum[i] <- y[i] - meanIQ
}
squaredError <- demeanedSum^2
variance <- sum(squaredError)/(length(y) - 1)
sdIQ <- sqrt(variance)
z90 <- qt((1-.9)/2, 24)
lower_90 <- meanIQ + z90*sdIQ/sqrt(length(y))
upper_90 <- meanIQ - z90*sdIQ/sqrt(length(y))
confint90 <- c(lower_90, upper_90)
confint90
t.test(y, conf.level = 0.9)

#The z-score of 1.71 in the normal distribution corresponded to a 90% confidence interval.
#The product of the z-score and the standard deviation of the dataset divided by square root of the group size is the margin of error, which is 4.48.
#The margin of error was added to and subtracted from the mean to obtain the confidence interval, which was verified with the R function t.test.
#Thus, the 90% confidence interval for the student IQ in the school is 94 to 103.

#####################
# Problem 2
#####################
## Dataset
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
## Use the mean and standard deviation to calculate the test statistic.
demeanedSum <- NULL
for(i in 1:length(y)){
  demeanedSum[i] <- y[i] - meanIQ
}
squaredError <- demeanedSum^2
variance <- sum(squaredError)/(length(y) - 1)
sdIQ <- sqrt(variance)
SE = sdIQ/sqrt(length(y))
meanIQ <- sum(y)/length(y)
TS = (meanIQ - 100)/SE
TS
## Use the test statistic to calculate the p-value from which a conclusion can be drawn.
pvalue <- pt(TS, df = 24, lower.tail=FALSE)
pvalue
t.test(y, mu = 100, alternative = "greater")

#The t-test was conducted so that the dataset was compared to the average national mean of 100.
#The t-score is -0.596 with degrees of freedom of 24, which correspond to a p-value of 0.7125.
#Because the p-value of 0.7215 is above 0.05, we have failed to reject the null hypothesis that the average IQ of students in the school is less than or equal to 100, the average IQ score of all schools in the country.

#####################
# Problem 3
#####################
## Recode dataset to make years labeled
y <- c(1, 2, 1, 3, 4, 1, 1, 4, 2, 1, 3, 4, 3, 2, 1, 3, 4, 1, 2, 3, 1, 1, 2, 1, 1, 3, 4)
y <- as.character(y)
y = recode(y, "1" = "Freshman","2" = "Sophomore", "3" = "Junior", "4" = "Senior")
y
## Import expenditure dataset and explore it
expenditure <- read.table("expenditure.txt", header=T)

summary(expenditure)









## Conduct linear regression on each relationship in A
library(readxl)
lmYX1 = lm(Y~X1, data = expenditure)
summary(lmYX1) #significant positive slope
lmYX2 = lm(Y~X2, data = expenditure)
summary(lmYX2)
lmYX3 = lm(Y~X3, data = expenditure)
summary(lmYX3)
lmX1X2 = lm(X1~X2, data = expenditure)
summary(lmX1X2) #significant negative slope
lmX1X3 = lm(X1~X3, data = expenditure)
summary(lmX1X3) #significant positive slope
lmX3X2 = lm(X3~X2, data = expenditure)
summary(lmX3X2) #significant negative slope
## Plot the relationships in A
library(gridExtra)
library(ggpubr)
X1Y <- ggplot(expenditure, aes(x=X1, y=Y)) + geom_point() + geom_smooth(method = "lm") +
  ggtitle("Figure 1: Y by X1") +
  labs(x="Per Capita Personal Income", y="Per Capita Expenditure on Public Education")
X2Y <- ggplot(expenditure, aes(x=X2, y=Y)) + geom_point()+ geom_smooth(method = "lm") +
  ggtitle("Figure 2: Y by X2") +
  labs(x="Number of Residents per 1000 under 18 Years of Age", y="Per Capita Expenditure on Public Education")
X3Y <- ggplot(expenditure, aes(x=X3, y=Y)) + geom_point()+ geom_smooth(method = "lm") +
  ggtitle("Figure 3: Y by X3") +
  labs(x="Number of People per 1000 Residing in Urban Areas", y="Per Capita Expenditure on Public Education")
X1X2 <- ggplot(expenditure, aes(x=X1, y=X2)) + geom_point()+ geom_smooth(method = "lm") +
  ggtitle("Figure 4: X2 by X1") +
  labs(x="Per Capita Personal Income", y="Number of Residents per 1000 under 18 Years of Age")
X1X3 <- ggplot(expenditure, aes(x=X1, y=X3)) + geom_point()+ geom_smooth(method = "lm") +
  ggtitle("Figure 5: X3 by X1") +
  labs(x="Per Capita Personal Income", y="Number of People per 1000 Residing in Urban Areas")
X2X3 <- ggplot(expenditure, aes(x=X2, y=X3)) + geom_point()+ geom_smooth(method = "lm") +
  ggtitle("Figure 6: X3 by X2") +
  labs(x="Number of Residents per 1000 under 18 Years of Age", y="Number of People per 1000 Residing in Urban Areas")
ggarrange(X1Y, X2Y, X3Y, X1X2, X1X3, X2X3,  ncol = 2, nrow = 3)

## Examine the relationship between region and per capita expenditure on public education
expenditure$REGION <- as.character(expenditure$Region)
expenditure$REGION <- recode(expenditure$REGION, "1" = "Northeast", "2" = "North Central", "3" = "South", "4" = "West")
ggplot(expenditure, aes(x=REGION, y=Y)) + 
  geom_bar(stat="identity") +
  ggtitle("Figure 7: Y by Region") +
  labs(x="Region", y="Per Capita Expenditure on Public Education")
group_by(expenditure, REGION) %>%
  summarize(meanY = mean(as.numeric(Y))) %>%
  arrange(desc(meanY))
## Examine the relationship between region, per capita personal income, and per capita expenditure on public education
ggplot(expenditure, aes(x=X1, y=Y)) + 
  geom_point(aes(shape=Region, color=Region)) + 
  ggtitle("Figure 8: Y by X1 and by Region") +
  labs(x="Per Capita Personal Income", y="Per Capita Expenditure on Public Education")


