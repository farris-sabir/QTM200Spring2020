#####################
# load libraries
# set wd
# clear global .envir
#####################

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

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("dplyr", "ggplot2"),  pkgTest)

# set working directory
setwd("~/Documents/GitHub/QTM200Spring2020/problem_sets/PS2/template")


#####################
# Question 1
#####################

# create a matrix of the data from which the chi-squared test can be conducted
bribematrix <- matrix(c(14, 6, 7, 7, 7, 1), byrow=T, nrow=2)


## part a
# create a new matrix of expected values using the sums of each row and sums of each column
rowsum <- rowSums(bribematrix)
columnsum <- colSums(bribematrix)
totalsum <- sum(rowsum)
expectedmatrix <- outer(rowsum, columnsum, "*") / totalsum
rownames(expectedmatrix) <- c("Upper class", "Lower class")
colnames(expectedmatrix) <- c("Not Stopped", "Bribe requested", "Stopped/given warning")
#some expected values are less than 5, meaning chi-squared test may not be appropriate

# use the observed matrix and expected matrix to compute the chi-squared test statistic and p-value
matrixdifference <- (bribematrix - expectedmatrix)^2/(expectedmatrix)
teststatistic <- sum(matrixdifference)

## part b
# use the test-statistic and degrees of freedom to find the p-value
df <- (ncol(bribematrix)-1)*(nrow(bribematrix)-1) #df stands for degrees of freedom
pvalue <- pchisq(teststatistic, df = df, lower.tail=FALSE)
chisq.test(bribematrix)
# the p-value of 0.1502 is greater than the alpha of 0.1
# this leads us to fail to reject the null hypothesis that the class of the employee and the police's response are statistically independent
# the chisq.test function in R verified the results by hand

## part c
# create an empty matrix to put in the residual values
residualmatrix <- matrix(data=NA, nrow = 2, ncol = 3)
rownames(residualmatrix) <- c("Upper class", "Lower class")
colnames(residualmatrix) <- c("Not Stopped", "Bribe requested", "Stopped/given warning")

# calculate the standardized residual for each value and place in the matrix
residualmatrix[1] <- (bribematrix[1]-expectedmatrix[1])/sqrt(expectedmatrix[1]*(1-columnsum[1]/totalsum)*(1-rowsum[1]/totalsum)) #upper class x not stopped
residualmatrix[2] <- (bribematrix[2]-expectedmatrix[2])/sqrt(expectedmatrix[2]*(1-columnsum[1]/totalsum)*(1-rowsum[2]/totalsum)) #lower class x not stopped
residualmatrix[3] <- (bribematrix[3]-expectedmatrix[3])/sqrt(expectedmatrix[3]*(1-columnsum[2]/totalsum)*(1-rowsum[1]/totalsum)) #upper class x bribe requested
residualmatrix[4] <- (bribematrix[4]-expectedmatrix[4])/sqrt(expectedmatrix[4]*(1-columnsum[2]/totalsum)*(1-rowsum[2]/totalsum)) #lower class x bribe request
residualmatrix[5] <- (bribematrix[5]-expectedmatrix[5])/sqrt(expectedmatrix[5]*(1-columnsum[3]/totalsum)*(1-rowsum[1]/totalsum)) #upper class x stopped/given warning
residualmatrix[6] <- (bribematrix[6]-expectedmatrix[6])/sqrt(expectedmatrix[6]*(1-columnsum[3]/totalsum)*(1-rowsum[2]/totalsum)) #lower class x stopped/given warning
residualmatrix

## part d
# After knowing that we have failed to reject the null hypothesis that our variables are independent, we explore the variables' relation further
# We failed to reject the null because the residuals are not great, meaning the expected and observed values are not greatly different.
# The conditions where the employees were not stopped had the least deviation from independence whereas the other conditions relatively experienced the most.
# However, the residuals are not different enough from each other to conclude any dependence.

#####################
# Question 2
#####################

## part a
# The null hypothesis is that the slope of the linear approximation of the relationship between the average number of new/repaired drinking-water facilities in villages and the proportion of reservation policy implemented or not is equal to zero.
# The alternative hypothesis is that the slope of the linear approximation of the relationship between the average number of new/repaired drinking-water facilities in villages and the proportion of reservation policy implemented or not is unequal to zero.

## part b
# inputting the data
women <- read.csv("WomenasPolicyMakers.csv")
women <- women[c(1:322),]

# In order to conduct linear regression, we must first assume the data were generated randomly, which we can assume because the policy randomly reserved some villages for women.
# We must also assume that the observations were independent, like one village's choice to change the number of facilities did not influence another.
# We must also assume there is a linear relationship between the variables of reservation policy and number of water-drinking facilities in the village.
# Finally, we must assume that the population values of number of water-drinking facilities at both reserved and non-reserved villages follow a normal distribution with the same standard deviation for both.
resid = women$water - predict(lm(water ~ reserved, data=women))
plot(density(resid), main="Density of residuals")
# As a result, the residuals are zero in expectation.

# calculating beta_hat
water_hat <- mean(women$water)
reserved_hat <- mean(women$reserved)
beta_hat <- (sum((women$water - water_hat)*(women$reserved - reserved_hat)))/(sum((women$reserved - reserved_hat)^2))
beta_hat
#beta hat = 9.252
 
# calculating alpha_hat
alpha_hat <- water_hat - reserved_hat*beta_hat
alpha_hat
#alpha hat = 14.738

# testing the null hypothesis that beta = 0
stdv_water_hat <- sqrt(sum(resid^2)/(length(women$water)-2))
se_water_hat <- stdv_water_hat/sqrt(sum((women$reserved - reserved_hat)^2))
tstat <- (beta_hat - 0)/se_water_hat
pval <- 2*pt(tstat, df = (length(women$water)), lower.tail=F)
pval
#p-value = 0.0197

# checking with R's linear model fitting function
womenmodel <- lm(water ~ reserved, data = women)
summary(womenmodel)

## part c
# Because the beta1_hat of 9.252 is associated with a t-value of 2.344 and p-value of 0.0197, with an alpha of 0.05, we reject the null hypothesis.
# As a result, the reservation policy led to a significant change in the number of new or repaired drinking-water facilities since the reserve policy started.
# A 1 unit increase in the number of villages with reservation policy leads to, on average, 9.252 more new or repaired drinking-water facilities.

#####################
# Question 3
#####################

## part 1
# importing the data set
fruitfly <- read.csv("fruitfly.csv")

# obtaining the summary statistics
summary(fruitfly)

# examining the distribution of the overall lifespan of the fruitflies
ggplot(fruitfly, aes(lifespan)) + geom_histogram(binwidth = 5) + ggtitle("Distribution of Fruitfly Lifespan")

## part 2
# plotting lifespan vs. thorax
ggplot(fruitfly, aes(x=thorax, y=lifespan)) + geom_point() + ggtitle("Lifespan vs. Thorax") + xlab("Thorax length in mm") + ylab("Lifespan (days)")

# calculating the correlation coefficient
lifespan_bar <- mean(fruitfly$lifespan)
lifespan_sd <- sd(fruitfly$lifespan)
thorax_bar <- mean(fruitfly$thorax)
thorax_sd <- sd(fruitfly$thorax)
r <- (1/(length(fruitfly$thorax)-1))*sum(((fruitfly$lifespan - lifespan_bar)/lifespan_sd)*((fruitfly$thorax - thorax_bar)/thorax_sd))
r
#The correlation coefficient is 0.6365 between lifespan and thorax

# checking with R's covariation and correlation functions
cov(fruitfly[,c(4,3)])[1,2]/(sd(fruitfly[,3]))/(sd(fruitfly[,4]))
cor(fruitfly$thorax, fruitfly$lifespan, method = "pearson")

## part 3
# To conduct a linear regression, we must first assume the data were generated randomly, and the types at least were likely randomized.
# We must also assume that the observations were independent, and the thorax length or lifespan of different flies did not influence each other.
# We must also assume there is a linaer relationship between the variables thorax length and lifespan.
# Finally, we must assume that the population values of length of thorax at each value of lifespan follow a normal distribution with the same standard deviation for all values of lifespan.
residf = fruitfly$lifespan - predict(lm(lifespan~thorax, data=fruitfly))
plot(density(residf), main="Density of residuals)")
# As a result, the residuals are zero in expectation.

# calculating beta_hat
beta_hatf <- (sum((fruitfly$lifespan - lifespan_bar)*(fruitfly$thorax - thorax_bar)))/(sum((fruitfly$thorax - thorax_bar)^2))
beta_hatf
#beta hat = 144.333

# calculating alpha_hat
alpha_hatf <- lifespan_bar - thorax_bar*beta_hatf
alpha_hatf
#alpha hat = -61.052

# checking with R's linear model fitting function
fruitflymodel <- lm(lifespan~thorax, data=fruitfly)
summary(fruitflymodel)

# interpreting the slope
# The slope of this regression indicates that for every 1 mm increase in thorax length, a fruitfly's lifespan is predicted to increase by 144.333 days on average.

## part 4

# hypotheses
# null hypothesis: there is no linear relationship between lifespan and thorax, meaning its correlation coefficient or rho is equal to 0.
# alternative hypothesis: there is a linear relationship between lifespan and thorax, meaning its correlation coefficient or rho is not equal to 0.

# testing the null hypothesis that rho = 0
TS <- (r*sqrt(length(fruitfly$lifespan)-2))/sqrt(1-r^2)
2*pt(TS, length(fruitfly$lifespan) - 2, lower.tail=F)
#p-value = 1.497*10^-15

# checking with R's correlation test function
cor.test(fruitfly$thorax, fruitfly$lifespan)

# Because the r value of 0.636 is associated with a t-value of 9.152 and p-value of 1.497*10^-15, with an alpha of 0.05, we reject the null hypothesis.
# As a result, there is a statistically significant linear relationship between thorax length and lifespan of fruitflies.

## part 5

# calculate confidence interval using equation
tfci <- qt(0.9, df = length(fruitfly) - 2)
me <- tfci*se_lifespan_hat
lowerlim <- beta_hatf - me
upperlim <- beta_hatf + me
ci <- c(lowerlim, upperlim)
ci
# The confidence interval is 118.196 to 170.470.

# checking with R's confidence interval function
confint(fruitflymodel, level=0.9)

## part 6

# (1) predict an individual fruitfly's lifespan at thorax = 0.8
thoraxi <- data.frame(thorax = 0.8)
lifespani <- predict(fruitflymodel, thoraxi)
lifespani
# The fruitfly's lifespan, given its thorax is 0.8, is predicted to be 54.415 days.
sei <- stdv_lifespan_hat*sqrt(1+1/length(fruitfly$lifespan)+((thoraxi-thorax_bar)^2)/(sum((fruitfly$thorax - thorax_bar)^2)))
sei
# The standard error of this prediction is 13.660.
mei <- qt(0.95, df = length(fruitfly)-2)*sei
cii <- c(lifespani - mei, lifespani + mei)
cii
#The confidence limits for the predicted value of lifespan 54.415 days with confidence coefficient 95% are 22.268 to 86.562.

# (2) average lifespan of fruitflies when thorax = 0.8 by the model
mui <- predict(fruitflymodel, data.frame(thorax = 0.8))
mui
# The fruitfly's average lifespan when thorax = 0.8 is 54.415.
se_mui <- stdv_lifespan_hat*sqrt(1/length(fruitfly$lifespan) + ((0.8-thorax_bar)^2)/(sum((fruitfly$thorax - thorax_bar)^2)))
se_mui
# The standard error of this estimate is 1.261.
me_mui <- qt(0.95, df = length(fruitfly)-2)*se_mui
ci_mui <- c(mui - me_mui, mui + me_mui)
ci_mui
#The confidence limits for the average lifespan at thorax = 0.8 with confidence coefficient 95% are 51.448 to 57.382.


## part 7

# plot with fitted values for lifespan with prediction intervals
xi <- runif(20, min(fruitfly$thorax), max(fruitfly$thorax))
yi <- alpha_hatf + beta_hatf*xi
sexi <- stdv_lifespan_hat*sqrt(1+1/length(fruitfly$lifespan)+((xi - thorax_bar)^2)/sum((fruitfly$thorax - thorax_bar)^2))
yimin <- yi - qt(0.95, df = length(fruitfly)-2)*sexi
yimax <- yi + qt(0.95, df = length(fruitfly)-2)*sexi
fitted <- data.frame(xi, yi, yimin, yimax)
ggplot(fitted, aes(x=xi, y=yi)) +
  geom_point() +
  geom_errorbar(width=.1, aes(ymin=yimin, ymax=yimax), color="blue") +
  ggtitle("Sequence of Thorax Values with Fitted Values for Lifespan and Prediction Interval") +
  labs(x = "Thorax Values", y = "Predicted Lifespan Values")

# plot with fitted values for lifespan with confidence intervals
mu_xi <- alpha_hatf + beta_hatf*xi
semu_xi <- stdv_lifespan_hat*sqrt(1/length(fruitfly$lifespan) + ((xi-thorax_bar)^2)/(sum((fruitfly$thorax - thorax_bar)^2)))
muimin <- mu_xi - qt(0.95, df = length(fruitfly)-2)*semu_xi
muimax <- mu_xi + qt(0.95, df = length(fruitfly)-2)*semu_xi
fittedmu <- data.frame(xi, mu_xi, muimin, muimax)
ggplot(fittedmu, aes(x=xi, y=mu_xi)) +
  geom_point() + 
  geom_errorbar(width=.1, aes(ymin=muimin, ymax=muimax), color="red") +
  ggtitle("Average Lifespan Given Sequence of Thorax Values and Confidence Interval") +
  labs(x="Thorax Values", y = "Average Lifespan Values")
