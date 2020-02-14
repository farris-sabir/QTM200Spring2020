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
setwd("~/Documents/GitHub/QTM200Spring2020/problem_sets/PS3/template")


#####################
# Question 1
#####################

## Part 1

#read incumbents subset data
incumbents <- read.csv("incumbents_subset.csv")
#run regression model with incumbent's vote share regressed on difference in campaign spending
incumbents_model <- lm(voteshare ~ difflog, data=incumbents)
#get summary of model
summary(incumbents_model)
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-0.26832 -0.05345 -0.00377  0.04780  0.32749 
#Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
#  (Intercept) 0.579031   0.002251  257.19   <2e-16 ***
#  difflog     0.041666   0.000968   43.04   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#Residual standard error: 0.07867 on 3191 degrees of freedom
#Multiple R-squared:  0.3673,	Adjusted R-squared:  0.3671 
#F-statistic:  1853 on 1 and 3191 DF,  p-value: < 2.2e-16

## Part 2

#produce scatterplot
plot(incumbents$difflog, incumbents$voteshare, xlab = "Logarithmic Difference in Campaign Spending", ylab = "Incumbent Vote Share (%)")
#add regression line
abline(0.579031, 0.041666)

## Part 3

residual1 <- incumbents$voteshare - (0.5970321 + 0.041666*incumbents$difflog)
residual1 <- incumbents_model$residuals

## Part 4

#Y_hat = 0.5970321 + 0.041666*X


#####################
# Question 2
#####################


## Part 1

#run regression model with vote share of the presidential candidate of incumbent party regressed on difference in campaign spending
incumbents_model2 <- lm(presvote ~ difflog, data=incumbents)
#get summary of model
summary(incumbents_model2)
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-0.32196 -0.07407 -0.00102  0.07151  0.42743 
#Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
#  (Intercept) 0.507583   0.003161  160.60   <2e-16 ***
#  difflog     0.023837   0.001359   17.54   <2e-16 ***
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#Residual standard error: 0.1104 on 3191 degrees of freedom
#Multiple R-squared:  0.08795,	Adjusted R-squared:  0.08767 
#F-statistic: 307.7 on 1 and 3191 DF,  p-value: < 2.2e-16

## Part 2

#produce scatterplot
plot(incumbents$difflog, incumbents$presvote, xlab = "Logarithmic Difference in Campaign Spending", ylab = "Vote Share of Presidential Candidate of Incumbent Party (%)")
#add regression line
abline(0.507583, 0.023837)

## Part 3

residual2 <- incumbents$presvote - 0.407583 - 0.023837*incumbents$difflog
residual2 <- incumbents_model2$residuals

## Part 4

#Y_hat = 0.507583 + 0.023837*X


#####################
# Question 3
#####################

## Part 1

#run regression model with vote share of the presidential candidate of incumbent party regressed on incumbent's electoral success
incumbents_model3 <- lm(voteshare ~ presvote, data=incumbents)
#get summary of model
summary(incumbents_model3)
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-0.27330 -0.05888  0.00394  0.06148  0.41365 
#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#  (Intercept) 0.441330   0.007599   58.08   <2e-16 ***
#  presvote    0.388018   0.013493   28.76   <2e-16 ***
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#Residual standard error: 0.08815 on 3191 degrees of freedom
#Multiple R-squared:  0.2058,	Adjusted R-squared:  0.2056 
#F-statistic:   827 on 1 and 3191 DF,  p-value: < 2.2e-16

## Part 2

#produce scatterplot
plot(incumbents$presvote, incumbents$voteshare, ylab = "Incumbent Vote Share (%)", xlab = "Vote Share of Presidential Candidate of Incumbent Party (%)")
#add regression line
abline(0.441330, 0.388018)

## Part 3

#Y_hat = 0.441330 + 0.388018*X


#####################
# Question 4
#####################


## Part 1

#run regression model with variation in voteshare not explained by difference in spending and variation in incumbent electoral success not explained by difference in spending
incumbents_model4 <- lm(residual1 ~ residual2)
#get summary of model
summary(incumbents_model4)
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-0.25928 -0.04737 -0.00121  0.04618  0.33126 
#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -4.860e-18  1.299e-03    0.00        1    
#residual2    2.569e-01  1.176e-02   21.84   <2e-16 ***
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#Residual standard error: 0.07338 on 3191 degrees of freedom
#Multiple R-squared:   0.13,	Adjusted R-squared:  0.1298 
#F-statistic:   477 on 1 and 3191 DF,  p-value: < 2.2e-16

## Part 2

#produce scatterplot
plot(residual2, residual1, xlab = "Residuals between difflog and presvote", ylab = "Residuals between difflog and voteshare")
#add regression line
abline(-4.860e-18, 2.569e-01)

## Part 3

#Y_hat = -4.860e-18 + 2.569e-01*X


#####################
# Question 5
#####################

## Part 1

#run regression model with incumbent's vote share against difference in campaign spending and vote share of presidential candidate of incumbent party
incumbents_model5 <- lm(voteshare ~ difflog + presvote, data=incumbents)
#get summary of model
summary(incumbents_model5)
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-0.25928 -0.04737 -0.00121  0.04618  0.33126 
#Coefficients:
#              Estimate   Std. Error t value Pr(>|t|)    
#  (Intercept) 0.4486442  0.0063297   70.88   <2e-16 ***
#  difflog     0.0355431  0.0009455   37.59   <2e-16 ***
#  presvote    0.2568770  0.0117637   21.84   <2e-16 ***
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#Residual standard error: 0.07339 on 3190 degrees of freedom
#Multiple R-squared:  0.4496,	Adjusted R-squared:  0.4493 
#F-statistic:  1303 on 2 and 3190 DF,  p-value: < 2.2e-16

## Part 2

#Y_hat = 0.4486442 + 0.0355431*X1 + 0.2568770*X2
#where X1 = difference in campaign spending
#and where X2 = vote share of presidential candidate of incumbent party


## Part 3

#The beta_1 in question 4 is identical to the beta_2 in question 5 i.e. the coefficient for presvote as well as the standard error, t statistic, and p-value.
#This similarity is likely question 4 involved variation in presvote (its variation not explained by difflog) i.e. residual from question 2
#that was explained by variation in voteshare (its variation not explained by difflog) i.e. residual from question 1.
#this isolated the impact of presvote on voteshare from any influence of difflog.
#Question 5 similarly accounts for voteshare's variation explained by presvote while holding difflog constant.
#Finding this beta_2 in question 5 requires a similar process of holding difflog constant, so their values are similar.

#Also, the residuals and residual standard error are similar, the only difference explained by difference in degrees of freedom from adding an extra variable in question 5.
#Question 4 calculates the difference between how the residuals in variation in voteshare explained by difflog predict the residuals in presvote explained by diff log
#versus how the residuals in presvote explained by difflog actually are. This is equivalent to, in question 5, calculating the difference in predicted voteshare from
#presvote, difflog, and their predicted coefficients versus the actual observed voteshare.

#The residual is the difference between observed and predicted values in y.

#Residual 1 = variation in voteshare not explained by difflog
#Residual 2 = variation in presvote not explained by difflog
#Model 4 = variation in variation in presvote not explained by difflog explained by variation in voteshare explained by difflog
#Model5 = variation in voteshare not explained by difflog, explained by presvote

#The residuals are the same.
#Beta_1 in Question 4 is identical to Beta2 for presvote in Question 5 as well as the standard error, t statistic, and p-value
#The residual standard error and degrees of freedom are very similar, only difference being in question 5 the change in degrees of freedom for accounting the extra variable







