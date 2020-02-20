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

## Setup

install.packages("car")
library(car)
data(Prestige)
help(Prestige)
##Prestige of Canadian Occupations
##Description
#The Prestige data frame has 102 rows and 6 columns. The observations are occupations.
##Usage
#Prestige
##Format
#This data frame contains the following columns:
#education = Average education of occupational incumbents, years, in 1971.
#income = Average income of incumbents, dollars, in 1971.
#women = Percentage of incumbents who are women.
#prestige = Pineo-Porter prestige score for occupation, from a social survey conducted in the mid-1960s.
#census = Canadian Census occupational code.
#type = Type of occupation. A factor with levels (note: out of order): bc, Blue Collar; prof, Professional, Managerial, and Technical; wc, White Collar.
##Source
#Canada (1971) Census of Canada. Vol. 3, Part 6. Statistics Canada [pp. 19-1–19-21].
#Personal communication from B. Blishen, W. Carroll, and C. Moore, Departments of Sociology, York University and University of Victoria.
##References
#Fox, J. (2016) Applied Regression Analysis and Generalized Linear Models, Third Edition. Sage.
#Fox, J. and Weisberg, S. (2019) An R Companion to Applied Regression, Third Edition, Sage.

## Part a

#Professionals recoded to 1, and blue and white collar workers coded as 0
Prestige$professional <- ifelse(Prestige$type=="prof", 1, 0)

## Part b

prestigemodel1 <- lm(prestige ~ income + professional + income:professional, data = Prestige)
summary(prestigemodel1)
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-14.852  -5.332  -1.272   4.658  29.932 
#Coefficients:
#                        Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)         21.1422589  2.8044261   7.539 2.93e-11 ***
#  income               0.0031709  0.0004993   6.351 7.55e-09 ***
#  professional        37.7812800  4.2482744   8.893 4.14e-14 ***
#  income:professional -0.0023257  0.0005675  -4.098 8.83e-05 ***
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#Residual standard error: 8.012 on 94 degrees of freedom
#(4 observations deleted due to missingness)
#Multiple R-squared:  0.7872,	Adjusted R-squared:  0.7804 
#F-statistic: 115.9 on 3 and 94 DF,  p-value: < 2.2e-16

## Part c

#prestige = 21.142 + 3.171*10^-3 * income + 37.781 * professional - 2.326*10^-3 * income * professional

## Part d

#This coefficient describes income's effect on prestige when the individual is blue or white collar.
#For an individual who is blue or white collar, on average every 1 dollar increase in income leads to 0.0031709 units of increase in Pineo-Porter prestige score for occupation.

## Part e

#This coefficient describes the effect of switching from blue or white collar to professional independent of income's effect.
#For individuals who receives no income i.e. $0, on average those who are professionals have 37.7812800 units of Pineo-Porter prestige score more than those who are blue or white collar.

## Part f

#Analytically deduce the answer
0.0031709 * 1000 - 0.0023257 * 1000 * 1
#Check the answer given hypothetical individual 1 vs. individual 2
i1 = 5000
i2 = 6000
p = 1
prestige1 <- 21.1422589 + 0.0031709 * i1 + 37.7812800 * p - 0.0023257 * i1 * p
prestige2 <- 21.1422589 + 0.0031709 * i2 + 37.7812800 * p - 0.0023257 * i2 * p
prestige2 - prestige1
#The change in prestige associated with a $1000 increase in income given a professional occupation is a 0.8452 increase in units of Pineo-Porter prestige score.

## Part g

#Analytically deduce the answer
37.7812800 * 1 - 0.0023257 * 6000 * 1
#Check the answer given hypothetical individual 1 vs. individual 2
i = 6000
p1 = 0
p2 = 1
prestige3 <- 21.1422589 + 0.0031709 * i + 37.7812800 * p1 - 0.0023257 * i * p1
prestige4 <- 21.1422589 + 0.0031709 * i + 37.7812800 * p2 - 0.0023257 * i * p2
prestige4 - prestige3
#The change in prestige associated with changing from non-professional to professoinal given an income of $6000 is a 23.827 increase in units of Pineo-Porter prestige score.

#####################
# Question 2
#####################

## Part a

#Hypotheses:
#Ho: beta_assigned = 0 & Ha: beta_assigned =/= 0
#Test statistic: 
beta_assigned = 0.042
se_beta_assigned = 0.016
t = beta_assigned/se_beta_assigned
t #2.625
#P-value:
p = 2*pt(2.625, df = 131 - 3, lower.tail = F)
p #0.00972
#Interpretation:
#Because the p-value of 0.00972 is below the alpha of 0.05, we reject the null hypothesis that having these yard signs in a precinct does not vote share.

## Part b

#Hypotheses:
#Ho: beta_adjacent = 0 & Ha: beta_adjacent =/= 0
#Test statistic: 
beta_adjacent = 0.042
se_beta_adjacent = 0.013
t = beta_adjacent/se_beta_adjacent
t #3.231
#P-value:
p = 2*pt(3.231, df = 131 - 3, lower.tail = F)
p #0.00157
#Interpretation:
#Because the p-value of 0.00157 is below the alpha of 0.05, we reject the null hypotheses that being next to precincts with these yard signs has no effect on vote share.

## Part c

#This means that for a precinct that is not assigned to have lawn signs and is not adjacent to a precinct with lawn signs, on average 30.2% of the vote went to McAuliff's opponent Ken Cuccinelli.

## Part d

#Given an R^2 value of 0.094, the strength of fit of the linaer model is 0.094.
#In other words, of the total variability in vote proportion for Cuccinelli, only 9.4% of it can be explained by yard signs.
#The other 90.6% is explained by factors that are not modeled.

  
  
  
  