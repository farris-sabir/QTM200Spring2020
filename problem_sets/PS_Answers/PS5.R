#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:car", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
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

lapply(c("faraway"),  pkgTest)

# set working directory
setwd("~/Documents/GitHub/QTM200Spring2020/problem_sets/PS5/template")


#####################
# Problem 1
#####################

# load data
gamble <- (data=teengamb)
# run regression on gamble with specified predictors
model1 <- lm(gamble ~ sex + status + income + verbal, gamble)

## part a

plot(model1, which=1)
#The constant variance assumption might be in question because the line of the in the graph is not completely straight, only slightly so.
#This may indicate variance may have fluctuated across different fitted values.


## part b

plot(model1, which=2)
#This Q-Q plot supports the normality assumption because most values fall on the line.
#Values falling on the line indicate that variation in those x-values are normally distributed.
#More error occurs at the end, but that is expected, especially since there are less data for these end values.

## part c

plot(hatvalues(model1))
abline(h=2*5/47, lty=2)
abline(h=3*5/47, lty=2)
identify(1:47, hatvalues(model1), row.names(gamble))
#There are four points with high leverage using threshold 2(k + 1)/n, but not threshold 3(k + 1)/n
#These four points have the potential to greatly influence the fitted model
gamble[c(31, 33, 35, 42), ]
#These are the points with which relationship may be influenced.

## part d

outlierTest(model1)
#The adjusted p-value for the largest studentized residual is less than 0.05
#Therefore, this 24th observation has an extreme residual or is an outlier.

## part e

plot(hatvalues(model1), rstudent(model1), type = "n")
cook <- sqrt(cooks.distance(model1))
points(hatvalues(model1), rstudent(model1), cex=10*cook/max(cook))
abline(h=c(-2, 0, 2), lty=2)
abline(v=c(2,3)*5/47, lty=2)
identify(hatvalues(model1) , rstudent(model1), row.names (gamble))
#The same four points from part c have high leverage under the 2(k+1)/n condition but not 3(k+1)/n condition.
gamble[c(31, 33, 35, 42), ]
#Two points have the largest regression residuals, the 24th and 39th observation.
gamble[c(24, 39), ]
#The 24th observation likely has the largest influence on the model.
#It has a large regression residual, as verified by part d, and large cook's distance.
