# Remove objects
rm(list=ls())

# Detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# Load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# Load any necessary packages
lapply(c("car", "stargazer"),  pkgTest)

# Set working directory for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

############################################################################
#Q1

install.packages('car')
library('car')
data('Prestige')
help('Prestige')

#####################################
#(a)
#Create a new variable professional by recoding the variable type so that professionals
#are coded as 1, and blue and white collar workers are coded as 0 (Hint: ifelse).

Prestige$professional <- ifelse(Prestige$type == 'prof', 0, 1)

View(Prestige)

#######################################
#(b)
#Run a linear model with prestige as an outcome and income, professional, and the
#interaction of the two as predictors (Note: this is a continuous * dummy interaction.)

model1 <- lm(prestige ~ income + professional + income * professional, data = Prestige)

summary(model1)

stargazer(model1)

########################################
#(c)
#Write the prediction equation based on the result.

#The formula for the prediction equation is:
#y(hat) = B0 + (B1)(X1)

#y(hat) = (the predeicted mean value of Voteshare)
#B0 = 0.57903 (the intercept, the value of voteshare when difflog=0)
#B1 = 0.04167 (the slope of the regression line) 
#X1 = The value of difflog.




