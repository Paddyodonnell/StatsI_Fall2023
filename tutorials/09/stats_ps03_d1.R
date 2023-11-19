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

lapply(c("car"),  pkgTest)

################################################################################

data <- read.csv("C:/Users/paddy/OneDrive/Documents/GitHub/StatsI_Fall2023/datasets/incumbents_subset.csv")

View(data)

#Run a regression where the outcome variable is voteshare
#and the explanatory variable is difflog.

data$voteshare
data$difflog

?lm

model <- lm(data$difflog ~ data$voteshare, data = data)

View(model)

plot(model)











        
                 