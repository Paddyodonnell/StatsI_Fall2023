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
  new.pkg <- pkg[!(pkg %in% installed.packages(dplyr)[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

if(!require(stargazer)){
  install.packages("stargazer")
  library(stargazer)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/incumbents_subset.csv")

View(inc.sub)

####################################################################################
#Q1.

#P1.
#Run a regression where the outcome variable is voteshare
#and the explanatory variable is difflog.

#Linear model voteshare(y, dependent) vs difflog(x, independent)
model1 <- lm(voteshare ~ difflog, data = inc.sub)
print(model1)

#Summary of the above model, for reference.
summary(model1)

stargazer(model1)

#P2
#Plot, using base R.
{plot(voteshare ~ difflog, data = inc.sub, col = 'black')+
  abline(model1, col = 'red', lwd = 3)+
  grid()+
  title(main = 'Scatter Plot Voteshare Vs. Difflog', y = "voteshare", x = "difflog")
}

#P3
#Model1 residuals, saved as an object.
model1_residuals <- resid(model1)
View(model1_residuals)

#P4
#Writing a prediction equation from the above linear regression.
#The linear regression tries to predict the voteshare, from difflog.
#The basic formula, below.
#y(hat) = B0 + (B1)(X1)

#y(hat) = (the predeicted mean value of Voteshare)
#B0 = 0.57903 (the intercept, the value of voteshare when difflog=0)
#B1 = 0.04167 (the slope of the regression line) 
#X1 = The value of difflog.

(0.57903)+(0.04167)*(X1)

################################################################################
#Q2.

#P1.
#Run a regression where the outcome variable is presvote
#and the explanitory variable is difflog.

model2 <- lm(presvote ~ difflog, data = inc.sub)
print(model2)

summary(model2)

stargazer(model2)

#P2.
{plot(presvote ~ difflog, data = inc.sub, col = 'black')+
  abline(model2, col = 'red', lwd = 3)+
  grid()+
  title(main = 'Scatter Plot Presvote Vs. Difflog', y = "presvote", x = "difflog")
}
#P3
model2_residuals <- resid(model2)

#P4

(0.50758)+(0.02384)*(X1)

###############################################################################
#Q3

#P1
#Run a regression where the outcome variable is voteshare 
#and the explanatory variable is presvote.

model3 <- lm(voteshare ~ presvote, data = inc.sub)
print(model3)

summary(model3)

stargazer(model3)

#P2
{plot(voteshare ~ presvote, data = inc.sub, col = 'black')+
  abline(model3, col = 'red', lwd = 3)+
  grid()+
  title(main = 'Scatter Plot Voteshare Vs. Presvote', y = "voteshare", x = "presvote")
}

#P3

(0.441)+(0.3880)*(X1)

###################################################################
#Q4

#P1

#Put the standard residuals from Questions 1 and 2 into a dataframe.
{df1 <- as.data.frame(model1_residuals)
df2 <- as.data.frame(model2_residuals)
list_df3 <- c(df1, df2)
df3 <- as.data.frame(list_df3)
}

View(df3)


model4 <- lm(model1_residuals ~ model2_residuals, data = df3)
print(model4)

stargazer(model4)

summary(model4)

#P2

{plot(model1_residuals ~ model2_residuals, data = df3, col = 'black')+
    abline(model4, col = 'red', lwd = 3)+
    grid()+
    title(main = 'Scatter Plot model1_residuals Vs. model2_residuals', y = "model1_residuals", x = "model2_residuals")
}

#P3
(-5.934e-18)+(2.569e-01)*(X1)

##############################################################
#Q5

#P1

model5 <- lm(voteshare ~ difflog + presvote, data = inc.sub)
print(model5)

summary(model5)

(0.44864)+(0.03554)*(X1)+(0.25688)*(X2)

cor(model2_residuals, inc.sub$presvote)

print(model4)
print(model5)

r1 <- resid(model4)
r2 <- resid(model5)

summary(r1)
summary(r2)

df_r1 <- as.data.frame(r1)
df_r2 <- as.data.frame(r2)

View(df_r1)
View(df_r2)

list_df_r1r2 <- c(df_r1, df_r2)
df_r1r2 <- as.data.frame(list_df_r1r2)

View(df_r1r2)

cor(r1, r2)
cor(df_r1, df_r2)

View(model2_residuals)
View(inc.sub$presvote)

