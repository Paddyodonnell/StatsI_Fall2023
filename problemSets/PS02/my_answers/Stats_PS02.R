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

lapply(c("car"),  pkgTest)

#####################
# Problem 1
#####################

#(a) Calculate the χ2 test statistic by hand/manually 
#(even better if you can do ”by hand”in R).

#H0: The variables are statistically independent.
#Ha: The variables are statistically dependent.

#F0: The observed frequency, the raw count
#Fe: The expected frequency, what we would expect for the independent samples

#If H0 is true we would expect F0 = Fe
  
#Fe = row total/grand total * column total

#Purely by hand. 
#First calculate the expected frequencies
#and then subtract the expected frequency from the observed frequeny
#Bring this result to the power of 2, and divide that by the expected freqency
#Do this for all the frequencies
#Add them together to get the chi squared test result for this table.

((27/42)*21)
((27/42)*13)
((27/42)*8)
((15/42)*21)
((15/42)*13)
((15/42)*8)

{(((14-13.5)^2)/13.5)+
    (((6-8.36)^2)/8.36)+
    (((7-7.5)^2)/7.5)+ 
    (((7-5.14)^2)/5.14)+
    (((7-4.64)^2)/4.64)+   
    (((1-2.86)^2)/2.86)
} 

#By hand, naming all the frequencies to avoid rounding.

F01 <- 14
F02 <- 6
F03 <- 7
F04 <- 7
F05 <- 7
F06 <- 1

Fe1 <- ((27/42)*21)
Fe2 <- ((27/42)*13)
Fe3 <- ((27/42)*8)
Fe4 <- ((15/42)*21)
Fe5 <- ((15/42)*13)
Fe6 <- ((15/42)*8)

print(Fe1)
print(Fe2)
print(Fe3)
print(Fe4)
print(Fe5)
print(Fe6)


{(((F01-Fe1)^2)/Fe1)+
    (((F02-Fe2)^2)/Fe2)+
    (((F03-Fe3)^2)/Fe3)+
    (((F04-Fe4)^2)/Fe4)+
    (((F05-Fe5)^2)/Fe5)+
    (((F06-Fe6)^2)/Fe6)
} -> test.result

print(test.result)

#(b) Now calculate the p-value from the test statistic you just created (in R).
#What do you conclude if α = 0.1?

pchisq(test.result, df=2, lower.tail = FALSE)

#p-value = 0.1502306

#As the level of significance α = 0.1, and our test result is over 0.1
#We cannot reject the null hypothesis.

#(c)Calculate the standardized residuals for each cell and put them in the table below.

sr1 <- (14-13.5)/sqrt((13.5)*(1-(27/42))*(1-(21/42)))
sr2 <- (6-8.36)/sqrt((8.36)*(1-(27/42))*(1-(13/42)))
sr3 <- (7-5.14)/sqrt((5.14)*(1-(27/42))*(1-(8/42)))

sr4 <- (7-7.5)/sqrt((7.5)*(1-(15/42))*(1-(21/42)))
sr5 <- (7-4.64)/sqrt((4.64)*(1-(15/42))*(1-(13/42)))
sr6 <-(1-2.86)/sqrt((2.86)*(1-(15/42))*(1-(8/42)))

print(sr1)
print(sr2)
print(sr3)
print(sr4)
print(sr5)
print(sr6)

#Plot the standarsised residuals in the table.
{sr_matrix <- matrix(c(sr1,sr2,sr3,sr4,sr5,sr6), nrow = 2, byrow = TRUE)
  colnames(sr_matrix) <- c("Not Stopped", "Bribe Requested", "Stopped/given warning")
  row.names(sr_matrix) <- c("Upper class", "Lower class")
  View(sr_matrix)
}
#As a data frame.
{df <- data.frame(sr_matrix)
  colnames(df) <- c("Not Stopped", "Bribe Requested", "Stopped/given warning")
  row.names(df) <- c("Upper class", "Lower class")
  View(df)
}

############################################################################
#Not by hand.

#Write table into R with code.
{observed_data <- matrix(c(14, 6, 7, 7, 7, 1), nrow = 2, byrow = TRUE)
colnames(observed_data) <- c("Not Stopped", "Bribe Requested", "Stopped/given warning")
row.names(observed_data) <- c("Upper class", "Lower class")
}

#View the table to check it is correct.
View(observed_data)

#Run the chi squared test.
chi_test <- chisq.test(observed_data)

#Extract the required data.
ls(chi_test)
chi_test$observed
chi_test$expected
chi_test$residuals
chi_test$stdres

#The results match the by hand method.

plot(chi_test$observed ~ chi_test$expected)

##########################################################################
###############
#Question 2.
###############

west_bengal_data <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")

View(west_bengal_data)
str(west_bengal_data)

#(a)
#State null and alternative hypothesis

#Null hypothesis: The reservation policy has no effect on number of new or repaired drinking water facilities.
#Alternative hypothesis: The reservation policy does have an effect on number of new or repaired drinking water facilities.

######################################################################
#DOES NOT APPLY, JUST PRACTICE.
#By hand bivariate regression analysis, pearson.
#First calculate the correlation coefficient, test statistic (t_stat), and p-value.
{wbd_matrix <- as.matrix(west_bengal_data)
r <- cov(wbd_matrix)[3,6]/(sd(wbd_matrix[,3])*sd(wbd_matrix[,6]))
n <- dim(wbd_matrix)[1]
t_stat <- (r*sqrt(n-2))/sqrt(1-r^2)
p_value <- 2*pt(t_stat,n-2,lower.tail=FALSE)
print(r)
print(t_stat)
print(p_value)
}

#r is the correlation coefficient, and n is the number of observations.
#Code taken from Jeffery Ziegler's slides on bivariate regression.

#Not by hand.
#find r, the correlation coefficient.
cor(wbd_matrix[,3], wbd_matrix[,6], method = "pearson")
#Hypothesis test.
cor.test(wbd_matrix[,3], wbd_matrix[,6])

#This does not apply because the analysis is being done on data with
#a binary/categorical independent variable.

###############################################################################

#Fitting a linear model, water facilities against reservation policy.

model1 <- lm(water ~ reserved, data=west_bengal_data)
summary(model1)

#Test statistic from intercept of model1.
(14.738-0)/2.286

#Test statistic.
(9.252-0)/3.948

#confidence interval (CI) for the slope (beta) of reservation policy.
#Beta(hat) +or- (t-score*SE)

confint(model1, "reserved", level=0.95)

9.252+(1.97*3.948)
9.252-(1.97*3.948)

#CI = (1.47444, 17.02956)

#Export model1 as LaTex code.
if(!require(stargazer)){
  install.packages("stargazer")
  library(stargazer)
}
stargazer(model1)

###############################################################################

#Binary independent variable.

#The predicted value of Y(hat) is equal to the level of significance plus the slope multiplied
#by the state of the binary independent variable (0 or 1).

#Model for reservation policy.
14.738+(9.252*1)
#Model for villages without the reservation policy.
14.738+(9.252*0)

###########################################################################
#Making plots of the data.

plot(west_bengal_data$reserved, west_bengal_data$water)

if(!require(car)){
  install.packages("car")
  library(car)
}

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(car)
}

ggplot(west_bengal_data, aes(x = reserved, y = water)) +
  geom_point() +  
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Reservation Policy (0 = Not Reserved, 1 = Reserved)",
       y = "Number of Water Facilities",
       title = "Relationship between Reservation Policy and Water Facilities")+
  theme_minimal() #Code from Hannah Frank's tutorial.

scatterplot(west_bengal_data$reserved, west_bengal_data$water)




