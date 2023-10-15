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

#H0: The variables are statistically independent
#Ha: The variables are statistically dependent

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
#we can conclude that our results are statistically significant.

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

{sr_matrix <- matrix(c(sr1,sr2,sr3,sr4,sr5,sr6), nrow = 2, byrow = TRUE)
df <- data.frame(sr_matrix)
colnames(df) <- c("Not Stopped", "Bribe Requested", "Stopped/given warning")
row.names(df) <- c("Upper class", "Lower class")
View(df)
} 

{colnames(sr_matrix) <- c("Not Stopped", "Bribe Requested", "Stopped/given warning")
row.names(sr_matrix) <- c("Upper class", "Lower class")
View(sr_matrix)
}

observed_data <- matrix(c(14, 6, 7, 7, 7, 1), nrow = 2, byrow = TRUE)

View(observed_data)

chisq.test(observed_data)


#############################################################
###############
#Question 2.
###############

west_bengal_data <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")

View(west_bengal_data)
str(west_bengal_data)

#(a)
#State null and alternative hypothesis

#Null hypothesis: Women do not promote different policies than men.
#Alternative hypothesis: women promote different policies than men.

#By hand bivariate regression analysis.
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

#Calculate the p-value.
{p_value <- 2*pt(t_stat,n-2,lower.tail=FALSE)
print(p_value)
}

#More efficiently
#find r, the correlation coefficient.
cor(wbd_matrix[,3], wbd_matrix[,6], method = "pearson")
#Hypothesis test.
cor.test(wbd_matrix[,3], wbd_matrix[,6])

plot(west_bengal_data$female, west_bengal_data$water)

if(!require(car)){
  install.packages("car")
  library(car)
}
scatterplot(west_bengal_data$female, west_bengal_data$water)




