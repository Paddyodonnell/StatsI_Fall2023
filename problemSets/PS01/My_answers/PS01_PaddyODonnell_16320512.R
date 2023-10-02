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

#A school counselor was curious about the average of IQ of the students in her school and
#took a random sample of 25 students’ IQ scores. The following is the data set:

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

#1. Find a 90% confidence interval for the average student IQ in the school.

#I will look at the structure of the data
str(y)

#First I will calculate the the mean, variance, standard deviarion, and the standard error for reference.
mean(y) 
var(y) 
sd(y) 
sd(y)/sqrt(length(y)) 
standarderror <- sd(y) / sqrt(length(y))

#I Will calculate the length of the vector.
length(y)

#Summary of the dataset.
summary(y)

#z-score for a 90% confidence interval = 1.645
#Using the qnorm function I can calculate and assign the upper and lower bounds
#of the confidence interval.
#This method assumes a normally distributed population with a known standard deviaion.
#It is also better used when the sample population size > 30.


{lower_95_n <- qnorm(0.05, 
                     mean(y), 
                     (sd(y)/sqrt(length(y))))
  print(lower_95_n)
  upper_95_n <- qnorm(0.95,
                      mean(y),
                      (sd(y)/sqrt(length(y))))
  print(upper_95_n)
}

#Using this method the 90% confidence interval = 94.13283 - 102.7472

#Another way of calculating the 90% confidence interval assuming a normal distribution.

{#ave +- z * se
  a<-mean(y)
  b<-sd(y)
  c<-length(y)
  error <- qnorm(0.95) * b/sqrt(c)
  print(a - error)
  print(a + error)
}

#The 90% confidence interval = 94.13283 - 102.7472
#This method provides the exact same results as the first.

#We can also use the t.test function to calculate the confidence interval.
#This method assumes the data follows a t-distribution.
#Given the sample size of 25 (25<30) and the unknown population standard deviation
#the t distribution should be used instead of the normal distribution. (see page 118, statistical methods for the social sciences)

#To manually calculate assuming a t distribution.

#Calculate the degrees of freedom

df <- length(y)-1

#Calculate the t-value.

tvalue <- qt(0.95, df)

#Calculate the margin of error.

moe <- tvalue * (sd(y) / sqrt(length(y)))

#Print results.

lower_95t <- mean(y) - moe
upper_95t <- mean(y) + moe

print(lower_95t)
print(upper_95t)

#This gives us a 90 percent confidence interval of 93.95993-102.9201

#Alternatively we could just use the t.test function to get the same result.

t.test(y, conf.level = 0.90, alternative = "two.sided")

#Using the t.test provides with a 90% confidence interval of 
#93.95993-102.92007



#------------------------------------------------------------------------------

#Part 2.

#Null hypothesis: The average student IQ in her school is the same as the average IQ score (100) among all the schools in the country.

#If the p-value is greater than α then we have to accept the null hypothesis.
#If the p-value id less than α then we have to reject the null hypothesis.

#H0: μ = 100
#Ha: μ =/= 100

#This is a two-sided test but it may be one-sided.

#Calcuate sample mean, standard deviation, length for reference.

mean(y)
sd(y)
length(y)

#Calculate the test statistic (TS).

test_statistic <- (mean(y) - 100) / (sd(y) / sqrt(length(y)))
print(test_statistic)

#Estimated standard error of the sample.

standarderror <- sd(y) / sqrt(length(y))
print(standarderror)

#Given the sample size we must use the t-score, rather then the z-score.

#Calculate the degrees of freedom

df <- length(y)-1

#Calculate the P-Value

help("pt")
pvalue <- 1- pt(test_statistic, df)
print(pvalue)

#P-Value = 0.7215383

#Given the level of significance (α) is 0.05, the calculated P-Value is much greater than
#α, so we have to accept the null hypothesis.
#The teacher cannot say that her students have an IQ higher than the average IQ score (100) among all the schools in the country.

#We could also use the t.test function to get the same result.
t_test_result <- t.test(y, mu = 100, alternative = "greater")
print(t_test_result)

#P-Value = 0.7215

#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/expenditure.txt", header=T)

#I will use some functions to look at the data for reference.
str(expenditure)
View(expenditure)
summary(expenditure)

#Part 1: Please plot the relationships among Y, X1, X2, and X3 ? What are the correlations
#among them (you just need to describe the graph and the relationships among them)?

#Subset and name the relevant data.

YX1X2X3 <- as.data.frame(expenditure[c("Y","X1","X2","X3")])
print(YX1X2X3)
View(YX1X2X3)

#Use the pairs function in base R to create a scatter plot matrix.

pairs(YX1X2X3, main = "Expenditure Data")

#For more detail we can use the car package to create a similar scatter plot matrix.

#install the car package. 
#Hannah Frank provided us with this code to avoid an error message if the package is already installed.
if(!require(car)){
  install.packages("car")
  library(car)
}

#Use scatterplotMatrix function to produce a scatter plot matrix off the four variables.

scatterplotMatrix(YX1X2X3, main = "Expenditure Data")

#With this scatterplot matrix we can get a better idea of the trends in the data.
#As spending on housing goes up so to do all other variable amounts on average.
#-------------------------------------------
#Part 2.
#Please plot the relationship between Y and Region? On average, which region has the
#highest per capita expenditure on housing assistance?

boxplot(expenditure$Y ~ expenditure$Region, 
        main="Boxplot of Expenditure on Housing by Region",
        ylab="Housing Expenditure",
        xlab="Region")


#Based on the plots I've made it seems as though Region 4 (West) has the highest per capita spending on housing on average.


#--------------------------------------------------------

#Part 3.
#Please plot the relationship between Y and X1? 
#Describe this graph and the relationship. Reproduce the above graph including one more variable Region and display
#different regions with different types of symbols and colors.
#I will try out a few different plots.

{boxplot(expenditure$Y ~ expenditure$X1, 
         main="Housing Expenditure by Personal Income",
         xlab="Per Capita Personal Income",
         ylab="Expenditure on Housing")
  
  scatter.smooth(expenditure$Y ~ expenditure$X1, 
                 main="Boxplot of Housing Expenditure by Personal Income",
                 xlab="Per Capita Personal Income",
                 ylab="Expenditure on Housing")
}
{scatterplot(expenditure$Y ~ expenditure$X1, 
             main="Scatterplot of Housing Expenditure by Personal Income",
             xlab="Per Capita Personal Income",
             ylab="Expenditure on Housing")
}
{plot(expenditure$Y ~ expenditure$X1,
      main="Scatterplot of Housing Expenditure by Personal Income",
      xlab="Per Capita Personal Income",
      ylab= "Expenditure on Housing")
}

#I think the basic plot function is the best in this case.


colors <- c("red", "green", "blue", "orange")
plot(Y ~ X1,
     data = expenditure,
     main="Scatterplot of Housing Expenditure by Personal Income",
     xlab="Per Capita Personal Income",
     ylab= "Expenditure on Housing",
     pch=expenditure$Region,
     col= colors[expenditure$Region],
     cex=1)
legend("right", legend = c("Northeast", "North Central", "South", "West"), 
       pch = 1:4,  
       col = colors,  
       title = "Region")

#These changes to the original plot code add the third variable (Region), signified by colour and symbol.








