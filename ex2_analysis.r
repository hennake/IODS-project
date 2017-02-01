# Henna Kettunen
# 2017-01-29
# Exercise 2: analysis

# Set working directory
setwd("C:\\Users\\MyDir\\iods\\IODS-project")

# Read the data into R
students2014 <- read.table(".\\data\\learning2014.txt", sep="\t", dec=".", header=T)

# Dimensions and structure of the data
str(students2014)

# > str(students2014)
# 'data.frame':	166 obs. of  7 variables:
# $ gender  : Factor w/ 2 levels "F","M": 1 2 1 2 2 1 2 1 2 1 ...
# $ age     : int  53 55 49 53 49 38 50 37 37 42 ...
# $ attitude: int  37 31 25 35 37 38 35 29 38 21 ...
# $ deep    : int  43 35 42 42 44 57 46 39 52 48 ...
# $ stra    : int  31 38 27 27 34 29 23 34 26 36 ...
# $ surf    : int  27 22 29 25 29 29 18 32 34 28 ...
# $ points  : int  25 12 24 10 22 21 21 31 24 26 ...

# # Description of the data:
# The dataset consists of data on the relationship between students' learning approaches and their achievements in an introductory statistics course in Finland.
# Students with zero points from the exam where excluded from the data in the data wrangling phase.
# Variables are as follows:
# gender   Gender: M (Male), F (Female)
# age      Age (in years) derived from the date of birth
# attitude Global attitude toward statistics
# deep     Sum of points from questions related to deep learning approach
# stra     Sum of points from questions related to strategic learning approach
# surf     Sum of points from questions related to surface learning approach
# points   Exam points

## Explorative analysis

# Summary table using the describe function from library psych
library(psych)
describe(students2014)
table(students2014$gender)

# Pairplot matrix with Ggally and ggplot2
library(GGally)
library(ggplot2)
p <- ggpairs(students2014, mapping = aes(col=gender, alpha=0.3), lower = list(combo = wrap("facethist", bins = 20)))
p

# Boxplots
boxplot(students2014)

# Histogram of points
hist(students2014$points, main="Points", xlab="")


## Regression analyses

# Model with three explanatory variables
fit1 <- lm(points ~ gender + attitude + surf, data = students2014)
summary(fit1)
AIC(fit1)

# Remove non-significant explanatory variables
fit2 <- lm(points ~ attitude, data = students2014)
summary(fit2)
AIC(fit2)

# Visualization of the second model
par(mfrow = c(1,1))
plot(students2014$attitude, students2014$points, xlim = c(0, max(students2014$attitude)), xlab="Attitude toward statistics", ylab="Exam points")
abline(fit2, col="red")

# Assumptions of linear regression model and how to examine the:
# The errors are normally distributed (QQ-plot)
# The errors are not correlated
# The errors have a constant variance (Residuals vs. fitted)
# The size of a given error does not depend on the explanatory variables

# Diagnostics of the second model
par(mfrow=c(2,2))
plot(fit2, which=c(1,2,5))







