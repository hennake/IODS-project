# Henna Kettunen
# 2017-02-07
# Exercise 3: analysis


# Set working directory
setwd("C:\\Users\\MyDir\\iods\\IODS-project")

# Dplyr
library(dplyr)

# Read the data into R
alc <- read.table("./data/alc.csv", sep=";", dec=".", header=T)

# Names of the variables
colnames(alc)

# Joined data that combines two data sets of students' alcohol consumption.
# More info: https://archive.ics.uci.edu/ml/datasets/STUDENT+ALCOHOL+CONSUMPTION

# Chosen variables:
# sex - men's alcohol consumption on average higher
# Pstatus - parents' cohabition reduces risk for high alcohol consumption
# absences - positive statistical correlation between absences and high alcohol consumption
# G3 (final grade) - better grade, lower alcohol consumption
# high_use - level of alcohol consumption (Boolean),
dd <- select(alc, one_of(c("sex", "Pstatus", "absences", "G3", "high_use")))


## Explorative analysis

# Summary table
library(psych)
describe(dd, skew=F)

# Distributions
tbl1 <- table(high_use=dd$high_use)
tbl2 <- table(sex=dd$sex)
tbl3 <- table(Pstatus=dd$Pstatus)

tblp1 <- prop.table(tbl1)
tblp2 <- prop.table(tbl2)
tblp3 <- prop.table(tbl3)

list(tbl1, tblp1, tbl2, tblp2, tbl3, tblp3,
     absences=quantile(dd$absences), grade=quantile(dd$G3))


# Crosstabulations
tab1 <- table(high_use=dd$high_use, sex=dd$sex)
tab2 <- table(high_use=dd$high_use, parents_status=dd$Pstatus)
tab3 <- table(high_use=dd$high_use, absences=dd$absences)
tab4 <- table(high_use=dd$high_use, grade=dd$G3)

tabp1 <- prop.table(tab1, 2)
tabp2 <- prop.table(tab2, 2)
tabp3 <- prop.table(tab3, 2)
tabp4 <- prop.table(tab4, 2)

list(addmargins(tab1), tabp1,
     addmargins(tab2), tabp2)

## Plotting

# Barplots 
layout(matrix(c(1,1,2,2,3), nrow=1, ncol=5, byrow=T))
par(mai=c(0.6,0.3,0.6,0.3))
barplot(tabp1, main="Sex vs high use", col=c("#F8766D","#00BFC4"))
barplot(tabp2, main="Pstatus vs high use", col=c("#F8766D","#00BFC4"))
par(mai=c(0.7,0.1,0.7,0.1))
plot.new()
legend("topleft", legend=c("FALSE","TRUE"), fill=c("#F8766D","#00BFC4"), title="High use")

library(ggplot2)

# Boxplots

# Initialize a plot of high_use and G3
g1 <- ggplot(dd, aes(x = high_use, y = G3, fill=high_use)) + ggtitle("Grade vs high use")

# Initialise a plot of high_use and absences
g2 <- ggplot(alc, aes(x = high_use, y = absences, fill=high_use)) + ggtitle("Absences vs high use")

# Combine ggplots
library(gridExtra)
bp3 <- g1 + geom_boxplot() + ylab("grade") + theme(legend.position="none", plot.title = element_text(hjust = 0.5))
bp4 <- g2 + geom_boxplot() + ylab("absences") + theme(legend.position="none", plot.title = element_text(hjust = 0.5))
grid.arrange(bp3, bp4, ncol=2, nrow=1)


## Logistic regression

# First model
fit1 <- glm(high_use ~ sex + Pstatus + absences + G3, family=binomial, data=dd)
summary(fit1)

# OR:s and confidence intervals
data.frame(OR=round(exp(summary(fit1)$coef[,1]), 3),
           CI=round(exp(confint(fit1)), 3),
           "p-value"=round(summary(fit1)$coef[,4], 3))

# LR-test
anova(fit1, test="Chisq")

# Refit
fit2 <- glm(high_use ~ sex + absences + G3, family=binomial, data=dd)
summary(fit2)

# OR:s and confidence intervals
data.frame(OR=round(exp(summary(fit2)$coef[,1]), 3),
           CI=round(exp(confint(fit2)), 3),
           "p-value"=round(summary(fit2)$coef[,4], 3))

# LR-test
anova(fit2, test="Chisq")

# AIC values
AIC(fit1, fit2)


## Model performance

# Prediction
pred <- predict(fit2, type="response")
dd$prob <- pred
pred2 <- ifelse(pred > 0.5, TRUE, FALSE)
dd$pred <- pred2

# Confusion matrix
conf <- table(obs=dd$high_use, pred=pred2)
test<-addmargins(conf)
prop.table(conf)

# Same as a plot
# initialize a plot of 'high_use' versus 'probability' in 'alc'
g <- ggplot(dd, aes(x = prob, y = high_use, col = pred2))
# define the geom as points and draw the plot
g + geom_point()


# Training error

# define a loss function (mean prediction error)
loss_func <- function(class, prob) {
  n_wrong <- abs(class - prob) > 0.5
  mean(n_wrong)
}
wrong <- (test[1,2] + test[2,1]) / test[3,3]

# call loss_func to compute the average number of wrong predictions in the (training) data
loss_func(class = dd$high_use, prob = dd$prob)



# Simple guessing strategy: nobody is a high user
dd$pred2 <- F
loss_func(class = dd$high_use, prob = dd$pred2)




