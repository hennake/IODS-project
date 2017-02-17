# Henna Kettunen
# 2017-02-15
# Exercise 4: analysis


# Load Boston data
library(dplyr)
library(MASS)
library(corrplot)
library(tidyr)
data(Boston)


# Structure and dimensions
str(Boston)

# Variable summaries and graphical overview
summary(Boston)
pairs(Boston, cex=0.1, pch=16)

# Correlation matrix
cor_matrix<-cor(Boston) 
corrplot(cor_matrix, method="circle", type="upper", cl.pos="b", tl.pos="d", tl.cex=0.6)

# Scaling
bs <- as.data.frame(scale(Boston))
summary(bs)

# Transform 'crim' to a categorical variable
bs$crim <- cut(bs$crim, breaks = quantile(bs$crim), include.lowest = TRUE, labels = c("low", "med_low", "med_high", "high"))

# Look at the table of the new factor crim
table(bs$crim)

# Divide the data to train and test data sets
set.seed(123)
ind <- sample(nrow(bs), nrow(bs)*0.8)
train <- bs[ind,]
test <- bs[-ind,]

# Linear discriminant analysis
lda.fit <- lda(formula=as.numeric(crim) ~ ., data = train)

# print the lda.fit object
lda.fit

# the function for lda biplot arrows
lda.arrows <- function(x, myscale = 1, arrow_heads = 0.1, color = "red", tex = 0.75, choices = c(1,2)){
  heads <- coef(x)
  arrows(x0 = 0, y0 = 0,
         x1 = myscale * heads[,choices[1]],
         y1 = myscale * heads[,choices[2]], col=color, length = arrow_heads)
  text(myscale * heads[,choices], labels = row.names(heads),
       cex = tex, col=color, pos=3)
}

# plot the lda results
classes <- as.numeric(train$crim)
plot(lda.fit, dimen = 2, col = classes)
lda.arrows(lda.fit, myscale = 2, col = "#666666")

# save and remove categorical crime data from test data
crime <- test$crim
test <- dplyr::select(test, -crim)

# predict classes with test data
lda.pred <- predict(lda.fit, newdata = test)

# cross tabulate the results
table(correct = as.numeric(crime), predicted = lda.pred$class)


## K-means

# load MASS and Boston
data('Boston')
bs2 <- as.data.frame(scale(Boston))

# euclidean distance matrix
dist_eu <- dist(bs2)

# k-means clustering
km <-kmeans(dist_eu, centers = 5)



# determine the number of clusters
k_max <- 10

# calculate the total within sum of squares
set.seed(123)
twcss <- sapply(1:k_max, function(k){kmeans(dist_eu, k)$tot.withinss})

# visualize the results
plot(1:k_max, twcss, type='b')

# Optimal number of clusters is when the total of within cluster sum of squares drops drastically
# -> 2 clusters
set.seed(123)
km <-kmeans(dist_eu, centers = 2)

# plot the scaled Boston dataset with clusters
pairs(bs, col = km$cluster,  cex=0.1, pch=16)



# Bonus

# k-means again
set.seed(123)
km2 <- kmeans(dist_eu, centers = 5)

# LDA with using the k-means clusters as target classes
bs2$cl <- km2$cluster
lda.fit2 <- lda(cl ~ ., data = bs2)
plot(lda.fit2, col=as.numeric(bs2$cl), dimen=2)
lda.arrows(lda.fit2, myscale = 3, col = "#666666")
summary(lda.fit2)


# Super bonus

model_predictors <- dplyr::select(train, -crim)
# check the dimensions
dim(model_predictors)
dim(lda.fit$scaling)
# matrix multiplication
matrix_product <- as.matrix(model_predictors) %*% lda.fit$scaling
matrix_product <- as.data.frame(matrix_product)

library(plotly)
plot_ly(x = matrix_product$LD1, y = matrix_product$LD2, z = matrix_product$LD3, type= 'scatter3d', mode='markers')

plot_ly(x = matrix_product$LD1, y = matrix_product$LD2, z = matrix_product$LD3, type= 'scatter3d', mode='markers', color=train$crim)

train$cl <- bs2$cl[match(rownames(train), rownames(bs2))]
plot_ly(x = matrix_product$LD1, y = matrix_product$LD2, z = matrix_product$LD3, type= 'scatter3d', mode='markers', color=as.factor(train$cl))





