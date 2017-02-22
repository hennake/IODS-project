# CH5 Data Camp
#
######################################
#
# Meet the human data
# 100xp
# Welcome to the Dimensionality reduction techniques chapter.
#
# In this chapter we will be using the human dataset to introduce Principal Components Analysis (PCA).
# The data originates from the United Nations Development Programme. See their data page for more information.
# For a nice overview see also the calculating the human development indices pdf.
#
# Most of the variable names have been shortened and two new variables have been computed.
# See the meta file for the modified data here for descriptions.
#
# Instructions
# Read the human data into memory
# Print out the (column) names of the data
# Look at the structure of the data
# Print out summaries of the variables in the data

# read the human data
human <- read.table("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human1.txt", sep  =",", header = T)

# look at the (column) names of human
names(human)

# look at the structure of human
str(human)

# print out summaries of the variables
summary(human)

############################################

# String manipulation
# 100xp
# Sometimes a variable is coded in a way that is not natural for R to understand. For example,
# large integers can sometimes be coded with a comma to separate thousands. In these cases, R interprets the variable
# as a factor or a character.
#
# In some cases you could use the dec argument in read.table() to get around this, but if the data also includes decimals
# separated by a dot, this is not an option. To get rid of the unwanted commas, we need string manipulation.
#
# In R, strings are of the basic type character and they can be created by using quotation marks or specific functions.
# There are quite a few functions in Base R that can be used to manipulate characters, but there is also a bit more
# consintent and simple tidyverse package stringr.
#
# Instructions
# Access the stringr package
# Look at the structure of the Gross National Income (GNI) variable in human
# Execute the sample code where the comma is removed from each value of GNI.
# Adjust the code: Use the pipe operator (%>%) to convert the resulting vector to numeric with as.numeric

# tidyr package and human are available

# access the stringr package
library(stringr)

# look at the structure of the GNI column in 'human'
str(human$GNI)

# remove the commas from GNI and print out a numeric version of it
str_replace(human$GNI, pattern=",", replace ="") %>% as.numeric()


######################################

# Dealing with not available (NA) values
# 100xp
# In R, NA stands for not available, which means that the data point is missing. If a variable you wish to analyse
# contains missing values, there are usually two main options:
#  
#   Remove the observations with missing values
# Replace the missing values with actual values using an imputation technique.
# We will use the first option, which is the simplest solution.
#
# Instructions
# Create a smaller version of the human data by selecting the variables defined in keep
# Use complete.cases() on human to print out a logical "completeness indicator" vector
# Adjust the code: Define comp as the completeness indicator and print out the resulting data frame. When is the
# indicator FALSE and when is it TRUE? (hint: ?complete.cases()).
# filter() out all the rows with any NA values. Right now, TRUE is recycled so that nothing is filtered out.
#

# human with modified GNI and dplyr are available

# columns to keep
keep <- c("Country", "Edu2.FM", "Labo.FM", "Life.Exp", "Edu.Exp", "GNI", "Mat.Mor", "Ado.Birth", "Parli.F")

# select the 'keep' columns
human <- select(human, one_of(keep))

# print out a completeness indicator of the 'human' data
complete.cases(human)

# print out the data along with a completeness indicator as the last column
data.frame(human[-1], comp = complete.cases(human))

# filter out all rows with NA values
human_ <- filter(human, comp)


####################################

# Excluding observations
# 100xp
# Besides missing values, there might be other reasons to exclude observations. In our human data, there are a few data points
# which have been computed from other observations. We want to remove them before further analysis.
#
# The basic way in R to reference the rows or columns of a data frame is to use brackets ([,]) along with indices or names.
# A comma is used to separate row and column references. In the examples below, df is a data frame.
#
# df[,] # select every row and every column
# df[1:5, ] # select first five rows
# df[, c(2, 5)] # select 2nd and 5th columns
# Instructions
# Use tail() to print out the last 10 observations of human (hint: ?tail). What are the last 10 country names?
# Create object last
# Create data frame human_ by selecting rows from the 1st to last from human.
# Define the rownames in human_ by the Country column
#

# human without NA is available

# look at the last 10 observations of human
tail(human, 10)

# define the last indice we want to keep
last <- nrow(human) - 7

# choose everything until the last 7 observations
human_ <- human[1:last, ]

# add countries as rownames
rownames(human_) <- human_$Country

####################################

# Exploring the countries
# 100xp
# Now that we have sufficiently wrangled the 'human' data for further analysis, let's explore the variables and their
# relationships more closely.
#
# A simple pairs plot or a more informative generalized pairs plot from the GGally package is a good way of visualizing a
# reasonably sized data frame.
#
# To study linear connections, correlations also can be computed with the cor() function and then visualized with the
# corrplot function from the corrplot package.
#
# Instructions
# Create the data frame human_ by removing the Country variable from human (the countries are still the row names)
# Access the GGally package and visualize all the human_ variables with ggpairs().
# Compute and print out the correlation matrix of human_
# Adjust the code: use the pipe operator (%>%) and visualize the correlation matrix with corrplot().

# modified human, dplyr and the corrplot functions are available

# remove the Country variable
human_ <- select(human, -Country)

# Access GGally
library(GGally)

# visualize the 'human_' variables
ggpairs(human_)

# compute the correlation matrix and visualize it with corrplot
cor(human_) %>% corrplot()

############################

#
# PCA with R
# 100xp
# Principal Component Analysis (PCA) can be performed by two sightly different matrix decomposition methods from
# linear algebra: the Eigenvalue Decomposition and the Singular Value Decomposition (SVD).
#
# There are two functions in the default package distribution of R that can be used to perform PCA: princomp() and prcomp().
# The prcomp() function uses the SVD and is the preferred, more numerically accurate method.
#
# Both methods quite literally decompose a data matrix into a product of smaller matrices, which let's us extract the
# underlying principal components. This makes it possible to approximate a lower dimensional representation of the data
# by choosing only a few principal components.
#
# Instructions
# Create human_std by standardizing the variables in human.
# Print out summaries of the standardized variables. What are the means? Do you know the standard deviations? (hint: ?scale)
# Use prcomp() to perform principal component analysis on the standardized data. Save the results in the object pca_human
# Use biplot() to draw a biplot of pca_human (Click next to "Plots" to view it larger)
# Experiment with the argument cex of biplot(). It should be a vector of length 2 and it can be used to scale the labels in the biplot. Try for example cex = c(0.8, 1). Which number affects what?
# Add the argument col = c("grey40", "deeppink2")


# modified human is available

# standardize the variables
human_std <- scale(human)

# print out summaries of the standardized variables
summary(human_std)

# perform principal component analysis (with the SVD method)
pca_human <- prcomp(human_std)

# draw a biplot of the principal component representation and the original variables
biplot(pca_human, choices = 1:2, cex = c(0.5, 0.7), col = c("grey40", "deeppink2"))

#######################################


# A biplot of PCA
#
# A biplot is a way of visualizing the connections between two representations of the same data.
# First, a simple scatter plot is drawn where the observations are represented by two principal
# components (PC's). Then, arrows are drawn to visualize the connections between the original variables and the PC's.
# The following connections hold:
#
# The angle between the arrows can be interpret as the correlation between the variables.
# The angle between a variable and a PC axis can be interpret as the correlation between the two.
# The length of the arrows are proportional to the standard deviations of the variables
# Instructions
# Create and print out a summary of pca_human (created in the previous exercise)
# Create object pca_pr and print it out
# Adjust the code: instead of proportions of variance, save the percentages of variance in the pca_pr object.
# Round the percentages to 1 digit.
# Execute the paste0() function. Then create a new object pc_lab by assigning the output to it.

# pca_human, dplyr are available

# create and print out a summary of pca_human
s <- summary(pca_human)
s

# rounded percetanges of variance captured by each PC
pca_pr <- round(100*s$importance[2, ], digits = 1)

# print out the percentages of variance
pca_pr

# create object pc_lab to be used as axis labels
pc_lab <- paste0(names(pca_pr), " (", pca_pr, "%)")

# draw a biplot
biplot(pca_human, cex = c(0.8, 1), col = c("grey40", "deeppink2"), xlab = pc_lab[1], ylab = pc_lab[2])


###################################3


# It's tea time!
# 100xp
# The Factominer package contains functions dedicated to multivariate explanatory data analysis. It contains for example
# methods (Multiple) Correspondence analysis , Multiple Factor analysis as well as PCA.
#
# In the next exercises we are going to use the tea dataset. The dataset contains the answers of a questionnaire on tea
# consumption.
#
# Let's dwell in teas for a bit!
#  
# Instructions
# Create the keep_columns object. Then select() the columns from tea to create a new dataset. Save the new data as tea_time.
# Look at the summaries and structure of the tea_time data.
# Visualize the dataset. Define the plot type by adding geom_bar() after initialization of the ggplot. (Ignore the warning.)
# Adjust the code: the labels of the x-axis are showing poorly. Make the plot more readable by adding
# theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) after barplot the code.

# the tea dataset and packages FactoMineR, ggplot2, dplyr and tidyr are available

# column names to keep in the dataset
keep_columns <- c("Tea", "How", "how", "sugar", "where", "lunch")

# select the 'keep_columns' to create a new dataset
tea_time <- select(tea, one_of(keep_columns))

# look at the summaries and structure of the data
summary(tea_time)
str(tea_time)

# visualize the dataset
gather(tea_time) %>% ggplot(aes(value)) + facet_wrap("key", scales = "free") + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))


####################################3

#
# Multiple Correspondence Analysis
# 100xp
# Multiple Correspondence Analysis (MCA) is a method to analyze qualitative data and it is an extension of Correspondence
# analysis (CA). MCA can be used to detect patterns or structure in the data as well as in dimension reduction.
#
# Instructions
# Do multiple correspondence analysis with the function MCA(). Give tea_time as the functions first argument.
# Note that the MCA() function visualizes the analysis by default, and the plots can be turned off with the argument
# graph = FALSE.
# Look at the summary of the model.
# Plot the variables of the model. You can either plot the variables or the individuals or both. You can change which one
# to plot with the invisible argument.
# Adjust the code: add argument habillage = "quali" (how french!) to the plot. Do you notice what changes?

# tea_time is available

# multiple correspondence analysis
mca <- MCA(tea_time, graph = FALSE)

# summary of the model
summary(mca)

# visualize MCA
plot(mca, invisible=c("ind"), habillage = "quali")


Draw the biplot again. Use the first value of the pc_lab vector as the label for the x-axis and the second value as the label for the y-axis.