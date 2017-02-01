# Henna Kettunen
# 2017-01-28
# Exercise 2: data wrangling
# Create learning2014 dataset

# Set working directory
setwd("C:\\Users\\MyDir\\iods\\IODS-project")

# Load dplyr
library(dplyr)

# Read in the data and metadata
data <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", header=T)
meta <- readLines("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS2-meta.txt")

# What does the data look like?
head(data)

# How is the data stored?
str(data)

# Dimensions of the data
dim(data)

# Create dataset and variables
d <- transmute(data,
  gender = gender,
  age = Age,
  attitude = Attitude,
  deep = (D03+D11+D19+D27 + D07+D14+D22+D30 + D06+D15+D23+D31) / 12,
  stra = (SU02+SU10+SU18+SU26 + SU05+SU13+SU21+SU29 + SU08+SU16+SU24+SU32) / 12,
  surf = (ST01+ST09+ST17+ST25 + ST04+ST12+ST20+ST28) / 8,
  points = Points
)

# Exclude observations where the exam points variable is zero
# The data should then have 166 observations and 7 variables
d <- filter(d, points!=0)
dim(d)

# Save the data
?write.table
write.table(d, ".\\data\\learning2014.txt", col.names=T, row.names=F, sep="\t", quote=F)
save.image(".\\data\\learning2014.RData")

# Does it work?
temp <- read.table(".\\data\\learning2014.txt", header=T, sep="\t")
head(temp)
str(temp)

# Clean the workspace
rm(tmp)
gc()


