# Henna Kettunen
# 2017-02-06
# Exercise 3: data wrangling

# Data source: https://archive.ics.uci.edu/ml/datasets/STUDENT+ALCOHOL+CONSUMPTION

# Load dplyr
library(dplyr)


# Read in data
mat <- read.table("./data/student-mat.csv", sep=";", dec=".", header=T)
por <- read.table("./data/student-por.csv", sep=";", dec=".", header=T)

# Explore dimensions and structure
str(mat)
str(por)


## Combine the two datasets

# Common columns to use as identifiers
join_by <- c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")

# Join the two datasets by the selected identifiers
# Inner join keeps only the students present in both data sets
mat_por <- inner_join(mat, por, by = join_by, suffix=c(".mat",".por"))

# Explore the structure and dimensions of the joined data
glimpse(mat_por)


## Combine the 'duplicated' answers in the joined data

# Column names of 'mat_por'
column_name <- colnames(mat_por)

# Create a new data frame with only the joined columns
alc <- select(mat_por, one_of(join_by))

# The columns in the datasets which were not used for joining the data
notjoined_columns <- colnames(mat)[!colnames(mat) %in% join_by]

# Print out the columns not used for joining
notjoined_columns

# for every column name not used for joining...
for(column_name in notjoined_columns) {
  # select two columns from 'mat_por' with the same original name
  two_columns <- select(mat_por, starts_with(column_name))
  # select the first column vector of those two columns
  first_column <- select(two_columns, 1)[[1]]
  
  # if that first column vector is numeric...
  if(is.numeric(first_column)) {
    # take a rounded average of each row of the two columns and
    # add the resulting vector to the alc data frame
    alc[column_name] <- round(rowMeans(two_columns))
  } else { # else if it's not numeric...
    # add the first column vector to the alc data frame
    alc[column_name] <- first_column
  }
}


# Take the average of the answers related to weekday and weekend alcohol consumption to create a new column 'alc_use' to the joined data
alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)

# Then use 'alc_use' to create a new logical column 'high_use' which is TRUE for students for which 'alc_use' is greater than 2 (and FALSE otherwise)
alc <- mutate(alc, high_use = alc_use > 2)

# Glimpse at the joined and modified data to make sure everything is in order
glimpse(alc)

# Save the joined and modified data set to the ‘data’ folder
write.table(alc, "./data/alc.csv", sep=";", dec=".", row.names=F, na="")
test <- read.table("./data/alc.csv", sep=";", dec=".", header=T)
glimpse(test, 50)

# Looks good





