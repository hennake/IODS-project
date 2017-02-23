# Henna Kettunen
# 2017-02-15
# Exercise 4: data wrangling


# Read the “Human development” and “Gender inequality” datas into R
hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)
gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")

# Explore the datasets: see the structure and dimensions of the data. Create summaries of the variables.
str(hd)
str(gii)
summary(hd)
summary(gii)

# Look at the meta files and rename the variables with (shorter) descriptive names.
colnames(hd) <- c("HDIrank","country","HDI","lifeexp","expedu","meanedu","GNI","rank2")
colnames(gii) <- c("GIIrank","country","GII","matmor","adbi","repparl","edu2F","edu2M","labF","labM")

# Mutate the “Gender inequality” data and create two new variables. 
# The first one should be the ratio of Female and Male populations with secondary education in each country. (i.e. edu2F / edu2M). 
# The second new variable should be the ratio of labour force participation of females and males in each country (i.e. labF / labM)
library(dplyr)
gii <- mutate(gii, edu2FMrat = edu2F / edu2M)
gii <- mutate(gii, labFMrat = labF / labM)

# Join together the two datasets using the variable Country as the identifier. 
# Keep only the countries in both data sets (Hint: inner join). 
human <- inner_join(hd, gii, by = "country")

# Call the new joined data human and save it in your data folder.
save(human, file="./data/human.RData")


# 2017-02-22
# Exercise 5: data wrangling

library(stringr)


# Mutate the data: transform the Gross National Income (GNI) variable to numeric (Using string manipulation.)
human$GNI <- str_replace(human$GNI, pattern=",", replace ="") %>% as.numeric()


# Exclude unneeded variables: keep only the columns matching the following variable names:  
# "Country", "Edu2.FM", "Labo.FM", "Edu.Exp", "Life.Exp", "GNI", "Mat.Mor", "Ado.Birth", "Parli.F"
# Note: I have different names
keep <- c("country", "edu2FMrat", "labFMrat", "lifeexp", "expedu", "GNI", "matmor", "adbi", "repparl")
human <- select(human, one_of(keep))


# Remove all rows with missing values.
human2 <- filter(human, complete.cases(human))


# Remove the observations which relate to regions instead of countries.
regions<-c("Arab States", "East Asia and the Pacific", "Europe and Central Asia", "Latin America and the Caribbean", "South Asia", 
           "Sub-Saharan Africa", "World")
human3 <- human2[-which(human2$country %in% regions),]


# Define the row names of the data by the country names and remove the country name column from the data. The data should now have 155 observations and 8 variables 
rownames(human3) <- human3$country
human4 <- select(human3, -country)

save(human4, file="./data/human.RData")



