# Load Libraries
library(plyr)
library(readr)
library(mice)
library(VIM)
library(ggplot2)
library(pwr)
library(corrplot)
library("factoextra")
library("FactoMineR")
library("corrplot")
library(factoextra)


# Loading individual data csv for the years 2012 - 2019.
property_df1 <- read.csv('C:/Users/Shylesh Patil/Downloads/CA3/PPR-2014.csv')
property_df2 <- read.csv('C:/Users/Shylesh Patil/Downloads/CA3/PPR-2015.csv')
property_df3 <- read.csv('C:/Users/Shylesh Patil/Downloads/CA3/PPR-2016.csv')
property_df4 <- read.csv('C:/Users/Shylesh Patil/Downloads/CA3/PPR-2017.csv')
property_df5 <- read.csv('C:/Users/Shylesh Patil/Downloads/CA3/PPR-2018.csv')
property_df6 <- read.csv('C:/Users/Shylesh Patil/Downloads/CA3/PPR-2019.csv')

# Setting working directory.
setwd('C:/Users/Shylesh Patil/Downloads/CA3/')

# Merging data sets.
filenames <- list.files(full.names = TRUE)
completeData <- lapply(filenames,function(i){read.csv(i, header = FALSE, skip = 1)})
completeData

# Binding all the datasets.
#df <- do.call(rbind.data.frame, completeData)

# Creating a sinngle CSV file.
write.csv(df,"completeDataFile.csv", row.names = FALSE)
property_complete <- read.csv('C:/Users/Shylesh Patil/Downloads/CA3/completeDataFile.csv')

str(property_complete)

#Data Cleaning
#Remove € symbol
property_complete$V5 = as.numeric(gsub("[\\€,]", "", property_complete$V5))
property_complete$V5

#displaying number of rows and columns
nrow(property_complete)
ncol(property_complete)

# Creating a complete Dataframe.
property_dataFrame <- data.frame(property_complete)


#Data Cleaning
#Column Name.
colnames(property_dataFrame) <- c("Date", "Address", "PostalCode", "County", "Price", "MarketPrice", "VATExcl", "PropertyDescription", "AdditionalInfo")
str(property_dataFrame)


# Checking for null Values in Dataframe.
sum(is.na(property_dataFrame))

# Plotting the null value.
md.pattern(property_dataFrame)

# Converting Date field from factor to date.
dateField <- as.character(property_dataFrame$Date)
str(dateField)

# Obtaining only Year from Date column.
yearField <- format(as.Date(property_dataFrame$Date, format="%d/%m/%Y"),"%Y")
property_dataFrame$Date <- yearField
str(property_dataFrame)

# Converting year to numeric data type.
property_dataFrame$Date <- as.numeric(property_dataFrame$Date)
str(property_dataFrame)

summary(property_dataFrame)

# Data Analysis

# Graphical Representations

# Checking the number of houses registered yearwise.

yearwisePlot <- property_dataFrame$Date
hist(yearwisePlot)
plot(x=property_dataFrame$Price, y=property_dataFrame$Date)

# Yearwise registration.
densityplot(property_dataFrame$Date)

# Barplot
barplot(table(property_dataFrame$Date))

# Normality test with Kolmogorov–Smirnov test.
ks.test(property_dataFrame$Date, property_dataFrame$Price)

# Displaying Counties with the number of houses.
sort(table(property_dataFrame$County), increasing = TRUE)

# Plotting the data in bar-chart.
barplot(table(property_dataFrame$County))

# Plotting data for Price/County
plot(x=property_dataFrame$Price, y=property_dataFrame$County)

# Year/Price Analysis
# Creating new dataframe with only Year and Price Column.
dataFrame_YP <- data.frame(property_dataFrame$Date, property_dataFrame$Price)
str(dataFrame_YP)

# Spearman model to check the corelation between Year and Price.
spearman <- cor.test(dataFrame_YP$property_dataFrame.Date, dataFrame_YP$property_dataFrame.Price, method = "spearman")
spearman

# Normality test County/Price
ks.test(property_dataFrame$County, property_dataFrame$Price)

# Chisquare test to check the Categorical correlation.
chisq.test(property_dataFrame$County, property_dataFrame$Price, correct=FALSE)

# As p value is less than 0.05, H0 to rejected.

# Finding the effective size (d value for powertest)
effective_size <- cohen.ES(test = "r", size = "large")
effective_size

# Considering Effective Size and Alpha as 5% ,Power analysis is calculated.
#pwr.t.test for corelation.
power_analysis <-pwr.t.test(d=0.5,n=NULL,sig.level=0.05,  power=0.95, type="one.sample",alternative="two.sided")
power_analysis

# Plotting power analysis
plot(power_analysis)

#corelation graph displaying values.
corDF <- data.frame(property_dataFrame$Date, property_dataFrame$Price)
M<- cor(corDF)
corrplot(M, method = "number")

# PCA Analysis

# Dataframe is in numerical format.

pca <- prcomp(corDF, center = TRUE, scale. = TRUE)
summary(pca)
str(pca)

# Finding the Eigen Value.
eigenValues <- get_eigenvalue(pca)
eigenValues

pca_2 <- PCA(corDF, graph = FALSE)
print(pca_2)

# Eigen Value of PCA 2
pca2_eigenValues <- get_eigenvalue(pca_2)
pca2_eigenValues

# Plotting Variances
fviz_eig(pca, addlabels = TRUE, ylim = c(0, 100))
variablePca <- get_pca_var(pca)
variablePca

variablesPca <- get_pca_var(pca)
variablesPca
fviz_cos2(pca, choice = "var", axes = 1:2)
fviz_pca_var(
  pca, col.var = "contrib",
  gradient.cols = c("red", "Blue", "Green"),
)
fviz_contrib(pca, choice = "var", axes =1, 2, top = 100)
