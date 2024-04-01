#install.packages("pacman")
install.packages()
require(ggplot2)
library()
# For data visualization
library(rpart.plot)
# Contains the data
library(ISLR)
dataset <- read.csv(file = "Intel_CPUs.csv")
#summary(dataset)
#head(dataset)
allVariables <- names(dataset)
#allVariables
usedVariables <- c(allVariables[6], allVariables[7], allVariables[8], allVariables[10])
usedVariables
usedDataset <- dataset[,usedVariables]

#describe(usedDataset)
summary(usedDataset)

