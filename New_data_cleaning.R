# Installing packages
install.packages('car')
install.packages('caret')
install.packages('dplyr')
install.packages('drc')
install.packages('ggplot2')
install.packages('lessR')
install.packages('randomForest')
install.packages('readr')
install.packages('nlme')
install.packages('nls.multstart')
install.packages('pacman')
install.packages('readxl')
install.packages('tidyverse')

library(car)
library(caret)
library(dplyr)
library(drc)
library(ggplot2)
library(lessR)
library(randomForest)
library(readr)
library(nlme)
library(nls.multstart)
library(pacman)
library(readxl)
library(tidyverse)

pacman::p_load(
  rio,     # for dealing with basic import export
  ggplot2, # for dealing with plot formats
  zoo      # for dealing with year quarter format
)

# Importing data
setwd("E:/2_LEARNING_BKU/2_File_2/K22_HK4/MT2013_XSTK/BTL/Dataset")
data <- import("./Intel_CPUs.csv")
View(data)

########################################################
data1 <- data[, c("Vertical_Segment", "Status", "Launch_Date", "Lithography",
                 "Recommended_Customer_Price", "nb_of_Cores",
                 "Processor_Base_Frequency", "TDP","T")] 

View(data1)

names(data1) <- c("market", "status", "ldate", "litho", "rprice", "ncore", "bfreq", "tdp", "temp")
names(data1)

View(data1)
export(data1, "cpu_choose_cols.csv")
# ----------------------------------------------------------------------------------
# Now cleaning the data
names(data1)
unique(data1$ldate) 

# Transform some wrong format in LDate to the right format
# exist "Q1 '15"
data1$ldate <- gsub("\\s*'(\\d{2})", "'\\1", data1$ldate)
unique(data1$ldate)

# Convert the string of characters to year quarters
data1[,"ldate"] <- (as.yearqtr(data1[,"ldate"], format = "Q%q'%y"))
unique(data1$ldate)
# ----------------------------------------------------------------------------------
# here we clean the data for the recommended price
# for those price in the range of a and b, we take the mean of them
# for those not, only convert the value
unique(data1$rprice)
data1$rprice <- ifelse(data1$rprice == "N/A", NA, data1$rprice)

# Function to convert price string to numeric
convert_price <- function(price_str) {
  # Check if the string is NA
  if (is.na(price_str)) {
    return(NA)
  }
  
  # Remove "$" sign and commas ","
  price_str <- gsub("\\$", "", price_str)
  price_str <- gsub(",", "", price_str)
  
  # Check if the string contains a range
  if (grepl("-", price_str)) {
    # Extract numeric parts from the range
    price_range <- as.numeric(unlist(strsplit(price_str, " - ")))
    # Calculate the average of the range
    price_numeric <- max(price_range, na.rm = TRUE)
  } else {
    # Extract numeric part from the string
    price_numeric <- as.numeric(price_str)
  }
  return(price_numeric)
}

# Apply the function to the entire column
data1$rprice <- sapply(data1$rprice, convert_price)

View(data1)
# ----------------------------------------------------------------------------------
names(data1)
unique(data1$litho)

data1[,"litho"] <- as.numeric(gsub(" nm", "", data1[,"litho"]))
# ----------------------------------------------------------------------------------
unique(data1$tdp)

data1[,"tdp"] <- as.numeric(gsub(" W", "", data1[,"tdp"]))

unique(data1$tdp)
# ----------------------------------------------------------------------------------
names(data1)
unique(data1$bfreq)

# Extract numeric part properly and convert to MHz for non-NA values
non_na_indices <- !is.na(data1$bfreq)
data1$bfreq[non_na_indices] <- 
  ifelse(grepl("GHz", data1$bfreq[non_na_indices]),
         as.numeric(sub("\\s*GHz", "", data1$bfreq[non_na_indices])),
         as.numeric(sub("\\s*MHz", "", data1$bfreq[non_na_indices]))*0.001)

# Unit GHz
data1$bfreq <- as.numeric(data1$bfreq)
data1 <- data1[!is.na(data1$bfreq), ]
unique(data1$bfreq)

# ----------------------------------------------------------------------------------
# Xu li data cua nhiet do
# extract so lon nhat cua string
names(data1)

unique(data1$temp)

# Function to extract numeric values and find the maximum value
get_max_numeric <- function(row) {
  # Extract numeric values from the row and remove non-numeric characters
  numeric_values <- str_extract_all(row, "-?\\d*\\.?\\d+")
  
  # Convert the extracted numeric values to numeric data type
  numeric_values <- lapply(numeric_values, function(x) as.numeric(x))
  
  # Filter out NA values
  numeric_values <- lapply(numeric_values, function(x) x[!is.na(x)])
  
  # If there are no numeric values, return NA
  if (length(unlist(numeric_values)) == 0) {
    return(NA)
  }
  
  # Find the largest numeric value
  max_value <- max(unlist(numeric_values))
  
  return(max_value)
}

# Apply the function to each row of the dataframe data3$T
max_values <- sapply(data1$temp, get_max_numeric)

# Replace the "T" column with the maximum values
data1$temp <- max_values

View(data1)

names(data1)

summary(data1)

xtabs(~status,data=data1)
xtabs(~ldate,data=data1)
xtabs(~litho,data=data1)
xtabs(~rprice,data=data1)
xtabs(~ncore,data=data1)
xtabs(~bfreq,data=data1)
xtabs(~tdp,data=data1)
xtabs(~temp,data=data1)

data1 <- data1[!is.na(data1$tdp), ]
data1 <- data1[!is.na(data1$litho), ]
data1 <- data1[!is.na(data1$bfreq), ]
data1 <- data1[!is.na(data1$ncore), ]
data1 <- data1[!is.na(data1$temp), ]

export(data1, "cpu_clean.csv")
# Note hien tai in ra nhung chua remove duplicates
# ----------------------------------------------------------------------------------