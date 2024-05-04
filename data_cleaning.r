# ----------------------------------------------------------------------------------
#                               INSTALLING PACKAGES                                #
# ----------------------------------------------------------------------------------
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
  zoo,     # for dealing with year quarter format
  car,     # for Levent and S   hapiro
  FSA      # for Dunn test
)

# Import data
data <- import("Dataset/cpu_raw.csv")       # rio::import
data1 <- data[, c("Vertical_Segment", "Status", "Launch_Date", "Lithography",
                 "Recommended_Customer_Price", "nb_of_Cores",
                 "Processor_Base_Frequency", "TDP","T")] 

names(data1) <- c("market", "status", "ldate", "litho", "rprice", "ncore", "bfreq", "tdp", "temp")
export(data1, "Dataset/cpu_choose_cols.csv")
View(data1)
# ----------------------------------------------------------------------------------
#                             NOW CLEANING THE DATA                                #
# ----------------------------------------------------------------------------------
# LAUNCHED DATE
# ----------------------------------------------------------------------------------
names(data1)
unique(data1$ldate) 

# Transform some wrong format (ex: "Q1 '15")
data1$ldate <- gsub("\\s*'(\\d{2})", "'\\1", data1$ldate)
# Convert to year quarters
data1[,"ldate"] <- (as.yearqtr(data1[,"ldate"], format = "Q%q'%y"))
unique(data1$ldate)
# ----------------------------------------------------------------------------------
# LITHOGRAPHY
# ----------------------------------------------------------------------------------
names(data1)
unique(data1$litho)

data1[,"litho"] <- as.numeric(gsub(" nm", "", data1[,"litho"]))
# ----------------------------------------------------------------------------------
# Here we clean the data for the RECOMMENDED PRICE
# for those price in the range of a and b, we take the Max of them
# for those not, only convert the value
# for those "N/A", convert to NA
# ----------------------------------------------------------------------------------
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
    price_range <- 
            as.numeric(unlist(strsplit(price_str, " - ")))
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
# PROCESSOR BASE FREQUENCY
# ----------------------------------------------------------------------------------
names(data1)
unique(data1$bfreq)

# Extract numeric part properly and convert to GHz (by multiply 0.001) for non-NA values
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
# THERMAL DESIGN POWER
# ----------------------------------------------------------------------------------
unique(data1$tdp)

data1[,"tdp"] <- as.numeric(gsub(" W", "", data1[,"tdp"]))

unique(data1$tdp)
# ----------------------------------------------------------------------------------
# TEMPERATURE
# ----------------------------------------------------------------------------------
# Take the Largest value
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
# ----------------------------------------------------------------------------------
View(data1)

names(data1)

# Select columns from 3rd column to the last column of the data1 dataframe and store it in df
df <- data1[, 3:ncol(data1)]

df <- summary(df)

# Convert the summary dataframe to a matrix
summary_df <- as.matrix(df)

# Open a PNG device to save the plot
png("Data_Cleaning/before_drop_NAs.png", width = 11, height = 4, unit = "in", res = 300)

# Add a title to the plot
grid.text("Dataset after cleaning", x = unit(0.5, "npc"), y = unit(0.9, "npc"), just = "center", gp = gpar(fontsize = 18, fontface = "bold"))

# Create a grid table from the summary matrix with text centered
grid_summary <- tableGrob(summary_df, theme = ttheme_default(
  core = list(fg_params = list(hjust = 0.5))  # Center text horizontally
))

# Draw the table onto the PNG
pushViewport(viewport(x = 0.5, y = 0.5, width = 1, height = 1))
grid.draw(grid_summary)
popViewport()

# Close the PNG device
dev.off()
# ----------------------------------------------------------------------------------
# Summary for each Variables
xtabs(~status,data=data1)
xtabs(~ldate,data=data1)
xtabs(~litho,data=data1)
xtabs(~rprice,data=data1)
xtabs(~ncore,data=data1)
xtabs(~bfreq,data=data1)
xtabs(~tdp,data=data1)
xtabs(~temp,data=data1)

# Drop NA values of these columns as they are not much
data1 <- data1[!is.na(data1$tdp), ]
data1 <- data1[!is.na(data1$litho), ]
data1 <- data1[!is.na(data1$temp), ]

summary(data1)

View(data1)

export(data1, "Dataset/cpu_clean.csv")
# ----------------------------------------------------------------------------------