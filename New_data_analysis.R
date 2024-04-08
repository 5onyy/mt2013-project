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
  rio,            # for imports & exports
  ggplot2,        # for plots
  zoo             # for year-quarter formats
)

library(rio)     # for imports & exports
library(ggplot2) # for plots
library(zoo)     # for year-quarter formats
library(randomForest)

data <- import("cpu_clean.csv") # rio::impor

summary(data)
unique(data$litho)
names(data)

# Data summary by xtabs
# Occurrences of TDP >= 150 is rare --> may delete
xtabs(~tdp,data=data)
xtabs(~litho,data=data)

# Remove data with few count group ldate and remove NA
data <- data[data$litho != 28, ]
data <- data[data$litho != 250, ]
xtabs(~litho,data=data)

summary(data)
data <- data[!is.na(data$tdp), ]
data <- data[!is.na(data$litho), ]
summary(data)
unique(data$litho)

data <- data[data$tdp < 150, ]
data <- data[!is.na(data$tdp), ]
data <- data[!is.na(data$bfreq), ]
data <- data[!is.na(data$litho), ]
data <- data[!is.na(data$ncore), ]
data <- data[!is.na(data$temp), ]
summary(data)

# Assuming 'data' is your dataframe
data2 <- data
xtabs(~tdp,data=data)

# Lithography as tdp is less convincing; 
# however, we see that recent lithography techniques 
# tend to have stable base frequency
plot_data <- data
plot_data$litho <- as.factor(plot_data$litho)
ggplot(plot_data, aes(x = litho, y = tdp)) + geom_boxplot(fill = "deepskyblue")

# ---------------------------------------------------------------------------
# In this small section, we will demonstrate why Lithography 
# as a better representative than Launch date. 
# To do that, we start by looking at the confidence interval 
# and the visualizations of Lithography over the years.
data$litho <- as.factor(data$litho)

retval <- data.frame(NA, NA, NA, NA)
names(retval) <- c("5% quantile","95% quantile", "STD Mean", "Confidence Interval")

for (lit in levels(data$litho))
{
  # this line calculates the 5th and 95th quantiles of the column ldate 
  # from the subset of data where litho is equal to the current level lit.
  quants <- quantile(data[data$litho == lit, ]$ldate, na.rm = T, probs = c(0.05,0.95))
  
  # extracts the subset of ldate for the current level lit.
  dates <- data[data$litho == lit, ]$ldate
  
  new_row <- data.frame(quants[1], quants[2], 
                        mean(sd(dates, na.rm=TRUE),
                        na.rm=TRUE),quants[2]-quants[1])
  
  names(new_row)<-c("5% quantile","95% quantile", "STD Mean", "Confidence Interval")
  
  # Append new row to the dataframe
  retval <- rbind(retval, new_row)
  rm(dates)
}

rownames(retval) <- c("NULL", levels(data$litho))
retval <- retval[-1,]

print(retval)

ggplot(data, aes(x = ldate, y = litho)) + geom_boxplot(fill = "deepskyblue")

# => Means are stable
# => Confidence Interval column tells us that most of the era of CPU design 
# spans for about two and a half years, and these era are approximately 
# mutually exclusive
# => Everytime we wants to refer to a period of CPU, use Lithography.
# ---------------------------------------------------------------------------
# Emphasize is the stability of TDP in recent eras, and the fact that 
# it is converging.
# => Do the ANOVA to test and see if there are a significant difference 
# in the tdp between the lithography era. We will have the null hypothesis:
# - H0: the mean of the tdp in each type of lithography is the same.
# - H1: there exist a pair of lithography type so that their mean is difference.

litho_anova_model <- aov(tdp ~ litho, data = data)
summary(litho_anova_model)
# => As pvalue < 0.05 --> reject the H0 --> exist a pair different mean

# To satisfy the requirements of One-way ANOVA, 
# we should check its assumptions on Normality
# and Homoscedasticity (homogeneous variance)

# visual-check: see if same distribution by see points fit line or not
# --> fail
qqPlot(residuals(litho_anova_model))

# test whether from same normal distribution --> fail
shapiro.test(residuals(litho_anova_model)) 

# test for homogeneous variance --> fail
leveneTest(tdp ~ litho, data = data) 

# => As fail, to make sure we will also be including the Kruskal - Wallis test 
# as a non parametric alternative to the ANOVA test.
# *Kruskal-Wallis H-Test
# - H0 : The TDP come from the same populations with same median.
# - H1 : These TDP come from populations with median that are not equal.
kruskal.test(tdp ~ litho, data = data)

# => fail
# => exist at least a pair of litho type so that their tdp median is different.


# Finally we will analyse the result with a post hoc test to see 
# which mean are different from each other. 
# For this, we will use the TUKEY HSD test for ANOVA and 
# Dunn test for Kruskal Wallis
Tukey <- TukeyHSD(litho_anova_model)
plot(Tukey,las = 2)
dunnTest(tdp ~ litho, data = data,method = "bonferroni")
# ---------------------------------------------------------------------------
# REGRESSION ANALYSIS
# ---------------------------------------------------------------------------
# Splitting to train and test 
# Set default seed for random
set.seed(123)

# Use 80% of data frame as training set and 20% as test set
train_indices <- sample(1:nrow(data), nrow(data) * 0.8)
train <- data[train_indices, ]
test <- data[-train_indices, ]

# Build the model
model.lr <- lm(tdp ~ ncore + bfreq + temp , data = train)

# Summary of the model
summary(model.lr)

# Testing for Residual Errors have a Mean Value of Zero
ggplot(model.lr, aes(x = resid(model.lr))) + 
  geom_histogram(binwidth = 2, fill = "deepskyblue")

# We can check assumption:
# Residual Errors have Constant Variance by using the Scale-Location plot.
plot(model.lr, which = 3)

# => equally spread out in a weird pattern
# => residuals scatter is not following any formal distribution and is random

# Testing for normality of the the errors:
# test if the residuals is normally distributed, 
# we plot the Q-Q plot using the command `plot()`.
plot(model.lr, which = 2)

# scatter plotting for the predicted value compared with the real value 
# in test set. The plotted red line in graph is (d) y = x. 
# The more concentration on this line the more correct the model does.

# Create data frame for actual tdp value and predicted tdp value
comtab.lr <- test['tdp']
comtab.lr['tdp_predicted'] <- as.data.frame(predict(model.lr, newdata = test))

# Plotting
# The majority of points lie near the line, so it is fine.
ggplot(comtab.lr, aes(x = tdp, y = tdp_predicted)) + 
  geom_point(shape=1, color="blue") + 
  geom_abline(mapping=aes(intercept= 0, slope=1), color="darkblue") + 
  labs(x = "TDP", y = "TDP Predicted")

# ---------------------------------------------------------------------------
# RANDOM FOREST REGRESSION MODEL
# Build the model
summary(data)

model.rfr <- randomForest(formula = tdp ~ bfreq + ncore + temp, data = train, ntree = 500)

# Print the model
print(model.rfr)

# Create data frame for real tdp value and predicted tdp value
comtab.rfr <- test['tdp']
comtab.rfr['tdp_predicted'] <- as.data.frame(predict(model.rfr, newdata = test), row.names = NULL)

# Evaluate model performance
accuracy <- sum(1-abs(comtab.rfr$tdp_predicted - comtab.rfr$tdp) / comtab.rfr$tdp) / nrow(comtab.rfr)
MAE <- sum(abs(comtab.rfr$tdp_predicted - comtab.rfr$tdp)) / nrow(comtab.rfr)

print(paste("Accuracy:", accuracy))
print(paste("MAE:", MAE))

# Calculate R-squared on testing data
r2_test <- cor(comtab.rfr$tdp, comtab.rfr$tdp_predicted)^2
print(r2_test)

# Calculate the residuals by subtracting the actual values from the predicted values
residuals <- comtab.rfr$tdp - comtab.rfr$tdp_predicted

# Create a normal probability plot of the residuals
qqnorm(residuals)
qqline(residuals)

# Plotting the predicted - actual
ggplot(comtab.rfr, aes(x = tdp, y = tdp_predicted)) + 
  geom_point(shape=1, color="blue") +
  geom_abline(mapping=aes(intercept= 0, slope=1),color="darkblue") + 
  labs(x = "TDP", y = "TDP Predicted")

