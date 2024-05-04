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
library(zoo)     # for year-quarter formats
library(rio)     # for imports & exports
library(grid)
library(gridExtra)
library(magick)

pacman::p_load(
  rio,            # for imports & exports
  ggplot2,        # for plots
  zoo             # for year-quarter formats
)

data <- import("Dataset/cpu_clean.csv") # rio::impor
# ----------------------------------------------------------------------------------
# SUMMARY OF DATA AND LITHO
# ----------------------------------------------------------------------------------
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

# Remove the NA values
data <- data[!is.na(data$tdp), ]
data <- data[!is.na(data$litho), ]
summary(data)
unique(data$litho)

# Occurrences of TDP >= 150 is rare --> may delete
data <- data[data$tdp < 150, ]
data <- data[!is.na(data$tdp), ]
data <- data[!is.na(data$bfreq), ]
data <- data[!is.na(data$litho), ]
data <- data[!is.na(data$ncore), ]
data <- data[!is.na(data$temp), ]
summary(data)

# Store dataframe data into data2
data2 <- data
xtabs(~tdp,data=data)

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

retval <- as.matrix(retval)

png("MLR/litho_confidence_interval.png", width = 11, height = 4, unit = "in", res = 300)

grid.text("Summary of Confidence Interval of Lithography over the years", x = unit(0.5, "npc"), y = unit(0.9, "npc"), just = "center", gp = gpar(fontsize = 18, fontface = "bold"))
# Create a grid table from the summary matrix with text centered
grid_summary <- tableGrob(retval, theme = ttheme_default(
  core = list(fg_params = list(hjust = 0.5))  # Center text horizontally
))

# Draw the table onto the PDF
pushViewport(viewport(x = 0.5, y = 0.5, width = 1, height = 1))
grid.draw(grid_summary)
popViewport()

# Close PDF device
dev.off()

ggplot(data, aes(x = ldate, y = litho)) + geom_boxplot(fill = "deepskyblue")

# => Means are stable
# => Confidence Interval column tells us that most of the era of CPU design 
# spans for about two and a half years, and these era are approximately 
# mutually exclusive
# => Everytime we wants to refer to a period of CPU, use Lithography.

plot_data <- data
plot_data$ncore <- as.factor(plot_data$ncore)
ggplot(plot_data, aes(x = ncore, y = tdp)) + geom_point(color = "deepskyblue")
ggplot(plot_data, aes(x = ncore, y = tdp)) + geom_boxplot(fill = "deepskyblue")

summary(plot_data)
plot_data$litho <- as.factor(plot_data$litho)
# Lithography as tdp is less convincing; however, we see that recent lithography techniques tend to have stable base frequency
ggplot(plot_data, aes(x = litho, y = tdp)) + geom_boxplot(fill = "deepskyblue")
ggplot(plot_data, aes(x = litho, y = tdp)) + geom_point(color = "deepskyblue")
# Different trends
ggplot(plot_data, aes(x = temp, y = tdp)) +
  geom_point(color = "deepskyblue", ) +
  geom_abline(mapping = aes(intercept= -50, slope = 1.2), color = "darkblue")

# Base frequqency is a bit random, but the trend of linearity is still evident
ggplot(plot_data, aes(x = bfreq, y = tdp)) + geom_point(color = "deepskyblue")

data$type <- ifelse(data$market == 'Server' | data$market == 'Desktop', "Computers", "Devices")
data$type <- as.factor(data$type)

ggplot(data, aes(x = market, y = tdp)) + geom_boxplot(fill = "deepskyblue")

ggplot(data, aes(x = temp, y = tdp)) + geom_point(color = "deepskyblue", ) + facet_wrap(~data$market)

ggplot(data, aes(x = type, y = tdp)) + geom_boxplot(fill = "deepskyblue")

ggplot(data, aes(x = temp, y = tdp)) + geom_point(color = "deepskyblue", ) + facet_wrap(~data$type)

# ---------------------------------------------------------------------------
# Emphasize is the stability of TDP in recent eras, and the fact that 
# it is converging.
# => Do the ANOVA to test and see if there are a significant difference 
# in the tdp between the lithography era. We will have the null hypothesis:
# - H0: the mean of the tdp in each type of lithography is the same.
# - H1: there exist a pair of lithography type so that their mean is difference.

litho_anova_model <- aov(tdp ~ litho, data = data)
pp <- summary(litho_anova_model)
print(p)
# => As pvalue < 0.05 --> reject the H0 --> exist a pair different mean

# To satisfy the requirements of One-way ANOVA, 
# we should check its assumptions on Normality
# and Homoscedasticity (homogeneous variance)

# visual-check: see if same distribution by see points fit line or not
# --> fail

#-----------------------------------------------------------------
png("MLR/qqplot_sptest_lvtest.png")
qqPlot(residuals(litho_anova_model))
dev.off()

# test whether from same normal distribution --> fail
sp_test <- shapiro.test(residuals(litho_anova_model)) 

# test for homogeneous variance --> fail
lv_test <- leveneTest(tdp ~ litho, data = data) 

#-------------------------------------------------------------------

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
# dunnTest(tdp ~ litho, data = data,method = "bonferroni")
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

#----------------------------------------------------------------------------
png("MLR/hist_residuals.png", width = 5, height = 7, unit = "in", res = 300)
p <- ggplot(model.lr, aes(x = resid(model.lr))) + 
  geom_histogram(binwidth = 2, fill = "darkorange") +
  labs(
    title = "Histogram of Residual Errors",
    x = "Residual Errors",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "darkblue", size = 14, face = "bold.italic"),
    axis.title.x = element_text(color = "blue", size = 12, face = "bold"),
    axis.title.y = element_text(color = "blue", size = 12, face = "bold")
  )
print(p)
dev.off()
#---------------------------------------------------------------------------
# We can check assumption:
# Residual Errors have Constant Variance by using the Scale-Location plot.
png("MLR/scale_location.png")
# Generate the plot
plot(model.lr, which = 3,
     col = "blue", pch = 20)
# Add a horizontal line at y = 0
abline(h = 0, col = "deepskyblue", lwd = 2)
dev.off()
#--------------------------------------------------------------------------
# => equally spread out in a weird pattern
# => residuals scatter is not following any formal distribution and is random

# Testing for normality of the the errors:
# test if the residuals is normally distributed, 
# we plot the Q-Q plot using the command `plot()`.
png("MLR/qq_plot.png")
# Generate the plot
plot(model.lr, which = 2,
     col = "firebrick", pch = 20)
# Add a horizontal line at y = 0
dev.off()

# scatter plotting for the predicted value compared with the real value 
# in test set. The plotted red line in graph is (d) y = x. 
# The more concentration on this line the more correct the model does.

#-------------------------------------------------------
# Summary of the model
ss <- summary(model.lr)
print(ss)
#-----------------------------------------------------
# Create data frame for actual tdp value and predicted tdp value
png("MLR/tdp_predicted.png", width = 5, height = 7, unit = "in", res = 300)
comtab.lr <- test['tdp']
comtab.lr['tdp_predicted'] <- as.data.frame(predict(model.lr, newdata = test))

p <- ggplot(comtab.lr, aes(x = tdp, y = tdp_predicted)) + 
  geom_point(color="firebrick", alpha = 0.6) + 
  geom_abline(mapping=aes(intercept= 0, slope=1), color="darkblue") + 
  labs(
    title = "TDP vs Predicted TDP",
    x = "TDP",
    y = "TDP Predicted"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "darkblue", size = 14, face = "bold.italic"),
    axis.title.x = element_text(color = "blue", size = 12, face = "bold"),
    axis.title.y = element_text(color = "blue", size = 12, face = "bold")
  )
print(p)
dev.off()
#------------------------------------------------------------------
# VIF 

model <- lm(tdp ~ ncore + bfreq + temp , data = train)
# Installing and loading the 'car' library
# install.packages("car")


# Calculating VIF
vif_values <- vif(model)
vif_values
# Visualizing VIF
barplot(vif_values, col = "skyblue", main = "Variance Inflation Factor (VIF)")

# Creating a correlation matrix
cor_matrix <- cor(train[c("ncore", "bfreq", "temp")])

# Visualizing the correlation matrix
image(cor_matrix, main = "Correlation Matrix", col = colorRampPalette(c("blue", "white", "red"))(20))

#perform Durbin-Watson test
durbinWatsonTest(model)

# ---------------------------------------------------------------------------
#Calculating MAE, MSE, RMSE
comtab.lr <- test['tdp']
comtab.lr['tdp_predicted'] <- as.data.frame(predict(model.lr, newdata = test), row.names = NULL)
print(paste("MSE: ", mean((comtab.lr$tdp - comtab.lr$tdp_predicted)^2)))
print(paste("MAE: ", caret::MAE(comtab.lr$tdp, comtab.lr$tdp_predicted)))
print(paste("RMSE: ", caret::RMSE(comtab.lr$tdp, comtab.lr$tdp_predicted)))

#----------------------------------------------------------------------------

