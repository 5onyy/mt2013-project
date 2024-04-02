pacman::p_load(
  rio,     # for imports & exports
  ggplot2, # for plots
  zoo,      # for year-quarter formats
  car,     # for levent and shapiro
  FSA,     # for Dunn test
)
# Load necessary packages
library(grid)
library(gridExtra)

#  IMPORT THE DATA
data <- import("Dataset/cpu_clean.csv")        # rio::import

# ---------------------------------------------------------------------------
# lets do some summary of the data first
# Generate summary statistics for the data frame
# Generate summary statistics for the data frame
summary_df <- summary(data)

# Convert summary to character matrix
summary_df <- as.matrix(summary_df)

# Open PDF device
pdf("Descriptive_statistics/summary_data_frame.pdf", width = 14, height = 4)

# Add a title
grid.text("Descriptive Statistics of Data", x = unit(0.5, "npc"), y = unit(0.9, "npc"), just = "center", gp = gpar(fontsize = 18, fontface = "bold"))

# Create a grid table from the summary matrix with text centered
grid_summary <- tableGrob(summary_df, theme = ttheme_default(
  core = list(fg_params = list(hjust = 0.5))  # Center text horizontally
))

# Adjust the viewport
pushViewport(viewport(x = 0.5, y = 0.5, width = 1, height = 1))

# Draw the table onto the PDF
grid.draw(grid_summary)

# Reset viewport
popViewport()

# Close PDF device
dev.off()

# ---------------------------------------------------------------------------
# Plot Litho and Launched Date
# Box-plotting
data <- data[!is.na(data$litho), ]
data
data$litho <- as.factor(data$litho)
data$litho
ggplot(data, aes(x = ldate, y = litho)) +
  geom_boxplot(fill = "deepskyblue")

# Scatter plotting
ggplot(data, aes(x = ldate, y = litho)) +
  geom_point(shape = 1,color = "blue") +
  labs(x = "Launch Date", y = "Lithography (nm)")

# By drawing out the LDate and Litho, we can see that
# Ldate increase and Litho decrease --> so we use Ltho instead of Ldata

# Now drawing the histogram of TDP
ggplot(data, aes(x = tdp)) +
  geom_histogram(binwidth = 5, fill = "deepskyblue")
summary(data$tdp)

# Plotting ncore vs tdp (box-plotting and scatter-plotting)
plot_data <- data
plot_data$ncore <- as.factor(plot_data$ncore)
ggplot(plot_data, aes(x = ncore, y = tdp)) + geom_boxplot(fill = "deepskyblue")
ggplot(plot_data, aes(x = ncore, y = tdp)) +
  geom_point(color = "deepskyblue")

# Base frequency is a bit random, but the trend of linearity is still evident
ggplot(plot_data, aes(x = bfreq, y = tdp)) + geom_point(color = "deepskyblue")

# Lithography as tdp is less convincing; 
# however, we see that recent lithography techniques 
# tend to have stable base frequency
plot_data$litho <- as.factor(plot_data$litho)
ggplot(plot_data, aes(x = litho, y = tdp)) + geom_boxplot(fill = "deepskyblue")

# Different trends: temp vs tdp
ggplot(plot_data, aes(x = temp, y = tdp)) +
  geom_point(color = "deepskyblue", ) +
  geom_abline(mapping = aes(intercept= -50, slope = 1.2), color = "darkblue")

#  tdp ~ temp and tdp ~ market : There are two different trends happening:
# above the reference line is increasing trend, while below is decreasing trend. 
# In fact, we have a feeling that these two trends come from different 
# market segments type = (Desktop + Server) --> Computers
#                        (Mobile + Embedded)--> Devices
# so we would plot them out to verify:
View(data)
unique(data$market)

# Counting occurrences of each market type
market_counts <- table(data$market)

# Displaying the counts
print(market_counts)

data1 <- data
data1$type <- ifelse(data1$market == 'Server' | data1$market == 'Desktop', "Computers", "Devices")
# data1$type <- ifelse(data1$market == 'Server', "Computers", "Devices")
data1$type <- as.factor(data1$type)

ggplot(data1, aes(x = type, y = tdp)) + geom_boxplot(fill = "deepskyblue")

ggplot(data1, aes(x = temp, y = tdp)) + geom_point(color = "deepskyblue", ) + facet_wrap(~data1$type)

# ---------------------------------------------------------------------------