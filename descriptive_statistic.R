# ----------------------------------------------------------------------------------
#                              LOADING PACKAGES                                    #
# ----------------------------------------------------------------------------------
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
library(tidyverse)

#  IMPORT THE DATA
data <- import("Dataset/cpu_clean.csv")        # rio::import

# ----------------------------------------------------------------------------------
# lets do some summary of the data first
df <- data[,3:ncol(data)]
# Generate summary statistics for the data frame
summary_df <- summary(df)
# Convert summary to character matrix
summary_df <- as.matrix(summary_df)

# ----------------------------------------------------------------------------------
# Open PDF device
png("Descriptive_statistics/summary_data_frame.png", width = 11, height = 4, unit = "in", res = 300)

# Add a title
grid.text("Descriptive Statistics of Data", x = unit(0.5, "npc"), y = unit(0.9, "npc"), just = "center", gp = gpar(fontsize = 18, fontface = "bold"))

# Create a grid table from the summary matrix with text centered
grid_summary <- tableGrob(summary_df, theme = ttheme_default(
  core = list(fg_params = list(hjust = 0.5))  # Center text horizontally
))

# Draw the table onto the PDF
pushViewport(viewport(x = 0.5, y = 0.5, width = 1, height = 1))
grid.draw(grid_summary)
popViewport()

# Close PDF device
dev.off()
# ----------------------------------------------------------------------------------
#                                  PLOTTING                                        #
# ----------------------------------------------------------------------------------
# Plot Litho VS Launched Date
# ----------------------------------------------------------------------------------
# Box-plotting
# ------------
data <- data[!is.na(data$litho), ]
data$litho <- as.factor(data$litho)

p1 <- ggplot(data, aes(x = ldate, y = litho)) +
  geom_boxplot(fill = "deepskyblue", color = "black", outlier.shape = 8, outlier.size = 2) +
  labs(title = "Boxplot of Lithography over Time", 
       subtitle = "Each box represents the distribution of lithography for a given time period",
       x = "Launch Date", 
       y = "Lithography (nm)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=14),
        plot.subtitle = element_text(hjust = 0.5, face="italic", size=12),
        plot.caption = element_text(hjust = 1, face="italic", size=10),
        axis.title.x = element_text(face="bold", size=12),
        axis.title.y = element_text(face="bold", size=12))

# -----------------
# Scatter plotting
# -----------------
p2 <- ggplot(data, aes(x = ldate, y = litho)) +
  geom_area(alpha = 0.5, fill = "deepskyblue") +
  #geom_smooth(method = lm, se = F, color = "red") +
  geom_point(color = "red", alpha = 0.5, size = 1) + 
  labs(title = "Lithography over time", 
       subtitle = "An area plot showing the change in lithography over time",
       x = "Launch Date", 
       y = "Lithography (nm)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=14),
        plot.subtitle = element_text(hjust = 0.5, face="italic", size=12),
        plot.caption = element_text(hjust = 1, face="italic", size=10),
        axis.title.x = element_text(face="bold", size=12),
        axis.title.y = element_text(face="bold", size=12))

# pdf("Descriptive_statistics/lito_and_ldate.pdf", width = 15, height = 6)
# grid.arrange(p1,p2,ncol = 2, widths = c(1,1))
# dev.off()
png("Descriptive_statistics/lito_and_ldate.png", width = 15, height = 6, units = "in", res = 300)
grid.arrange(p1,p2,ncol = 2, widths = c(1,1))
dev.off()

#---------------------------------------------------------------------------------
# HISTOGRAM of TDP
#---------------------------------------------------------------------------------
p <- ggplot(data, aes(x = tdp)) +
  geom_histogram(binwidth = 5, fill = "orange") +
  xlab("TDP (W)") +
  ylab("Number of CPUs") +
  geom_segment(aes(x = -Inf, xend = Inf, y = 0, yend = 0), 
               arrow = arrow(length = unit(0.02, "npc")), color = "black") +
  geom_segment(aes(x = 0, xend = 0, y = -Inf, yend = Inf), 
               arrow = arrow(length = unit(0.02, "npc")), color = "black") +
  annotate("text", x = 0, y = 0, label = "W, cpus", hjust = 1.5) + 
  scale_y_continuous(breaks = seq(0, max(hist(data$tdp, plot=FALSE)$counts), by = 50))

# Create the summary table
mode_calc <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

summary_data <- c(summary(data$tdp), Mode = mode_calc(data$tdp))
summary_df <- as.matrix(summary_data)
summary_table <- tableGrob(summary_df)

# Arrange the plot and the table on the same page
# pdf("Descriptive_statistics/TDP_summary.pdf", width = 10, height = 7)
# grid.arrange(p, summary_table, ncol = 2, widths = c(3, 2))
# dev.off()
png("Descriptive_statistics/TDP_summary.png", width = 10, height = 7, unit = "in", res = 300)
grid.arrange(p, summary_table, ncol = 2, widths = c(3, 2))
dev.off()

#----------------------------------------------------------------------------------
# Plotting ncore VS tdp (box-plotting and scatter-plotting)
#----------------------------------------------------------------------------------
# Create the first plot (boxplot)
# -------------------------------
plot1 <- ggplot(plot_data, aes(x = ncore, y = tdp)) +
  geom_boxplot(fill = "deepskyblue", outlier.shape = 8, outlier.size =2) +
  labs(title = "Boxplot of TDP for each Ncore",
       x = "Ncore",
       y = "TDP (Watts)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# -----------------------------------------
# Create the second plot (grouped bar plot)
# -----------------------------------------
# Calculate the first and third quartiles (Q1 and Q3) for each ncore
filtered_data <- plot_data
filtered_data <- filtered_data %>%
  group_by(ncore) %>%
  mutate(Q1 = quantile(tdp, 0.25),
         Q2 = quantile(tdp, 0.5),
         Q3 = quantile(tdp, 0.75))

# Filter the tdp for each ncore to its Q3 value
filtered_data <- filtered_data %>%
  group_by(ncore) %>%
  filter(tdp <= Q1)

# Your plotting code with the new filtered data set
plot2 <- ggplot(filtered_data, aes(x=ncore, y=tdp, fill=ncore)) +
  geom_bar(position="dodge", stat="identity") +
  labs(title = "Grouped Bar Plot of TDP for each Ncore",
       x = "Ncore",
       y = "TDP (Watts)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

print(plot2)

# pdf("Descriptive_statistics/TDP_Ncore.pdf", width = 14, height = 7)
# grid.arrange(plot1, plot2, ncol = 2)
# dev.off()
png("Descriptive_statistics/TDP_Ncore.png", width = 14, height = 7, res = 300, unit = "in")
grid.arrange(plot1, plot2, ncol = 2)
dev.off()
#----------------------------------------------------------------------------------
# Litho VS tdp
#----------------------------------------------------------------------------------
# Lithography as tdp is less convincing; 
# however, we see that recent lithography techniques 
# tend to have stable tdp
#----------------------------------------------------------------------------------
# ---------------------------------
# Box plot for lithography and tdp
# ---------------------------------
# Convert litho to a factor
plot_data$litho <- as.factor(plot_data$litho)
# Create the base plot
p <- ggplot(plot_data, aes(x = litho, y = tdp)) + 
  geom_boxplot(aes(fill = litho), alpha = 0.7, outlier.shape = 8, outlier.size = 3)
# Add title and labels
p <- p + labs(title = "Boxplot of TDP by Litho",
              x = "Litho",
              y = "TDP")
p <- p + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        legend.position = "none")

# Save the plot
# pdf("Descriptive_statistics/TDP_litho.pdf", width = 14, height = 7)
# print(p)
# dev.off()
png("Descriptive_statistics/TDP_litho.png", width = 14, height = 7, unit = "in", res = 300)
print(p)
dev.off()
#-----------------------------------------------------------------------
# Plotting TDP and Processor Base Frequency
#-----------------------------------------------------------------------
p <- ggplot(plot_data, aes(x = bfreq, y = tdp)) +
  geom_point(color = "firebrick", size = 1, alpha = 0.7) +
  geom_smooth(method = "lm") +
  labs(title = "TDP vs. Bfreq",
         x = "Bfreq (GHz)",
         y = "TDP (Watts") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
png("Descriptive_statistics/tdp_breq.png", width = 7, height = 6, unit = "in", res = 300)
print(p)
dev.off()

# TEMP VS TDP
#----------------------------------------------------------------------------
p <- ggplot(plot_data, aes(x = temp, y = tdp)) +
geom_point(color = "blue", size = 0.4, alpha = 0.7) +
geom_abline(intercept= -50, slope = 1.2, color = "red") +
  labs(title = "TDP vs. Temperature",
     x = "Temperature (°C)",
     y = "TDP (Watts)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Create a grob (graphical object) for the plot
plot_grob <- ggplotGrob(p)

png("Descriptive_statistics/TDP_temp.png", width=8, height=4, unit = "in", res = 300)
grid.arrange(plot_grob, ncol = 1)
dev.off()


# TDP v.s. TEMP AND TDP v.s. MARKET
#----------------------------------------------------------------------------
View(data)
market_plot <- ggplot(data, aes(x = tdp, fill = market)) +
  #geom_bar(position="dodge", stat="identity") +
  geom_density(alpha = 0.6) + 
  labs(title = "Bar plot for each market",
       x = "Market",
       y = "TDP (Watts)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())
png("Descriptive_statistics/tdp_market.png", width = 5, height = 7, unit = "in", res = 300)
print(market_plot)
dev.off()

View(unique(data$market))
# Counting occurrences of each market type
market_counts <- table(data$market)
# Displaying the counts
print(market_counts)
data1 <- data
data1$type <- ifelse(data1$market == 'Server' | data1$market == 'Desktop', "Computers", "Devices")
# This line creates a new column type in the data1 dataframe. The ifelse() function is used to assign values to this new column based on the values in the market column. If the market value is ‘Server’ or ‘Desktop’, ‘Computers’ is assigned to type. Otherwise, ‘Devices’ is assigned.
View(data1)
data1$type <- as.factor(data1$type)


# Create the DENSITY PLOT of TDP for EACH TYPE
#----------------------------------------------------------------------------------
p <- ggplot(data1, aes(x = tdp, fill = type)) + 
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("Computers" = "deepskyblue", "Devices" = "firebrick3")) +
  labs(title = "Density Plot of TDP for Each Type",
       x = "TDP",
       y = "Density",
       fill = "Type") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))


# PLOT SEPERATE TYPES OF CPU
#-------------------------------------------------------------------------
p1 <- ggplot(data1, aes(x = temp, y = tdp)) + 
  geom_point(aes(color = type), alpha = 0.6, size = 1) +
  scale_color_manual(values = c("Computers" = "deepskyblue", "Devices" = "firebrick3")) +
  facet_wrap(~type, scales = "free") 
# Add title and labels
p1 <- p1 + labs(title = "Scatterplot of TDP by Temp for Each Type",
              x = "Temp",
              y = "TDP",
              color = "Type")

p1 <- p1 + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

png("Descriptive_statistics/type.png", width = 15, height = 5, unit = "in", res = 300)
grid.arrange(p, p1, ncol = 2)
dev.off()



