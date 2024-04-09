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

#----------------------------------------------------------------------------
#Function
mode_calc <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# ---------------------------------------------------------------------------
# lets do some summary of the data first
df <- data[,3:ncol(data)]
# Generate summary statistics for the data frame
summary_df <- summary(df)
# Convert summary to character matrix
summary_df <- as.matrix(summary_df)

# Open PDF device
pdf("Descriptive_statistics/summary_data_frame.pdf", width = 12, height = 4)

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

# ---------------------------------------------------------------------------
# Plot Litho and Launched Date
# Box-plotting
data <- data[!is.na(data$litho), ]
data$litho <- as.factor(data$litho)

pdf("Descriptive_statistics/lito_and_ldate.pdf", width = 15, height = 6)

p1 <- ggplot(data, aes(x = ldate, y = litho)) +
  geom_boxplot(fill = "deepskyblue", color = "black", outlier.shape = 8, outlier.size = 2) +
  labs(title = "Boxplot of Lithography over Time", 
       subtitle = "Each box represents the distribution of lithography for a given time period",
       x = "Launch Date", 
       y = "Lithography (nm)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=14),
        plot.subtitle = element_text(hjust = 0.5, face="italic", size=12),
        plot.caption = element_text(hjust = 1, face="italic", size=10),
        axis.title.x = element_text(face="bold", size=12),
        axis.title.y = element_text(face="bold", size=12))

# Scatter plotting
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

grid.arrange(p1,p2,ncol = 2, widths = c(1,1))
dev.off()

# By drawing out the LDate and Litho, we can see that
# Ldate increase and Litho decrease --> so we use Ltho instead of Ldata

#---------------------------------------------------------------------------------
# Now drawing the histogram of TDP
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
summary_data <- c(summary(data$tdp), Mode = mode_calc(data$tdp))

summary_df <- as.matrix(summary_data)
summary_table <- tableGrob(summary_df)

# Arrange the plot and the table on the same page
pdf("Descriptive_statistics/TDP_summary.pdf", width = 10, height = 7)
grid.arrange(p, summary_table, ncol = 2, widths = c(3, 2))
dev.off()

#----------------------------------------------------------------------------------
# Plotting ncore vs tdp (box-plotting and scatter-plotting)
# Create the first plot (boxplot)
plot1 <- ggplot(plot_data, aes(x = ncore, y = tdp)) +
  geom_boxplot(fill = "deepskyblue") +
  labs(title = "Boxplot of TDP for each Ncore",
       x = "Ncore",
       y = "TDP (Watts)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Create the second plot (grouped bar plot)
plot2 <- ggplot(plot_data, aes(x=ncore, y=tdp, fill=ncore)) +
  geom_bar(position="dodge", stat="identity") +
  labs(title = "Grouped Bar Plot of TDP for each Ncore",
       x = "Ncore",
       y = "TDP (Watts)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# Open a PDF device
pdf("Descriptive_statistics/TDP_Ncore.pdf", width = 14, height = 7)

# Arrange the plots in two columns
grid.arrange(plot1, plot2, ncol = 2)
dev.off()

# Create the stacked bar plot
# ggplot(plot_data, aes(fill=ncore, y=tdp, x=ncore)) +
#   geom_bar(position="stack", stat="identity") +
#   labs(title = "Stacked Bar Plot of TDP for each Ncore", x = "Ncore", y = "Total TDP") +
#   theme_minimal()


# Lithography as tdp is less convincing; 
# however, we see that recent lithography techniques 
# tend to have stable base frequency
plot_data$litho <- as.factor(plot_data$litho)
ggplot(plot_data, aes(x = litho, y = tdp)) + geom_boxplot(fill = "deepskyblue")

# Different trends: temp vs tdp
p <- ggplot(plot_data, aes(x = temp, y = tdp)) +
geom_point(color = "blue", size = 0.4, alpha = 0.7) +
geom_abline(intercept= -50, slope = 1.2, color = "red") +
labs(title = "TDP vs. Temperature",
     x = "Temperature (°C)",
     y = "TDP (Watts)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Create a grob (graphical object) for the plot
plot_grob <- ggplotGrob(p)

# Create a text grob for the descriptive features
text_grob <- textGrob("Descriptive features go here", gp=gpar(fontsize=10, col="black"))

# Open a PDF device with the specified width and height ratio of 4:2
pdf("Descriptive_statistics/TDP_temp.pdf", width=8, height=4)

# Arrange the plot and text grob side by side with the specified ratio
grid.arrange(plot_grob, text_grob, widths=c(4, 2))

# Close the PDF device
dev.off()

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