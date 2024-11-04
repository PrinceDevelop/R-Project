# Load necessary libraries

install.packages("reshape2")
library(ggplot2)
library(dplyr)
library(readr)# To read CSV files
library(reshape2)

# Load the Titanic dataset
titanic <- read.csv("C:/Users/Prince/Downloads/train.csv")

# 1. View the first few rows of the dataset
head(titanic)

# 2. Summary Statistics of the numerical columns
print("Summary Statistics:")
summary(titanic)

# 3. Check for missing values
print("Missing Values:")
print(colSums(is.na(titanic)))

# 4. Histogram: Distribution of Age
ggplot(titanic, aes(x = Age)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  ggtitle("Distribution of Age") +
  xlab("Age") +
  ylab("Frequency") +
  xlim(c(1, 25)) + 
  ylim(c(0, 9))
  

# . Boxplot: Fare by Passenger Class (Pclass)
ggplot(titanic, aes(x = factor(Pclass), y = Fare)) +
  geom_boxplot(fill = c("red","yellow","orange")) +
  ggtitle("Boxplot of Fare by Passenger Class") +
  xlab("Passenger Class") +
  ylab("Fare")+
  theme_minimal()

# 7. Scatter Plot: Age vs Fare, colored by Survival status
ggplot(titanic, aes(x = Age, y = Fare, color = factor(Survived))) +
  geom_point(size = 3) +
  ggtitle("Scatter Plot: Age vs Fare by Survival Status") +
  xlab("Age") +
  ylab("Fare") +
  scale_color_manual(values = c("yellow", "blue"), 
                     labels = c("Not Survived", "Survived"))

# 8. Correlation Matrix: Only numerical columns
numeric_data <- titanic %>% 
  select(Age, Fare, SibSp, Parch)  # Select numerical columns
correlation_matrix <- cor(numeric_data, use = "complete.obs")
print("Correlation Matrix:")
print(correlation_matrix)

# 9. Heatmap: Correlation Matrix

melted_corr <- melt(correlation_matrix)

ggplot(data = melted_corr, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  theme_minimal() +
  ggtitle("Correlation Matrix Heatmap") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
