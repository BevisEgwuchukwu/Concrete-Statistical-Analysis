install.packages('caret')
install.packages('corrplot')
install.packages('car')
install.packages('RVAideMemoire')
install.packages('ggplot2')
install.packages('qqplotr')
install.packages('datarium')
install.packages('tidyverse')
install.packages('reshape2')
install.packages('randomForest')
install.packages('mgcv')

library(caret)
library(corrplot)
library(car)
library(RVAideMemoire)
library(ggplot2)
library(qqplotr)
library(datarium)
library(tidyverse)
library(dplyr)
library(reshape2)
library(randomForest)
library(mgcv)


df <- read_csv('concrete compressive strength.csv',
                  col_types = cols("Cement (component 1)(kg in a m^3 mixture)" = col_double(),
                                   "Blast Furnace Slag (component 2)(kg in a m^3 mixture)" = col_double(),
                                   "Fly Ash (component 3)(kg in a m^3 mixture)" = col_double(),
                                   "Water  (component 4)(kg in a m^3 mixture)" = col_double(),
                                   "Superplasticizer (component 5)(kg in a m^3 mixture)" = col_double(),
                                   "Coarse Aggregate  (component 6)(kg in a m^3 mixture)" = col_double(),
                                   "Fine Aggregate (component 7)(kg in a m^3 mixture)" = col_double(),
                                   "Age (day)" = col_integer(),
                                   "Concrete Category" = col_character(),
                                   "Contains Fly Ash" = col_character(),
                                   "Concrete compressive strength(MPa, megapascals)" = col_double())
)

#check data type
str(df)

#check for missing data 
sum(is.na(df))

#Check for missing values in each column
missing_columns <- colSums(is.na(df))
print(missing_columns)

summary(df)
head(df)
dim(df)

# Check for duplicates in the entire dataset
duplicates <- duplicated(df)
# Count the number of duplicate rows
num_duplicates <- sum(duplicates)
# Print the number of duplicate rows
print(paste("Number of duplicate rows:", num_duplicates))
# Display the duplicate rows
print(df[duplicates, ])
# Keep only unique rows
unique_data <- distinct(df)
# Calculate the number of duplicates removed
num_duplicates <- nrow(df) - nrow(unique_data)

summary(unique_data)

# Create the histogram for compressive strength
ggplot(unique_data, aes(x = `Concrete compressive strength(MPa, megapascals)`)) + 
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") + 
  labs(title = "Histogram of Concrete Compressive Strength", 
       x = "Concrete Compressive Strength (MPa)", 
       y = "Frequency") +
  theme_minimal()

# Boxplot of compressive strength by category
ggplot(unique_data, aes(x = `Concrete Category`, y = `Concrete compressive strength(MPa, megapascals)`)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Compressive Strength by Concrete Category",
       x = "Concrete Category", y = "Compressive Strength (MPa)")

# Select only numerical columns
df_numeric <- unique_data[, sapply(unique_data, is.numeric)]

# Calculate the correlation matrix
cor_matrix <- cor(df_numeric, use = "complete.obs")

# Melt the correlation matrix for plotting
melted_cor_matrix <- melt(cor_matrix)

# Create the heatmap
ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                       limit = c(-1,  
                                 1), space = "Lab", name = "Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,  
                                   hjust = 1)) +
  labs(title = "Correlation Heatmap of Numerical Features") +
  geom_text(aes(label = round(value, 2)))

# Get the names of all numeric columns except the target variable
numeric_cols <- names(df_numeric)[sapply(df_numeric, is.numeric) & 
                                     names(df_numeric) != "Concrete compressive strength(MPa, megapascals) "]

# Create a function to generate scatter plots
generate_scatterplots <- function(df, x_cols, y_col) {
  for (col in x_cols) {
    plot <- ggplot(df, aes(x = .data[[col]], y = .data[[y_col]])) +
      geom_point() +
      labs(title = paste(y_col, "vs.", col),
           x = col,
           y = y_col) +
      theme_minimal()
    
    print(plot) 
  }
}

# Call the function to generate the scatterplots
generate_scatterplots(df_numeric, numeric_cols, "Concrete compressive strength(MPa, megapascals)")                                 

# Extract correlations with Concrete compressive strength(MPa, megapascals) 
compressive_strength_cor <- cor_matrix[, "Concrete compressive strength(MPa, megapascals)"]

# Sort in descending order
sorted_cor <- sort(compressive_strength_cor, decreasing = TRUE)

# Print the sorted correlations
print(sorted_cor)

# Remove rows with missing values
concrete_df_numeric <- na.omit(df_numeric)

# Calculate the mean of each column
mean_values <- colMeans(concrete_df_numeric)

# Print the mean values
print(mean_values)

# Define the strength intervals
breaks <- c(0, 20, 40, 60, 80, Inf)  
labels <- c("0-20", "20-40", "40-60", "60-80", "80+")

# Create a new column with strength intervals
df_numeric$strength_group <- cut(df_numeric$`Concrete compressive strength(MPa, megapascals)`, 
                                  breaks = breaks, labels = labels, include.lowest = TRUE)

# Select the component columns for aggregation
component_cols <- c(
  "Cement (component 1)(kg in a m^3 mixture)",
  "Fly Ash (component 3)(kg in a m^3 mixture)",
  "Water  (component 4)(kg in a m^3 mixture)",
  "Coarse Aggregate  (component 6)(kg in a m^3 mixture)",
  "Fine Aggregate (component 7)(kg in a m^3 mixture)"
)

# Calculate the mean values for each component within each strength interval
aggregate_values <- aggregate(df_numeric[, component_cols], 
                              by = list(df_numeric$strength_group), 
                              FUN = mean)

# Reshape the data for plotting
library(reshape2)
melted_data <- melt(aggregate_values, id.vars = "Group.1")

# Create the plot
ggplot(melted_data, aes(x = Group.1, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Values of Components on Different Concrete Strength Intervals",
       x = "Concrete Strength Intervals (MPa)",
       y = "Mean Value",
       fill = "Components") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

#Getting average compressive strenghth for both coarse and fine
# Calculate the average compressive strength for each concrete category
category_mean <- aggregate(unique_data$`Concrete compressive strength(MPa, megapascals)`,
                              by = list(unique_data$`Concrete Category`),
                              FUN = mean)

# Display the result
print(category_mean)

# Subset the data for "Coarse" concrete
coarse_data <- subset(unique_data, `Concrete Category` == "Coarse")

# Calculate the mean of all numeric columns for "Coarse" concrete
coarse_means <- colMeans(coarse_data[, sapply(coarse_data, is.numeric)], na.rm = TRUE)

# Subset the data for "Fine" concrete
fine_data <- subset(unique_data, `Concrete Category` == "Fine")

# Calculate the mean of all numeric columns for "Fine" concrete
fine_means <- colMeans(fine_data[, sapply(fine_data, is.numeric)], na.rm = TRUE)

# Print the results
print("Mean values for Coarse Concrete:")
print(coarse_means)

print("Mean values for Fine Concrete:")
print(fine_means)

# Calculate the mean values for each component for "Coarse" and "Fine" concrete
mean_values <- unique_data %>%
  group_by(`Concrete Category`) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = -`Concrete Category`, names_to = "Component", values_to = "Mean_Value")

# Create the bar chart
ggplot(mean_values, aes(x = Component, y = Mean_Value, fill = `Concrete Category`)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Values of Coarse and Fine Aggregates",  
       x = "Variables",                # Label for x-axis
       y = "Mean Values") +            # Label for y-axis
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  

# Calculate average compressive strength for each group (with and without fly ash)
avg_strength <- unique_data %>%
  group_by(`Contains Fly Ash`) %>%
  summarize(avg_compressive_strength = mean(`Concrete compressive strength(MPa, megapascals)`))

# Create the bar chart
ggplot(avg_strength, aes(x = `Contains Fly Ash`, y = avg_compressive_strength, fill = `Contains Fly Ash`)) +
  geom_col() +  # Use geom_col() for bar chart with already aggregated data
  labs(title = "Average Compressive Strength: With and Without Ash",
       x = "",  # Remove x-axis label
       y = "Average Compressive Strength") +
  scale_fill_manual(values = c("forestgreen", "orangered")) +  # Set custom colors
  theme_minimal()

# Define the strength intervals
breakss <- c(0, 20, 40, 60, 80, Inf)  # Adjust these breaks as needed
labelss <- c("0-20", "20-40", "40-60", "60-80", "80+")

# Create a new column with strength intervals
unique_data$strength_group <- cut(unique_data$`Concrete compressive strength(MPa, megapascals)`, 
                                  breaks = breakss, labels = labelss, include.lowest = TRUE)

# Calculate average compressive strength for each group and interval
avg_strength <- unique_data %>%
  group_by(`Contains Fly Ash`, strength_group) %>%
  summarize(avg_compressive_strength = mean(`Concrete compressive strength(MPa, megapascals)`), .groups = 'drop')

# Create the grouped bar chart
ggplot(avg_strength, aes(x = strength_group, y = avg_compressive_strength, fill = `Contains Fly Ash`)) +
  geom_col(position = "dodge") +
  labs(title = "Average Compressive Strength: With and Without Ash Across Strength Groups",
       x = "Concrete Strength Intervals (MPa)",
       y = "Average Compressive Strength (MPa)",
       fill = "Sample Type") +  # Change legend title
  scale_fill_manual(values = c("forestgreen", "orangered"), labels = c("With Ash", "Without Ash")) +  # Set custom colors and labels
  theme_bw()  # Use a black and white theme

# Normality Checks
# Visual Inspection: Histogram with density plot
ggplot(unique_data, aes(x = `Concrete compressive strength(MPa, megapascals)`)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Distribution of Concrete Compressive Strength",
       x = "Compressive Strength (MPa)", y = "Frequency")

# Visual Inspection: Q-Q Plot
qqnorm(unique_data$`Concrete compressive strength(MPa, megapascals)`)
qqline(unique_data$`Concrete compressive strength(MPa, megapascals)`, col = "red")

# Statistical Test: Shapiro-Wilk Test
shapiro_test <- shapiro.test(unique_data$`Concrete compressive strength(MPa, megapascals)`)
print(shapiro_test)

# Statistical Test: Kolmogorov-Smirnov Test
ks_test <- ks.test(unique_data$`Concrete compressive strength(MPa, megapascals)`, "pnorm",
                   mean = mean(unique_data$`Concrete compressive strength(MPa, megapascals)`, na.rm = TRUE),
                   sd = sd(unique_data$`Concrete compressive strength(MPa, megapascals)`, na.rm = TRUE))
print(ks_test)

#Homogeneity of Variances
# Bartlett's Test
bartlett_category <- bartlett.test(`Concrete compressive strength(MPa, megapascals)` ~ `Concrete Category`, data = unique_data)
bartlett_flyash <- bartlett.test(`Concrete compressive strength(MPa, megapascals)` ~ `Contains Fly Ash`, data = unique_data)
bartlett_age <- bartlett.test(`Concrete compressive strength(MPa, megapascals)` ~ `Age (day)`, data = unique_data)
print(bartlett_category)
print(bartlett_flyash)
print(bartlett_age)

# Hypothesis Testing
# ANOVA for Concrete Category
anova_category <- aov(`Concrete compressive strength(MPa, megapascals)` ~ `Concrete Category`, data = unique_data)
summary(anova_category)

# Welch's ANOVA for Fly Ash
welch_flyash <- oneway.test(
  `Concrete compressive strength(MPa, megapascals)` ~ `Contains Fly Ash`, 
  data = unique_data, 
  var.equal = FALSE
)
print(welch_flyash)

# Welch's ANOVA for Age
welch_age <- oneway.test(
  `Concrete compressive strength(MPa, megapascals)` ~ `Age (day)`, 
  data = unique_data, 
  var.equal = FALSE
)
print(welch_age)

#Extract residuals from ANOVA models
residuals_category <- residuals(anova_category)
residuals_flyash <- residuals(welch_flyash)
residuals_age <- residuals(welch_age)

#Normality Check for Residuals for ANOVA
#Histogram and Q-Q Plot for residuals
hist(residuals_category, main = "Residuals for Concrete Category ANOVA", xlab = "Residuals")
qqnorm(residuals_category)
qqline(residuals_category, col = "red")

#Contains fly ash residuals ----------------------------------------------------
# Step 1: Compute Group Means (Fitted Values)
group_means_flyash <- tapply(unique_data$`Concrete compressive strength(MPa, megapascals)`, 
                      unique_data$`Contains Fly Ash`, 
                      mean, 
                      na.rm = TRUE)

# Assign fitted values
unique_data$fitted_values <- group_means_flyash[as.character(unique_data$`Contains Fly Ash`)]

# Step 2: Calculate Residuals
unique_data$residuals <- unique_data$`Concrete compressive strength(MPa, megapascals)` - unique_data$fitted_values

# Step 3: Visual Residual Analysis
# Histogram
hist(unique_data$residuals, main = "Residuals for Welch's ANOVA", xlab = "Residuals", col = "blue")

# Q-Q Plot
qqnorm(unique_data$residuals, main = "Q-Q Plot of Residuals")
qqline(unique_data$residuals, col = "red")

# Residuals vs Fitted Values
plot(unique_data$fitted_values, unique_data$residuals, 
     main = "Residuals vs Fitted Values", 
     xlab = "Fitted Values", 
     ylab = "Residuals", 
     pch = 20)
abline(h = 0, col = "red", lwd = 2)


#Age residuals------------------------------------------------------------------
# Step 1: Compute Group Means (Fitted Values)
group_means <- tapply(unique_data$`Concrete compressive strength(MPa, megapascals)`, 
                      unique_data$`Age (day)`, 
                      mean, 
                      na.rm = TRUE)

# Assign fitted values
unique_data$fitted_values <- group_means[as.character(unique_data$`Age (day)`)]

# Step 2: Calculate Residuals
unique_data$residuals <- unique_data$`Concrete compressive strength(MPa, megapascals)` - unique_data$fitted_values

# Step 3: Visual Residual Analysis
# Histogram
hist(unique_data$residuals, main = "Residuals for Welch's ANOVA", xlab = "Residuals", col = "blue")

# Q-Q Plot
qqnorm(unique_data$residuals, main = "Q-Q Plot of Residuals")
qqline(unique_data$residuals, col = "red")

# Residuals vs Fitted Values
plot(unique_data$fitted_values, unique_data$residuals, 
     main = "Residuals vs Fitted Values", 
     xlab = "Fitted Values", 
     ylab = "Residuals", 
     pch = 20)
abline(h = 0, col = "red", lwd = 2)

#-------------------------------------------------------------------------------------------------------------------------------------
# Linear Regression
linear_model <- lm(`Concrete compressive strength(MPa, megapascals)` ~ `Cement (component 1)(kg in a m^3 mixture)` + `Blast Furnace Slag (component 2)(kg in a m^3 mixture)`+
                     `Fly Ash (component 3)(kg in a m^3 mixture)`+`Water  (component 4)(kg in a m^3 mixture)`+`Superplasticizer (component 5)(kg in a m^3 mixture)`+`Age (day)`+
                     `Coarse Aggregate  (component 6)(kg in a m^3 mixture)`+`Fine Aggregate (component 7)(kg in a m^3 mixture)`+
                     `Concrete Category`+`Contains Fly Ash`, data = unique_data)
summary(linear_model)
#multicollinearity
vif(linear_model)

# Linear Regression 1
linear_model1 <- lm(`Concrete compressive strength(MPa, megapascals)` ~ `Cement (component 1)(kg in a m^3 mixture)` + 
                      `Blast Furnace Slag (component 2)(kg in a m^3 mixture)`+`Water  (component 4)(kg in a m^3 mixture)`+
                      `Superplasticizer (component 5)(kg in a m^3 mixture)`+`Age (day)`+
                     `Concrete Category`+`Contains Fly Ash`, data = unique_data)

summary(linear_model1)

#multicollinearity
vif(linear_model1)

#checking linearity
pairs(unique_data[,c(1,2,4,5,6)], lower.panel = NULL, pch = 1,cex = 0.6)

#residual independence, normalty and equal variance
plot(linear_model1)

#-------------------------------------------------------------------------------------------------------------------------------------------
#Random Forest Regression
rf_model <- randomForest(`Concrete compressive strength(MPa, megapascals)` ~ unique_data$`Cement (component 1)(kg in a m^3 mixture)`+ 
                           unique_data$`Blast Furnace Slag (component 2)(kg in a m^3 mixture)`+
                           unique_data$`Fly Ash (component 3)(kg in a m^3 mixture)`+
                           unique_data$`Water  (component 4)(kg in a m^3 mixture)`+
                           unique_data$`Superplasticizer (component 5)(kg in a m^3 mixture)`+
                           unique_data$`Age (day)`+
                           unique_data$`Coarse Aggregate  (component 6)(kg in a m^3 mixture)`+
                           unique_data$`Fine Aggregate (component 7)(kg in a m^3 mixture)`+
                           unique_data$`Concrete Category`+unique_data$`Contains Fly Ash`, data = unique_data)
summary(rf_model)

# Feature importance
importance_values <- importance(rf_model)
print(importance_values)

#plot OOB error
plot(rf_model)

# Plot feature importance
varImpPlot(rf_model, main = "Feature Importance")

# Residuals
actual <- rf_model$y
predicted <- rf_model$predicted
residuals <- actual - predicted

# Residual Plot
plot(predicted, residuals, main = "Residual Plot", 
     xlab = "Predicted Values", ylab = "Residuals", pch = 16, col = "blue")
abline(h = 0, col = "red", lty = 2)

# Histogram of Residuals
hist(residuals, breaks = 30, main = "Residual Histogram", 
     xlab = "Residuals", col = "lightblue", border = "black")


#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Generalized Additive Model 
# Clean column names to make them syntactically valid
colnames(unique_data) <- make.names(colnames(unique_data))

# Check the updated column names
print(colnames(unique_data))

# Fit the Generalized Additive Model with cleaned column names
mgcv_model <- gam(
  Concrete.compressive.strength.MPa..megapascals. ~ 
    s(Cement..component.1..kg.in.a.m.3.mixture.)+ 
    s(Blast.Furnace.Slag..component.2..kg.in.a.m.3.mixture.) +
    s(Fly.Ash..component.3..kg.in.a.m.3.mixture.) +
    s(Water...component.4..kg.in.a.m.3.mixture.) +
    s(Superplasticizer..component.5..kg.in.a.m.3.mixture.) +
    s(Age..day.) +
    s(Coarse.Aggregate...component.6..kg.in.a.m.3.mixture.) +
    s(Fine.Aggregate..component.7..kg.in.a.m.3.mixture.), 
  data = unique_data
)

summary(mgcv_model)

#Residual independence, normality
plot(mgcv_model, pages = 1, rug = TRUE, se = TRUE)

#Check Residuals
gam.check(mgcv_model)
