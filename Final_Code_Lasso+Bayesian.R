# Load necessary libraries
library(tidyverse)
library(janitor)
library(glmnet)
library(ggplot2)


# Define a function to clean and standardize a dataset
clean_and_standardize <- function(file_path) {
  # Load the dataset
  data <- read_csv(file_path)
  
  # Clean column names for consistency
  data <- data %>%
    clean_names()
  
  # Remove betting-related columns (e.g., odds columns)
  betting_columns <- colnames(data)[grepl("b365|wh|vc|ps|lb|iw|bw|div|odds", colnames(data), ignore.case = TRUE)]
  data <- data %>%
    select(-all_of(betting_columns))
  
  # Retain only useful columns (customize as needed)
  key_columns <- c("date", "home_team", "away_team", "fthg", "ftag", "ftr", "hs", "as", "hst", "ast", "hf", "af", "hc", "ac", "hy", "ay", "hr", "ar")
  data <- data %>%
    select(any_of(key_columns))
  
  # Convert date column to Date format
  data <- data %>%
    mutate(date = as.Date(date, format = "%d/%m/%Y"))
  
  return(data)
}

# File paths for the datasets
file_paths <- list(
  "2014-15.csv" = "C:/Users/Lenovo/Downloads/OM 286 Fall 2 Supply Chain Analytics/Project/2014-15.csv",
  "2015-16.csv" = "C:/Users/Lenovo/Downloads/OM 286 Fall 2 Supply Chain Analytics/Project/2015-16.csv",
  "2016-17.csv" = "C:/Users/Lenovo/Downloads/OM 286 Fall 2 Supply Chain Analytics/Project/2016-17.csv",
  "2017-18.csv" = "C:/Users/Lenovo/Downloads/OM 286 Fall 2 Supply Chain Analytics/Project/2017-18.csv"
)

# Apply the cleaning function to all files
datasets <- lapply(file_paths, clean_and_standardize)

# Combine all datasets into one
combined_data <- bind_rows(datasets)

# Save the standardized dataset for verification
write_csv(combined_data, "C:/Users/Lenovo/Downloads/OM 286 Fall 2 Supply Chain Analytics/Project/cleaned_combined_data.csv")







# Train and Test Split
# Split data into train and test sets based on date
train_data <- combined_data %>%
  filter(date < as.Date("31-07-2017"))

test_data <- combined_data %>%
  filter(date > as.Date("01-08-2017"))

# Save train and test datasets
write_csv(train_data, "C:/Users/Lenovo/Downloads/OM 286 Fall 2 Supply Chain Analytics/Project/train_data.csv")
write_csv(test_data, "C:/Users/Lenovo/Downloads/OM 286 Fall 2 Supply Chain Analytics/Project/test_data.csv")

# View a summary of the splits
cat("Train Data:", nrow(train_data), "rows\n")
cat("Test Data:", nrow(test_data), "rows\n")









#Lasso Regression

# Load the training dataset
train_data <- read_csv("C:/Users/Lenovo/Downloads/OM 286 Fall 2 Supply Chain Analytics/Project/train_data.csv")

# Clean column names for consistency
train_data <- train_data %>%
  clean_names()

# Define the predictors and response variable
# Exclude home_team and away_team from predictors
predictors <- train_data %>%
  select(-c(date, home_team, away_team, ftr,fthg,ftag))  # Exclude categorical/non-numeric columns like result and teams

response <- ifelse(train_data$ftr == "H", 1, 0)  # Binary response: 1 if Home Win, 0 otherwise

# Convert predictors to matrix for glmnet
x <- as.matrix(predictors)
y <- as.factor(response)

# Run Lasso regression using glmnet
lasso_model <- cv.glmnet(
  x, y,
  family = "binomial",         # Logistic regression for binary outcome
  alpha = 1,                  # Alpha = 1 corresponds to Lasso
  type.measure = "class"      # Measure classification accuracy
)

# Plot cross-validated performance
plot(lasso_model)

# Extract the best lambda (penalty) value
best_lambda <- lasso_model$lambda.min
cat("Best Lambda Value:", best_lambda, "\n")

# Get the coefficients of the Lasso model for the best lambda
lasso_coefficients <- coef(lasso_model, s = "lambda.min")
print("Lasso Coefficients:")
print(lasso_coefficients)

# Identify important predictors (non-zero coefficients)
important_predictors <- rownames(lasso_coefficients)[which(lasso_coefficients != 0)]
print("Important Predictors Selected by Lasso:")
print(important_predictors)






#Dummy Encoding (Done on Python)





#Bayesian Logistic Regression (To get coefficients)
# Load required libraries
library(brms)
library(tidyverse)

# Load the dummy-encoded dataset
data <- read_csv("C:/Users/Lenovo/Downloads/OM 286 Fall 2 Supply Chain Analytics/Project/data_with_dummies.csv")

# View the first few rows to confirm the structure
head(data)

library(janitor)

# Clean column names
data <- data %>%
  clean_names()


# Add binary target column for HomeWin
data <- data %>%
  mutate(HomeWin = as.integer(fthg > ftag))  # 1 if home team goals > away team goals, else 0

# Remove unnecessary columns
data <- data %>%
  select(-date, -fthg, -ftag)


# Fit the Bayesian Logistic Regression model
model <- brm(
  formula = HomeWin ~ .,  # Use all dummy-encoded variables as predictors
  data = data,
  family = bernoulli(link = "logit"),  # Logistic regression
  prior = c(
    prior(normal(0, 10), class = "b"),  # Priors for coefficients
    prior(normal(0, 10), class = "Intercept")  # Prior for intercept
  ),
  chains = 4, cores = 4, iter = 2000,  # Sampling settings
  seed = 42  # Set a seed for reproducibility
)

# View a summary of the model
summary(model)

# Check diagnostic plots for convergence
plot(model)


# Extract coefficients dynamically
coefficients <- as.list(fixef(model)[, "Estimate"])



coefficients






#Predicting Results

# Load necessary libraries
library(tidyverse)

# Step 1: Load test data
test_data <- read_csv("C:/Users/Lenovo/Downloads/OM 286 Fall 2 Supply Chain Analytics/Project/data_with_dummies_test.csv")

# Step 2: Load coefficients
coefficients <- read_csv("C:/Users/Lenovo/Downloads/OM 286 Fall 2 Supply Chain Analytics/Project/Coefficients.csv")

# Convert coefficients to a named list
coefficients_list <- setNames(coefficients$Estimate, coefficients$Variable)

# Step 3: Identify active dummy columns (TRUE values in team dummy columns)
test_data <- test_data %>%
  rowwise() %>%
  mutate(
    ActiveHomeTeam = names(.)[which(across(starts_with("HomeTeam")) == TRUE)],  # Identify active home team
    ActiveAwayTeam = names(.)[which(across(starts_with("AwayTeam")) == TRUE)]   # Identify active away team
  ) %>%
  ungroup()

# Step 4: Calculate Log Odds based on active dummy columns
test_data <- test_data %>%
  mutate(
    LogOdds = coefficients_list[["Intercept"]] +  # Start with the intercept
      ifelse(!is.na(ActiveHomeTeam), coefficients_list[ActiveHomeTeam], 0) +  # Add coefficient for active home team
      ifelse(!is.na(ActiveAwayTeam), coefficients_list[ActiveAwayTeam], 0),  # Add coefficient for active away team
    ProbabilityHomeWin = 1 / (1 + exp(-LogOdds))  # Convert log odds to probability
  )

# Step 5: Save the results
write_csv(test_data, "C:/Users/Lenovo/Downloads/OM 286 Fall 2 Supply Chain Analytics/Project/2017_18_predictions_with_active_teams.csv")
print("Predictions saved to '2017_18_predictions_with_active_teams.csv'")

# View a few rows to verify
print(head(test_data, 10))










# Load necessary libraries
library(tidyverse)
library(ggplot2)

# Step 1: Load the coefficients data
coefficients <- read_csv("C:/Users/Lenovo/Downloads/OM 286 Fall 2 Supply Chain Analytics/Project/Coefficients.csv")

# Step 2: Rename columns for clarity (if needed)
coefficients <- coefficients %>%
  rename(
    Variable = Variable,
    Estimate = Estimate,
    LowerCI = `l-95% CI`,
    UpperCI = `u-95% CI`
  )

# Step 3: Separate the intercept, home team, and away team coefficients
coefficients <- coefficients %>%
  mutate(
    Category = case_when(
      Variable == "Intercept" ~ "Intercept",
      str_detect(Variable, "home_team") ~ "Home Team",
      str_detect(Variable, "away_team") ~ "Away Team"
    )
  )

# Step 4: Create a forest plot
ggplot(coefficients, aes(x = Estimate, y = reorder(Variable, Estimate), color = Category)) +
  geom_point(size = 3) + # Points for the mean estimate
  geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI), height = 0.2) + # Error bars for credible intervals
  facet_wrap(~ Category, scales = "free_y") + # Separate plots for each category
  labs(
    title = "Bayesian Logistic Regression Coefficients",
    x = "Estimate (Log-Odds)",
    y = "Variable",
    color = "Category"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14)
  )

# Save the plot (optional)
ggsave("coefficients_forest_plot.png", width = 10, height = 8, dpi = 300)









#Visual Representation of output

# Load necessary libraries
library(dplyr)
library(pROC)
library(ggplot2)
library(caret)

# Step 1: Load the data
predictions <- read.csv("C:/Users/Lenovo/Downloads/OM 286 Fall 2 Supply Chain Analytics/Project/Predictions.csv")

# Step 2: Define the predicted classes based on probabilities
predictions <- predictions %>%
  mutate(Predicted_Result = ifelse(Probability >= 0.5, "H", "Not_Home"))  # Home win if Prob >= 0.5, else Not Home

# Step 3: Combine Draw and Away as "Not_Home" for Actual Result
predictions <- predictions %>%
  mutate(Actual_Class = ifelse(Actual.Result == "H", "H", "Not_Home"))

# Step 4: Confusion Matrix
conf_matrix <- table(Predicted_Result = predictions$Predicted_Result, Actual_Class = predictions$Actual_Class)
print("Confusion Matrix:")
print(conf_matrix)

# Step 5: Calculate Performance Metrics
confusion <- confusionMatrix(as.factor(predictions$Predicted_Result), as.factor(predictions$Actual_Class))
print("Performance Metrics:")
print(confusion)

# Step 6: Generate ROC Curve
roc_curve <- roc(predictions$Actual_Class, predictions$Probability, levels = c("Not_Home", "H"))
plot(roc_curve, main = "ROC Curve", col = "blue")
auc_value <- auc(roc_curve)
cat("Area Under the Curve (AUC):", auc_value, "\n")

# Step 7: Precision-Recall Curve
precision_recall <- predictions %>%
  mutate(Precision = cumsum(Actual_Class == "H") / seq_along(Probability),
         Recall = cumsum(Actual_Class == "H") / sum(Actual_Class == "H"))

ggplot(precision_recall, aes(x = Recall, y = Precision)) +
  geom_line(color = "blue") +
  labs(title = "Precision-Recall Curve", x = "Recall", y = "Precision") +
  theme_minimal()

# Step 8: Save results
write.csv(predictions, "Updated_Predictions_with_Results.csv", row.names = FALSE)




