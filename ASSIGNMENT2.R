# 1. Load your dataset
data <- mtcars
target_variable <- data$mpg

# 2. Define your Null Hypothesis (H0) value and Alpha
null_mean <- 20              # Null hypothesis - mean value of of mpg is 20
alpha <- 0.05                # Significance level (95% confidence)

# 3. Perform the T-Test
test_result <- t.test(target_variable, mu = null_mean)

# 4. Extract the p-value
p_value <- test_result$p.value

# 5. Prediction Logic
cat("--- Statistical Hypothesis Test --- \n")
cat("P-value calculated:", round(p_value, 4), "\n")
cat("Significance Level (Alpha):", alpha, "\n\n")

if (p_value < alpha) {
  cat("PREDICTION: REJECT the Null Hypothesis.\n")
  cat("Reason: The p-value is less than alpha, suggesting significant evidence.")
} else {
  cat("PREDICTION: FAIL TO REJECT (Accept) the Null Hypothesis.\n")
  cat("Reason: The p-value is greater than alpha, suggesting no significant difference.")
}