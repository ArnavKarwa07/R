# Load dataset
data(iris)

# Create binary outcome: 1 = Setosa, 0 = Others
iris$IsSetosa <- ifelse(iris$Species == "setosa", 1, 0)

# Check structure
str(iris)

# Fit logistic regression model
model <- glm(IsSetosa ~ Petal.Length,
             data = iris,
             family = binomial)

summary(model)

# Predicted probabilities
iris$prob <- predict(model, type = "response")

head(iris[, c("Petal.Length", "Species", "prob")])

# Install if needed
# install.packages("ggplot2")

library(ggplot2)

ggplot(iris, aes(x = Petal.Length, y = IsSetosa)) +
  geom_point(position = position_jitter(height = 0.05),
             alpha = 0.6) +
  stat_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE,
              color = "blue") +
  labs(title = "Logistic Regression: Predicting Setosa",
       x = "Petal Length",
       y = "Probability of Setosa")

# Convert probabilities to predicted class
iris$pred_class <- ifelse(iris$prob > 0.5, 1, 0)

# Confusion matrix
table(Predicted = iris$pred_class,
      Actual = iris$IsSetosa)

exp(coef(model))
