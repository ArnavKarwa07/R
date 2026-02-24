data(iris)

str(iris)

# Fit linear regression model
lin_model <- lm(Sepal.Length ~ Petal.Length, data = iris)

# View summary
summary(lin_model)

iris$predicted <- predict(lin_model)

head(iris[, c("Petal.Length", "Sepal.Length", "predicted")])

plot(iris$Petal.Length, iris$Sepal.Length,
     xlab = "Petal Length",
     ylab = "Sepal Length",
     main = "Linear Regression",
     pch = 16)

abline(lin_model, col = "blue", lwd = 2)

# install.packages("ggplot2") # if needed
library(ggplot2)

ggplot(iris, aes(x = Petal.Length, y = Sepal.Length)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Linear Regression: Sepal Length vs Petal Length",
       x = "Petal Length",
       y = "Sepal Length")
