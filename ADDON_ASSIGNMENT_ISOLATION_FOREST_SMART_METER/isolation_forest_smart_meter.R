if (!requireNamespace("isotree", quietly = TRUE)) {
  install.packages("isotree", repos = "https://cloud.r-project.org")
}

library(isotree)

set.seed(42)

generate_smart_meter_data <- function(n_normal = 450, n_anomaly = 50) {
  hour_normal <- sample(0:23, n_normal, replace = TRUE)
  temp_normal <- runif(n_normal, 20, 38)
  household_size_normal <- sample(1:6, n_normal, replace = TRUE)

  peak_bonus <- ifelse(hour_normal >= 18 & hour_normal <= 22, 1.8, 0.2)
  noise <- rnorm(n_normal, mean = 0, sd = 0.25)

  consumption_normal <- 0.9 +
    0.22 * household_size_normal +
    0.04 * temp_normal +
    peak_bonus + noise

  hour_anom <- sample(0:23, n_anomaly, replace = TRUE)
  temp_anom <- runif(n_anomaly, 10, 45)
  household_size_anom <- sample(1:7, n_anomaly, replace = TRUE)

  type_switch <- sample(c(1, 2), n_anomaly, replace = TRUE)
  consumption_anom <- ifelse(
    type_switch == 1,
    8 + 2 * runif(n_anomaly) + 0.1 * temp_anom,
    0.05 + 0.2 * runif(n_anomaly)
  )

  normal_df <- data.frame(
    hour = hour_normal,
    temperature = temp_normal,
    household_size = household_size_normal,
    consumption_kwh = consumption_normal,
    label = 0
  )

  anomaly_df <- data.frame(
    hour = hour_anom,
    temperature = temp_anom,
    household_size = household_size_anom,
    consumption_kwh = consumption_anom,
    label = 1
  )

  all_df <- rbind(normal_df, anomaly_df)
  all_df <- all_df[sample(seq_len(nrow(all_df))), ]
  rownames(all_df) <- NULL
  all_df
}

compute_metrics <- function(actual, predicted) {
  tp <- sum(actual == 1 & predicted == 1)
  tn <- sum(actual == 0 & predicted == 0)
  fp <- sum(actual == 0 & predicted == 1)
  fn <- sum(actual == 1 & predicted == 0)

  accuracy <- (tp + tn) / (tp + tn + fp + fn)
  precision <- ifelse((tp + fp) == 0, 0, tp / (tp + fp))
  recall <- ifelse((tp + fn) == 0, 0, tp / (tp + fn))
  f1 <- ifelse((precision + recall) == 0, 0, (2 * precision * recall) / (precision + recall))

  list(tp = tp, tn = tn, fp = fp, fn = fn,
       accuracy = accuracy, precision = precision, recall = recall, f1 = f1)
}

data_df <- generate_smart_meter_data(450, 50)

features <- data_df[, c("hour", "temperature", "household_size", "consumption_kwh")]

start_time <- Sys.time()
model <- isolation.forest(
  as.matrix(features),
  ntrees = 200,
  sample_size = 128,
  ndim = 1,
  nthreads = 1,
  seed = 42
)
scores <- predict(model, as.matrix(features), type = "score")
end_time <- Sys.time()

contamination <- mean(data_df$label)
threshold <- as.numeric(quantile(scores, probs = 1 - contamination))
pred <- ifelse(scores >= threshold, 1, 0)

metrics <- compute_metrics(data_df$label, pred)

cat("==============================================\n")
cat("ISOLATION FOREST OUTPUT\n")
cat("==============================================\n")
cat("Dataset Size:", nrow(data_df), "records\n")
cat("Normal Points:", sum(data_df$label == 0), "\n")
cat("Anomalies    :", sum(data_df$label == 1), "\n\n")

cat("Execution Time:", round(as.numeric(difftime(end_time, start_time, units = "secs")), 4), "seconds\n\n")

cat("Confusion Matrix:\n")
cat("TP:", metrics$tp, " FP:", metrics$fp, "\n")
cat("FN:", metrics$fn, " TN:", metrics$tn, "\n\n")

cat("Performance Metrics:\n")
cat("Accuracy :", round(metrics$accuracy, 4), "\n")
cat("Precision:", round(metrics$precision, 4), "\n")
cat("Recall   :", round(metrics$recall, 4), "\n")
cat("F1-Score :", round(metrics$f1, 4), "\n")
cat("==============================================\n")

cat("Top 10 Highest Anomaly Scores:\n")
ordered_idx <- order(scores, decreasing = TRUE)
for (k in seq_len(10)) {
  idx <- ordered_idx[k]
  cat(
    "Row", idx,
    "| Score:", round(scores[idx], 5),
    "| Actual:", data_df$label[idx],
    "| Predicted:", pred[idx], "\n"
  )
}
