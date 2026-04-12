# Add-on Assignment Report

## 1. Student Name, PRN and Batch

- Student Name: Arnav Karwa
- PRN: <Enter PRN>
- Batch: <Enter Batch>

## 2. Title of the Add-on Assignment

Implementation of Anomaly Detection in Smart Meter Energy Consumption Using Isolation Forest in R

## 3. Objective

To detect abnormal household electricity consumption patterns from smart meter data using a practical Isolation Forest implementation in R.

## 4. Name of the Platform

RStudio

## 5. Methodology / Implementation Description

### Problem Statement

Smart meter systems produce large volumes of consumption data. Manual monitoring is difficult, so anomaly detection is useful for identifying unusual electricity usage such as theft, device faults, or metering issues.

### Approach Used

The solution uses the `isotree` library for Isolation Forest to keep the implementation practical, shorter, and easier to understand.

### Libraries Used

- `isotree`: model training and anomaly score prediction
- `base R`: data generation, thresholding, evaluation, and output printing

### Step-by-Step Implementation

1. Created a custom dataset of 500 records:

- 450 normal points
- 50 anomaly points
- Features: `hour`, `temperature`, `household_size`, `consumption_kwh`
- Target label: `0` (normal), `1` (anomaly)

2. Generated realistic normal behavior using random distributions and peak-hour consumption effect.

3. Injected anomalous behavior as:

- extremely high consumption spikes
- unusually low consumption values

4. Trained an Isolation Forest model using:

- `ntrees = 200`
- `sample_size = 128`
- fixed `seed = 42` for reproducibility

5. Predicted anomaly scores using `predict(..., type = "score")`.

6. Chose decision threshold from contamination rate:

   $$
   \text{threshold} = Q_{1-c}(\text{scores})
   $$

   where $c$ is anomaly ratio and $Q$ is quantile.

7. Converted scores to predicted labels:

- score >= threshold -> anomaly (1)
- score < threshold -> normal (0)

8. Computed confusion matrix and metrics (Accuracy, Precision, Recall, F1-score).

### Justification

- The library-based model avoids unnecessary code complexity.
- The workflow is easy to explain, execute, and evaluate.
- It is suitable for practical academic demonstration and real-world extension.

## 6. Results and Observations

### Execution Output Summary

- Dataset size: 500
- Normal points: 450
- Anomalies: 50
- Execution time: 0.0732 seconds

### Confusion Matrix

- TP: 48
- FP: 2
- FN: 2
- TN: 448

### Performance Metrics

- Accuracy: 0.992
- Precision: 0.96
- Recall: 0.96
- F1-score: 0.96

### Observations

1. The model detected almost all anomalies with very low false positives.
2. Execution time is very low, showing strong efficiency.
3. The method is simple to maintain and can scale to larger datasets.
4. Isolation Forest is highly suitable for unsupervised anomaly scoring in utility data.

## 7. Application Areas

1. Electricity theft detection in smart grids.
2. Fault detection in energy meters.
3. Abnormal load pattern monitoring for utilities.
4. Industrial sensor anomaly detection.
5. Fraud and risk monitoring in other domains.

## 8. Conclusion

The assignment successfully solves a real-world smart meter anomaly detection problem using a clean and practical R implementation. By using the `isotree` library and a custom dataset, the solution stays simple while achieving high performance (Accuracy 0.992, F1-score 0.96). The approach is implementation-friendly, scalable, and suitable for academic as well as applied use.

---

## Files in Submission Folder

- `isolation_forest_smart_meter.R`
- `ADDON_ASSIGNMENT_REPORT.md`
