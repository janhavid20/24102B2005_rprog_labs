
library(tidyverse)
library(lubridate)
library(factoextra)
library(cluster)
library(caret)
library(randomForest)
library(e1071)
library(plotly)



 df <- read.excel("OnlineRetail.xlsx", stringsAsFactors = FALSE)

# Simulated data loading for demonstration
set.seed(123)

# Clean data: Remove cancellations, missing CustomerIDs, and negative quantities
df_clean <- df %>%
  filter(!is.na(CustomerID), Quantity > 0, UnitPrice > 0) %>%
  mutate(TotalPrice = Quantity * UnitPrice,
         InvoiceDate = as.Date(InvoiceDate))

# Set reference date for Recency
ref_date <- max(df_clean$InvoiceDate) + 1

# Calculate RFM and other features per customer
customer_features <- df_clean %>%
  group_by(CustomerID) %>%
  summarise(
    Recency = as.numeric(ref_date - max(InvoiceDate)),
    Frequency = n_distinct(InvoiceNo),
    Monetary = sum(TotalPrice),
    AvgTransactionValue = mean(TotalPrice),
    TotalQuantity = sum(Quantity)
  ) %>%
  ungroup()

# Handle Outliers (Capping at 95th percentile)
cap_outliers <- function(x) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = TRUE)
  caps <- quantile(x, probs=c(.05, .95), na.rm = TRUE)
  H <- 1.5 * IQR(x, na.rm = TRUE)
  x[x > (qnt[2] + H)] <- caps[2]
  return(x)
}

customer_features_capped <- customer_features %>%
  mutate(across(c(Recency, Frequency, Monetary, AvgTransactionValue, TotalQuantity), cap_outliers))

# Feature Scaling
rfm_scaled <- scale(customer_features_capped[, -1])
rownames(rfm_scaled) <- customer_features_capped$CustomerID

# ---------------------------------------------------------
# 2. Clustering: Elbow Method, K-Means, & Hierarchical
# ---------------------------------------------------------
# Elbow Method
fviz_nbclust(rfm_scaled, kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal Clusters")

# K-Means Clustering (Assuming k=3 based on typical RFM)
set.seed(42)
kmeans_res <- kmeans(rfm_scaled, centers = 3, nstart = 25)
customer_features_capped$Cluster <- as.factor(kmeans_res$cluster)

# Hierarchical Clustering & Dendrogram
dist_matrix <- dist(rfm_scaled, method = "euclidean")
hc_res <- hclust(dist_matrix, method = "ward.D2")
plot(hc_res, labels = FALSE, main = "Hierarchical Clustering Dendrogram")
rect.hclust(hc_res, k = 3, border = "red")

# Silhouette Score
sil <- silhouette(kmeans_res$cluster, dist_matrix)
fviz_silhouette(sil)

# ---------------------------------------------------------
# 3. Dimensionality Reduction (PCA) & Visualizations
# ---------------------------------------------------------
pca_res <- prcomp(rfm_scaled, center = FALSE, scale. = FALSE)

# 2D PCA Visualization
fviz_pca_ind(pca_res, 
             geom = "point", 
             col.ind = customer_features_capped$Cluster, 
             palette = "jco", 
             addEllipses = TRUE, 
             legend.title = "Cluster",
             title = "2D PCA Customer Segments")

# Interactive 3D Visualization using Plotly
pca_data <- data.frame(pca_res$x[, 1:3], Cluster = customer_features_capped$Cluster)
plot_ly(pca_data, x = ~PC1, y = ~PC2, z = ~PC3, color = ~Cluster, colors = c('#1f77b4', '#ff7f0e', '#2ca02c')) %>%
  add_markers(size = 3) %>%
  layout(title = "3D PCA Customer Segments", scene = list(xaxis = list(title = 'PC1'), yaxis = list(title = 'PC2'), zaxis = list(title = 'PC3')))

# ---------------------------------------------------------
# 4. Predictive Analytics: Supervised Models
# ---------------------------------------------------------
# Define High-Value Segment (Assuming Cluster 1 is high-value based on profiling)
# Note: You should profile clusters first (e.g., highest Monetary & Frequency)
customer_features_capped <- customer_features_capped %>%
  mutate(IsHighValue = as.factor(ifelse(Cluster == 1, "Yes", "No")))

# Prepare data for modeling
model_data <- customer_features_capped %>% select(-CustomerID, -Cluster)

# Train-Test Split
set.seed(101)
trainIndex <- createDataPartition(model_data$IsHighValue, p = .8, list = FALSE)
train_data <- model_data[trainIndex, ]
test_data <- model_data[-trainIndex, ]

# Model 1: Random Forest
rf_model <- randomForest(IsHighValue ~ ., data = train_data, importance = TRUE)
rf_preds <- predict(rf_model, test_data)

# Model 2: Support Vector Machine (SVM)
svm_model <- svm(IsHighValue ~ ., data = train_data, probability = TRUE)
svm_preds <- predict(svm_model, test_data)

# ---------------------------------------------------------
# 5. Evaluation and Feature Importance
# ---------------------------------------------------------
# Confusion Matrix & Metrics (Random Forest)
cat("Random Forest Performance:\n")
rf_cm <- confusionMatrix(rf_preds, test_data$IsHighValue, positive = "Yes")
print(rf_cm)

# Confusion Matrix & Metrics (SVM)
cat("\nSVM Performance:\n")
svm_cm <- confusionMatrix(svm_preds, test_data$IsHighValue, positive = "Yes")
print(svm_cm)

# ROC-AUC (Requires numeric probabilities)
rf_prob <- predict(rf_model, test_data, type = "prob")[, "Yes"]
svm_prob <- attr(predict(svm_model, test_data, probability = TRUE), "probabilities")[, "Yes"]

library(pROC)
rf_roc <- roc(test_data$IsHighValue, rf_prob)
svm_roc <- roc(test_data$IsHighValue, svm_prob)

cat("\nROC-AUC Scores:\n")
cat("Random Forest AUC:", auc(rf_roc), "\n")
cat("SVM AUC:", auc(svm_roc), "\n")

# Plot ROC Curves
plot(rf_roc, col = "blue", main = "ROC Curves")
plot(svm_roc, col = "red", add = TRUE)
legend("bottomright", legend = c("Random Forest", "SVM"), col = c("blue", "red"), lwd = 2)

# Feature Importance (Random Forest)
varImpPlot(rf_model, main = "Feature Importance for High-Value Prediction")