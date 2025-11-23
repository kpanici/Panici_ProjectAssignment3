library(dplyr)
library(ggplot2)
library(cluster)

setwd("C:/Users/kelly/OneDrive/Documents/ESCI 620")
df <- read.csv("CORAL_TEMPERATURE_CLEANED_20251104.csv")

# Prepare SST data and handle missing values by imputing column means
sst_df <- df %>% select(starts_with("WeeklySST_C_"))
sst_df <- sst_df %>% mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Scale SST data
sst_scaled <- scale(sst_df)

# Determine number of clusters using elbow method
wss <- sapply(1:6, function(k){
  kmeans(sst_scaled, centers = k, nstart = 25)$tot.withinss
})
plot(1:6, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters K",
     ylab = "Total within-cluster sum of squares",
     main = "Elbow Method for Optimal K")

# K-means clustering
set.seed(123)
kmeans_result <- kmeans(sst_scaled, centers = 3, nstart = 25)
df$Cluster <- as.factor(kmeans_result$cluster)

# Silhouette score
sil <- silhouette(kmeans_result$cluster, dist(sst_scaled))
avg_silhouette <- mean(sil[, 3])
cat("Average silhouette width:", round(avg_silhouette, 3), "\n")

# Visualize clusters
ggplot(df, aes(x = WeeklySST_C_1, y = WeeklySST_C_2, color = Cluster)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(title = "Clustering of Sites by SST Patterns (Weeks 1-2)",
       x = "Week 1 SST (°C)",
       y = "Week 2 SST (°C)") +
  theme_minimal(base_size = 14)