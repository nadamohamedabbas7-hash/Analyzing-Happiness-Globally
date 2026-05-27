# =====================================================
# Load required libraries
# =====================================================
library(tidyverse)
library(psych) # Factor Analysis
library(factoextra) # Clustering visualization
library(VIM) # kNN imputation if needed
library(wbacon) # Outlier detection (BACON)
library(corrplot)
library(MVN)
# =====================================================
# Load the dataset
# =====================================================
df <- read.csv("2019.csv", stringsAsFactors = FALSE)
# =====================================================
# Outlier detection (BACON)
# =====================================================
w <- wBACON(df %>%
              select(-Overall.rank, -Country.or.region))
w
outlier_indices <- which(is_outlier(w))
# Remove outliers
df_clean <- df[-outlier_indices, ]
# =====================================================
# DESCRIPTIVE ANALYSIS AFTER REMOVING OUTLIERS
# =====================================================
cat("\n=========== DESCRIPTIVE ANALYSIS (AFTER OUTLIER REMOVAL) ===========\n")
# 1. Detailed descriptive statistics
cat("\n--- DETAILED DESCRIPTIVE STATISTICS ---\n")
desc_stats <- describe(df_clean %>% select(-Country.or.region, -Overall.rank))
print(desc_stats)
# 2. CORRELATION MATRIX
cat("\n--- CORRELATION MATRIX ---\n")
cor_matrix <- cor(df_clean %>% select(-Overall.rank, -Country.or.region))
print(round(cor_matrix, 3))
# Reduce top margin
  corrplot(cor_matrix,
           method = "color",
           type = "upper",
           tl.col = "black",
           tl.srt = 25,
           tl.cex = 0.7,
           addCoef.col = "black",
           number.cex = 0.7,
           main = "Correlation Matrix of Variables",
           mar = c(0, 0, 1, 0))

# 3. Top and bottom 5 countries by Score
cat("\n--- TOP 5 COUNTRIES BY HAPPINESS SCORE ---\n")
top_5 <- df_clean %>%
  arrange(desc(Score)) %>%
  head(5)
print(top_5)
cat("\n--- BOTTOM 5 COUNTRIES BY HAPPINESS SCORE ---\n")
bottom_5 <- df_clean %>%
  arrange(Score) %>%
  head(5)
print(bottom_5)
#4. Histograms for all Variables
# Get numeric variables names only
numeric_vars <- df_clean %>%
  select(-Country.or.region, -Overall.rank) %>%
  names()
# Histogram 1
hist(df_clean[[numeric_vars[1]]],
     main = paste("Histogram of", numeric_vars[1]),
     xlab = numeric_vars[1],
     col = "#E69F00",
     border = "white",
     breaks = 15)
# Histogram 2
hist(df_clean[[numeric_vars[2]]],
     main = paste("Histogram of", numeric_vars[2]),
     xlab = numeric_vars[2],
     col = "#56B4E9",
     border = "white",
     breaks = 15)
# Histogram 3
hist(df_clean[[numeric_vars[3]]],
     main = paste("Histogram of", numeric_vars[3]),
     xlab = numeric_vars[3],
     col = "#009E73",
     border = "white",
     breaks = 15)
# Histogram 4
hist(df_clean[[numeric_vars[4]]],
     main = paste("Histogram of", numeric_vars[4]),
     xlab = numeric_vars[4],
     col = "#F0E442",
     border = "white",
     breaks = 15)
# Histogram 5
hist(df_clean[[numeric_vars[5]]],
     main = paste("Histogram of", numeric_vars[5]),
     xlab = numeric_vars[5],
     col = "#0072B2",
     border = "white",
     breaks = 15)
# Histogram 6
hist(df_clean[[numeric_vars[6]]],
     main = paste("Histogram of", numeric_vars[6]),
     xlab = numeric_vars[6],
     col = "#D55E00",
     border = "white",
     breaks = 15)
# =====================================================
# Keep numeric columns only and Exclude Y which is Score
# =====================================================
df_numeric <- df_clean %>%
  select(-Overall.rank, -Country.or.region, -Score)
# =====================================================
# Scale the data
# =====================================================
df_scaled <- as.data.frame(scale(df_numeric))
# =====================================================
# Factor Analysis - check suitability
# =====================================================
KMO_result <- KMO(df_scaled)
print(KMO_result)
bartlett_result <- cortest.bartlett(cor(df_scaled), n = nrow(df_scaled))
print(bartlett_result)

# =====================================================
# Determine number of factors
# =====================================================
scree(df_scaled) # Scree plot
# =====================================================
# Factor Analysis
# =====================================================
mvn_result <- mvn(data = df_scaled, mvn_test = "royston")
mvn_result$multivariate_normality
mvn_result$univariate_normality
library(psych)
run_fa <- function(data, nfactors, rotate, fm, subtitle) {
  cat("\n====================", subtitle, "====================\n")
  fa_res <- fa(data, nfactors = nfactors, rotate = rotate, fm = fm)
  # Print summary
  summary(fa_res)
  # Print loadings >= 0.4
  print(fa_res$loadings, cutoff = 0.4)
  # Print communalities
  cat("\nCommunalities:\n")
  print(fa_res$communality)
  # Print eigenvalues
  cat("\nEigenvalues:\n")
  print(fa_res$e.values)
  return(fa_res)
}
# =====================================================
# Run Factor Analyses
# =====================================================
fa_1 <- run_fa(df_scaled, nfactors = 1, rotate = "none", fm = "pa", "1 Factor, No Rotation")
fa_2 <- run_fa(df_scaled, nfactors = 2, rotate = "none", fm = "pa", "2 Factors, No Rotation")
fa_2_ml <- run_fa(df_scaled, nfactors = 2, rotate = "none", fm = "minchi", "2 Factors, minchi
Method, No Rotation")
fa_2_var <- run_fa(df_scaled, nfactors = 2, rotate = "varimax", fm = "minchi", "2 Factors,
minchi Method, Varimax Rotation")
fa_2_obl<-run_fa(df_scaled, nfactors = 2, rotate = "oblimin", fm = "minchi", "2 Factors,
minchi Method, Oblimin Rotation")
# =====================================================
# Extract factor scores for regression
# =====================================================
factor_scores <- as.data.frame(fa_2_var$scores)
# Rename the columns
colnames(factor_scores) <- c("Wealth_Life_Quality", "Freedom_Social_Capital")
# Append the Score column
factor_scores$Score <- df_clean$Score
# =====================================================
# Multiple Linear Regression using factor scores
# =====================================================
lm_model <- lm(Score ~ ., data = factor_scores)
summary(lm_model)
# =====================================================
# Check diagnostics
# =====================================================
par(mfrow=c(2,2))
plot(lm_model)
par(mfrow=c(1,1))
# =====================================================
# Hierarchical clustering (Ward method)
# =====================================================
dist_matrix <- dist(df_scaled, method = "euclidean")
hc <- hclust(dist_matrix, method = "ward.D2")
# Plot dendrogram
plot(hc, labels = FALSE, hang = -1, main = "Hierarchical Clustering Dendrogram")
rect.hclust(hc, k = 2, border = "red") # Cut dendrogram into 2 clusters
# =====================================================
# Elbow method to determine optimal number of clusters
# =====================================================
fviz_nbclust(df_scaled, FUN = hcut, method = "wss",
             
             hc_method = "ward.D",
             k.max = 10) +
  
  labs(title = "Elbow Plot - Agglomerative Clustering (Complete Linkage)") +
  theme_minimal()
# =====================================================
# K-means clustering
# =====================================================
set.seed(123)
k <- 2
kmeans_result <- kmeans(df_scaled, centers = k, nstart = 25)
kmeans_result
# Add cluster assignment to original data
df_clean$Cluster_1 <- kmeans_result$cluster
df_clean$Cluster_1 <- factor(df_clean$Cluster_1,
                               levels = c(1, 2),
                             labels = c("Lower Well-being",
                                        "Higher Well-being"))
# =====================================================
# Visualize K-means clusters
# =====================================================
fviz_cluster(kmeans_result, data = df_scaled, geom = "point",
             
             ellipse.type = "convex", repel = TRUE,
             palette = c("#E64B35", "#4DBBD5")) +
  ggtitle("K-means Clustering of Countries") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
# =====================================================
# Hierarchical clustering (Ward method) for 3 clusters
# =====================================================
dist_matrix <- dist(df_scaled, method = "euclidean")
hc <- hclust(dist_matrix, method = "ward.D2")
# Plot dendrogram
plot(hc, labels = FALSE, hang = -1, main = "Hierarchical Clustering Dendrogram")
rect.hclust(hc, k = 3, border = "red") # Cut dendrogram into 3 clusters

# =====================================================
# K-means clustering for k = 3
# =====================================================
set.seed(123)
k <- 3
kmeans_result <- kmeans(df_scaled, centers = k, nstart = 25)
kmeans_result
# Add cluster assignment to original data
df_clean$Cluster_2 <- kmeans_result$cluster
df_clean$Cluster_2 <- factor(df_clean$Cluster_2,
                             levels = c(1, 2, 3),
                             labels = c("Lower Well-being",
                                        "Higher Well-being",
                                        "Medium Well-being"))

# =====================================================
# Visualize K-means clusters (3 clusters)
# =====================================================
fviz_cluster(kmeans_result, data = df_scaled, geom = "point",
             
             ellipse.type = "convex", repel = TRUE,
             palette = c("#E64B35", "#4DBBD5", "#00A087")) +
  ggtitle("K-means Clustering of Countries (3 clusters)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))