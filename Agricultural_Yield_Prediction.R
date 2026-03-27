# ============================================================
#   AGRICULTURAL YIELD PREDICTION USING LINEAR REGRESSION
#   Data Analysis Project — R Implementation
#   Submitted By: Sreyanandha Santhosh, Stephy Ann Biju,
#                 Steve Thomas Shylu
#   Department of Computer Science and Engineering
#   Amal Jyothi College of Engineering (Autonomous)
# ============================================================


# ============================================================
# SECTION 0: INSTALL & LOAD REQUIRED PACKAGES
# ============================================================
setwd("C:/Users/DELL/Downloads/RDataAnalytics")  # Windows example
required_packages <- c(
  "readr",       # CSV loading
  "dplyr",       # Data manipulation
  "ggplot2",     # Visualizations
  "corrplot",    # Correlation heatmap
  "caret",       # Model training & evaluation
  "Metrics",     # RMSE, MAE
  "gridExtra",   # Arrange multiple plots
  "ggcorrplot",  # ggplot2-style correlation plot
  "scales"       # Axis formatting
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cran.r-project.org")
  }
}

library(readr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(caret)
library(Metrics)
library(gridExtra)
library(scales)


# ============================================================
# SECTION 1: LOAD DATASET
# ============================================================

# Load the dataset (make sure Agricultural_Yield_Dataset.csv is
# in your working directory, or set full path below)
df <- read_csv("Agricultural_Yield_Dataset.csv")

cat("=== Dataset Loaded ===\n")
cat("Dimensions:", nrow(df), "rows x", ncol(df), "columns\n\n")

# Preview
print(head(df, 10))
cat("\n")
print(str(df))


# ============================================================
# SECTION 2: EXPLORATORY DATA ANALYSIS (EDA)
# ============================================================

cat("\n=== Summary Statistics ===\n")
print(summary(df))

# 2.1 Check missing values
cat("\n=== Missing Values Per Column ===\n")
print(colSums(is.na(df)))

# 2.2 Check duplicate rows
cat("\n=== Duplicate Rows:", sum(duplicated(df)), "===\n")

# 2.3 Sentiment / Yield distribution plot
p1 <- ggplot(df, aes(x = Crop_Yield_tons_ha)) +
  geom_histogram(fill = "#2E75B6", color = "white", bins = 30) +
  labs(
    title = "Distribution of Crop Yield",
    subtitle = "Checking if yield is normally distributed",
    x = "Crop Yield (tons/ha)",
    y = "Count"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", color = "#1F3864"),
    plot.subtitle = element_text(color = "#2E75B6")
  )
print(p1)

# 2.4 Rainfall vs Yield
p2 <- ggplot(df, aes(x = Rainfall_mm, y = Crop_Yield_tons_ha)) +
  geom_point(color = "#2E75B6", alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "#1F3864") +
  labs(
    title    = "Rainfall vs Crop Yield",
    subtitle = "Positive correlation expected",
    x = "Rainfall (mm)",
    y = "Crop Yield (tons/ha)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", color = "#1F3864"),
    plot.subtitle = element_text(color = "#2E75B6")
  )
print(p2)

# 2.5 Temperature vs Yield
p3 <- ggplot(df, aes(x = Temperature_C, y = Crop_Yield_tons_ha)) +
  geom_point(color = "#E05C2A", alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "#1F3864") +
  labs(
    title    = "Temperature vs Crop Yield",
    subtitle = "Excess temperature negatively impacts yield",
    x = "Temperature (deg C)",
    y = "Crop Yield (tons/ha)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", color = "#1F3864"),
    plot.subtitle = element_text(color = "#2E75B6")
  )
print(p3)

# 2.6 Fertilizer vs Yield
p4 <- ggplot(df, aes(x = Fertilizer_kg_ha, y = Crop_Yield_tons_ha)) +
  geom_point(color = "#27A844", alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "#1F3864") +
  labs(
    title    = "Fertilizer Usage vs Crop Yield",
    subtitle = "Strongest positive driver of yield",
    x = "Fertilizer Usage (kg/ha)",
    y = "Crop Yield (tons/ha)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", color = "#1F3864"),
    plot.subtitle = element_text(color = "#2E75B6")
  )
print(p4)

# 2.7 Soil Type vs Yield (Boxplot)
p5 <- ggplot(df, aes(x = Soil_Type, y = Crop_Yield_tons_ha, fill = Soil_Type)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title    = "Soil Type vs Crop Yield",
    subtitle = "Silt Loam shows highest productivity",
    x = "Soil Type",
    y = "Crop Yield (tons/ha)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", color = "#1F3864"),
    plot.subtitle = element_text(color = "#2E75B6"),
    legend.position = "none"
  )
print(p5)

# 2.8 Year-wise Yield Trend
yearly <- df %>%
  group_by(Year) %>%
  summarise(Avg_Yield = mean(Crop_Yield_tons_ha), .groups = "drop")

p6 <- ggplot(yearly, aes(x = Year, y = Avg_Yield)) +
  geom_line(color = "#2E75B6", linewidth = 1.2) +
  geom_point(color = "#1F3864", size = 3) +
  labs(
    title    = "Year-wise Average Crop Yield Trend",
    subtitle = "Historical productivity trend",
    x = "Year",
    y = "Average Yield (tons/ha)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", color = "#1F3864"),
    plot.subtitle = element_text(color = "#2E75B6")
  )
print(p6)


# ============================================================
# SECTION 3: DATA PREPROCESSING
# ============================================================

cat("\n=== Preprocessing ===\n")

# 3.1 Remove duplicates
df <- df %>% distinct()
cat("Rows after removing duplicates:", nrow(df), "\n")

# 3.2 Handle missing values — impute numeric columns with median
numeric_cols <- c("Rainfall_mm", "Temperature_C", "Humidity_pct",
                  "Fertilizer_kg_ha", "Area_ha", "Crop_Yield_tons_ha")

for (col in numeric_cols) {
  if (any(is.na(df[[col]]))) {
    df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
    cat("Imputed missing values in:", col, "\n")
  }
}

# 3.3 Encode Soil_Type as numeric factor
soil_levels <- c("Sandy Loam", "Clay", "Silt Loam", "Black Cotton", "Red Laterite")
df$Soil_Type_Encoded <- as.integer(factor(df$Soil_Type, levels = soil_levels))

cat("\nSoil Type Encoding:\n")
encoding_map <- data.frame(
  Soil_Type = soil_levels,
  Encoded   = 1:5
)
print(encoding_map)

# 3.4 Select model features
model_df <- df %>%
  select(
    Rainfall_mm,
    Temperature_C,
    Humidity_pct,
    Fertilizer_kg_ha,
    Soil_Type_Encoded,
    Crop_Yield_tons_ha
  )

cat("\nFinal model dataset dimensions:", nrow(model_df), "x", ncol(model_df), "\n")
print(head(model_df))


# ============================================================
# SECTION 4: CORRELATION ANALYSIS
# ============================================================

cat("\n=== Pearson Correlation Matrix ===\n")
cor_matrix <- cor(model_df, use = "complete.obs")
print(round(cor_matrix, 3))

# Correlation with target
cat("\n=== Correlation with Crop Yield ===\n")
yield_cor <- sort(cor_matrix[, "Crop_Yield_tons_ha"], decreasing = TRUE)
print(round(yield_cor, 3))

# Correlation heatmap
corrplot(
  cor_matrix,
  method    = "color",
  type      = "upper",
  tl.col    = "#1F3864",
  tl.srt    = 45,
  addCoef.col = "black",
  number.cex  = 0.8,
  col         = colorRampPalette(c("#D5E8F0", "white", "#1F3864"))(200),
  title       = "Correlation Matrix — Agricultural Variables",
  mar         = c(0, 0, 2, 0)
)


# ============================================================
# SECTION 5: TRAIN-TEST SPLIT
# ============================================================

set.seed(42)  # Reproducibility

train_idx  <- createDataPartition(model_df$Crop_Yield_tons_ha, p = 0.8, list = FALSE)
train_data <- model_df[train_idx, ]
test_data  <- model_df[-train_idx, ]

cat("\n=== Train-Test Split ===\n")
cat("Training rows:", nrow(train_data), "\n")
cat("Test rows    :", nrow(test_data),  "\n")


# ============================================================
# SECTION 6: MODEL BUILDING — LINEAR REGRESSION
# ============================================================

cat("\n=== Training Linear Regression Model ===\n")

lm_model <- lm(
  Crop_Yield_tons_ha ~ Rainfall_mm + Temperature_C + Humidity_pct +
    Fertilizer_kg_ha + Soil_Type_Encoded,
  data = train_data
)

cat("\n=== Regression Summary ===\n")
print(summary(lm_model))

# Coefficients table
cat("\n=== Learned Coefficients ===\n")
coef_df <- as.data.frame(summary(lm_model)$coefficients)
coef_df$Significance <- ifelse(coef_df[,4] < 0.001, "***",
                         ifelse(coef_df[,4] < 0.01,  "**",
                         ifelse(coef_df[,4] < 0.05,  "*", "ns")))
print(coef_df)

# ============================================================
# SECTION 7: MODEL EVALUATION
# ============================================================

# Predictions on test set
predictions <- predict(lm_model, newdata = test_data)

# Metrics
r2_val   <- R2(predictions, test_data$Crop_Yield_tons_ha)
rmse_val <- rmse(test_data$Crop_Yield_tons_ha, predictions)
mae_val  <- mae(test_data$Crop_Yield_tons_ha, predictions)

# Adjusted R2
n  <- nrow(test_data)
p  <- 5   # number of predictors
adj_r2 <- 1 - (1 - r2_val) * (n - 1) / (n - p - 1)

cat("\n=== Model Performance on Test Set ===\n")
cat(sprintf("R-squared        : %.4f\n", r2_val))
cat(sprintf("Adjusted R2      : %.4f\n", adj_r2))
cat(sprintf("RMSE (tons/ha)   : %.4f\n", rmse_val))
cat(sprintf("MAE  (tons/ha)   : %.4f\n", mae_val))

# Performance bar chart
metrics_df <- data.frame(
  Metric = c("R-squared", "Adj R-squared"),
  Score  = c(r2_val, adj_r2) * 100
)

p_metrics <- ggplot(metrics_df, aes(x = Metric, y = Score, fill = Metric)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = sprintf("%.2f%%", Score)),
            vjust = -0.4, fontface = "bold", size = 4.5) +
  scale_fill_manual(values = c("#2E75B6", "#1F3864")) +
  scale_y_continuous(limits = c(0, 105)) +
  labs(
    title    = "Model Performance Metrics",
    subtitle = "Linear Regression | Agricultural Yield Prediction",
    x = "",
    y = "Score (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", color = "#1F3864"),
    plot.subtitle = element_text(color = "#2E75B6"),
    legend.position = "none"
  )
print(p_metrics)


# ============================================================
# SECTION 8: RESIDUAL ANALYSIS
# ============================================================

residuals_df <- data.frame(
  Fitted    = fitted(lm_model),
  Residuals = residuals(lm_model)
)

# Residuals vs Fitted
p_resid <- ggplot(residuals_df, aes(x = Fitted, y = Residuals)) +
  geom_point(color = "#2E75B6", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#1F3864") +
  labs(
    title    = "Residuals vs Fitted Values",
    subtitle = "No systematic pattern confirms homoscedasticity",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", color = "#1F3864"),
    plot.subtitle = element_text(color = "#2E75B6")
  )
print(p_resid)

# Normal Q-Q Plot of residuals
qqdf <- data.frame(
  Theoretical = qqnorm(residuals(lm_model), plot.it = FALSE)$x,
  Sample      = qqnorm(residuals(lm_model), plot.it = FALSE)$y
)
p_qq <- ggplot(qqdf, aes(x = Theoretical, y = Sample)) +
  geom_point(color = "#2E75B6", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#1F3864") +
  labs(
    title    = "Normal Q-Q Plot of Residuals",
    subtitle = "Points close to line confirm normal residual distribution",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", color = "#1F3864"),
    plot.subtitle = element_text(color = "#2E75B6")
  )
print(p_qq)


# ============================================================
# SECTION 9: ACTUAL vs PREDICTED PLOT
# ============================================================

actual_vs_pred <- data.frame(
  Actual    = test_data$Crop_Yield_tons_ha,
  Predicted = predictions
)

p_avp <- ggplot(actual_vs_pred, aes(x = Actual, y = Predicted)) +
  geom_point(color = "#2E75B6", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "#1F3864",
              linetype = "dashed", linewidth = 1) +
  labs(
    title    = "Actual vs Predicted Crop Yield",
    subtitle = "Points close to dashed line = accurate predictions",
    x = "Actual Yield (tons/ha)",
    y = "Predicted Yield (tons/ha)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", color = "#1F3864"),
    plot.subtitle = element_text(color = "#2E75B6")
  )
print(p_avp)


# ============================================================
# SECTION 10: TOP INFLUENCING FACTORS
# ============================================================

# Standardized coefficients (beta weights)
train_scaled        <- as.data.frame(scale(train_data))
lm_scaled           <- lm(Crop_Yield_tons_ha ~ ., data = train_scaled)
std_coefs           <- coef(lm_scaled)[-1]   # remove intercept

coef_plot_df <- data.frame(
  Variable = names(std_coefs),
  StdBeta  = as.numeric(std_coefs)
) %>%
  mutate(
    Direction = ifelse(StdBeta > 0, "Positive", "Negative"),
    Variable  = recode(Variable,
      Rainfall_mm       = "Rainfall",
      Temperature_C     = "Temperature",
      Humidity_pct      = "Humidity",
      Fertilizer_kg_ha  = "Fertilizer Usage",
      Soil_Type_Encoded = "Soil Type"
    )
  ) %>%
  arrange(desc(abs(StdBeta)))

p_coef <- ggplot(coef_plot_df,
                 aes(x = reorder(Variable, StdBeta), y = StdBeta, fill = Direction)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#2E75B6", "Negative" = "#E05C2A")) +
  geom_text(aes(label = round(StdBeta, 3)),
            hjust = ifelse(coef_plot_df$StdBeta > 0, -0.1, 1.1),
            size = 4, fontface = "bold") +
  labs(
    title    = "Standardized Regression Coefficients",
    subtitle = "Positive = increases yield | Negative = reduces yield",
    x = "Variable",
    y = "Standardized Beta"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", color = "#1F3864"),
    plot.subtitle = element_text(color = "#2E75B6"),
    legend.title  = element_blank()
  )
print(p_coef)


# ============================================================
# SECTION 11: LIVE PREDICTION FUNCTION
# ============================================================

predict_yield <- function(rainfall, temperature, humidity,
                           fertilizer, soil_type) {
  # Encode soil type
  soil_levels  <- c("Sandy Loam", "Clay", "Silt Loam",
                    "Black Cotton", "Red Laterite")
  soil_encoded <- which(soil_levels == soil_type)

  if (length(soil_encoded) == 0) {
    stop("Invalid soil type. Choose from: Sandy Loam, Clay, Silt Loam, Black Cotton, Red Laterite")
  }

  new_data <- data.frame(
    Rainfall_mm       = rainfall,
    Temperature_C     = temperature,
    Humidity_pct      = humidity,
    Fertilizer_kg_ha  = fertilizer,
    Soil_Type_Encoded = soil_encoded
  )

  pred  <- predict(lm_model, newdata = new_data)
  lower <- pred - 1.96 * rmse_val
  upper <- pred + 1.96 * rmse_val

  cat("============================================\n")
  cat("  AGRICULTURAL YIELD PREDICTION\n")
  cat("============================================\n")
  cat(sprintf("  Rainfall      : %.1f mm\n",       rainfall))
  cat(sprintf("  Temperature   : %.1f deg C\n",    temperature))
  cat(sprintf("  Humidity      : %.1f %%\n",        humidity))
  cat(sprintf("  Fertilizer    : %.1f kg/ha\n",    fertilizer))
  cat(sprintf("  Soil Type     : %s (Code: %d)\n", soil_type, soil_encoded))
  cat("--------------------------------------------\n")
  cat(sprintf("  Predicted Yield : %.2f tons/ha\n", pred))
  cat(sprintf("  95%% CI         : [%.2f, %.2f]\n", lower, upper))
  cat("============================================\n\n")

  return(invisible(pred))
}

# --- Demo predictions ---
cat("\n=== Live Prediction Demonstrations ===\n\n")

predict_yield(550, 26, 72, 200, "Silt Loam")
predict_yield(300, 32, 60, 100, "Red Laterite")
predict_yield(700, 24, 78, 280, "Sandy Loam")
predict_yield(200, 38, 50,  60, "Clay")


# ============================================================
# SECTION 12: SAVE RESULTS
# ============================================================

# Save predictions to CSV
results_df <- data.frame(
  Actual_Yield    = test_data$Crop_Yield_tons_ha,
  Predicted_Yield = round(predictions, 3),
  Residual        = round(test_data$Crop_Yield_tons_ha - predictions, 3)
)
write_csv(results_df, "Yield_Predictions.csv")
cat("Predictions saved to: Yield_Predictions.csv\n")

cat("\n=== Project Complete ===\n")
cat(sprintf("Final Model: R2 = %.4f | RMSE = %.4f | MAE = %.4f\n",
            r2_val, rmse_val, mae_val))
