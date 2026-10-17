# 04_generate_graphs.R
# Generates every graph used in the README and saves it to images/.
# EDIT the CONFIG section so the names match YOUR dataset, then run from the project root.

library(tidyverse)
library(corrplot)
library(caret)
library(randomForest)

# ---------------- CONFIG: change these ----------------
data_path   <- "data/clean_data.csv"
target      <- "target"        # numeric outcome column
predictor   <- "predictor_1"   # numeric predictor for the scatter plot
category    <- "category"      # categorical column for the boxplot
# ------------------------------------------------------

dir.create("images", showWarnings = FALSE)
df <- read_csv(data_path, show_col_types = FALSE)
df[[category]] <- as.factor(df[[category]])
df <- df %>% mutate(across(where(is.character), as.factor))

save_gg <- function(p, name) ggsave(file.path("images", name), p, width = 8, height = 5, dpi = 300)

# 1. Target distribution
p1 <- ggplot(df, aes(x = .data[[target]])) +
  geom_histogram(bins = 30, fill = "steelblue", colour = "white") +
  labs(title = "Distribution of Target Variable", x = target, y = "Count") +
  theme_minimal(base_size = 13)
save_gg(p1, "01_target_distribution.png")

# 2. Correlation matrix
png("images/02_correlation_matrix.png", width = 2400, height = 2000, res = 300)
corrplot(cor(select(df, where(is.numeric)), use = "complete.obs"),
         method = "color", type = "upper", addCoef.col = "black", number.cex = 0.6,
         tl.col = "black", tl.cex = 0.8)
dev.off()

# 3. Scatter plot
p3 <- ggplot(df, aes(x = .data[[predictor]], y = .data[[target]])) +
  geom_point(alpha = 0.5, colour = "steelblue") +
  geom_smooth(method = "lm", colour = "firebrick") +
  labs(title = paste(target, "vs", predictor)) +
  theme_minimal(base_size = 13)
save_gg(p3, "03_scatter_predictor_target.png")

# 4. Boxplot by category
p4 <- ggplot(df, aes(x = .data[[category]], y = .data[[target]], fill = .data[[category]])) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = paste(target, "by", category), x = category) +
  theme_minimal(base_size = 13)
save_gg(p4, "04_boxplot_by_category.png")

# ---------- Modeling ----------
set.seed(123)
f     <- as.formula(paste(target, "~ ."))
idx   <- createDataPartition(df[[target]], p = 0.8, list = FALSE)
train <- df[idx, ]
test  <- df[-idx, ]

lm_fit <- lm(f, data = train)
rf_fit <- train(f, data = train, method = "rf",
                trControl = trainControl(method = "cv", number = 5))

# 5. Residual diagnostics (linear model)
png("images/05_residual_diagnostics.png", width = 2400, height = 2000, res = 300)
par(mfrow = c(2, 2)); plot(lm_fit); dev.off()

# 6. Predicted vs actual (random forest)
pred <- predict(rf_fit, test)
p6 <- ggplot(tibble(actual = test[[target]], predicted = pred), aes(actual, predicted)) +
  geom_point(alpha = 0.6, colour = "steelblue") +
  geom_abline(slope = 1, intercept = 0, colour = "firebrick", linetype = "dashed") +
  labs(title = "Predicted vs Actual (Random Forest, test set)") +
  theme_minimal(base_size = 13)
save_gg(p6, "06_predicted_vs_actual.png")

# 7. Variable importance
png("images/07_variable_importance.png", width = 2400, height = 1800, res = 300)
print(plot(varImp(rf_fit), top = 10, main = "Top 10 Variable Importances"))
dev.off()

# Print metrics to copy into the README table
cat("RMSE:", RMSE(pred, test[[target]]), " MAE:", MAE(pred, test[[target]]),
    " R2:", R2(pred, test[[target]]), "\n")
message("Done. Graphs saved in images/")
