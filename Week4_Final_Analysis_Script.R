# Final analysis script: data preparation, visualization, statistics and modelling
# Run top to bottom. Sections are separated by '# ----' markers.

# ---- setup
# Install once if needed:
# install.packages(c("ggplot2", "dplyr", "car", "lmtest"))
library(ggplot2)
library(dplyr)
library(car)      # vif()
library(lmtest)   # bptest()

data(mtcars)          # Dataset 1: 32 cars, 11 variables
data(AirPassengers)   # Dataset 2: monthly airline passengers, 1949-1960

# ---- clean
# 1. Structure and data types
str(mtcars)
class(AirPassengers)

# 2. Missing values and duplicate rows
colSums(is.na(mtcars))
sum(is.na(AirPassengers))
sum(duplicated(mtcars))

# 3. Plausibility checks: ranges and allowed values
sapply(mtcars[, c("mpg", "wt", "hp", "qsec")], range)
table(mtcars$cyl)
all(mtcars$am %in% c(0, 1))

# 4. Outlier screen with the 1.5 x IQR rule (flag, then decide)
flag_outliers <- function(x, names) {
  q   <- quantile(x, c(0.25, 0.75))
  iqr <- diff(q)
  names[x < q[1] - 1.5 * iqr | x > q[2] + 1.5 * iqr]
}
lapply(mtcars[, c("mpg", "hp", "wt", "qsec")], flag_outliers,
       names = rownames(mtcars))

# 5. Is the time series complete (144 months, no gaps)?
length(AirPassengers)
frequency(AirPassengers)
start(AirPassengers)
end(AirPassengers)
any(diff(as.numeric(time(AirPassengers))) > 1/12 + 1e-8)

# 6. Recoding: readable labels, car names as a column, time series -> data frame
cars <- mtcars %>%
  mutate(
    car     = rownames(mtcars),
    cyl_f   = factor(cyl),                                   # for tests and models
    cyl_lab = factor(cyl, levels = c(4, 6, 8),
                     labels = c("4 cyl", "6 cyl", "8 cyl")), # for chart labels
    trans   = factor(am, levels = c(0, 1), labels = c("Automatic", "Manual"))
  )

ap <- data.frame(
  date       = seq(as.Date("1949-01-01"), by = "month", length.out = 144),
  passengers = as.numeric(AirPassengers)
)
ap$year  <- as.integer(format(ap$date, "%Y"))
ap$month <- factor(format(ap$date, "%b"), levels = month.abb)

pal_cyl <- c("4 cyl" = "#2A9D8F", "6 cyl" = "#E9C46A", "8 cyl" = "#E76F51")

# ---- w2_explore
summary(cars$mpg)
sd(cars$mpg)
table(cars$cyl_lab)
summary(ap$passengers)

# ---- w2_p1
avg_mpg <- cars %>%
  group_by(cyl_lab) %>%
  summarise(mean_mpg = mean(mpg), n = n())

p1 <- ggplot(avg_mpg, aes(x = cyl_lab, y = mean_mpg, fill = cyl_lab)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = round(mean_mpg, 1)), vjust = -0.5, fontface = "bold") +
  scale_fill_manual(values = pal_cyl) +
  labs(title    = "Fewer cylinders, better fuel economy",
       subtitle = "Average mpg by engine size, 32 cars (mtcars)",
       x = "Number of cylinders", y = "Average miles per gallon") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("p1_bar.png", p1, width = 8, height = 5, dpi = 300)

# ---- w2_p2
p2 <- ggplot(cars, aes(x = mpg)) +
  geom_histogram(binwidth = 3, boundary = 10, closed = "left",
                 fill = "#264653", color = "white") +
  geom_vline(xintercept = mean(cars$mpg), linetype = "dashed",
             color = "#E76F51", linewidth = 0.9) +
  annotate("text", x = mean(cars$mpg) + 0.6, y = 8.4, hjust = 0,
           label = paste0("Mean = ", round(mean(cars$mpg), 1), " mpg"),
           color = "#E76F51", fontface = "bold") +
  labs(title    = "Most cars cluster between 15 and 25 mpg",
       subtitle = "Distribution of fuel economy (bin width = 3 mpg)",
       x = "Miles per gallon", y = "Number of cars") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("p2_hist.png", p2, width = 8, height = 5, dpi = 300)

# ---- w2_p3
fit <- lm(mpg ~ wt, data = cars)
coef(fit)                      # intercept and slope
summary(fit)$r.squared         # R-squared
cor(cars$wt, cars$mpg)         # correlation

label_cars <- c("Cadillac Fleetwood", "Lincoln Continental", "Toyota Corolla")

p3 <- ggplot(cars, aes(x = wt, y = mpg)) +
  geom_smooth(method = "lm", se = TRUE, color = "grey30",
              fill = "grey80", linetype = "dashed") +
  geom_point(aes(color = cyl_lab), size = 3) +
  geom_text(data = subset(cars, car %in% label_cars),
            aes(label = car), size = 3, vjust = -1) +
  scale_color_manual(values = pal_cyl, name = "Cylinders") +
  labs(title    = "Heavier cars burn more fuel",
       subtitle = "Weight vs. mpg with linear trend",
       x = "Weight (1000 lbs)", y = "Miles per gallon") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("p3_scatter.png", p3, width = 8, height = 5.4, dpi = 300)

# ---- w2_p4
cars %>%
  group_by(trans) %>%
  summarise(n = n(), mean_mpg = mean(mpg), median_mpg = median(mpg),
            mean_wt = mean(wt))

t.test(mpg ~ trans, data = cars)   # Welch two-sample t-test

p4 <- ggplot(cars, aes(x = trans, y = mpg, fill = trans)) +
  geom_boxplot(width = 0.5, alpha = 0.85, outlier.shape = NA,
               show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.6, size = 2) +
  scale_fill_manual(values = c("Automatic" = "#8ECAE6", "Manual" = "#FFB703")) +
  labs(title    = "Manual cars are more fuel efficient",
       subtitle = "mpg by transmission type (points = individual cars)",
       x = "Transmission", y = "Miles per gallon") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("p4_box.png", p4, width = 7, height = 5, dpi = 300)

# ---- w2_p5
p5 <- ggplot(ap, aes(x = date, y = passengers)) +
  geom_line(color = "#1D3557", linewidth = 0.9) +
  geom_smooth(method = "lm", se = FALSE, color = "#E63946",
              linetype = "dashed") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(title    = "Air travel grew steadily through the 1950s",
       subtitle = "Monthly international airline passengers, 1949-1960",
       x = "Year", y = "Passengers (thousands)") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("p5_line.png", p5, width = 9, height = 5, dpi = 300)

# ---- w2_p6
p6 <- ggplot(ap, aes(x = month, y = passengers, group = year, color = year)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.2) +
  scale_color_viridis_c(name = "Year") +
  labs(title    = "Travel peaks every summer",
       subtitle = "Seasonal pattern: one line per year",
       x = "Month", y = "Passengers (thousands)") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("p6_season.png", p6, width = 9, height = 5, dpi = 300)

# Average passengers per calendar month
aggregate(passengers ~ month, data = ap, FUN = mean)

# ---- w2_p7
vars    <- c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec")
cor_mat <- round(cor(mtcars[, vars]), 2)
cor_df  <- as.data.frame(as.table(cor_mat))
names(cor_df) <- c("Var1", "Var2", "r")

p7 <- ggplot(cor_df, aes(x = Var1, y = Var2, fill = r)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", r)), size = 3.5) +
  scale_fill_gradient2(low = "#B2182B", mid = "white", high = "#2166AC",
                       midpoint = 0, limits = c(-1, 1),
                       name = "Correlation (r)") +
  scale_y_discrete(limits = rev) +
  coord_fixed() +
  labs(title    = "Weight, displacement and cylinders move together",
       subtitle = "Correlation matrix of mtcars variables",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid = element_blank())

ggsave("p7_heat.png", p7, width = 7.5, height = 6.4, dpi = 300)

# ---- w3_desc
summary(cars[, c("mpg", "wt", "hp", "qsec")])
sapply(cars[, c("mpg", "wt", "hp", "qsec")], sd)

# ---- w3_normality
shapiro.test(cars$mpg)
shapiro.test(cars$wt)
shapiro.test(cars$hp)

# Histogram with normal curve, and Q-Q plot
s1a <- ggplot(cars, aes(x = mpg)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 3, boundary = 10,
                 closed = "left", fill = "#264653", color = "white") +
  stat_function(fun = dnorm,
                args = list(mean = mean(cars$mpg), sd = sd(cars$mpg)),
                color = "#E76F51", linewidth = 1) +
  labs(x = "Miles per gallon", y = "Density") + theme_minimal(base_size = 13)

s1b <- ggplot(cars, aes(sample = mpg)) +
  stat_qq(color = "#264653") + stat_qq_line(color = "#E76F51") +
  labs(x = "Theoretical normal quantiles", y = "Sample quantiles (mpg)") +
  theme_minimal(base_size = 13)

# ---- w3_corr
num_vars <- c("cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb")
cor_mpg  <- sapply(mtcars[, num_vars], cor, y = mtcars$mpg)
cor_df   <- data.frame(variable = names(cor_mpg), r = cor_mpg) %>%
  mutate(variable = reorder(variable, r))

s2 <- ggplot(cor_df, aes(x = r, y = variable, fill = r > 0)) +
  geom_col(show.legend = FALSE) +
  geom_vline(xintercept = 0) +
  scale_fill_manual(values = c("TRUE" = "#2A9D8F", "FALSE" = "#E76F51")) +
  labs(x = "Pearson correlation with mpg", y = NULL) +
  theme_minimal(base_size = 13)

round(sort(cor_mpg), 2)

# ---- w3_h1
# H0: true correlation between weight and mpg is 0;  H1: it is not 0
cor.test(cars$wt, cars$mpg)                         # Pearson
cor.test(cars$wt, cars$mpg, method = "spearman",    # rank-based check
         exact = FALSE)

# ---- w3_h2
# H0: mean mpg is equal for automatic and manual cars;  H1: it is not
var.test(mpg ~ trans, data = cars)       # equal-variance check
t.test(mpg ~ trans, data = cars)         # Welch t-test (default)
wilcox.test(mpg ~ trans, data = cars,    # non-parametric check
            exact = FALSE)

# ---- w3_h3
# H0: mean mpg is equal across 4, 6 and 8 cylinder cars;  H1: at least one differs
anova_fit <- aov(mpg ~ cyl_f, data = cars)
summary(anova_fit)
TukeyHSD(anova_fit)

# Assumption checks and robust alternatives
leveneTest(mpg ~ cyl_f, data = cars)     # equal variances?
oneway.test(mpg ~ cyl_f, data = cars)    # Welch ANOVA (unequal variances)
kruskal.test(mpg ~ cyl_f, data = cars)   # non-parametric

s3 <- ggplot(cars, aes(x = cyl_f, y = mpg, fill = cyl_f)) +
  geom_boxplot(width = 0.5, alpha = 0.85, outlier.shape = NA, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.6) +
  scale_fill_manual(values = c("#2A9D8F", "#E9C46A", "#E76F51")) +
  labs(x = NULL, y = "Miles per gallon") + theme_minimal(base_size = 13)

# ---- w3_cv
# Candidate models, from simple to complex
model_formulas <- list(
  M1 = mpg ~ wt,
  M2 = mpg ~ wt + hp,
  M3 = mpg ~ wt + qsec + am,
  M4 = mpg ~ wt + hp + am,
  M5 = mpg ~ wt * hp,
  M6 = mpg ~ wt + cyl_f,
  M7 = mpg ~ cyl + disp + hp + drat + wt + qsec + am
)

# Fixed (non-random) fold labels 1,2,3,4,5,1,2,... so results are reproducible.
# With folds = 1:n the same function performs leave-one-out cross-validation.
folds <- rep(1:5, length.out = nrow(cars))

cv_predictions <- function(formula, data, folds, inv = identity) {
  pred <- numeric(nrow(data))
  for (k in sort(unique(folds))) {
    fit <- lm(formula, data = data[folds != k, ])          # train on other folds
    pred[folds == k] <- inv(predict(fit, newdata = data[folds == k, ]))
  }
  pred                                                       # out-of-fold predictions
}

rmse <- function(obs, pred) sqrt(mean((obs - pred)^2))
mae  <- function(obs, pred) mean(abs(obs - pred))
r2   <- function(obs, pred) 1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2)

results <- do.call(rbind, lapply(names(model_formulas), function(m) {
  f    <- model_formulas[[m]]
  fit  <- lm(f, data = cars)
  pred <- cv_predictions(f, cars, folds)
  loo  <- cv_predictions(f, cars, seq_len(nrow(cars)))
  data.frame(Model      = m,
             Adj_R2     = summary(fit)$adj.r.squared,
             AIC        = AIC(fit),
             CV5_RMSE   = rmse(cars$mpg, pred),
             LOOCV_RMSE = rmse(cars$mpg, loo))
}))
print(results, row.names = FALSE, digits = 4)

res_long <- data.frame(
  Model = rep(results$Model, 2),
  Type  = rep(c("5-fold CV RMSE", "Leave-one-out RMSE"), each = nrow(results)),
  RMSE  = c(results$CV5_RMSE, results$LOOCV_RMSE))

s4 <- ggplot(res_long, aes(x = Model, y = RMSE, fill = Type)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("#264653", "#E9C46A"), name = NULL) +
  labs(y = "Prediction error, RMSE (mpg)") + theme_minimal(base_size = 13)

# ---- w3_final
final <- lm(mpg ~ wt * hp, data = cars)
summary(final)
confint(final)

# Predict a new car: 3000 lbs and 150 hp
predict(final, newdata = data.frame(wt = 3.0, hp = 150), interval = "prediction")
predict(final, newdata = data.frame(wt = 3.0, hp = 150), interval = "confidence")

# Interaction plot: predicted mpg at three horsepower levels
grid <- expand.grid(wt = seq(1.5, 5.4, length.out = 100),
                    hp = quantile(cars$hp, c(0.10, 0.50, 0.90)))
grid$mpg_hat <- predict(final, newdata = grid)
s7 <- ggplot(grid, aes(x = wt, y = mpg_hat, color = factor(round(hp)))) +
  geom_line(linewidth = 1.2) +
  geom_point(data = cars, aes(x = wt, y = mpg), inherit.aes = FALSE,
             color = "grey60", alpha = 0.7) +
  labs(x = "Weight (1000 lbs)", y = "Predicted mpg", color = "Horsepower") +
  theme_minimal(base_size = 13)

# ---- w3_perf
cv_final <- cv_predictions(model_formulas$M5, cars, folds)
cv_base  <- cv_predictions(model_formulas$M1, cars, folds)
loo_fin  <- cv_predictions(model_formulas$M5, cars, seq_len(nrow(cars)))

perf <- data.frame(
  Model  = c("Baseline mpg ~ wt (5-fold)", "Final mpg ~ wt * hp (5-fold)",
             "Final mpg ~ wt * hp (leave-one-out)"),
  RMSE   = c(rmse(cars$mpg, cv_base), rmse(cars$mpg, cv_final), rmse(cars$mpg, loo_fin)),
  MAE    = c(mae(cars$mpg, cv_base),  mae(cars$mpg, cv_final),  mae(cars$mpg, loo_fin)),
  R2     = c(r2(cars$mpg, cv_base),   r2(cars$mpg, cv_final),   r2(cars$mpg, loo_fin)))
print(perf, row.names = FALSE, digits = 4)

# Largest out-of-fold errors
err <- cars$mpg - cv_final
head(data.frame(car = cars$car, actual = cars$mpg, predicted = cv_final,
                error = err)[order(-abs(err)), ], 4)

s6 <- ggplot(data.frame(pred = cv_final, actual = cars$mpg),
             aes(x = pred, y = actual)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#E76F51") +
  geom_point(size = 3, color = "#264653") +
  coord_equal(xlim = c(8, 36), ylim = c(8, 36)) +
  labs(x = "Predicted mpg (5-fold cross-validation)", y = "Actual mpg") +
  theme_minimal(base_size = 13)

# ---- w3_diag
png("f5_diag.png", width = 3000, height = 2500, res = 300)
par(mfrow = c(2, 2))
plot(final)                      # residuals vs fitted, Q-Q, scale-location, leverage
par(mfrow = c(1, 1))
dev.off()

shapiro.test(residuals(final))   # are residuals normal?
bptest(final)                    # constant variance? (Breusch-Pagan)
vif(final)                       # multicollinearity

# Centring the predictors removes most of the collinearity
cars$wt_c <- cars$wt - mean(cars$wt)
cars$hp_c <- cars$hp - mean(cars$hp)
final_c   <- lm(mpg ~ wt_c * hp_c, data = cars)
vif(final_c)

# Influential observations
cooks <- cooks.distance(final)
sort(cooks, decreasing = TRUE)[1:5]
names(which(cooks > 4 / nrow(cars)))

# ---- w3_improve
candidates <- list(
  list(name = "mpg ~ wt * hp (final)",   f = mpg ~ wt * hp,          inv = identity),
  list(name = "log(mpg) ~ wt * hp",      f = log(mpg) ~ wt * hp,     inv = exp),
  list(name = "mpg ~ wt * hp + qsec",    f = mpg ~ wt * hp + qsec,   inv = identity),
  list(name = "mpg ~ wt * hp + am",      f = mpg ~ wt * hp + am,     inv = identity),
  list(name = "mpg ~ wt * hp + disp",    f = mpg ~ wt * hp + disp,   inv = identity),
  list(name = "mpg ~ wt * hp + I(wt^2)", f = mpg ~ wt * hp + I(wt^2), inv = identity)
)

improve <- do.call(rbind, lapply(candidates, function(m) {
  data.frame(
    Model      = m$name,
    CV5_RMSE   = rmse(cars$mpg, cv_predictions(m$f, cars, folds, m$inv)),
    LOOCV_RMSE = rmse(cars$mpg, cv_predictions(m$f, cars, seq_len(nrow(cars)), m$inv)))
}))
print(improve, row.names = FALSE, digits = 4)

# ---- save_w3
# Save the Week 3 figures
ggsave("f1a_hist.png", s1a, width = 5, height = 4.5, dpi = 300)
ggsave("f1b_qq.png",   s1b, width = 5, height = 4.5, dpi = 300)
ggsave("f2_corr.png",  s2,  width = 8, height = 5,   dpi = 300)
ggsave("f3_anova.png", s3,  width = 7.5, height = 5, dpi = 300)
ggsave("f4_models.png", s4, width = 9, height = 5,   dpi = 300)
ggsave("f6_pred.png",  s6,  width = 7, height = 6.2, dpi = 300)
ggsave("f7_interaction.png", s7, width = 8, height = 5, dpi = 300)
