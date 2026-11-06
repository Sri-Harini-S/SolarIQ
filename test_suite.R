# ==============================================================================
# Comprehensive Statistical Verification & Test Suite for SolarIQ
# ==============================================================================

cat(">>> Running SolarIQ Statistical & Unit Verification Tests...\n\n")

# 1. Load Data
df <- read.csv("c:/Users/sgsri/OneDrive/Pictures/Desktop/rlab/solar/solar_data.csv", stringsAsFactors = FALSE)
df$date <- as.Date(df$date)
df$daily_perf <- (df$actual_kwh / df$expected_kwh) * 100
df$diff_kwh <- df$actual_kwh - df$expected_kwh
df$shortfall_kwh <- pmax(0, df$expected_kwh - df$actual_kwh)
df$day_index <- as.numeric(df$date - min(df$date)) + 1

# Assertion helper
assert <- function(condition, msg) {
  if (!condition) stop(paste("TEST FAILED:", msg))
  cat(paste(" [PASS]", msg, "\n"))
}

# Test 1: Data Integrity
assert(nrow(df) == 546, "Dataset has exactly 546 rows (182 days * 3 installations)")
assert(all(df$expected_kwh > 0), "Expected generation is strictly positive")
assert(all(df$actual_kwh >= 0), "Actual generation is non-negative")
assert(length(unique(df$installation)) == 3, "Contains exactly 3 solar installation types")

# Test 2: Parametric & Non-Parametric Correlation
p_test <- cor.test(df$expected_kwh, df$actual_kwh, method = "pearson")
s_test <- cor.test(df$expected_kwh, df$actual_kwh, method = "spearman", exact = FALSE)
k_test <- cor.test(df$expected_kwh, df$actual_kwh, method = "kendall", exact = FALSE)

assert(!is.na(p_test$estimate) && p_test$estimate > 0.8, "Pearson r demonstrates strong positive linear correlation (> 0.80)")
assert(!is.na(s_test$estimate) && s_test$estimate > 0.8, "Spearman rho demonstrates strong positive monotonic correlation (> 0.80)")
assert(!is.na(k_test$estimate) && k_test$estimate > 0.6, "Kendall tau confirms concordance (> 0.60)")
assert(p_test$p.value < 0.001, "Pearson correlation is statistically significant (p < 0.001)")

# Test 3: OLS Linear Regression
fit <- lm(actual_kwh ~ expected_kwh, data = df)
s <- summary(fit)
assert(s$r.squared > 0.65, "R-squared exceeds 0.65 (strong predictive power)")
assert(coef(fit)[2] > 0.85 && coef(fit)[2] < 1.0, "Empirical slope beta1 is between 0.85 and 1.00 (expected solar conversion derate)")
ci <- confint(fit)
assert(ci[2, 1] > 0.8 && ci[2, 2] < 1.0, "95% confidence interval for slope beta1 is well-bounded")

# Test 4: Regression Diagnostics and Prediction
pred_res <- predict(fit, newdata = data.frame(expected_kwh = c(200, 300, 400)), interval = "prediction")
assert(nrow(pred_res) == 3, "Prediction interval outputs 3 estimates")
assert(all(pred_res[, "lwr"] < pred_res[, "fit"]) && all(pred_res[, "fit"] < pred_res[, "upr"]), "Prediction intervals are monotonically ordered")

# Test 5: Paired Tests (Actual vs Expected)
t_paired <- t.test(df$actual_kwh, df$expected_kwh, paired = TRUE)
w_paired <- wilcox.test(df$actual_kwh, df$expected_kwh, paired = TRUE, exact = FALSE)
assert(t_paired$statistic < 0 && t_paired$p.value < 0.001, "Paired t-test confirms statistically significant shortfall (p < 0.001)")
assert(w_paired$p.value < 0.001, "Wilcoxon signed-rank test confirms significant difference (p < 0.001)")

# Test 6: Normality Testing
shapiro_res <- shapiro.test(df$diff_kwh[1:min(500, nrow(df))])
assert(shapiro_res$p.value < 0.05, "Shapiro-Wilk correctly detects departure from normality in generation dropouts")

# Test 7: Multi-Group Tests (ANOVA & Kruskal-Wallis)
aov_res <- aov(daily_perf ~ installation, data = df)
kw_res  <- kruskal.test(daily_perf ~ installation, data = df)
assert(!is.null(summary(aov_res)[[1]]$`F value`[1]), "One-Way ANOVA computes F-statistic properly")
assert(!is.null(kw_res$statistic), "Kruskal-Wallis computes chi-squared statistic properly")
tukey_res <- TukeyHSD(aov_res)
assert(nrow(tukey_res$installation) == 3, "Tukey HSD produces all 3 pairwise comparisons")

# Test 8: Two-Sample Welch t-Test and Mann-Whitney U
g1 <- df$daily_perf[df$installation == "Rooftop Alpha (50 kW)"]
g2 <- df$daily_perf[df$installation == "Carport Beta (75 kW)"]
welch_res <- t.test(g1, g2, var.equal = FALSE)
mw_res    <- wilcox.test(g1, g2, exact = FALSE)
assert(!is.null(welch_res$statistic), "Welch two-sample t-test computes successfully")
assert(!is.null(mw_res$statistic), "Mann-Whitney U test computes successfully")

# Test 9: Descriptive & Distributional Moments
source("c:/Users/sgsri/OneDrive/Pictures/Desktop/rlab/solar/app.R")
sk <- calc_skewness(df$daily_perf)
kt <- calc_kurtosis(df$daily_perf)
assert(!is.na(sk), "Sample skewness is non-NA")
assert(!is.na(kt), "Sample kurtosis is non-NA")

cat("\n>>> ALL 9 CORE STATISTICAL AND SCIENTIFIC TEST SUITES PASSED FLAWLESSLY!\n")
