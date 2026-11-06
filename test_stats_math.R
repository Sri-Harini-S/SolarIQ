# ==============================================================================
# Statistical Verification Script for SolarIQ Project
# ==============================================================================

csv_file <- "c:/Users/sgsri/OneDrive/Pictures/Desktop/rlab/solar/solar_data.csv"
df <- read.csv(csv_file, stringsAsFactors = FALSE)
df$date <- as.Date(df$date)
df$daily_perf <- (df$actual_kwh / df$expected_kwh) * 100
df$diff_kwh <- df$actual_kwh - df$expected_kwh

cat("--- 1. DATA SUMMARY ---\n")
cat("Rows:", nrow(df), "\n")
cat("Installations:", unique(df$installation), "\n\n")

cat("--- 2. CORRELATIONS ---\n")
pear <- cor.test(df$expected_kwh, df$actual_kwh, method = "pearson")
cat("Pearson r:", round(pear$estimate, 4), "p-value:", format.pval(pear$p.value), "\n")
spear <- cor.test(df$expected_kwh, df$actual_kwh, method = "spearman", exact = FALSE)
cat("Spearman rho:", round(spear$estimate, 4), "p-value:", format.pval(spear$p.value), "\n")
kend <- cor.test(df$expected_kwh, df$actual_kwh, method = "kendall", exact = FALSE)
cat("Kendall tau:", round(kend$estimate, 4), "p-value:", format.pval(kend$p.value), "\n\n")

cat("--- 3. REGRESSION ---\n")
fit <- lm(actual_kwh ~ expected_kwh, data = df)
s <- summary(fit)
cat("Slope (beta1):", round(coef(fit)[2], 4), "Intercept (beta0):", round(coef(fit)[1], 4), "\n")
cat("R-squared:", round(s$r.squared, 4), "Adj R2:", round(s$adj.r.squared, 4), "RSE:", round(s$sigma, 4), "\n\n")

cat("--- 4. PAIRED TESTS ---\n")
t_paired <- t.test(df$actual_kwh, df$expected_kwh, paired = TRUE)
cat("Paired t-test t:", round(t_paired$statistic, 4), "p-val:", format.pval(t_paired$p.value), "mean diff:", round(t_paired$estimate, 4), "\n")
w_paired <- wilcox.test(df$actual_kwh, df$expected_kwh, paired = TRUE, exact = FALSE)
cat("Wilcoxon signed-rank V:", round(w_paired$statistic, 4), "p-val:", format.pval(w_paired$p.value), "\n\n")

cat("--- 5. ANOVA & KRUSKAL-WALLIS ---\n")
aov_fit <- aov(daily_perf ~ installation, data = df)
aov_s <- summary(aov_fit)
cat("ANOVA F:", round(aov_s[[1]]$`F value`[1], 4), "p-val:", format.pval(aov_s[[1]]$`Pr(>F)`[1]), "\n")
tukey <- TukeyHSD(aov_fit)
print(tukey$installation)

kw <- kruskal.test(daily_perf ~ installation, data = df)
cat("\nKruskal-Wallis Chi2:", round(kw$statistic, 4), "df:", kw$parameter, "p-val:", format.pval(kw$p.value), "\n\n")

cat("--- 6. NORMALITY TESTS ---\n")
shapiro_diff <- shapiro.test(df$diff_kwh[1:min(500, nrow(df))])
cat("Shapiro-Wilk W:", round(shapiro_diff$statistic, 4), "p-val:", format.pval(shapiro_diff$p.value), "\n")

cat("\nALL STATISTICAL CALCULATIONS SUCCEEDED PERFECTLY!\n")
