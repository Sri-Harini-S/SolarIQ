# ☀️ SolarIQ — Renewable Energy Performance Analyzer & Statistical Computing Suite
### Interactive R Shiny Web Application for Solar PV Diagnostics & Advanced Inferential Statistics

Built with **R**, **Shiny**, **bslib (Bootstrap 5)**, **ggplot2**, **Plotly**, and **DT**.

---

## 🚀 Quick Start Guide

### 1. Prerequisites
Make sure R (>= 4.2.0) is installed. The required packages are:
```r
install.packages(c("shiny", "bslib", "ggplot2", "plotly", "dplyr", "DT"))
```

### 2. Launch the Application
Navigate to the project folder in R or RStudio and run:
```r
# From within R / RStudio:
shiny::runApp()

# Or from Terminal / PowerShell:
Rscript -e "shiny::runApp(launch.browser = TRUE)"
```
The app will launch in your default web browser at `http://127.0.0.1:7860` (or another available local port).

---

## 📊 Statistical Methods & Formulas Added

This upgraded version of **SolarIQ** elevates the operational solar dashboard into a comprehensive, university-grade statistical computing and data diagnostics platform:

| Statistical Domain | Parametric Method | Non-Parametric / Robust Method | Engineering Domain Interpretation |
| :--- | :--- | :--- | :--- |
| **Bivariate Association** | **Pearson Correlation ($r$)** with $t$-test & 95% Fisher CI | **Spearman Rank ($\rho$)** & **Kendall's Tau ($\tau$)** | Measures association between theoretical solar irradiance and delivered electrical kWh; Spearman handles cloudy weather drops. |
| **Linear Modeling** | **Ordinary Least Squares (OLS)** ($\hat{\beta}_1, \hat{\beta}_0, R^2, \text{RSE}, F$) | **LOESS Local Regression** / Spline smoothing | Slope $\hat{\beta}_1$ quantifies empirical PV plant efficiency (e.g. 0.906 = 90.6% energy delivery rate, ~9.4% derating). |
| **Regression Diagnostics** | Residuals vs Fitted, Normal Q-Q, Scale-Location | Residual Frequency Histogram & Density | Tests Gauss-Markov assumptions (Linearity, Homoscedasticity, Normality of Errors). |
| **Yield Prediction** | 95% Confidence Band (Mean Response) | 95% Prediction Band (Individual Day) | Allows estimating future actual kWh given planned irradiance target. |
| **Within-Subject Test** | **Paired Student's $t$-Test** ($t, df, p$) | **Wilcoxon Signed-Rank Test** ($V, p$) | Tests whether actual generation systematically underperforms the expected design model ($H_0: \mu_d = 0$). |
| **Distribution Normality** | **Shapiro-Wilk Test ($W$)** on discrepancies | Normal Q-Q plot & Empirical Density overlay | Demonstrates whether discrepancies depart from normality, justifying the Wilcoxon test. |
| **Multi-Group Comparison** | **One-Way ANOVA ($F$-Test)** with $SS, MS, F$ | **Kruskal-Wallis Rank Sum ($\chi^2$)** | Tests if performance ratio differs across Rooftop (50 kW), Carport (75 kW), and Ground Mount (100 kW). |
| **Post-Hoc Pairwise** | **Tukey's HSD** (Studentized Range $q$) | **Pairwise Wilcoxon** with Holm correction | Controls Family-Wise Error Rate (FWER) across pairwise installation comparisons. |
| **Two-Group Comparison** | **Welch's Two-Sample $t$-Test** (unequal var) | **Mann-Whitney $U$ / Wilcoxon Rank-Sum** | Head-to-head comparison of any two installation types with Cohen's $d$ effect size. |
| **Temporal Drift** | **OLS Linear Trend** ($\beta_{\text{time}}$ %/day & %/month) | **Spearman Rank Trend Test** | Tests whether arrays suffer from progressive dust/soiling accumulation or hardware degradation. |
| **Descriptive Diagnostics** | Mean, SD, Variance, SE, 95% CI of Mean | Median, IQR, MAD, Min, Max, Skewness, Kurtosis | 16-parameter summary matrix across all solar variables. |

---

## 📐 Mathematical Formulation Reference

### 1. Pearson vs. Spearman vs. Kendall Correlation
- **Pearson $r$ (Parametric)**:
  $$r = \frac{\sum_{i=1}^n (X_i - \bar{X})(Y_i - \bar{Y})}{\sqrt{\sum_{i=1}^n (X_i - \bar{X})^2 \sum_{i=1}^n (Y_i - \bar{Y})^2}}, \quad t = r \sqrt{\frac{n-2}{1-r^2}} \sim t_{n-2}$$
- **Spearman $\rho$ (Non-Parametric)**:
  $$\rho = 1 - \frac{6 \sum_{i=1}^n d_i^2}{n(n^2 - 1)}, \quad d_i = \text{rank}(X_i) - \text{rank}(Y_i)$$
- **Kendall's $\tau$ (Concordance)**:
  $$\tau = \frac{C - D}{\frac{1}{2} n (n - 1)}$$

### 2. Ordinary Least Squares (OLS) Linear Regression
$$\text{Actual}_i = \beta_0 + \beta_1 \cdot \text{Expected}_i + \epsilon_i, \quad \epsilon_i \sim \mathcal{N}(0, \sigma^2)$$
$$\hat{\beta}_1 = \frac{\sum (X_i - \bar{X})(Y_i - \bar{Y})}{\sum (X_i - \bar{X})^2}, \quad \hat{\beta}_0 = \bar{Y} - \hat{\beta}_1 \bar{X}$$
$$R^2 = 1 - \frac{SS_{\text{res}}}{SS_{\text{tot}}} = 1 - \frac{\sum (Y_i - \hat{Y}_i)^2}{\sum (Y_i - \bar{Y})^2}$$

### 3. Paired Student's $t$-Test vs. Wilcoxon Signed-Rank Test
- **Paired $t$-Test**:
  $$t = \frac{\bar{d} - 0}{s_d / \sqrt{n}}, \quad df = n - 1, \quad d_i = \text{Actual}_i - \text{Expected}_i$$
- **Wilcoxon Signed-Rank**:
  $$W = \sum_{i=1}^n [\text{rank}(|d_i|) \cdot \text{sgn}(d_i)], \quad V = \sum_{d_i > 0} \text{rank}(|d_i|)$$

### 4. One-Way ANOVA vs. Kruskal-Wallis Test
- **One-Way ANOVA**:
  $$F = \frac{MS_{\text{between}}}{MS_{\text{within}}} = \frac{SS_B / (k - 1)}{SS_W / (N - k)}$$
- **Kruskal-Wallis $H$-Test**:
  $$H = \frac{12}{N(N+1)} \sum_{j=1}^k \frac{R_j^2}{n_j} - 3(N+1) \sim \chi^2_{k-1}$$

---

## 🎓 University Viva / Oral Exam Q&A

### Q1: Why did you report both Pearson and Spearman correlation coefficients?
> **Answer**: Pearson's $r$ assumes bivariate normality and measures linear association. However, renewable energy generation is susceptible to non-linear dips caused by intermittent cloud cover, rain, and inverter power clipping. Spearman's $\rho$ evaluates monotonic relationships based on rank orders, providing a robust estimate that does not rely on normality assumptions and is immune to severe weather outliers.

### Q2: What does the regression slope $\hat{\beta}_1$ represent physically?
> **Answer**: The slope $\hat{\beta}_1$ represents the empirical solar power delivery factor per unit of expected irradiance. An ideal PV array without loss would have $\hat{\beta}_1 = 1.0$. In our dataset, $\hat{\beta}_1 \approx 0.906$, demonstrating that approximately $90.6\%$ of theoretical irradiance energy is converted into grid power, reflecting a $\approx 9.4\%$ combined loss due to thermal derating, inverter clipping, and AC/DC resistive dissipation.

### Q3: When should you prefer the Wilcoxon Signed-Rank test over the Paired $t$-test?
> **Answer**: The paired $t$-test requires that the differences ($d = \text{Actual} - \text{Expected}$) be normally distributed. When weather dropouts or inverter trips skew the error distribution, the Shapiro-Wilk test rejects normality ($p < 0.05$). Under such violations, the Wilcoxon Signed-Rank test is methodologically superior because it does not assume normality.

### Q4: How does Tukey's HSD test control the Family-Wise Error Rate (FWER)?
> **Answer**: Running multiple independent two-sample $t$-tests across 3 installations inflates the chance of a false positive (Type I error) to $1 - (1 - 0.05)^3 \approx 14.3\%$. Tukey's Honest Significant Difference test uses the Studentized Range distribution ($q$) to simultaneously evaluate all pairwise group differences while guaranteeing the overall Family-Wise Error Rate does not exceed $\alpha = 0.05$.

---

## 📁 Project Structure

```
solar/
├── app.R               # Complete Shiny Application with all statistical modules
├── solar_data.csv      # 546-row clean solar PV generation dataset (Jan - Jun 2024)
├── test_suite.R        # Automated test suite with 9 core assertions covering all statistical math
├── test_stats_math.R   # Mathematical verification script
└── README.md           # Project documentation, mathematical formulas & viva guide
```
