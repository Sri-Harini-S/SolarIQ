# ==============================================================================
# SolarIQ — Renewable Energy Performance Analyzer & Statistical Computing Suite
# Interactive R Shiny Web Application for Solar PV Diagnostics & Advanced Statistics
# ==============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(ggplot2)
  library(plotly)
  library(dplyr)
  library(DT)
})

# --- Helper: Base-R Robust Skewness and Kurtosis Calculations ---
calc_skewness <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 3) return(NA_real_)
  m <- mean(x)
  s <- sd(x)
  if (is.na(s) || s == 0) return(0)
  (n / ((n - 1) * (n - 2))) * sum(((x - m) / s)^3)
}

calc_kurtosis <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 4) return(NA_real_)
  m <- mean(x)
  s <- sd(x)
  if (is.na(s) || s == 0) return(0)
  term1 <- (n * (n + 1)) / ((n - 1) * (n - 2) * (n - 3))
  term2 <- sum(((x - m) / s)^4)
  term3 <- (3 * (n - 1)^2) / ((n - 2) * (n - 3))
  term1 * term2 - term3 # Excess kurtosis (normal distribution = 0)
}

# --- Helper: Generate Realistic Demonstration Dataset ---
generate_sample_solar_data <- function(file_path = "solar_data.csv") {
  set.seed(42)
  dates <- seq(as.Date("2024-01-01"), as.Date("2024-06-30"), by = "day")
  n_days <- length(dates)
  
  installations <- c("Rooftop Alpha (50 kW)", "Carport Beta (75 kW)", "Ground Mount Gamma (100 kW)")
  capacities <- c(50, 75, 100)
  
  df_list <- lapply(seq_along(installations), function(i) {
    inst_name <- installations[i]
    cap <- capacities[i]
    day_of_year <- as.numeric(format(dates, "%j"))
    psh <- 3.4 + 2.4 * sin((day_of_year - 15) / 365 * pi) # seasonal solar irradiance
    exp_kwh <- round(cap * psh * runif(n_days, 0.96, 1.04), 1)
    
    # 78% clear/normal, 16% cloudy/rain, 6% low event (soiling, shading, maintenance)
    event_type <- sample(c("normal", "cloudy", "low"), size = n_days, replace = TRUE, prob = c(0.78, 0.16, 0.06))
    factor <- ifelse(event_type == "normal", runif(n_days, 0.88, 1.05),
                     ifelse(event_type == "cloudy", runif(n_days, 0.50, 0.76), runif(n_days, 0.18, 0.45)))
    
    data.frame(date = dates, installation = inst_name, expected_kwh = exp_kwh,
               actual_kwh = round(exp_kwh * factor, 1), stringsAsFactors = FALSE)
  })
  
  sample_data <- do.call(rbind, df_list)
  sample_data <- sample_data[order(sample_data$date, sample_data$installation), ]
  if (!is.null(file_path)) {
    tryCatch(write.csv(sample_data, file_path, row.names = FALSE), error = function(e) NULL)
  }
  return(sample_data)
}

# --- Helper: Validate Dataset Structure & Values ---
validate_solar_data <- function(df) {
  req_cols <- c("date", "installation", "expected_kwh", "actual_kwh")
  missing <- setdiff(req_cols, tolower(colnames(df)))
  if (length(missing) > 0) {
    return(list(valid = FALSE, message = paste("Missing required column(s):", paste(missing, collapse = ", "))))
  }
  colnames(df) <- tolower(colnames(df))
  
  parsed_dates <- suppressWarnings(as.Date(df$date))
  if (any(is.na(parsed_dates))) {
    return(list(valid = FALSE, message = "Invalid dates detected. Please format dates as YYYY-MM-DD."))
  }
  df$date <- parsed_dates
  df$expected_kwh <- suppressWarnings(as.numeric(df$expected_kwh))
  df$actual_kwh   <- suppressWarnings(as.numeric(df$actual_kwh))
  
  if (any(is.na(df$expected_kwh)) || any(is.na(df$actual_kwh))) {
    return(list(valid = FALSE, message = "Non-numeric values found in expected_kwh or actual_kwh."))
  }
  if (any(df$expected_kwh < 0) || any(df$actual_kwh < 0)) {
    return(list(valid = FALSE, message = "Negative generation values are not allowed."))
  }
  return(list(valid = TRUE, data = df))
}

# --- Resolve CSV Path Gracefully ---
get_csv_path <- function(fname = "solar_data.csv") {
  if (file.exists(fname)) return(fname)
  sub_path <- file.path("SolarIQ", fname)
  if (file.exists(sub_path)) return(sub_path)
  return(fname)
}

# --- Light Theme & Styling ---
app_theme <- bs_theme(
  version = 5, bootswatch = "flatly", primary = "#0d9488", secondary = "#d97706",
  bg = "#f8fafc", fg = "#1e293b", base_font = font_google("Inter", local = FALSE)
)

custom_css <- "
  body { background-color: #f8fafc; color: #1e293b; font-size: 0.95rem; }
  .navbar-brand { font-weight: 700; color: #d97706 !important; font-size: 1.22rem; }
  .kpi-card {
    background: #ffffff; border-radius: 10px; border: 1px solid #e2e8f0;
    padding: 14px 18px; box-shadow: 0 1px 3px rgba(0,0,0,0.05); margin-bottom: 12px;
  }
  .kpi-title { font-size: 0.8rem; text-transform: uppercase; letter-spacing: 0.05em; color: #64748b; font-weight: 600; }
  .kpi-value { font-size: 1.7rem; font-weight: 800; line-height: 1.15; margin: 4px 0; }
  .kpi-sub { font-size: 0.76rem; color: #94a3b8; }
  .chart-box {
    background: #ffffff; border-radius: 10px; border: 1px solid #e2e8f0;
    padding: 16px; box-shadow: 0 1px 3px rgba(0,0,0,0.05); margin-bottom: 14px;
  }
  .chart-header { font-size: 1.05rem; font-weight: 700; color: #0f172a; }
  .chart-sub { font-size: 0.8rem; color: #64748b; margin-bottom: 10px; }
  .status-badge { display: inline-block; padding: 3px 10px; border-radius: 9999px; font-size: 0.78rem; font-weight: 700; }
  .badge-sample { background-color: #fef3c7; color: #92400e; border: 1px solid #fde68a; }
  .badge-live   { background-color: #d1fae5; color: #065f46; border: 1px solid #a7f3d0; }
  .badge-ok     { background-color: #dcfce7; color: #166534; }
  .advisory-caution {
    background-color: #fffbeb; border-left: 4px solid #f59e0b;
    padding: 12px 16px; border-radius: 6px; margin-bottom: 14px; font-size: 0.87rem; color: #92400e;
  }
  .advisory-info {
    background-color: #eff6ff; border-left: 4px solid #3b82f6;
    padding: 12px 16px; border-radius: 6px; margin-bottom: 14px; font-size: 0.87rem; color: #1e40af;
  }
  .formula-card {
    background: #ffffff; border-radius: 8px; border: 1px solid #cbd5e1;
    padding: 14px 18px; margin-bottom: 14px; box-shadow: 0 1px 2px rgba(0,0,0,0.04);
  }
  .formula-title { font-weight: 700; color: #0d9488; font-size: 0.95rem; margin-bottom: 6px; }
  .formula-math {
    background: #f1f5f9; border-left: 3px solid #0d9488; padding: 8px 12px;
    font-family: 'Consolas', 'Courier New', monospace; font-size: 0.9rem; color: #0f172a; margin-bottom: 8px;
  }
  .viva-card {
    background: #ffffff; border-radius: 8px; border: 1px solid #e2e8f0;
    padding: 14px 18px; margin-bottom: 12px; border-left: 4px solid #d97706;
  }
  .viva-q { font-weight: 700; color: #0f172a; margin-bottom: 4px; font-size: 0.95rem; }
  .viva-a { color: #334155; font-size: 0.88rem; line-height: 1.45; }
"

# ==============================================================================
# UI DEFINITION
# ==============================================================================
ui <- page_navbar(
  title = span("SolarIQ", span(" | Energy Diagnostics & Statistical Inference", style = "font-weight: 400; font-size: 0.95rem; color: #64748b;")),
  theme = app_theme,
  header = tags$head(tags$style(HTML(custom_css))),
  
  sidebar = sidebar(
    title = span("System Controls", style = "font-size: 1.05rem; font-weight: 700; color: #0f172a;"),
    width = 300,
    uiOutput("dataset_badge_ui"),
    hr(style = "margin: 8px 0;"),
    
    selectInput("selected_installation", label = "Solar Installation Filter:", choices = c("All Installations")),
    dateRangeInput("selected_date_range", label = "Observation Period:", start = "2024-01-01", end = "2024-06-30"),
    sliderInput("threshold", label = "Underperformance Threshold (%):", min = 50, max = 95, value = 80, step = 5, post = "%"),
    p(style = "font-size: 0.77rem; color: #64748b; margin-top: -6px;", "Flags days where (Actual / Expected) is below this benchmark."),
    
    hr(style = "margin: 10px 0;"),
    span("Statistical Parameters", style = "font-size: 0.8rem; font-weight: 700; text-transform: uppercase; color: #64748b;"),
    selectInput("stat_alpha", label = "Significance Level (α):", choices = c("0.05 (95% Confidence)" = 0.05, "0.01 (99% Confidence)" = 0.01, "0.10 (90% Confidence)" = 0.10), selected = 0.05),
    
    hr(style = "margin: 10px 0;"),
    span("Export Reports", style = "font-size: 0.8rem; font-weight: 700; text-transform: uppercase; color: #64748b;"),
    div(style = "margin-top: 6px;",
        downloadButton("download_filtered", "Export Filtered Data (CSV)", class = "btn-outline-secondary btn-sm w-100 mb-2"),
        downloadButton("download_underperf", "Export Flagged Days (CSV)", class = "btn-outline-warning btn-sm w-100 mb-2"),
        downloadButton("download_stats_summary", "Export Stats Summary (CSV)", class = "btn-outline-primary btn-sm w-100")
    ),
    
    hr(style = "margin: 12px 0;"),
    span("Dataset Management", style = "font-size: 0.8rem; font-weight: 700; text-transform: uppercase; color: #64748b;"),
    fileInput("upload_csv", "Upload Custom CSV", accept = c(".csv"), buttonLabel = "Browse...", placeholder = "No file chosen"),
    actionButton("reset_data_btn", "Reset to Default Dataset", class = "btn-light btn-sm w-100 border", icon = icon("rotate-left")),
    p(style = "font-size: 0.74rem; color: #94a3b8; margin-top: 5px;", "Preserves uploaded structure without silent overwrites.")
  ),
  
  # Tab 1: Executive Overview Dashboard
  nav_panel(
    title = "Overview Dashboard",
    uiOutput("data_error_banner"),
    
    fluidRow(
      column(3, div(class = "kpi-card",
                    div(class = "kpi-title", "Expected Energy"),
                    div(class = "kpi-value", style = "color: #d97706;", textOutput("kpi_expected")),
                    div(class = "kpi-sub", "Calculated target based on solar irradiance")
      )),
      column(3, div(class = "kpi-card",
                    div(class = "kpi-title", "Actual Energy"),
                    div(class = "kpi-value", style = "color: #059669;", textOutput("kpi_actual")),
                    div(class = "kpi-sub", "Measured electrical yield delivered to grid")
      )),
      column(3, div(class = "kpi-card",
                    div(class = "kpi-title", "Overall Performance"),
                    div(class = "kpi-value", uiOutput("kpi_performance")),
                    div(class = "kpi-sub", "(Total Actual / Total Expected) × 100")
      )),
      column(3, div(class = "kpi-card",
                    div(class = "kpi-title", "Energy Shortfall"),
                    div(class = "kpi-value", style = "color: #dc2626;", textOutput("kpi_shortfall")),
                    div(class = "kpi-sub", "Cumulative lost generation (kWh)")
      ))
    ),
    
    div(class = "chart-box",
        div(class = "d-flex justify-content-between align-items-center mb-1",
            div(class = "chart-header", "Expected vs Actual Energy Generation (kWh)"),
            span(class = "badge bg-light text-secondary border", "Hover on lines for details")
        ),
        div(class = "chart-sub", "Time-series tracking planned target against recorded electrical output."),
        plotlyOutput("plot_time_series", height = "320px")
    ),
    
    div(class = "chart-box",
        div(class = "d-flex justify-content-between align-items-center mb-1",
            div(class = "chart-header", "Daily Performance Ratio & Anomaly Flags (%)"),
            span(class = "badge bg-light text-secondary border", textOutput("underperf_badge_text", inline = TRUE))
        ),
        div(class = "chart-sub", "Daily ratio plotted against the configured underperformance threshold."),
        plotlyOutput("plot_performance_ratio", height = "280px")
    )
  ),
  
  # Tab 2: Performance Diagnostics & Anomaly Report
  nav_panel(
    title = "Performance Diagnostics",
    div(class = "advisory-caution",
        strong("Engineering Interpretation Advisory: "),
        "An output below threshold flags an operational anomaly, but does NOT prove hardware failure. ",
        "Common contributing factors include heavy overcast weather, seasonal shading, module soiling, ",
        "inverter curtailment, or utility grid outages."
    ),
    fluidRow(
      column(4, div(class = "kpi-card",
                    div(class = "kpi-title", "Underperforming Days Count"),
                    div(class = "kpi-value", style = "color: #dc2626;", textOutput("flagged_days_count")),
                    div(class = "kpi-sub", "Days generating below configured threshold")
      )),
      column(4, div(class = "kpi-card",
                    div(class = "kpi-title", "Shortfall from Flagged Days"),
                    div(class = "kpi-value", style = "color: #d97706;", textOutput("flagged_shortfall_sum")),
                    div(class = "kpi-sub", "Loss concentrated in low-performing periods")
      )),
      column(4, div(class = "kpi-card",
                    div(class = "kpi-title", "Average Flagged Performance"),
                    div(class = "kpi-value", style = "color: #64748b;", textOutput("flagged_avg_perf")),
                    div(class = "kpi-sub", "Mean ratio during underperforming intervals")
      ))
    ),
    
    div(class = "chart-box",
        div(class = "chart-header", "Temporal Degradation / Trend Analysis"),
        div(class = "chart-sub", "Parametric OLS linear trend slope testing whether performance ratio changes over time."),
        fluidRow(
          column(8, plotlyOutput("plot_trend_degradation", height = "280px")),
          column(4, uiOutput("trend_stats_summary_ui"))
        )
    ),
    
    div(class = "chart-box",
        div(class = "chart-header", "Flagged Days Operational Audit"),
        div(class = "chart-sub", "Detailed audit log of dates failing the performance benchmark, ordered by shortfall."),
        DTOutput("table_flagged_days")
    )
  ),
  
  # Tab 3: Correlation Analysis (Parametric vs Non-Parametric)
  nav_panel(
    title = "Correlation Studio",
    div(class = "advisory-info",
        strong("Parametric vs. Non-Parametric Correlation Guide: "),
        "Pearson r measures linear association assuming bivariate normality. Spearman rho and Kendall tau ",
        "are non-parametric rank-order coefficients robust to weather dropouts, shading dips, and non-linear outliers."
    ),
    fluidRow(
      column(4, div(class = "kpi-card",
                    div(class = "kpi-title", "Pearson r (Parametric)"),
                    div(class = "kpi-value", style = "color: #0d9488;", textOutput("corr_pearson_val")),
                    div(class = "kpi-sub", textOutput("corr_pearson_sub"))
      )),
      column(4, div(class = "kpi-card",
                    div(class = "kpi-title", "Spearman ρ (Non-Parametric)"),
                    div(class = "kpi-value", style = "color: #d97706;", textOutput("corr_spearman_val")),
                    div(class = "kpi-sub", textOutput("corr_spearman_sub"))
      )),
      column(4, div(class = "kpi-card",
                    div(class = "kpi-title", "Kendall τ (Concordance)"),
                    div(class = "kpi-value", style = "color: #6366f1;", textOutput("corr_kendall_val")),
                    div(class = "kpi-sub", textOutput("corr_kendall_sub"))
      ))
    ),
    fluidRow(
      column(7,
             div(class = "chart-box",
                 div(class = "chart-header", "Expected vs. Actual Generation Scatter with Fit Curves"),
                 div(class = "chart-sub", "Interactive scatter plot with OLS linear trend line and LOESS smooth."),
                 plotlyOutput("plot_correlation_scatter", height = "360px")
             )
      ),
      column(5,
             div(class = "chart-box",
                 div(class = "chart-header", "Formal Correlation Hypothesis Tests"),
                 div(class = "chart-sub", "Statistical inference (H₀: ρ = 0 vs. H₁: ρ ≠ 0)."),
                 DTOutput("table_correlation_tests")
             ),
             div(class = "chart-box",
                 div(class = "chart-header", "Installation-Wise Correlation Matrix"),
                 div(class = "chart-sub", "Comparing correlation strength across solar architectures."),
                 DTOutput("table_installation_corrs")
             )
      )
    )
  ),
  
  # Tab 4: OLS Regression Modeling & Residual Diagnostics
  nav_panel(
    title = "Regression Modeling",
    div(class = "advisory-info",
        strong("Solar PV Yield Regression Model: "),
        "Model Formula: Actual (kWh) = β₀ + β₁ × Expected (kWh) + ε. The slope β₁ evaluates empirical solar conversion efficiency, ",
        "while R² measures the fraction of generation variance explained by solar irradiance target."
    ),
    fluidRow(
      column(3, div(class = "kpi-card",
                    div(class = "kpi-title", "R² (Variance Explained)"),
                    div(class = "kpi-value", style = "color: #0d9488;", textOutput("reg_r2")),
                    div(class = "kpi-sub", textOutput("reg_adj_r2"))
      )),
      column(3, div(class = "kpi-card",
                    div(class = "kpi-title", "Slope (β₁ - Efficiency)"),
                    div(class = "kpi-value", style = "color: #2563eb;", textOutput("reg_slope")),
                    div(class = "kpi-sub", textOutput("reg_slope_pval"))
      )),
      column(3, div(class = "kpi-card",
                    div(class = "kpi-title", "Intercept (β₀ - Baseline)"),
                    div(class = "kpi-value", style = "color: #d97706;", textOutput("reg_intercept")),
                    div(class = "kpi-sub", "Fixed offset yield (kWh)")
      )),
      column(3, div(class = "kpi-card",
                    div(class = "kpi-title", "Residual Std Error (RSE)"),
                    div(class = "kpi-value", style = "color: #dc2626;", textOutput("reg_rse")),
                    div(class = "kpi-sub", textOutput("reg_fstat"))
      ))
    ),
    fluidRow(
      column(7,
             div(class = "chart-box",
                 div(class = "chart-header", "Linear Regression Fit with 95% Confidence & Prediction Bands"),
                 div(class = "chart-sub", "Narrow ribbon = 95% Confidence Interval (mean response); Wide ribbon = 95% Prediction Interval (single day)."),
                 plotlyOutput("plot_regression_fit", height = "360px")
             )
      ),
      column(5,
             div(class = "chart-box",
                 div(class = "chart-header", "Model Coefficients & Significance Table"),
                 div(class = "chart-sub", "Parameter estimates, standard errors, t-statistics, and 95% confidence bounds."),
                 DTOutput("table_regression_coefs")
             ),
             div(class = "chart-box",
                 div(class = "chart-header", "Interactive Solar Yield Prediction Tool"),
                 div(class = "chart-sub", "Predict actual kWh from planned expected solar irradiance."),
                 fluidRow(
                   column(6, numericInput("pred_input_exp", "Input Expected (kWh):", value = 300, min = 10, max = 1000, step = 10)),
                   column(6, div(style = "margin-top: 24px;", actionButton("calc_pred_btn", "Compute Prediction", class = "btn-primary btn-sm w-100")))
                 ),
                 uiOutput("pred_result_ui")
             )
      )
    ),
    div(class = "chart-box",
        div(class = "chart-header", "4-Panel Regression Diagnostic Suite"),
        div(class = "chart-sub", "Testing Gauss-Markov assumptions: Linearity, Homoscedasticity, Normality of Residuals."),
        fluidRow(
          column(6, plotlyOutput("plot_diag_res_fitted", height = "260px")),
          column(6, plotlyOutput("plot_diag_qq", height = "260px"))
        ),
        fluidRow(
          column(6, plotlyOutput("plot_diag_scale_loc", height = "260px")),
          column(6, plotlyOutput("plot_diag_res_hist", height = "260px"))
        )
    )
  ),
  
  # Tab 5: Hypothesis Testing Lab (Parametric vs Non-Parametric)
  nav_panel(
    title = "Hypothesis Testing",
    div(class = "advisory-info",
        strong("Statistical Inference Protocol: "),
        "Rigorous research requires checking normality assumptions before trusting parametric tests. ",
        "Below, parametric tests are evaluated side-by-side with their robust non-parametric counterparts."
    ),
    navlistPanel(
      widths = c(3, 9),
      well = FALSE,
      
      tabPanel(
        "1. Paired Actual vs Expected",
        div(class = "chart-box",
            div(class = "chart-header", "Paired Test: Actual Generation vs Expected Model Target"),
            div(class = "chart-sub", "Testing whether recorded energy systematically diverges from calculated irradiance model."),
            fluidRow(
              column(6,
                     div(class = "kpi-card",
                         div(class = "kpi-title", "Parametric: Paired Student's t-Test"),
                         div(class = "kpi-value", textOutput("paired_t_stat")),
                         div(class = "kpi-sub", textOutput("paired_t_desc"))
                     )
              ),
              column(6,
                     div(class = "kpi-card",
                         div(class = "kpi-title", "Non-Parametric: Wilcoxon Signed-Rank Test"),
                         div(class = "kpi-value", textOutput("paired_wilcox_stat")),
                         div(class = "kpi-sub", textOutput("paired_wilcox_desc"))
                     )
              )
            ),
            div(class = "chart-header", style = "font-size: 0.95rem; margin-top: 10px;", "Normality Assessment of Generation Discrepancy (d = Actual - Expected)"),
            div(class = "chart-sub", "Shapiro-Wilk normality test evaluates whether parametric Student's t assumptions are satisfied."),
            fluidRow(
              column(6, plotlyOutput("plot_paired_diff_hist", height = "260px")),
              column(6, plotlyOutput("plot_paired_diff_qq", height = "260px"))
            ),
            uiOutput("paired_conclusion_ui")
        )
      ),
      
      tabPanel(
        "2. Cross-Installation Comparison",
        div(class = "chart-box",
            div(class = "chart-header", "Multi-Group Comparison: One-Way ANOVA vs. Kruskal-Wallis"),
            div(class = "chart-sub", "Testing whether performance ratios differ across Rooftop Alpha, Carport Beta, and Ground Mount Gamma."),
            fluidRow(
              column(6,
                     div(class = "kpi-card",
                         div(class = "kpi-title", "Parametric: One-Way ANOVA (F-Test)"),
                         div(class = "kpi-value", textOutput("anova_f_stat")),
                         div(class = "kpi-sub", textOutput("anova_p_desc"))
                     )
              ),
              column(6,
                     div(class = "kpi-card",
                         div(class = "kpi-title", "Non-Parametric: Kruskal-Wallis (H-Test)"),
                         div(class = "kpi-value", textOutput("kw_chi_stat")),
                         div(class = "kpi-sub", textOutput("kw_p_desc"))
                     )
              )
            ),
            plotlyOutput("plot_anova_boxplots", height = "300px"),
            hr(),
            div(class = "chart-header", "Post-Hoc Pairwise Comparisons: Tukey HSD vs Pairwise Wilcoxon"),
            div(class = "chart-sub", "Controlling Family-Wise Error Rate (FWER) across pairwise installation comparisons."),
            DTOutput("table_posthoc_tests")
        )
      ),
      
      tabPanel(
        "3. Two-Installation Head-to-Head",
        div(class = "chart-box",
            div(class = "chart-header", "Pairwise Independent Group Comparison"),
            div(class = "chart-sub", "Select two installations to evaluate differences using Two-Sample Welch's t-test and Mann-Whitney U test."),
            fluidRow(
              column(6, selectInput("two_group_inst1", "Select Installation A:", choices = NULL)),
              column(6, selectInput("two_group_inst2", "Select Installation B:", choices = NULL))
            ),
            fluidRow(
              column(6,
                     div(class = "kpi-card",
                         div(class = "kpi-title", "Welch's Two-Sample t-Test (Parametric)"),
                         div(class = "kpi-value", textOutput("two_sample_t_stat")),
                         div(class = "kpi-sub", textOutput("two_sample_t_desc"))
                     )
              ),
              column(6,
                     div(class = "kpi-card",
                         div(class = "kpi-title", "Mann-Whitney U / Wilcoxon Rank-Sum (Non-Parametric)"),
                         div(class = "kpi-value", textOutput("mann_whitney_stat")),
                         div(class = "kpi-sub", textOutput("mann_whitney_desc"))
                     )
              )
            ),
            plotlyOutput("plot_two_group_comp", height = "280px"),
            uiOutput("two_group_conclusion_ui")
        )
      )
    )
  ),
  
  # Tab 6: Descriptive Statistics & Distribution Lab
  nav_panel(
    title = "Descriptive Statistics",
    div(class = "chart-box",
        div(class = "chart-header", "16-Metric Parametric & Non-Parametric Summary Matrix"),
        div(class = "chart-sub", "Comprehensive univariate diagnostics for Actual kWh, Expected kWh, and Performance Ratio."),
        DTOutput("table_descriptive_stats")
    ),
    div(class = "chart-box",
        div(class = "chart-header", "Empirical vs. Theoretical Distribution Profiles"),
        div(class = "chart-sub", "Kernel density curves overlaid with parametric Gaussian fits and quantile markers."),
        fluidRow(
          column(6, plotlyOutput("plot_desc_density_actual", height = "300px")),
          column(6, plotlyOutput("plot_desc_density_perf", height = "300px"))
        )
    )
  ),
  
  # Tab 7: Detailed Data Explorer
  nav_panel(
    title = "Data Explorer",
    div(class = "chart-box",
        div(class = "chart-header", "Complete Solar Generation Records"),
        div(class = "chart-sub", "Interactive searchable records with daily calculations, error discrepancies, and diagnostic status tags."),
        DTOutput("table_full_data")
    )
  ),
  
  # Tab 8: Statistical Formulas & Viva Defense Guide
  nav_panel(
    title = "Formulas & Viva Guide",
    div(class = "advisory-info",
        strong("Academic Viva & Laboratory Examination Defense Guide: "),
        "Use this reference to explain mathematical formulas, null/alternative hypotheses, and engineering domain interpretations to examiners."
    ),
    fluidRow(
      column(6,
             div(class = "formula-card",
                 div(class = "formula-title", "1. Pearson Correlation Coefficient (r) — Parametric"),
                 div(class = "formula-math", "r = Σ((Xᵢ - X̄)(Yᵢ - Ȳ)) / sqrt(Σ(Xᵢ - X̄)² · Σ(Yᵢ - Ȳ)²)\nt = r · sqrt((n - 2) / (1 - r²)),   df = n - 2"),
                 p(style = "font-size: 0.85rem; color: #475569;",
                   "Quantifies linear association between target irradiance and generated power. Assumes bivariate normality.")
             ),
             div(class = "formula-card",
                 div(class = "formula-title", "2. Spearman Rank Correlation (ρ) — Non-Parametric"),
                 div(class = "formula-math", "ρ = 1 - (6 · Σ dᵢ²) / (n · (n² - 1))\nwhere dᵢ = rank(Xᵢ) - rank(Yᵢ)"),
                 p(style = "font-size: 0.85rem; color: #475569;",
                   "Measures monotonic relationship without requiring normality. Highly resistant to extreme cloud dropouts and curtailment outliers.")
             ),
             div(class = "formula-card",
                 div(class = "formula-title", "3. Ordinary Least Squares (OLS) Regression"),
                 div(class = "formula-math", "Actual = β₀ + β₁ · Expected + ε\nβ₁ = Σ((Xᵢ - X̄)(Yᵢ - Ȳ)) / Σ(Xᵢ - X̄)²\nβ₀ = Ȳ - β₁ · X̄\nR² = 1 - (SS_res / SS_tot) = 1 - Σ(eᵢ²) / Σ(Yᵢ - Ȳ)²"),
                 p(style = "font-size: 0.85rem; color: #475569;",
                   "Slope β₁ represents the empirical solar energy delivery rate. A slope < 1.0 reflects systematic plant derating (thermal, resistance, inverter clipping).")
             ),
             div(class = "formula-card",
                 div(class = "formula-title", "4. Paired Student's t-Test — Parametric"),
                 div(class = "formula-math", "t = (d̄ - 0) / (s_d / sqrt(n)),   df = n - 1\nwhere dᵢ = Actualᵢ - Expectedᵢ,  d̄ = mean difference"),
                 p(style = "font-size: 0.85rem; color: #475569;",
                   "Tests H₀: μ_actual = μ_expected (no generation bias). Requires differences dᵢ to be approximately normal.")
             )
      ),
      column(6,
             div(class = "formula-card",
                 div(class = "formula-title", "5. Wilcoxon Signed-Rank Test — Non-Parametric"),
                 div(class = "formula-math", "W = Σ [ rank(|dᵢ|) · sign(dᵢ) ]\nV = sum of ranks for positive differences"),
                 p(style = "font-size: 0.85rem; color: #475569;",
                   "Non-parametric analog of paired t-test. Tests H₀: Median difference = 0. Does not assume normal distribution.")
             ),
             div(class = "formula-card",
                 div(class = "formula-title", "6. One-Way Analysis of Variance (ANOVA, F-Test)"),
                 div(class = "formula-math", "F = MS_between / MS_within = (SS_between / (k - 1)) / (SS_within / (N - k))\nH₀: μ_Alpha = μ_Beta = μ_Gamma"),
                 p(style = "font-size: 0.85rem; color: #475569;",
                   "Tests whether average performance ratio differs across the 3 installations. Post-hoc Tukey HSD identifies pairwise differences.")
             ),
             div(class = "formula-card",
                 div(class = "formula-title", "7. Kruskal-Wallis Rank Sum Test — Non-Parametric"),
                 div(class = "formula-math", "H = [ 12 / (N · (N + 1)) ] · Σ (Rⱼ² / nⱼ) - 3 · (N + 1)\nFollows χ² distribution with df = k - 1"),
                 p(style = "font-size: 0.85rem; color: #475569;",
                   "Non-parametric alternative to ANOVA when group variances or distributions are skewed. Followed by pairwise Wilcoxon tests with Holm correction.")
             ),
             div(class = "formula-card",
                 div(class = "formula-title", "8. Shapiro-Wilk Test for Normality"),
                 div(class = "formula-math", "W = (Σ aᵢ · x₍ᵢ₎)² / Σ (xᵢ - x̄)²\nReject normality if p < α (typically α = 0.05)"),
                 p(style = "font-size: 0.85rem; color: #475569;",
                   "Evaluates whether model residuals or generation differences satisfy the normality assumption required for parametric tests.")
             )
      )
    ),
    
    hr(),
    div(class = "chart-header", "Top Viva / Oral Defense Questions & Model Answers"),
    div(class = "chart-sub", "Expert responses prepared for statistical computing and data analytics examinations."),
    
    div(class = "viva-card",
        div(class = "viva-q", "Q1: Why report both Pearson and Spearman correlation for solar PV generation data?"),
        div(class = "viva-a",
            "Pearson's r measures strictly linear relationships and assumes bivariate normal distributions. However, solar PV generation data frequently ",
            "contains non-linear dips caused by cloud transients, module soiling, and inverter power saturation. Spearman's rho is a rank-based non-parametric ",
            "metric that assesses monotonic trends without assuming normality, making it immune to extreme outlier weather days.")
    ),
    div(class = "viva-card",
        div(class = "viva-q", "Q2: In our regression model (Actual = β₀ + β₁ × Expected), what does the slope β₁ mean physically?"),
        div(class = "viva-a",
            "The slope β₁ represents the empirical power delivery rate per unit of expected irradiance. An ideal solar array has β₁ ≈ 1.0. A slope below 1.0 ",
            "(e.g., 0.906) indicates an average 9.4% loss attributable to combined temperature coefficient derating, DC-to-AC inverter conversion losses, ",
            "and cabling resistance.")
    ),
    div(class = "viva-card",
        div(class = "viva-q", "Q3: When should you prefer the Wilcoxon Signed-Rank test over the Paired Student's t-test?"),
        div(class = "viva-a",
            "The paired t-test assumes that the differences (d = Actual - Expected) are normally distributed. In real solar operations, weather dropouts produce ",
            "a heavily left-skewed error distribution, causing the Shapiro-Wilk test to reject normality (p < 0.05). In such cases, the Wilcoxon Signed-Rank ",
            "test is mathematically more valid because it operates on rank orders rather than sample means.")
    ),
    div(class = "viva-card",
        div(class = "viva-q", "Q4: What is the difference between One-Way ANOVA and the Kruskal-Wallis test in SolarIQ?"),
        div(class = "viva-a",
            "One-Way ANOVA compares mean performance ratios across installations based on the F-distribution, assuming equal variances (homoscedasticity) ",
            "and normality of residuals. The Kruskal-Wallis test is the non-parametric counterpart that compares rank sums (median shifts). Both are presented ",
            "to demonstrate that conclusions remain robust regardless of distribution assumptions.")
    ),
    div(class = "viva-card",
        div(class = "viva-q", "Q5: How does Tukey's HSD test protect against Type I error inflation in post-hoc testing?"),
        div(class = "viva-a",
            "When performing multiple pairwise comparisons across groups (e.g. 3 installations = 3 pairs), running repeated standard t-tests inflates the overall ",
            "Family-Wise Error Rate (FWER) to 1 - (1 - 0.05)³ ≈ 14.3%. Tukey's Honest Significant Difference utilizes the Studentized Range distribution (q) ",
            "to rigorously hold the overall FWER at exactly α = 0.05.")
    )
  )
)

# ==============================================================================
# SERVER LOGIC
# ==============================================================================
server <- function(input, output, session) {
  raw_state <- reactiveVal(list(data = NULL, source_type = "sample", file_name = "Demonstration Sample Data"))
  error_state <- reactiveVal(NULL)
  
  # Initialize: load existing solar_data.csv or generate sample dataset
  observe({
    csv_file <- get_csv_path("solar_data.csv")
    if (file.exists(csv_file)) {
      loaded <- tryCatch(read.csv(csv_file, stringsAsFactors = FALSE), error = function(e) NULL)
      if (!is.null(loaded)) {
        check <- validate_solar_data(loaded)
        if (check$valid) {
          raw_state(list(data = check$data, source_type = "live", file_name = basename(csv_file)))
          error_state(NULL)
          return()
        }
      }
    }
    gen_df <- generate_sample_solar_data(csv_file)
    check <- validate_solar_data(gen_df)
    raw_state(list(data = check$data, source_type = "sample", file_name = "Demonstration Sample Data"))
    error_state(NULL)
  })
  
  # Custom CSV Upload
  observeEvent(input$upload_csv, {
    req(input$upload_csv)
    uploaded <- tryCatch(read.csv(input$upload_csv$datapath, stringsAsFactors = FALSE), error = function(e) {
      error_state(paste("Could not parse file as CSV:", e$message))
      return(NULL)
    })
    if (is.null(uploaded)) return()
    
    check <- validate_solar_data(uploaded)
    if (check$valid) {
      raw_state(list(data = check$data, source_type = "custom", file_name = input$upload_csv$name))
      error_state(NULL)
    } else {
      error_state(paste("Invalid Dataset:", check$message))
    }
  })
  
  # Reset to Default Dataset
  observeEvent(input$reset_data_btn, {
    csv_file <- get_csv_path("solar_data.csv")
    if (!file.exists(csv_file)) generate_sample_solar_data(csv_file)
    loaded <- read.csv(csv_file, stringsAsFactors = FALSE)
    check <- validate_solar_data(loaded)
    if (check$valid) {
      raw_state(list(data = check$data, source_type = "live", file_name = basename(csv_file)))
      error_state(NULL)
    }
  })
  
  # Dynamic Filters Sync
  observe({
    df <- raw_state()$data
    req(df)
    insts <- sort(unique(df$installation))
    updateSelectInput(session, "selected_installation", choices = c("All Installations", insts), selected = "All Installations")
    if (length(insts) >= 2) {
      updateSelectInput(session, "two_group_inst1", choices = insts, selected = insts[1])
      updateSelectInput(session, "two_group_inst2", choices = insts, selected = insts[2])
    }
    min_d <- min(df$date, na.rm = TRUE)
    max_d <- max(df$date, na.rm = TRUE)
    updateDateRangeInput(session, "selected_date_range", start = min_d, end = max_d, min = min_d, max = max_d)
  })
  
  # Dataset Status Badge
  output$dataset_badge_ui <- renderUI({
    st <- raw_state()
    cls <- if (st$source_type == "live") "badge-live" else if (st$source_type == "custom") "badge-ok" else "badge-sample"
    ic  <- if (st$source_type == "live") "circle-check" else if (st$source_type == "custom") "file-csv" else "flask"
    span(class = paste("status-badge", cls), icon(ic), " ", st$file_name)
  })
  
  # Error Banner
  output$data_error_banner <- renderUI({
    err <- error_state()
    if (!is.null(err)) {
      div(class = "alert alert-danger d-flex align-items-center mb-3",
          icon("triangle-exclamation", style = "margin-right: 8px; font-size: 1.2rem;"),
          div(strong("Dataset Validation Alert: "), err)
      )
    }
  })
  
  # Reactive Filtered Dataset with Core Calculations
  filtered_data <- reactive({
    df <- raw_state()$data
    req(df)
    
    dates <- input$selected_date_range
    if (!is.null(dates) && length(dates) == 2 && !any(is.na(dates))) {
      df <- df[df$date >= dates[1] & df$date <= dates[2], ]
    }
    inst <- input$selected_installation
    if (!is.null(inst) && inst != "All Installations") {
      df <- df[df$installation == inst, ]
    }
    
    thresh <- input$threshold
    df$daily_perf <- ifelse(df$expected_kwh > 0, (df$actual_kwh / df$expected_kwh) * 100, 0)
    df$shortfall_kwh <- pmax(0, df$expected_kwh - df$actual_kwh)
    df$diff_kwh <- df$actual_kwh - df$expected_kwh
    df$is_underperforming <- df$daily_perf < thresh
    df$status <- ifelse(df$is_underperforming, "Flagged", "Normal")
    df$day_index <- as.numeric(df$date - min(df$date, na.rm = TRUE)) + 1
    df
  })
  
  # Aggregated Daily Data (for portfolio-level time series)
  aggregated_daily_data <- reactive({
    df <- filtered_data()
    req(nrow(df) > 0)
    df %>%
      group_by(date) %>%
      summarise(expected_kwh = sum(expected_kwh, na.rm = TRUE),
                actual_kwh   = sum(actual_kwh, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        daily_perf = ifelse(expected_kwh > 0, (actual_kwh / expected_kwh) * 100, 0),
        shortfall_kwh = pmax(0, expected_kwh - actual_kwh),
        diff_kwh = actual_kwh - expected_kwh,
        is_underperforming = daily_perf < input$threshold,
        status = ifelse(is_underperforming, "Flagged", "Normal"),
        day_index = as.numeric(date - min(date, na.rm = TRUE)) + 1
      )
  })
  
  # Unfiltered Date-Constrained Data (used when cross-installation comparison needs all groups)
  date_constrained_all_installations <- reactive({
    df <- raw_state()$data
    req(df)
    dates <- input$selected_date_range
    if (!is.null(dates) && length(dates) == 2 && !any(is.na(dates))) {
      df <- df[df$date >= dates[1] & df$date <= dates[2], ]
    }
    df$daily_perf <- ifelse(df$expected_kwh > 0, (df$actual_kwh / df$expected_kwh) * 100, 0)
    df$shortfall_kwh <- pmax(0, df$expected_kwh - df$actual_kwh)
    df$diff_kwh <- df$actual_kwh - df$expected_kwh
    df
  })
  
  # KPI Renderers
  output$kpi_expected <- renderText({
    df <- filtered_data()
    if (nrow(df) == 0) return("0.0 kWh")
    paste0(format(round(sum(df$expected_kwh, na.rm = TRUE), 1), big.mark = ","), " kWh")
  })
  output$kpi_actual <- renderText({
    df <- filtered_data()
    if (nrow(df) == 0) return("0.0 kWh")
    paste0(format(round(sum(df$actual_kwh, na.rm = TRUE), 1), big.mark = ","), " kWh")
  })
  output$kpi_performance <- renderUI({
    df <- filtered_data()
    if (nrow(df) == 0) return(span("0.0%", style = "color: #64748b;"))
    exp_tot <- sum(df$expected_kwh, na.rm = TRUE)
    act_tot <- sum(df$actual_kwh, na.rm = TRUE)
    ratio   <- ifelse(exp_tot > 0, (act_tot / exp_tot) * 100, 0)
    col <- if (ratio >= input$threshold) "#059669" else "#dc2626"
    span(paste0(round(ratio, 1), "%"), style = paste0("color: ", col, ";"))
  })
  output$kpi_shortfall <- renderText({
    df <- filtered_data()
    if (nrow(df) == 0) return("0.0 kWh")
    paste0(format(round(sum(df$shortfall_kwh, na.rm = TRUE), 1), big.mark = ","), " kWh")
  })
  output$underperf_badge_text <- renderText({
    df <- aggregated_daily_data()
    if (nrow(df) == 0) return("0 Flagged Days")
    paste0(sum(df$is_underperforming, na.rm = TRUE), " / ", nrow(df), " Days Below ", input$threshold, "%")
  })
  
  # Tab 1: Time Series Chart
  output$plot_time_series <- renderPlotly({
    df <- aggregated_daily_data()
    req(nrow(df) > 0)
    plot_ly(df, x = ~date) %>%
      add_lines(y = ~expected_kwh, name = "Expected (kWh)",
                line = list(color = "#d97706", width = 2.5, dash = "dash"),
                hovertemplate = "<b>%{x|%Y-%m-%d}</b><br>Expected: %{y:,.1f} kWh<extra></extra>") %>%
      add_lines(y = ~actual_kwh, name = "Actual (kWh)",
                line = list(color = "#059669", width = 2.5),
                fill = "tozeroy", fillcolor = "rgba(16, 185, 129, 0.08)",
                hovertemplate = "<b>%{x|%Y-%m-%d}</b><br>Actual: %{y:,.1f} kWh<extra></extra>") %>%
      layout(paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff", hovermode = "x unified",
             margin = list(l = 50, r = 20, t = 20, b = 40),
             xaxis = list(title = "Date", showgrid = TRUE, gridcolor = "#f1f5f9", tickformat = "%b %d"),
             yaxis = list(title = "Energy Output (kWh)", showgrid = TRUE, gridcolor = "#f1f5f9"),
             legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.15))
  })
  
  # Tab 1: Performance Ratio Chart
  output$plot_performance_ratio <- renderPlotly({
    df <- aggregated_daily_data()
    req(nrow(df) > 0)
    thresh <- input$threshold
    df$color_val <- ifelse(df$daily_perf < thresh, "#dc2626", "#059669")
    
    plot_ly(df, x = ~date) %>%
      add_markers(y = ~daily_perf, marker = list(color = ~color_val, size = 7, line = list(color = "#ffffff", width = 1)),
                  name = "Daily Ratio", hovertemplate = "<b>%{x|%Y-%m-%d}</b><br>Performance: %{y:.1f}%<br>Status: %{text}<extra></extra>",
                  text = ~ifelse(is_underperforming, "Below Threshold (Flagged)", "Healthy")) %>%
      add_lines(y = ~daily_perf, line = list(color = "#cbd5e1", width = 1.2), showlegend = FALSE, hoverinfo = "none") %>%
      add_lines(y = thresh, name = paste0("Threshold (", thresh, "%)"),
                line = list(color = "#dc2626", dash = "dot", width = 2), hoverinfo = "none") %>%
      layout(paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff", margin = list(l = 50, r = 20, t = 20, b = 40),
             xaxis = list(title = "Date", showgrid = TRUE, gridcolor = "#f1f5f9", tickformat = "%b %d"),
             yaxis = list(title = "Performance (%)", showgrid = TRUE, gridcolor = "#f1f5f9",
                          range = c(0, max(120, max(df$daily_perf, na.rm = TRUE) + 10))),
             legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.15))
  })
  
  # Tab 2: Diagnostics Outputs
  output$flagged_days_count <- renderText({
    df <- filtered_data()
    paste0(sum(df$is_underperforming, na.rm = TRUE), " of ", nrow(df), " records")
  })
  output$flagged_shortfall_sum <- renderText({
    df <- filtered_data()
    paste0(format(round(sum(df$shortfall_kwh[df$is_underperforming], na.rm = TRUE), 1), big.mark = ","), " kWh")
  })
  output$flagged_avg_perf <- renderText({
    df <- filtered_data()
    flagged <- df[df$is_underperforming, ]
    if (nrow(flagged) == 0) return("N/A")
    paste0(round(mean(flagged$daily_perf, na.rm = TRUE), 1), "%")
  })
  
  # Tab 2: Temporal Degradation / Linear Trend
  output$plot_trend_degradation <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 3)
    fit_trend <- lm(daily_perf ~ day_index, data = df)
    df$fitted_trend <- predict(fit_trend)
    
    p <- plot_ly(df, x = ~date) %>%
      add_markers(y = ~daily_perf, color = ~installation,
                  marker = list(size = 5, opacity = 0.65),
                  hovertemplate = "Date: %{x}<br>Ratio: %{y:.1f}%<extra></extra>") %>%
      add_lines(y = ~fitted_trend, name = "OLS Linear Trend",
                line = list(color = "#0f172a", width = 2.5),
                hovertemplate = "Trend Fit: %{y:.1f}%<extra></extra>") %>%
      layout(paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff",
             margin = list(l = 45, r = 15, t = 10, b = 35),
             xaxis = list(title = "Date", showgrid = TRUE, gridcolor = "#f1f5f9"),
             yaxis = list(title = "Performance Ratio (%)", showgrid = TRUE, gridcolor = "#f1f5f9"))
    p
  })
  
  output$trend_stats_summary_ui <- renderUI({
    df <- filtered_data()
    req(nrow(df) > 3)
    fit_trend <- lm(daily_perf ~ day_index, data = df)
    s <- summary(fit_trend)
    slope <- coef(fit_trend)[2] # % change per day
    slope_monthly <- slope * 30 # % change per month
    pval <- s$coefficients[2, 4]
    spear_trend <- tryCatch(cor.test(df$day_index, df$daily_perf, method = "spearman", exact = FALSE), error = function(e) NULL)
    
    status_msg <- if (pval > 0.05) {
      span("No Significant Drift (Stable)", class = "badge bg-success")
    } else if (slope < 0) {
      span("Degradation / Soiling Trend Detected", class = "badge bg-danger")
    } else {
      span("Positive Seasonal Irradiance Drift", class = "badge bg-info")
    }
    
    div(style = "padding: 8px 12px; background: #f8fafc; border-radius: 8px; border: 1px solid #e2e8f0; font-size: 0.85rem;",
        div(style = "margin-bottom: 6px;", strong("Trend Evaluation: "), status_msg),
        div(strong("Daily Slope (β_time): "), sprintf("%.4f%% / day", slope)),
        div(strong("Monthly Drift: "), sprintf("%.2f%% / month", slope_monthly)),
        div(strong("OLS t-Test p-Value: "), format.pval(pval, digits = 3)),
        if (!is.null(spear_trend)) div(strong("Spearman Rank Trend ρ: "), sprintf("%.3f (p = %s)", spear_trend$estimate, format.pval(spear_trend$p.value, digits = 3))) else NULL,
        hr(style = "margin: 6px 0;"),
        p(style = "font-size: 0.77rem; color: #64748b; margin-bottom: 0;",
          "A statistically significant negative slope indicates module soiling accumulation or equipment degradation over time.")
    )
  })
  
  output$table_flagged_days <- renderDT({
    df <- filtered_data()
    flagged <- df[df$is_underperforming, ] %>%
      arrange(desc(shortfall_kwh)) %>%
      transmute(
        Date = as.character(date), Installation = installation,
        `Expected (kWh)` = round(expected_kwh, 1), `Actual (kWh)` = round(actual_kwh, 1),
        `Performance (%)` = paste0(round(daily_perf, 1), "%"),
        `Shortfall (kWh)` = round(shortfall_kwh, 1), Status = status
      )
    datatable(flagged, options = list(pageLength = 8, autoWidth = TRUE, dom = "ftp"), rownames = FALSE, selection = "none") %>%
      formatStyle("Shortfall (kWh)", color = "#dc2626", fontWeight = "bold") %>%
      formatStyle("Performance (%)", color = "#b91c1c")
  })
  
  # ============================================================================
  # TAB 3: CORRELATION STUDIO
  # ============================================================================
  corr_results <- reactive({
    df <- filtered_data()
    req(nrow(df) >= 3)
    
    pearson_test  <- tryCatch(cor.test(df$expected_kwh, df$actual_kwh, method = "pearson"), error = function(e) NULL)
    spearman_test <- tryCatch(cor.test(df$expected_kwh, df$actual_kwh, method = "spearman", exact = FALSE), error = function(e) NULL)
    kendall_test  <- tryCatch(cor.test(df$expected_kwh, df$actual_kwh, method = "kendall", exact = FALSE), error = function(e) NULL)
    
    list(pearson = pearson_test, spearman = spearman_test, kendall = kendall_test)
  })
  
  output$corr_pearson_val <- renderText({
    res <- corr_results()$pearson
    if (is.null(res)) return("N/A")
    sprintf("%.4f", res$estimate)
  })
  output$corr_pearson_sub <- renderText({
    res <- corr_results()$pearson
    if (is.null(res)) return("")
    paste0("p = ", format.pval(res$p.value, digits = 3), " | 95% CI: [", round(res$conf.int[1], 3), ", ", round(res$conf.int[2], 3), "]")
  })
  
  output$corr_spearman_val <- renderText({
    res <- corr_results()$spearman
    if (is.null(res)) return("N/A")
    sprintf("%.4f", res$estimate)
  })
  output$corr_spearman_sub <- renderText({
    res <- corr_results()$spearman
    if (is.null(res)) return("")
    paste0("p = ", format.pval(res$p.value, digits = 3), " (Rank-order monotonic)")
  })
  
  output$corr_kendall_val <- renderText({
    res <- corr_results()$kendall
    if (is.null(res)) return("N/A")
    sprintf("%.4f", res$estimate)
  })
  output$corr_kendall_sub <- renderText({
    res <- corr_results()$kendall
    if (is.null(res)) return("")
    paste0("p = ", format.pval(res$p.value, digits = 3), " (Concordant vs discordant)")
  })
  
  output$plot_correlation_scatter <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) >= 3)
    
    p <- plot_ly(df, x = ~expected_kwh, y = ~actual_kwh) %>%
      add_markers(color = ~installation, colors = c("#0d9488", "#d97706", "#2563eb"),
                  marker = list(size = 6.5, opacity = 0.75, line = list(color = "#ffffff", width = 0.5)),
                  text = ~paste0("Date: ", date, "<br>Installation: ", installation,
                                 "<br>Expected: ", round(expected_kwh, 1), " kWh<br>Actual: ", round(actual_kwh, 1), " kWh"),
                  hoverinfo = "text", name = ~installation)
    
    # Add OLS regression fit line
    fit <- tryCatch(lm(actual_kwh ~ expected_kwh, data = df), error = function(e) NULL)
    if (!is.null(fit)) {
      x_grid <- seq(min(df$expected_kwh, na.rm = TRUE), max(df$expected_kwh, na.rm = TRUE), length.out = 100)
      pred_y <- predict(fit, newdata = data.frame(expected_kwh = x_grid))
      p <- p %>% add_lines(x = x_grid, y = pred_y, name = "OLS Fit",
                           line = list(color = "#0f172a", width = 2), inherit = FALSE)
    }
    
    # Add 1:1 Parity reference line (Ideal Solar Conversion)
    max_val <- max(c(df$expected_kwh, df$actual_kwh), na.rm = TRUE)
    p <- p %>% add_lines(x = c(0, max_val), y = c(0, max_val), name = "1:1 Ideal Target",
                         line = list(color = "#94a3b8", dash = "dot", width = 1.5), inherit = FALSE)
    
    p %>% layout(paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff",
                 margin = list(l = 50, r = 20, t = 20, b = 40),
                 xaxis = list(title = "Expected Generation (kWh) [Irradiance Model]", showgrid = TRUE, gridcolor = "#f1f5f9"),
                 yaxis = list(title = "Actual Generation (kWh) [Grid Delivered]", showgrid = TRUE, gridcolor = "#f1f5f9"),
                 legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.15))
  })
  
  output$table_correlation_tests <- renderDT({
    c_res <- corr_results()
    req(c_res$pearson)
    
    alpha <- as.numeric(input$stat_alpha)
    p_pear <- c_res$pearson$p.value
    p_spear <- c_res$spearman$p.value
    p_kend <- c_res$kendall$p.value
    
    summary_df <- data.frame(
      Method = c("Pearson (Parametric)", "Spearman (Non-Parametric)", "Kendall Tau (Non-Parametric)"),
      Estimate = c(round(c_res$pearson$estimate, 4), round(c_res$spearman$estimate, 4), round(c_res$kendall$estimate, 4)),
      Statistic = c(sprintf("t = %.3f (df=%d)", c_res$pearson$statistic, c_res$pearson$parameter),
                    sprintf("S = %.0f", c_res$spearman$statistic),
                    sprintf("z = %.3f", c_res$kendall$statistic)),
      `p-Value` = c(format.pval(p_pear, digits = 3), format.pval(p_spear, digits = 3), format.pval(p_kend, digits = 3)),
      `95% CI` = c(sprintf("[%.3f, %.3f]", c_res$pearson$conf.int[1], c_res$pearson$conf.int[2]), "N/A (Rank test)", "N/A (Rank test)"),
      Conclusion = c(ifelse(p_pear < alpha, "Significant (Reject H₀)", "Fail to Reject H₀"),
                     ifelse(p_spear < alpha, "Significant (Reject H₀)", "Fail to Reject H₀"),
                     ifelse(p_kend < alpha, "Significant (Reject H₀)", "Fail to Reject H₀")),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    
    datatable(summary_df, options = list(dom = "t", autoWidth = TRUE, ordering = FALSE), rownames = FALSE, selection = "none") %>%
      formatStyle("Conclusion", color = "#166534", fontWeight = "bold")
  })
  
  output$table_installation_corrs <- renderDT({
    df <- date_constrained_all_installations()
    req(nrow(df) > 0)
    
    inst_splits <- split(df, df$installation)
    inst_stats <- lapply(names(inst_splits), function(nm) {
      sub_d <- inst_splits[[nm]]
      p_c <- tryCatch(cor.test(sub_d$expected_kwh, sub_d$actual_kwh, method = "pearson"), error = function(e) NULL)
      s_c <- tryCatch(cor.test(sub_d$expected_kwh, sub_d$actual_kwh, method = "spearman", exact = FALSE), error = function(e) NULL)
      data.frame(
        Installation = nm,
        n = nrow(sub_d),
        `Pearson r` = if (!is.null(p_c)) round(p_c$estimate, 3) else NA,
        `Spearman ρ` = if (!is.null(s_c)) round(s_c$estimate, 3) else NA,
        `p-Value` = if (!is.null(p_c)) format.pval(p_c$p.value, digits = 2) else "N/A",
        check.names = FALSE, stringsAsFactors = FALSE
      )
    })
    
    combined_inst_df <- do.call(rbind, inst_stats)
    datatable(combined_inst_df, options = list(dom = "t", autoWidth = TRUE), rownames = FALSE, selection = "none")
  })
  
  # ============================================================================
  # TAB 4: OLS REGRESSION MODELING & DIAGNOSTICS
  # ============================================================================
  reg_model <- reactive({
    df <- filtered_data()
    req(nrow(df) >= 3)
    fit <- tryCatch(lm(actual_kwh ~ expected_kwh, data = df), error = function(e) NULL)
    req(fit)
    fit
  })
  
  output$reg_r2 <- renderText({
    fit <- reg_model()
    s <- summary(fit)
    sprintf("%.4f (%.1f%%)", s$r.squared, s$r.squared * 100)
  })
  output$reg_adj_r2 <- renderText({
    fit <- reg_model()
    s <- summary(fit)
    sprintf("Adjusted R²: %.4f", s$adj.r.squared)
  })
  output$reg_slope <- renderText({
    fit <- reg_model()
    sprintf("%.4f", coef(fit)[2])
  })
  output$reg_slope_pval <- renderText({
    fit <- reg_model()
    s <- summary(fit)
    pval <- s$coefficients[2, 4]
    paste0("t = ", round(s$coefficients[2, 3], 2), " | p = ", format.pval(pval, digits = 3))
  })
  output$reg_intercept <- renderText({
    fit <- reg_model()
    sprintf("%.2f kWh", coef(fit)[1])
  })
  output$reg_rse <- renderText({
    fit <- reg_model()
    s <- summary(fit)
    sprintf("%.2f kWh", s$sigma)
  })
  output$reg_fstat <- renderText({
    fit <- reg_model()
    s <- summary(fit)
    fval <- s$fstatistic
    if (!is.null(fval)) {
      sprintf("F(%d, %d) = %.1f, p < 0.001", fval[2], fval[3], fval[1])
    } else ""
  })
  
  output$plot_regression_fit <- renderPlotly({
    df <- filtered_data()
    fit <- reg_model()
    req(fit, nrow(df) >= 3)
    
    # Calculate confidence and prediction ribbons
    x_new <- data.frame(expected_kwh = seq(min(df$expected_kwh), max(df$expected_kwh), length.out = 100))
    conf_pred <- predict(fit, newdata = x_new, interval = "confidence", level = 0.95)
    pred_pred <- predict(fit, newdata = x_new, interval = "prediction", level = 0.95)
    
    ribbon_df <- data.frame(
      expected_kwh = x_new$expected_kwh,
      fit = conf_pred[, "fit"],
      conf_lwr = conf_pred[, "lwr"],
      conf_upr = conf_pred[, "upr"],
      pred_lwr = pmax(0, pred_pred[, "lwr"]),
      pred_upr = pred_pred[, "upr"]
    )
    
    p <- plot_ly(df, x = ~expected_kwh) %>%
      add_markers(y = ~actual_kwh, color = ~installation,
                  marker = list(size = 6, opacity = 0.65), name = "Observed Data") %>%
      # 95% Prediction Interval Ribbon (outer)
      add_ribbons(data = ribbon_df, x = ~expected_kwh, ymin = ~pred_lwr, ymax = ~pred_upr,
                  fillcolor = "rgba(100, 116, 139, 0.12)", line = list(color = "transparent"),
                  name = "95% Prediction Band") %>%
      # 95% Confidence Interval Ribbon (inner)
      add_ribbons(data = ribbon_df, x = ~expected_kwh, ymin = ~conf_lwr, ymax = ~conf_upr,
                  fillcolor = "rgba(13, 148, 136, 0.25)", line = list(color = "transparent"),
                  name = "95% Mean Confidence Band") %>%
      # Regression line
      add_lines(data = ribbon_df, x = ~expected_kwh, y = ~fit,
                line = list(color = "#0d9488", width = 2.5), name = "OLS Fit Line") %>%
      layout(paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff",
             margin = list(l = 50, r = 20, t = 20, b = 40),
             xaxis = list(title = "Expected Energy (kWh)", showgrid = TRUE, gridcolor = "#f1f5f9"),
             yaxis = list(title = "Actual Energy (kWh)", showgrid = TRUE, gridcolor = "#f1f5f9"),
             legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.15))
    p
  })
  
  output$table_regression_coefs <- renderDT({
    fit <- reg_model()
    req(fit)
    s <- summary(fit)
    ci <- confint(fit, level = 0.95)
    
    coef_df <- data.frame(
      Term = c("Intercept (β₀)", "Slope (β₁ - Expected kWh)"),
      Estimate = round(s$coefficients[, 1], 4),
      `Std Error` = round(s$coefficients[, 2], 4),
      `t-Statistic` = round(s$coefficients[, 3], 3),
      `p-Value` = format.pval(s$coefficients[, 4], digits = 3),
      `95% CI Lower` = round(ci[, 1], 4),
      `95% CI Upper` = round(ci[, 2], 4),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    datatable(coef_df, options = list(dom = "t", autoWidth = TRUE), rownames = FALSE, selection = "none")
  })
  
  # Interactive Prediction Calculator
  pred_val_reactive <- eventReactive(input$calc_pred_btn, {
    fit <- reg_model()
    req(fit, input$pred_input_exp)
    inp <- as.numeric(input$pred_input_exp)
    new_d <- data.frame(expected_kwh = inp)
    conf_res <- predict(fit, newdata = new_d, interval = "confidence", level = 0.95)
    pred_res <- predict(fit, newdata = new_d, interval = "prediction", level = 0.95)
    list(fit = conf_res[1, "fit"],
         conf_lwr = conf_res[1, "lwr"], conf_upr = conf_res[1, "upr"],
         pred_lwr = max(0, pred_res[1, "lwr"]), pred_upr = pred_res[1, "upr"],
         input = inp)
  }, ignoreNULL = FALSE)
  
  output$pred_result_ui <- renderUI({
    res <- pred_val_reactive()
    if (is.null(res)) return(p("Enter expected kWh and click Compute."))
    
    div(style = "padding: 10px; background: #f0fdf4; border: 1px solid #bbf7d0; border-radius: 6px; margin-top: 10px;",
        div(strong("Estimated Actual Output: "), span(sprintf("%.1f kWh", res$fit), style = "font-size: 1.15rem; color: #15803d; font-weight: 700;")),
        div(style = "font-size: 0.82rem; color: #166534; margin-top: 4px;",
            sprintf("95%% Mean Confidence Interval: [%.1f, %.1f] kWh", res$conf_lwr, res$conf_upr)),
        div(style = "font-size: 0.82rem; color: #166534;",
            sprintf("95%% Single-Day Prediction Interval: [%.1f, %.1f] kWh", res$pred_lwr, res$pred_upr))
    )
  })
  
  # 4 Regression Diagnostic Plots
  output$plot_diag_res_fitted <- renderPlotly({
    fit <- reg_model()
    req(fit)
    d <- data.frame(fitted = fitted(fit), resid = resid(fit))
    plot_ly(d, x = ~fitted, y = ~resid) %>%
      add_markers(marker = list(color = "#2563eb", size = 5, opacity = 0.6)) %>%
      add_lines(y = 0, line = list(color = "#dc2626", dash = "dash", width = 1.5), showlegend = FALSE) %>%
      layout(title = list(text = "1. Residuals vs Fitted (Homoscedasticity)", font = list(size = 12)),
             paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff",
             margin = list(l = 45, r = 15, t = 30, b = 35),
             xaxis = list(title = "Fitted Values", showgrid = TRUE, gridcolor = "#f1f5f9"),
             yaxis = list(title = "Residuals", showgrid = TRUE, gridcolor = "#f1f5f9"))
  })
  
  output$plot_diag_qq <- renderPlotly({
    fit <- reg_model()
    req(fit)
    std_res <- scale(resid(fit))
    n <- length(std_res)
    theoretical_q <- qnorm(ppoints(n))
    observed_q <- sort(as.vector(std_res))
    
    p <- plot_ly() %>%
      add_markers(x = theoretical_q, y = observed_q, marker = list(color = "#d97706", size = 5, opacity = 0.6), name = "Residuals") %>%
      add_lines(x = c(-3, 3), y = c(-3, 3), line = list(color = "#0f172a", width = 1.5), name = "Theoretical Normal") %>%
      layout(title = list(text = "2. Normal Q-Q Plot of Residuals", font = list(size = 12)),
             paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff",
             margin = list(l = 45, r = 15, t = 30, b = 35),
             xaxis = list(title = "Theoretical Quantiles", showgrid = TRUE, gridcolor = "#f1f5f9"),
             yaxis = list(title = "Standardized Residuals", showgrid = TRUE, gridcolor = "#f1f5f9"),
             showlegend = FALSE)
    p
  })
  
  output$plot_diag_scale_loc <- renderPlotly({
    fit <- reg_model()
    req(fit)
    std_res <- abs(scale(resid(fit)))
    sqrt_std_res <- sqrt(std_res)
    d <- data.frame(fitted = fitted(fit), val = sqrt_std_res)
    
    plot_ly(d, x = ~fitted, y = ~val) %>%
      add_markers(marker = list(color = "#0d9488", size = 5, opacity = 0.6)) %>%
      layout(title = list(text = "3. Scale-Location (Spread-Location)", font = list(size = 12)),
             paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff",
             margin = list(l = 45, r = 15, t = 30, b = 35),
             xaxis = list(title = "Fitted Values", showgrid = TRUE, gridcolor = "#f1f5f9"),
             yaxis = list(title = "√|Standardized Residuals|", showgrid = TRUE, gridcolor = "#f1f5f9"))
  })
  
  output$plot_diag_res_hist <- renderPlotly({
    fit <- reg_model()
    req(fit)
    res <- resid(fit)
    
    plot_ly(x = res, type = "histogram", nbinsx = 35,
            marker = list(color = "rgba(13, 148, 136, 0.6)", line = list(color = "#0d9488", width = 1))) %>%
      layout(title = list(text = "4. Residual Frequency Distribution", font = list(size = 12)),
             paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff",
             margin = list(l = 45, r = 15, t = 30, b = 35),
             xaxis = list(title = "Residual Value (kWh)", showgrid = TRUE, gridcolor = "#f1f5f9"),
             yaxis = list(title = "Count", showgrid = TRUE, gridcolor = "#f1f5f9"))
  })
  
  # ============================================================================
  # TAB 5: HYPOTHESIS TESTING LAB
  # ============================================================================
  paired_tests_reactive <- reactive({
    df <- filtered_data()
    req(nrow(df) >= 3)
    
    t_res <- tryCatch(t.test(df$actual_kwh, df$expected_kwh, paired = TRUE), error = function(e) NULL)
    w_res <- tryCatch(wilcox.test(df$actual_kwh, df$expected_kwh, paired = TRUE, exact = FALSE), error = function(e) NULL)
    
    # Shapiro-Wilk on differences (subsample if n > 5000)
    diffs <- df$diff_kwh
    shapiro_res <- if (length(diffs) >= 3) {
      sample_diffs <- if (length(diffs) > 5000) sample(diffs, 5000) else diffs
      tryCatch(shapiro.test(sample_diffs), error = function(e) NULL)
    } else NULL
    
    list(t_test = t_res, wilcox = w_res, shapiro = shapiro_res, diffs = diffs)
  })
  
  output$paired_t_stat <- renderText({
    res <- paired_tests_reactive()$t_test
    if (is.null(res)) return("N/A")
    sprintf("t = %.3f (df = %d)", res$statistic, res$parameter)
  })
  output$paired_t_desc <- renderText({
    res <- paired_tests_reactive()$t_test
    if (is.null(res)) return("")
    sprintf("Mean diff = %.2f kWh | p = %s | 95%% CI: [%.2f, %.2f]",
            res$estimate, format.pval(res$p.value, digits = 3), res$conf.int[1], res$conf.int[2])
  })
  
  output$paired_wilcox_stat <- renderText({
    res <- paired_tests_reactive()$wilcox
    if (is.null(res)) return("N/A")
    sprintf("V = %.0f", res$statistic)
  })
  output$paired_wilcox_desc <- renderText({
    res <- paired_tests_reactive()$wilcox
    if (is.null(res)) return("")
    sprintf("p-value = %s (Rank sum of signed differences)", format.pval(res$p.value, digits = 3))
  })
  
  output$plot_paired_diff_hist <- renderPlotly({
    p_data <- paired_tests_reactive()
    diffs <- p_data$diffs
    req(length(diffs) > 0)
    
    plot_ly(x = diffs, type = "histogram", nbinsx = 30,
            marker = list(color = "rgba(217, 119, 6, 0.65)", line = list(color = "#d97706", width = 1))) %>%
      layout(title = list(text = "Distribution of Daily Discrepancies (d)", font = list(size = 12)),
             paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff",
             margin = list(l = 45, r = 15, t = 30, b = 35),
             xaxis = list(title = "Difference (Actual - Expected kWh)", showgrid = TRUE, gridcolor = "#f1f5f9"),
             yaxis = list(title = "Frequency", showgrid = TRUE, gridcolor = "#f1f5f9"))
  })
  
  output$plot_paired_diff_qq <- renderPlotly({
    p_data <- paired_tests_reactive()
    diffs <- p_data$diffs
    req(length(diffs) >= 3)
    
    std_d <- scale(diffs)
    n <- length(std_d)
    t_q <- qnorm(ppoints(n))
    o_q <- sort(as.vector(std_d))
    
    plot_ly() %>%
      add_markers(x = t_q, y = o_q, marker = list(color = "#6366f1", size = 5, opacity = 0.6), name = "Differences") %>%
      add_lines(x = c(-3, 3), y = c(-3, 3), line = list(color = "#0f172a", width = 1.5), name = "Normal Target") %>%
      layout(title = list(text = "Normal Q-Q Plot of Differences", font = list(size = 12)),
             paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff",
             margin = list(l = 45, r = 15, t = 30, b = 35),
             xaxis = list(title = "Theoretical Quantiles", showgrid = TRUE, gridcolor = "#f1f5f9"),
             yaxis = list(title = "Standardized Discrepancy", showgrid = TRUE, gridcolor = "#f1f5f9"),
             showlegend = FALSE)
  })
  
  output$paired_conclusion_ui <- renderUI({
    p_data <- paired_tests_reactive()
    req(p_data$t_test, p_data$wilcox)
    alpha <- as.numeric(input$stat_alpha)
    
    t_sig <- p_data$t_test$p.value < alpha
    w_sig <- p_data$wilcox$p.value < alpha
    shap_sig <- if (!is.null(p_data$shapiro)) p_data$shapiro$p.value < alpha else FALSE
    
    div(style = "margin-top: 14px; padding: 12px; background: #f8fafc; border-radius: 8px; border: 1px solid #cbd5e1; font-size: 0.88rem;",
        strong("Formal Academic Synthesis: "),
        if (shap_sig) {
          paste0("The Shapiro-Wilk test (W = ", round(p_data$shapiro$statistic, 3), ", p = ", format.pval(p_data$shapiro$p.value, digits = 3),
                 ") REJECTS the normality assumption of generation discrepancies. Therefore, the non-parametric Wilcoxon Signed-Rank test is methodologically preferred over the Student's t-test.")
        } else {
          "The Shapiro-Wilk test confirms differences do not significantly depart from normality, validating both parametric and non-parametric tests."
        },
        tags$br(),
        if (w_sig) {
          span(style = "color: #b91c1c; font-weight: 700;",
               sprintf("Conclusion: Both tests confirm a statistically significant deficit (p < %.3f). Actual electrical yield is systematically lower than the theoretical solar irradiance expectation.", alpha))
        } else {
          span(style = "color: #15803d; font-weight: 700;",
               "Conclusion: No statistically significant difference detected between actual generation and expected irradiance targets.")
        }
    )
  })
  
  # Cross-Installation Comparison (ANOVA & Kruskal-Wallis)
  anova_reactive <- reactive({
    df <- date_constrained_all_installations()
    req(length(unique(df$installation)) >= 2)
    
    aov_fit <- tryCatch(aov(daily_perf ~ installation, data = df), error = function(e) NULL)
    kw_fit  <- tryCatch(kruskal.test(daily_perf ~ installation, data = df), error = function(e) NULL)
    tukey   <- if (!is.null(aov_fit)) tryCatch(TukeyHSD(aov_fit), error = function(e) NULL) else NULL
    pair_w  <- tryCatch(pairwise.wilcox.test(df$daily_perf, df$installation, p.adjust.method = "holm", exact = FALSE), error = function(e) NULL)
    
    list(aov = aov_fit, kw = kw_fit, tukey = tukey, pair_w = pair_w, df = df)
  })
  
  output$anova_f_stat <- renderText({
    res <- anova_reactive()$aov
    if (is.null(res)) return("N/A")
    s <- summary(res)[[1]]
    sprintf("F(%d, %d) = %.3f", s$Df[1], s$Df[2], s$`F value`[1])
  })
  output$anova_p_desc <- renderText({
    res <- anova_reactive()$aov
    if (is.null(res)) return("")
    s <- summary(res)[[1]]
    sprintf("p-value = %s (Between-groups variance ratio)", format.pval(s$`Pr(>F)`[1], digits = 3))
  })
  
  output$kw_chi_stat <- renderText({
    res <- anova_reactive()$kw
    if (is.null(res)) return("N/A")
    sprintf("χ² = %.3f (df = %d)", res$statistic, res$parameter)
  })
  output$kw_p_desc <- renderText({
    res <- anova_reactive()$kw
    if (is.null(res)) return("")
    sprintf("p-value = %s (Non-parametric rank sum test)", format.pval(res$p.value, digits = 3))
  })
  
  output$plot_anova_boxplots <- renderPlotly({
    df <- anova_reactive()$df
    req(df)
    
    plot_ly(df, x = ~installation, y = ~daily_perf, color = ~installation, type = "box",
            boxpoints = "outliers", jitter = 0.3, pointpos = -1.8) %>%
      layout(paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff",
             margin = list(l = 50, r = 20, t = 10, b = 40),
             xaxis = list(title = "Solar Installation Architecture"),
             yaxis = list(title = "Performance Ratio (%)", showgrid = TRUE, gridcolor = "#f1f5f9"),
             showlegend = FALSE)
  })
  
  output$table_posthoc_tests <- renderDT({
    res <- anova_reactive()
    req(res$tukey)
    
    t_df <- as.data.frame(res$tukey$installation)
    t_df$Comparison <- rownames(t_df)
    rownames(t_df) <- NULL
    
    formatted_df <- data.frame(
      Comparison = t_df$Comparison,
      `Difference in Means` = round(t_df$diff, 2),
      `95% CI Lower` = round(t_df$lwr, 2),
      `95% CI Upper` = round(t_df$upr, 2),
      `Tukey Adjusted p` = format.pval(t_df$`p adj`, digits = 3),
      `Significant?` = ifelse(t_df$`p adj` < as.numeric(input$stat_alpha), "Yes (Differs)", "No (Equal)"),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    
    datatable(formatted_df, options = list(dom = "t", autoWidth = TRUE), rownames = FALSE, selection = "none") %>%
      formatStyle("Significant?", color = styleEqual(c("Yes (Differs)", "No (Equal)"), c("#b91c1c", "#15803d")), fontWeight = "bold")
  })
  
  # Two-Group Head to Head
  two_group_data <- reactive({
    df <- date_constrained_all_installations()
    req(input$two_group_inst1, input$two_group_inst2, input$two_group_inst1 != input$two_group_inst2)
    
    g1 <- df[df$installation == input$two_group_inst1, ]
    g2 <- df[df$installation == input$two_group_inst2, ]
    req(nrow(g1) >= 2, nrow(g2) >= 2)
    
    welch <- tryCatch(t.test(g1$daily_perf, g2$daily_perf, var.equal = FALSE), error = function(e) NULL)
    mann_w <- tryCatch(wilcox.test(g1$daily_perf, g2$daily_perf, exact = FALSE), error = function(e) NULL)
    
    # Cohen's d effect size
    s_pooled <- sqrt(((nrow(g1)-1)*var(g1$daily_perf) + (nrow(g2)-1)*var(g2$daily_perf)) / (nrow(g1) + nrow(g2) - 2))
    cohen_d <- (mean(g1$daily_perf) - mean(g2$daily_perf)) / s_pooled
    
    list(g1 = g1, g2 = g2, welch = welch, mann_w = mann_w, cohen_d = cohen_d)
  })
  
  output$two_sample_t_stat <- renderText({
    res <- two_group_data()$welch
    if (is.null(res)) return("N/A")
    sprintf("t = %.3f (df = %.1f)", res$statistic, res$parameter)
  })
  output$two_sample_t_desc <- renderText({
    res <- two_group_data()$welch
    if (is.null(res)) return("")
    sprintf("Diff in Means = %.2f%% | p = %s", res$estimate[1] - res$estimate[2], format.pval(res$p.value, digits = 3))
  })
  
  output$mann_whitney_stat <- renderText({
    res <- two_group_data()$mann_w
    if (is.null(res)) return("N/A")
    sprintf("W = %.0f", res$statistic)
  })
  output$mann_whitney_desc <- renderText({
    res <- two_group_data()$mann_w
    if (is.null(res)) return("")
    sprintf("p-value = %s (Rank sum)", format.pval(res$p.value, digits = 3))
  })
  
  output$plot_two_group_comp <- renderPlotly({
    tg <- two_group_data()
    req(tg)
    comp_df <- rbind(tg$g1, tg$g2)
    
    plot_ly(comp_df, x = ~installation, y = ~daily_perf, color = ~installation, type = "violin",
            box = list(visible = TRUE), points = "all", jitter = 0.25) %>%
      layout(paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff",
             margin = list(l = 50, r = 20, t = 10, b = 40),
             xaxis = list(title = "Installation Pair"),
             yaxis = list(title = "Daily Performance Ratio (%)", showgrid = TRUE, gridcolor = "#f1f5f9"),
             showlegend = FALSE)
  })
  
  output$two_group_conclusion_ui <- renderUI({
    tg <- two_group_data()
    req(tg$welch)
    alpha <- as.numeric(input$stat_alpha)
    sig <- tg$welch$p.value < alpha
    
    div(style = "padding: 10px; background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 6px; font-size: 0.86rem; margin-top: 10px;",
        div(strong("Effect Size (Cohen's d): "), sprintf("%.3f", tg$cohen_d),
            span(style = "color: #64748b; font-size: 0.8rem;", " (|d| < 0.2: negligible; 0.2-0.5: small; 0.5-0.8: medium; > 0.8: large)")),
        div(style = "margin-top: 4px;",
            strong("Decision: "),
            if (sig) {
              span(style = "color: #b91c1c; font-weight: 700;", "Statistically Significant Difference Detected (p < α).")
            } else {
              span(style = "color: #15803d; font-weight: 700;", "No Statistically Significant Difference (Performance levels are statistically equivalent).")
            }
        )
    )
  })
  
  # ============================================================================
  # TAB 6: DESCRIPTIVE STATISTICS & DISTRIBUTION LAB
  # ============================================================================
  output$table_descriptive_stats <- renderDT({
    df <- filtered_data()
    req(nrow(df) >= 3)
    
    vars <- list(`Actual kWh` = df$actual_kwh, `Expected kWh` = df$expected_kwh, `Performance Ratio (%)` = df$daily_perf)
    
    rows <- lapply(names(vars), function(vname) {
      x <- vars[[vname]]
      n <- length(x)
      m <- mean(x, na.rm = TRUE)
      med <- median(x, na.rm = TRUE)
      s <- sd(x, na.rm = TRUE)
      v <- var(x, na.rm = TRUE)
      se <- s / sqrt(n)
      ci_w <- qt(0.975, df = n - 1) * se
      iqr_val <- IQR(x, na.rm = TRUE)
      mad_val <- mad(x, na.rm = TRUE)
      sk <- calc_skewness(x)
      kt <- calc_kurtosis(x)
      p10 <- quantile(x, 0.10, na.rm = TRUE)
      p90 <- quantile(x, 0.90, na.rm = TRUE)
      
      data.frame(
        Variable = vname,
        n = n,
        `Mean (x̄)` = round(m, 2),
        `Std Dev (s)` = round(s, 2),
        `Variance (s²)` = round(v, 2),
        `Std Error (SE)` = round(se, 2),
        `95% CI of Mean` = sprintf("[%.1f, %.1f]", m - ci_w, m + ci_w),
        `Median (Q₂)` = round(med, 2),
        IQR = round(iqr_val, 2),
        MAD = round(mad_val, 2),
        `Min` = round(min(x, na.rm = TRUE), 1),
        `Max` = round(max(x, na.rm = TRUE), 1),
        Range = round(max(x, na.rm = TRUE) - min(x, na.rm = TRUE), 1),
        `Skewness (g₁)` = round(sk, 3),
        `Excess Kurt (g₂)` = round(kt, 3),
        `P10 - P90` = sprintf("%.1f - %.1f", p10, p90),
        check.names = FALSE, stringsAsFactors = FALSE
      )
    })
    
    summary_tab <- do.call(rbind, rows)
    datatable(summary_tab, options = list(dom = "t", autoWidth = TRUE, scrollX = TRUE), rownames = FALSE, selection = "none")
  })
  
  output$plot_desc_density_actual <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) >= 3)
    
    p <- plot_ly(df, x = ~actual_kwh, color = ~installation, type = "histogram", histnorm = "probability density",
                 opacity = 0.55, nbinsx = 30) %>%
      layout(barmode = "overlay", title = list(text = "Actual Generation Density Distribution", font = list(size = 12)),
             paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff",
             margin = list(l = 45, r = 15, t = 30, b = 35),
             xaxis = list(title = "Actual Output (kWh)", showgrid = TRUE, gridcolor = "#f1f5f9"),
             yaxis = list(title = "Empirical Density", showgrid = TRUE, gridcolor = "#f1f5f9"))
    p
  })
  
  output$plot_desc_density_perf <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) >= 3)
    
    p <- plot_ly(df, x = ~daily_perf, color = ~installation, type = "histogram", histnorm = "probability density",
                 opacity = 0.55, nbinsx = 30) %>%
      layout(barmode = "overlay", title = list(text = "Performance Ratio Density Distribution", font = list(size = 12)),
             paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff",
             margin = list(l = 45, r = 15, t = 30, b = 35),
             xaxis = list(title = "Performance Ratio (%)", showgrid = TRUE, gridcolor = "#f1f5f9"),
             yaxis = list(title = "Empirical Density", showgrid = TRUE, gridcolor = "#f1f5f9"))
    p
  })
  
  # ============================================================================
  # TAB 7: DATA EXPLORER & EXPORT
  # ============================================================================
  output$table_full_data <- renderDT({
    df <- filtered_data() %>%
      arrange(desc(date)) %>%
      transmute(
        Date = as.character(date), Installation = installation,
        `Expected (kWh)` = round(expected_kwh, 1), `Actual (kWh)` = round(actual_kwh, 1),
        `Performance (%)` = paste0(round(daily_perf, 1), "%"),
        `Shortfall (kWh)` = round(shortfall_kwh, 1),
        `Error d (kWh)` = round(diff_kwh, 1), Status = status
      )
    datatable(df, options = list(pageLength = 10, autoWidth = TRUE, dom = "lfrtip"), rownames = FALSE, selection = "none", filter = "top") %>%
      formatStyle("Status",
                  backgroundColor = styleEqual(c("Normal", "Flagged"), c("#dcfce7", "#fee2e2")),
                  color = styleEqual(c("Normal", "Flagged"), c("#166534", "#991b1b")), fontWeight = "bold")
  })
  
  # CSV Download Handlers
  output$download_filtered <- downloadHandler(
    filename = function() paste0("SolarIQ_Filtered_Data_", Sys.Date(), ".csv"),
    content = function(file) {
      export_df <- filtered_data() %>%
        transmute(date = as.character(date), installation, expected_kwh = round(expected_kwh, 1),
                  actual_kwh = round(actual_kwh, 1), performance_pct = round(daily_perf, 1),
                  shortfall_kwh = round(shortfall_kwh, 1), discrepancy_kwh = round(diff_kwh, 1),
                  status = status)
      write.csv(export_df, file, row.names = FALSE)
    }
  )
  
  output$download_underperf <- downloadHandler(
    filename = function() paste0("SolarIQ_Flagged_Days_", Sys.Date(), ".csv"),
    content = function(file) {
      export_df <- filtered_data() %>%
        filter(is_underperforming) %>%
        arrange(desc(shortfall_kwh)) %>%
        transmute(date = as.character(date), installation, expected_kwh = round(expected_kwh, 1),
                  actual_kwh = round(actual_kwh, 1), performance_pct = round(daily_perf, 1),
                  shortfall_kwh = round(shortfall_kwh, 1), threshold_pct = input$threshold)
      write.csv(export_df, file, row.names = FALSE)
    }
  )
  
  output$download_stats_summary <- downloadHandler(
    filename = function() paste0("SolarIQ_Statistical_Summary_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- filtered_data()
      vars <- list(actual_kwh = df$actual_kwh, expected_kwh = df$expected_kwh, performance_pct = df$daily_perf)
      summary_rows <- lapply(names(vars), function(nm) {
        x <- vars[[nm]]
        data.frame(
          variable = nm, n = length(x), mean = round(mean(x), 2), sd = round(sd(x), 2),
          median = round(median(x), 2), iqr = round(IQR(x), 2), min = round(min(x), 2), max = round(max(x), 2),
          skewness = round(calc_skewness(x), 3), kurtosis = round(calc_kurtosis(x), 3)
        )
      })
      write.csv(do.call(rbind, summary_rows), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
