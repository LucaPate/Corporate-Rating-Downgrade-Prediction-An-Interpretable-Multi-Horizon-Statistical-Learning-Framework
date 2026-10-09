# Step 10 - Produce descriptive appendix tables and figures.
# GitHub-ready copy: paths are repository-relative and data files are intentionally excluded.

## This script is intentionally separate from the manuscript. It creates
## appendix-ready CSV, LaTeX tables, a figure copy, and a paste-ready snippet.

required_packages <- c("dplyr", "readr", "tidyr", "ggplot2", "scales", "knitr", "lubridate")
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0L) {
  stop("Install missing package(s): ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(knitr)
})

options(stringsAsFactors = FALSE)

get_script_dir <- function() {
  file_arg <- "--file="
  args <- commandArgs(trailingOnly = FALSE)
  script_arg <- args[startsWith(args, file_arg)]
  if (length(script_arg) > 0L) {
    return(dirname(normalizePath(sub(file_arg, "", script_arg[[1]]), winslash = "/", mustWork = TRUE)))
  }
  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(dirname(normalizePath(sys.frames()[[1]]$ofile, winslash = "/", mustWork = TRUE)))
  }
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

fmt_int <- function(x) formatC(as.integer(round(x)), format = "d", big.mark = ",")
fmt_num <- function(x, digits = 2) ifelse(is.na(x), "", formatC(x, format = "f", digits = digits, big.mark = ","))
fmt_pct <- function(x, digits = 2) ifelse(is.na(x), "", paste0(formatC(100 * x, format = "f", digits = digits), "\\%"))

write_csv_and_tex <- function(data, base_name, align = NULL) {
  csv_path <- file.path(AUDIT_DIR, paste0(base_name, ".csv"))
  tex_path <- file.path(TABLE_DIR, paste0(base_name, ".tex"))
  readr::write_csv(data, csv_path)
  writeLines(
    knitr::kable(data, format = "latex", booktabs = TRUE, align = align, escape = FALSE),
    tex_path
  )
  invisible(list(csv = csv_path, tex = tex_path))
}

SCRIPT_DIR <- get_script_dir()
V5_DIR <- normalizePath(file.path(SCRIPT_DIR, ".."), winslash = "/", mustWork = TRUE)
OUTPUT_DIR <- file.path(V5_DIR, "Sample_Descriptive_Appendix_Outputs")
AUDIT_DIR <- file.path(OUTPUT_DIR, "audits")
TABLE_DIR <- file.path(OUTPUT_DIR, "tables_latex")
FIGURE_DIR <- file.path(OUTPUT_DIR, "figures")
SNIPPET_DIR <- file.path(OUTPUT_DIR, "latex_snippets")
dir.create(AUDIT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TABLE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIGURE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(SNIPPET_DIR, recursive = TRUE, showWarnings = FALSE)

PANEL_RDS <- file.path(V5_DIR, "Part_3_Outputs", "data", "panel_europe_v5_rating_macro_with_downgrade.rds")
QUARTERLY_FIGURE_SOURCE <- file.path(
  V5_DIR,
  "Part_3_Outputs",
  "figures",
  "fig_01_quarterly_downgrade_count_v5.png"
)

if (!file.exists(PANEL_RDS)) {
  stop("Missing final panel RDS: ", PANEL_RDS, call. = FALSE)
}

panel <- readRDS(PANEL_RDS) %>%
  mutate(
    Dates = as.Date(Dates),
    Downgrade = as.integer(Downgrade),
    downgrade_count = ifelse(is.na(downgrade_count), 0L, as.integer(downgrade_count))
  )

sample_overview <- tibble::tibble(
  Statistic = c(
    "Observation period",
    "Firms",
    "Countries",
    "Firm-quarter observations",
    "Median firm-quarter observations per firm",
    "Current-quarter downgrade observations",
    "Current-quarter downgrade actions",
    "Current-quarter downgrade rate"
  ),
  Value = c(
    paste0(format(min(panel$Dates, na.rm = TRUE), "%YQ"), lubridate::quarter(min(panel$Dates, na.rm = TRUE)),
           "--", format(max(panel$Dates, na.rm = TRUE), "%YQ"), lubridate::quarter(max(panel$Dates, na.rm = TRUE))),
    fmt_int(n_distinct(panel$firm_id)),
    fmt_int(n_distinct(panel$Country)),
    fmt_int(nrow(panel)),
    fmt_int(median(table(panel$firm_id))),
    fmt_int(sum(panel$Downgrade == 1L, na.rm = TRUE)),
    fmt_int(sum(panel$downgrade_count, na.rm = TRUE)),
    fmt_pct(mean(panel$Downgrade == 1L, na.rm = TRUE))
  )
)

rating_group_summary <- panel %>%
  group_by(rating_group) %>%
  summarise(
    Firms = n_distinct(firm_id),
    `Firm-quarters` = n(),
    `Observation share` = n() / nrow(panel),
    `Downgrade observations` = sum(Downgrade == 1L, na.rm = TRUE),
    `Downgrade actions` = sum(downgrade_count, na.rm = TRUE),
    `Downgrade rate` = mean(Downgrade == 1L, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(match(rating_group, c("AAA", "AA", "A", "BBB", "BB", "B", "CCC", "CC", "C")), rating_group) %>%
  transmute(
    `Rating group` = rating_group,
    Firms = fmt_int(Firms),
    `Firm-quarters` = fmt_int(`Firm-quarters`),
    `Observation share` = fmt_pct(`Observation share`),
    `Downgrade observations` = fmt_int(`Downgrade observations`),
    `Downgrade actions` = fmt_int(`Downgrade actions`),
    `Downgrade rate` = fmt_pct(`Downgrade rate`)
  )

country_summary <- panel %>%
  group_by(Country) %>%
  summarise(
    Firms = n_distinct(firm_id),
    `Firm-quarters` = n(),
    `Observation share` = n() / nrow(panel),
    `Downgrade observations` = sum(Downgrade == 1L, na.rm = TRUE),
    `Downgrade actions` = sum(downgrade_count, na.rm = TRUE),
    `Downgrade rate` = mean(Downgrade == 1L, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(as.integer(`Firm-quarters`)), Country) %>%
  transmute(
    Country,
    Firms = fmt_int(Firms),
    `Firm-quarters` = fmt_int(`Firm-quarters`),
    `Observation share` = fmt_pct(`Observation share`),
    `Downgrade observations` = fmt_int(`Downgrade observations`),
    `Downgrade actions` = fmt_int(`Downgrade actions`),
    `Downgrade rate` = fmt_pct(`Downgrade rate`)
  )

summary_variable_labels <- c(
  rating_rank = "Rating rank",
  ta = "Total assets",
  td = "Total debt",
  mc = "Market capitalization",
  wc_ta = "Working capital / total assets",
  ebit_ta = "EBIT / total assets",
  td_ta = "Total debt / total assets",
  mc_ta = "Market capitalization / total assets",
  fin_lev = "Financial leverage",
  ROA_pc = "Return on assets",
  current_ratio = "Current ratio",
  ebitda_tie = "EBITDA interest coverage",
  gdp_real_yoy = "Real GDP growth",
  unemployment_rate_q = "Unemployment rate",
  hicp_inflation_yoy_q = "HICP inflation",
  long_term_gov_yield_q = "Long-term government yield",
  public_debt_gdp_q = "Public debt / GDP",
  ciss_country_or_euro_q_mean = "CISS",
  vstoxx_q_mean = "VSTOXX"
)

summary_vars <- intersect(names(summary_variable_labels), names(panel))

numeric_summary <- lapply(summary_vars, function(var_name) {
  x <- panel[[var_name]]
  x <- x[!is.na(x)]
  tibble::tibble(
    Variable = unname(summary_variable_labels[[var_name]]),
    Obs = length(x),
    Mean = ifelse(length(x) == 0L, NA_real_, mean(x)),
    SD = ifelse(length(x) <= 1L, NA_real_, sd(x)),
    P25 = ifelse(length(x) == 0L, NA_real_, unname(quantile(x, 0.25))),
    Median = ifelse(length(x) == 0L, NA_real_, median(x)),
    P75 = ifelse(length(x) == 0L, NA_real_, unname(quantile(x, 0.75)))
  )
}) %>%
  bind_rows() %>%
  mutate(
    Obs = fmt_int(Obs),
    across(c(Mean, SD, P25, Median, P75), ~ fmt_num(.x, digits = 3))
  )

downgrades_by_year <- panel %>%
  group_by(year) %>%
  summarise(
    Firms = n_distinct(firm_id),
    `Firm-quarters` = n(),
    `Downgrade observations` = sum(Downgrade == 1L, na.rm = TRUE),
    `Downgrade actions` = sum(downgrade_count, na.rm = TRUE),
    `Downgrade rate` = mean(Downgrade == 1L, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year) %>%
  transmute(
    Year = year,
    Firms = fmt_int(Firms),
    `Firm-quarters` = fmt_int(`Firm-quarters`),
    `Downgrade observations` = fmt_int(`Downgrade observations`),
    `Downgrade actions` = fmt_int(`Downgrade actions`),
    `Downgrade rate` = fmt_pct(`Downgrade rate`)
  )

write_csv_and_tex(sample_overview, "table_A1_sample_overview_v5", align = c("l", "r"))
write_csv_and_tex(rating_group_summary, "table_A2_rating_group_composition_v5", align = c("l", rep("r", 6)))
write_csv_and_tex(numeric_summary, "table_A3_selected_summary_statistics_v5", align = c("l", rep("r", 6)))
write_csv_and_tex(country_summary, "table_A4_country_composition_v5", align = c("l", rep("r", 6)))
write_csv_and_tex(downgrades_by_year, "table_A5_downgrades_by_year_v5", align = c("r", rep("r", 5)))

quarterly_figure_dest <- file.path(FIGURE_DIR, "fig_A1_quarterly_downgrade_count_v5.png")
if (file.exists(QUARTERLY_FIGURE_SOURCE)) {
  invisible(file.copy(QUARTERLY_FIGURE_SOURCE, quarterly_figure_dest, overwrite = TRUE))
} else {
  quarterly_counts <- panel %>%
    group_by(Dates, quarter) %>%
    summarise(
      Downgrade_Observations = sum(Downgrade == 1L, na.rm = TRUE),
      Firm_Quarters = n(),
      .groups = "drop"
    )
  ggplot(quarterly_counts, aes(x = Dates, y = Downgrade_Observations)) +
    geom_col(fill = "#4472C4") +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    scale_y_continuous(labels = comma) +
    labs(x = "Quarter", y = "Downgrade observations", title = "Quarterly downgrade count") +
    theme_minimal(base_size = 11)
  ggsave(quarterly_figure_dest, width = 9, height = 5.2, dpi = 300)
}

snippet_lines <- c(
  "% Main-text sentence for the data section:",
  "Additional descriptive evidence on the final empirical sample is reported in Appendix~\\ref{app:sample_description}, including the time profile of downgrade events, rating-group composition, selected summary statistics, geographic coverage, and yearly downgrade counts.",
  "",
  "% Appendix text:",
  "\\bmsection{Descriptive sample evidence\\label{app:sample_description}}",
  "",
  "This appendix provides additional descriptive evidence on the final firm-quarter panel used in the empirical analysis. Figure~\\ref{fig:app_quarterly_downgrade_count} reports the quarter-by-quarter number of downgrade observations, while Tables~\\ref{tab:app_sample_overview}--\\ref{tab:app_downgrades_by_year} summarize the sample size, rating composition, main firm-level and macro-financial variables, country coverage, and downgrade counts by calendar year.",
  "",
  "\\begin{figure}[t]",
  "\\centering",
  "\\includegraphics[width=0.92\\textwidth]{Sample_Descriptive_Appendix_Outputs/figures/fig_A1_quarterly_downgrade_count_v5.png}",
  "\\caption{Quarterly downgrade observations in the final sample. The figure reports the number of firm-quarter observations with at least one S\\&P long-term issuer-rating downgrade in the reference quarter.}",
  "\\label{fig:app_quarterly_downgrade_count}",
  "\\end{figure}",
  "",
  "\\begin{table}[t]",
  "\\centering",
  "\\caption{Final sample overview.}",
  "\\label{tab:app_sample_overview}",
  "\\input{Sample_Descriptive_Appendix_Outputs/tables_latex/table_A1_sample_overview_v5.tex}",
  "\\end{table}",
  "",
  "\\begin{table*}[t]",
  "\\centering",
  "\\caption{Sample composition by rating group.}",
  "\\label{tab:app_rating_group_composition}",
  "\\resizebox{\\textwidth}{!}{\\input{Sample_Descriptive_Appendix_Outputs/tables_latex/table_A2_rating_group_composition_v5.tex}}",
  "\\vspace{3pt}",
  "\\begin{minipage}{0.95\\textwidth}\\footnotesize Notes: downgrade observations count firm-quarters with at least one downgrade; downgrade actions count all downward rating actions occurring in those quarters.\\end{minipage}",
  "\\end{table*}",
  "",
  "\\begin{table*}[t]",
  "\\centering",
  "\\caption{Selected summary statistics.}",
  "\\label{tab:app_summary_statistics}",
  "\\resizebox{\\textwidth}{!}{\\input{Sample_Descriptive_Appendix_Outputs/tables_latex/table_A3_selected_summary_statistics_v5.tex}}",
  "\\vspace{3pt}",
  "\\begin{minipage}{0.95\\textwidth}\\footnotesize Notes: statistics are computed at the firm-quarter level on the final rated panel before horizon-specific target censoring. Accounting variables are reported in the units of the source data; ratios and macro-financial variables follow the definitions in the data section.\\end{minipage}",
  "\\end{table*}",
  "",
  "\\begin{table*}[t]",
  "\\centering",
  "\\caption{Geographic composition of the final sample.}",
  "\\label{tab:app_country_composition}",
  "\\resizebox{\\textwidth}{!}{\\input{Sample_Descriptive_Appendix_Outputs/tables_latex/table_A4_country_composition_v5.tex}}",
  "\\end{table*}",
  "",
  "\\begin{table*}[t]",
  "\\centering",
  "\\caption{Downgrade observations by calendar year.}",
  "\\label{tab:app_downgrades_by_year}",
  "\\resizebox{\\textwidth}{!}{\\input{Sample_Descriptive_Appendix_Outputs/tables_latex/table_A5_downgrades_by_year_v5.tex}}",
  "\\end{table*}"
)

snippet_path <- file.path(SNIPPET_DIR, "descriptive_sample_appendix_snippet.tex")
writeLines(snippet_lines, snippet_path)

manifest <- tibble::tibble(
  Output = c(
    "Sample overview table",
    "Rating-group composition table",
    "Selected summary statistics table",
    "Country composition table",
    "Downgrades by year table",
    "Quarterly downgrade-count figure",
    "Paste-ready LaTeX snippet"
  ),
  Path = c(
    file.path(TABLE_DIR, "table_A1_sample_overview_v5.tex"),
    file.path(TABLE_DIR, "table_A2_rating_group_composition_v5.tex"),
    file.path(TABLE_DIR, "table_A3_selected_summary_statistics_v5.tex"),
    file.path(TABLE_DIR, "table_A4_country_composition_v5.tex"),
    file.path(TABLE_DIR, "table_A5_downgrades_by_year_v5.tex"),
    quarterly_figure_dest,
    snippet_path
  )
)
readr::write_csv(manifest, file.path(AUDIT_DIR, "table_A0_descriptive_appendix_manifest_v5.csv"))

message("Sample descriptive appendix materials written to: ", OUTPUT_DIR)

