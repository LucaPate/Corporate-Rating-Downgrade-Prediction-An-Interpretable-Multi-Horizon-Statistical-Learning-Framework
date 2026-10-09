# Step 03 - Merge firm, rating, macro, systemic-risk, and downgrade-target data.
# GitHub-ready copy: paths are repository-relative and data files are intentionally excluded.

## The modelling manifest is narrowed to the definitive benchmark and main specification.

required_packages <- c("readxl", "dplyr", "stringr", "lubridate", "readr", "tidyr", "purrr", "ggplot2", "scales", "knitr")
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0L) {
  stop("Install missing package(s): ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(lubridate)
  library(readr)
  library(tidyr)
  library(purrr)
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

first_existing <- function(paths) {
  existing <- paths[file.exists(paths)]
  if (length(existing) == 0L) {
    stop("None of these files exists: ", paste(paths, collapse = " | "), call. = FALSE)
  }
  existing[[1]]
}

SCRIPT_DIR <- get_script_dir()
V5_DIR <- normalizePath(file.path(SCRIPT_DIR, ".."), winslash = "/", mustWork = TRUE)
INPUT_DIR <- file.path(V5_DIR, "input")

PANEL_FILE <- first_existing(file.path(INPUT_DIR, "Panel Europe.xlsx"))
RATING_PANEL_RDS <- file.path(V5_DIR, "Part_1_Outputs", "data", "company_quarter_rating_panel_v5.rds")
MACRO_RDS <- file.path(V5_DIR, "Part_2_Outputs", "data", "final_macro_dataset_v5.rds")

if (!file.exists(RATING_PANEL_RDS)) {
  stop("Missing Part 1 output: ", RATING_PANEL_RDS, call. = FALSE)
}
if (!file.exists(MACRO_RDS)) {
  stop("Missing Part 2 output: ", MACRO_RDS, call. = FALSE)
}

OUTPUT_DIR <- file.path(V5_DIR, "Part_3_Outputs")
DATA_DIR <- file.path(OUTPUT_DIR, "data")
RAW_DIR <- file.path(OUTPUT_DIR, "data_raw")
IMPUTED_DIR <- file.path(OUTPUT_DIR, "data_imputed")
AUDIT_DIR <- file.path(OUTPUT_DIR, "audits")
FIGURE_DIR <- file.path(OUTPUT_DIR, "figures")
TABLE_DIR <- file.path(OUTPUT_DIR, "tables_latex")
dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(IMPUTED_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(AUDIT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIGURE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TABLE_DIR, recursive = TRUE, showWarnings = FALSE)

main_plot_colours <- c(
  blue = "#4472C4",
  navy = "#182642",
  orange = "#ED7D31"
)

main_plot_palette <- function(n) {
  unname(rep(main_plot_colours, length.out = n))
}

raw_financial_vars <- c("wc", "ta", "re", "ebit", "td", "s", "mc")
financial_ratio_vars <- c("fin_lev", "ROA_pc", "current_ratio", "ebitda_tie", "env_score", "g_score")
macro_vars <- c(
  "gdp_real_yoy",
  "d_gdp_real_yoy_qoq_pp",
  "d_gdp_real_yoy_yoy_pp",
  "industrial_production_index_q",
  "industrial_production_growth_yoy",
  "industrial_production_growth_qoq",
  "d_industrial_production_growth_qoq_pp",
  "unemployment_rate_q",
  "d_unemployment_qoq_pp",
  "d_unemployment_yoy_pp",
  "hicp_inflation_yoy_q",
  "d_hicp_inflation_qoq_pp",
  "d_hicp_inflation_yoy_pp",
  "long_term_gov_yield_q",
  "d_long_term_gov_yield_qoq_pp",
  "d_long_term_gov_yield_yoy_pp",
  "public_debt_gdp_q",
  "d_public_debt_gdp_qoq_pp",
  "d_public_debt_gdp_yoy_pp"
)

systemic_vars <- c(
  "ciss_euro_area_q_mean",
  "ciss_euro_area_q_max",
  "ciss_euro_area_q_sd",
  "ciss_euro_area_q_p90",
  "ciss_country_q_mean",
  "ciss_country_q_max",
  "ciss_country_q_sd",
  "ciss_country_q_p90",
  "ciss_country_or_euro_q_mean",
  "ciss_country_or_euro_q_max",
  "ciss_country_or_euro_q_sd",
  "ciss_country_or_euro_q_p90",
  "d_ciss_country_or_euro_qoq",
  "d_ciss_country_or_euro_yoy",
  "ciss_country_or_euro_q_mean_lag1",
  "ciss_country_or_euro_q_mean_lag2",
  "mean_ciss_country_or_euro_last_4q",
  "max_ciss_country_or_euro_last_4q",
  "vstoxx_q_mean",
  "vstoxx_q_max",
  "vstoxx_q_sd",
  "vstoxx_q_p90",
  "d_vstoxx_qoq",
  "d_vstoxx_yoy",
  "vstoxx_q_mean_lag1",
  "vstoxx_q_mean_lag2",
  "high_vstoxx_90",
  "mean_vstoxx_last_4q",
  "max_vstoxx_last_4q",
  "p90_vstoxx_last_4q",
  "n_high_vstoxx_last_4q"
)

macro_vars <- c(macro_vars, systemic_vars)

HORIZONS <- 1:4
TEST_START <- as.Date("2019-03-31")
VALIDATION_YEARS <- 2012:2018
ROLLING_TRAIN_YEARS <- 8

# Firm-size diagnostics use total assets after imputation. The default split is
# the sample median, a standard size divisor in empirical credit-risk work; set
# SIZE_BUCKET_COUNT to 3 for terciles if a three-way split is preferred.
SIZE_VARIABLE <- "ta"
SIZE_LOG_TRANSFORM <- "auto" # "auto", "log", or "log1p"
SIZE_BUCKET_COUNT <- 2L
SIZE_BUCKET_REFERENCE_ROLES <- c("Training")

ZERO_DENOMINATOR_TOLERANCE <- 1e-12
ZERO_DENOMINATOR_CAP_PROB <- 0.99

computed_ratio_vars <- c(
  "wc_ta",
  "re_ta",
  "ebit_ta",
  "td_ta",
  "s_ta",
  "mc_ta",
  "mc_td",
  "td_market_value"
)
existing_ratio_vars_for_models <- c("fin_lev", "ROA_pc", "current_ratio", "ebitda_tie")
rating_vars_for_carry <- c("rating", "rating_rank", "rating_number", "rating_group", "macro_rating", "rating_grade")
rating_history_vars <- c(
  "Downgrade_Previous_4Q",
  "N_Downgrade_Quarters_Previous_4Q",
  "N_Downgrades_Previous_4Q",
  "N_Downgrade_Events_Previous_4Q",
  "Notches_Lost_Previous_4Q",
  "N_Rating_Actions_Previous_4Q",
  "N_Upgrades_Previous_4Q"
)
rating_history_bucket_vars <- c("N_Downgrades_Previous_4Q_Bucket")
seasonality_vars <- character(0)
parsimonious_macro_vars <- c(
  "gdp_real_yoy",
  "industrial_production_growth_yoy",
  "hicp_inflation_yoy_q",
  "d_unemployment_yoy_pp",
  "d_long_term_gov_yield_yoy_pp",
  "d_public_debt_gdp_yoy_pp"
)
change_macro_vars <- c(
  "d_gdp_real_yoy_qoq_pp",
  "industrial_production_growth_qoq",
  "d_unemployment_qoq_pp",
  "d_hicp_inflation_qoq_pp",
  "d_long_term_gov_yield_qoq_pp",
  "d_public_debt_gdp_qoq_pp"
)
parsimonious_ciss_vars <- c(
  "ciss_country_or_euro_q_mean",
  "d_ciss_country_or_euro_qoq"
)
parsimonious_vstoxx_vars <- c(
  "vstoxx_q_mean",
  "d_vstoxx_qoq",
  "n_high_vstoxx_last_4q"
)
parsimonious_systemic_vars <- c(parsimonious_ciss_vars, parsimonious_vstoxx_vars)

rating_history_model_vars <- c(
  "Downgrade_Previous_4Q",
  "N_Downgrade_Events_Previous_4Q",
  "Notches_Lost_Previous_4Q",
  "N_Upgrades_Previous_4Q"
)

ratio_specs <- tribble(
  ~Variable, ~Numerator, ~Denominator,
  "wc_ta", "wc", "ta",
  "re_ta", "re", "ta",
  "ebit_ta", "ebit", "ta",
  "td_ta", "td", "ta",
  "s_ta", "s", "ta",
  "mc_ta", "mc", "ta",
  "mc_td", "mc", "td",
  "td_market_value", "td", "td_plus_mc"
)

quarter_end_candidates <- function(date_value) {
  candidate_years <- seq(year(date_value) - 1L, year(date_value) + 1L)
  as.Date(unlist(lapply(
    candidate_years,
    function(y) {
      c(
        sprintf("%04d-03-31", y),
        sprintf("%04d-06-30", y),
        sprintf("%04d-09-30", y),
        sprintf("%04d-12-31", y)
      )
    }
  )))
}

nearest_quarter_end <- function(date_value) {
  as.Date(vapply(
    date_value,
    function(d) {
      if (is.na(d)) {
        return(NA_real_)
      }
      d <- as.Date(d, origin = "1970-01-01")
      candidates <- quarter_end_candidates(d)
      as.numeric(candidates[which.min(abs(as.integer(candidates - d)))])
    },
    numeric(1)
  ), origin = "1970-01-01")
}

date_to_nearest_quarter <- function(date_value) {
  q_end <- nearest_quarter_end(date_value)
  sprintf("%d Q%d", year(q_end), quarter(q_end))
}

normalise_quarter <- function(x) {
  x_chr <- str_squish(as.character(x))
  x_chr[x_chr == ""] <- NA_character_

  already_quarter <- str_detect(x_chr, "^\\d{4}\\s*Q[1-4]$")
  out <- rep(NA_character_, length(x_chr))
  out[already_quarter] <- str_replace_all(x_chr[already_quarter], "\\s+", " ")

  needs_date_parse <- !already_quarter & !is.na(x_chr)
  date_values <- rep(as.Date(NA), length(x_chr))

  if (any(needs_date_parse)) {
    parsed_dates <- suppressWarnings(as.Date(parse_date_time(
      x_chr[needs_date_parse],
      orders = c("ymd HMS", "ymd", "dmy HMS", "dmy", "mdy HMS", "mdy"),
      tz = "UTC"
    )))

    numeric_dates <- suppressWarnings(as.numeric(x_chr[needs_date_parse]))
    numeric_date_rows <- is.na(parsed_dates) & !is.na(numeric_dates)
    parsed_dates[numeric_date_rows] <- as.Date(numeric_dates[numeric_date_rows], origin = "1899-12-30")

    date_values[needs_date_parse] <- parsed_dates
  }

  parsed_ok <- !already_quarter & !is.na(date_values)
  out[parsed_ok] <- date_to_nearest_quarter(date_values[parsed_ok])
  out
}

quarter_to_end_date <- function(quarter_label) {
  year_value <- as.integer(str_extract(quarter_label, "^\\d{4}"))
  quarter_value <- as.integer(str_extract(quarter_label, "(?<=Q)[1-4]"))
  quarter_start <- as.Date(sprintf("%04d-%02d-01", year_value, (quarter_value - 1L) * 3L + 1L))
  quarter_start %m+% months(3L) - days(1L)
}

save_audit <- function(x, filename) {
  write_csv(x, file.path(AUDIT_DIR, filename), na = "")
}

write_dataset <- function(x, directory, name, csv = FALSE) {
  saveRDS(x, file.path(directory, paste0(name, ".rds")))
  if (isTRUE(csv)) {
    write_csv(x, file.path(directory, paste0(name, ".csv")), na = "")
  }
  invisible(x)
}

median_or_na <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) NA_real_ else median(x, na.rm = TRUE)
}

quantile_or_na <- function(x, prob) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) {
    NA_real_
  } else {
    as.numeric(quantile(x, probs = prob, na.rm = TRUE, names = FALSE))
  }
}

safe_cor <- function(x, y) {
  keep <- is.finite(x) & is.finite(y)
  sx <- if (sum(keep) >= 3L) sd(x[keep]) else NA_real_
  sy <- if (sum(keep) >= 3L) sd(y[keep]) else NA_real_
  if (sum(keep) < 3L || !is.finite(sx) || !is.finite(sy) || sx == 0 || sy == 0) {
    NA_real_
  } else {
    cor(x[keep], y[keep])
  }
}

safe_ks_pvalue <- function(x, y) {
  x <- x[is.finite(x)]
  y <- y[is.finite(y)]
  if (length(x) < 5L || length(y) < 5L) {
    NA_real_
  } else {
    suppressWarnings(ks.test(x, y)$p.value)
  }
}

safe_quantile_value <- function(x, prob) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) NA_real_ else as.numeric(quantile(x, prob, na.rm = TRUE, names = FALSE))
}

safe_mean <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) NA_real_ else mean(x)
}

safe_sd_value <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) <= 1L) NA_real_ else sd(x)
}

outlier_audit_table <- function(data, variables) {
  purrr::map_dfr(variables, function(variable) {
    x <- as.numeric(data[[variable]])
    p01 <- safe_quantile_value(x, 0.01)
    p05 <- safe_quantile_value(x, 0.05)
    p95 <- safe_quantile_value(x, 0.95)
    p99 <- safe_quantile_value(x, 0.99)
    finite_x <- x[is.finite(x)]
    min_x <- if (length(finite_x) > 0L) min(finite_x) else NA_real_
    max_x <- if (length(finite_x) > 0L) max(finite_x) else NA_real_
    winsor_mean <- if (length(finite_x) > 0L && is.finite(p01) && is.finite(p99)) {
      mean(pmin(pmax(finite_x, p01), p99), na.rm = TRUE)
    } else {
      NA_real_
    }
    tibble(
      Variable = variable,
      N = length(x),
      N_Finite = length(finite_x),
      Missing_Rate = mean(is.na(x) | !is.finite(x)),
      Mean = safe_mean(x),
      SD = safe_sd_value(x),
      Min = min_x,
      P01 = p01,
      P05 = p05,
      Median = safe_quantile_value(x, 0.50),
      P95 = p95,
      P99 = p99,
      Max = max_x,
      N_Below_P01 = sum(is.finite(x) & x < p01, na.rm = TRUE),
      N_Above_P99 = sum(is.finite(x) & x > p99, na.rm = TRUE),
      Winsorized_Mean_P01_P99 = winsor_mean
    )
  })
}

correlation_pair_table <- function(data, variables) {
  variables <- variables[vapply(variables, function(v) is.numeric(data[[v]]), logical(1))]
  if (length(variables) < 2L) {
    return(tibble())
  }
  combn(variables, 2L, simplify = FALSE) %>%
    purrr::map_dfr(function(pair) {
      x <- data[[pair[[1]]]]
      y <- data[[pair[[2]]]]
      keep <- is.finite(x) & is.finite(y)
      tibble(
        Variable_1 = pair[[1]],
        Variable_2 = pair[[2]],
        Correlation = safe_cor(x, y),
        Abs_Correlation = abs(Correlation),
        N_Pairs = sum(keep)
      )
    }) %>%
    arrange(desc(Abs_Correlation), Variable_1, Variable_2)
}

distribution_shift_table <- function(data, variables, group_var, reference_level, comparison_level, by_vars = character()) {
  variables <- variables[vapply(variables, function(v) is.numeric(data[[v]]), logical(1))]
  purrr::map_dfr(variables, function(variable) {
    grouped <- if (length(by_vars) > 0L) {
      data %>% group_by(across(all_of(by_vars))) %>% group_split()
    } else {
      list(data)
    }

    purrr::map_dfr(grouped, function(g) {
      x_ref <- g[[variable]][g[[group_var]] == reference_level]
      x_cmp <- g[[variable]][g[[group_var]] == comparison_level]
      pooled_sd <- sqrt((stats::var(x_ref, na.rm = TRUE) + stats::var(x_cmp, na.rm = TRUE)) / 2)
      keys <- if (length(by_vars) > 0L) g %>% slice(1L) %>% select(all_of(by_vars)) else tibble()
      bind_cols(
        keys,
        tibble(
          Variable = variable,
          Reference_Group = reference_level,
          Comparison_Group = comparison_level,
          N_Reference = sum(is.finite(x_ref)),
          N_Comparison = sum(is.finite(x_cmp)),
          Mean_Reference = safe_mean(x_ref),
          Mean_Comparison = safe_mean(x_cmp),
          Difference = Mean_Comparison - Mean_Reference,
          Standardized_Difference = if_else(is.finite(pooled_sd) & pooled_sd > 0, Difference / pooled_sd, NA_real_),
          Median_Reference = safe_quantile_value(x_ref, 0.50),
          Median_Comparison = safe_quantile_value(x_cmp, 0.50),
          KS_P_Value = safe_ks_pvalue(x_ref, x_cmp)
        )
      )
    })
  })
}

within_firm_lag_correlation <- function(data, variables, lag_n) {
  purrr::map_dfr(variables, function(variable) {
    lagged <- data %>%
      select(Country, firm_id, Quarter_Index, all_of(variable)) %>%
      arrange(Country, firm_id, Quarter_Index) %>%
      group_by(Country, firm_id) %>%
      mutate(.lag_value = lag(.data[[variable]], lag_n)) %>%
      ungroup() %>%
      filter(is.finite(.data[[variable]]), is.finite(.lag_value))

    tibble(
      Variable = variable,
      Lag = lag_n,
      Within_Firm_Correlation = safe_cor(lagged[[variable]], lagged$.lag_value),
      N_Pairs = nrow(lagged),
      N_Firms = n_distinct(lagged$firm_id),
      N_Countries = n_distinct(lagged$Country)
    )
  })
}

icc_by_variable <- function(data, variables) {
  purrr::map_dfr(variables, function(variable) {
    clean <- data %>%
      select(Country, firm_id, all_of(variable)) %>%
      filter(is.finite(.data[[variable]])) %>%
      group_by(Country, firm_id) %>%
      mutate(.firm_mean = mean(.data[[variable]], na.rm = TRUE)) %>%
      ungroup()

    between_var <- if (n_distinct(clean$firm_id) > 1L) var(unique(clean %>% select(Country, firm_id, .firm_mean))$.firm_mean, na.rm = TRUE) else NA_real_
    within_var <- if (nrow(clean) > 1L) var(clean[[variable]] - clean$.firm_mean, na.rm = TRUE) else NA_real_
    icc <- between_var / (between_var + within_var)

    tibble(
      Variable = variable,
      ICC = icc,
      Sigma2_Between = between_var,
      Sigma2_Within = within_var,
      N_Observations = nrow(clean),
      N_Firms = n_distinct(clean$firm_id)
    )
  })
}

compute_raw_ratio <- function(data, variable, numerator, denominator) {
  numer <- data[[numerator]]
  denom <- data[[denominator]]
  regular <- !is.na(numer) &
    !is.na(denom) &
    is.finite(numer) &
    is.finite(denom) &
    abs(denom) > ZERO_DENOMINATOR_TOLERANCE
  zero_denom <- !is.na(numer) &
    !is.na(denom) &
    is.finite(numer) &
    is.finite(denom) &
    abs(denom) <= ZERO_DENOMINATOR_TOLERANCE
  ratio <- rep(NA_real_, length(numer))
  ratio[regular] <- numer[regular] / denom[regular]
  ratio[!is.finite(ratio)] <- NA_real_
  list(value = ratio, zero_denominator = zero_denom)
}

create_validation_calendar <- function(validation_years) {
  tibble(
    Fold = paste0("Validation_", validation_years),
    Validation_Start = as.Date(sprintf("%s-01-01", validation_years)),
    Validation_End = as.Date(sprintf("%s-01-01", validation_years + 1L))
  )
}

create_expanding_partition <- function(data, fold_name, validation_start, validation_end) {
  data %>%
    filter(Temporal_Sample == "Pre_Test") %>%
    mutate(
      Split_Scheme = "Expanding_Window",
      Fold = fold_name,
      Validation_Start = validation_start,
      Validation_End = validation_end,
      Fold_Role = case_when(
        Target_Date < validation_start ~ "Training",
        Target_Date >= validation_start & Target_Date < validation_end ~ "Validation",
        Target_Date >= validation_end & Target_Date < TEST_START ~ "Future_Pre_Test",
        TRUE ~ "Unassigned"
      )
    )
}

create_rolling_partition <- function(data, fold_name, validation_start, validation_end) {
  training_start <- validation_start %m-% years(ROLLING_TRAIN_YEARS)
  data %>%
    filter(Temporal_Sample == "Pre_Test") %>%
    mutate(
      Split_Scheme = "Rolling_Window",
      Fold = paste0("Rolling_", fold_name),
      Validation_Start = validation_start,
      Validation_End = validation_end,
      Training_Start = training_start,
      Fold_Role = case_when(
        Target_Date >= training_start & Target_Date < validation_start ~ "Training",
        Target_Date >= validation_start & Target_Date < validation_end ~ "Validation",
        Target_Date < training_start ~ "Past_Outside_Window",
        Target_Date >= validation_end & Target_Date < TEST_START ~ "Future_Pre_Test",
        TRUE ~ "Unassigned"
      )
    )
}

fit_apply_fold_imputation <- function(data, split_scheme, fold, horizon, imputation_vars, model_predictor_vars = imputation_vars) {
  train <- data %>% filter(Fold_Role == "Training")
  if (nrow(train) == 0L) {
    stop("No training rows for ", split_scheme, " / ", fold, " / horizon ", horizon, call. = FALSE)
  }

  out <- data
  row_audit <- vector("list", length(imputation_vars))

  for (variable in imputation_vars) {
    raw_x <- out[[variable]]
    x <- raw_x
    raw_missing <- is.na(raw_x) | !is.finite(raw_x)
    x[!is.finite(x)] <- NA_real_

    ratio_row <- ratio_specs %>% filter(Variable == variable)
    zero_replaced <- rep(FALSE, nrow(out))

    # Zero-denominator ratio handling: use training-only tail caps before the
    # standard median fallback to preserve leakage control.
    if (nrow(ratio_row) == 1L) {
      zero_flag <- paste0(variable, "_zero_denominator")
      train_finite <- train[[variable]]
      train_finite <- train_finite[is.finite(train_finite)]
      positive_cap <- quantile_or_na(train_finite, ZERO_DENOMINATOR_CAP_PROB)
      negative_cap <- quantile_or_na(train_finite, 1 - ZERO_DENOMINATOR_CAP_PROB)
      if (is.na(positive_cap)) positive_cap <- median_or_na(train_finite)
      if (is.na(negative_cap)) negative_cap <- median_or_na(train_finite)
      if (is.na(positive_cap)) positive_cap <- 0
      if (is.na(negative_cap)) negative_cap <- 0

      numerator_var <- ratio_row$Numerator[[1]]
      if (zero_flag %in% names(out) && numerator_var %in% names(out)) {
        zero_replaced <- !is.na(out[[zero_flag]]) & out[[zero_flag]]
        x[zero_replaced & out[[numerator_var]] >= 0] <- positive_cap
        x[zero_replaced & out[[numerator_var]] < 0] <- negative_cap
      }
    }

    train_for_maps <- train %>%
      mutate(.x = .data[[variable]]) %>%
      mutate(.x = if_else(is.finite(.x), .x, NA_real_))

    firm_map <- train_for_maps %>%
      group_by(Country, firm_id) %>%
      summarise(.median = median_or_na(.x), .groups = "drop") %>%
      filter(!is.na(.median))

    country_map <- train_for_maps %>%
      group_by(Country) %>%
      summarise(.median = median_or_na(.x), .groups = "drop") %>%
      filter(!is.na(.median))

    global_median <- median_or_na(train_for_maps$.x)
    if (is.na(global_median)) global_median <- 0

    firm_key_out <- paste(out$Country, out$firm_id, sep = "__")
    firm_lookup <- setNames(firm_map$.median, paste(firm_map$Country, firm_map$firm_id, sep = "__"))
    country_lookup <- setNames(country_map$.median, country_map$Country)
    source <- if_else(raw_missing, "Missing_Raw", "Observed")

    missing_now <- is.na(x)
    firm_fill <- unname(firm_lookup[firm_key_out])
    use_firm <- missing_now & !is.na(firm_fill)
    x[use_firm] <- firm_fill[use_firm]
    source[use_firm] <- "Training_Firm_Median"

    missing_now <- is.na(x)
    country_fill <- unname(country_lookup[out$Country])
    use_country <- missing_now & !is.na(country_fill)
    x[use_country] <- country_fill[use_country]
    source[use_country] <- "Training_Country_Median"

    missing_now <- is.na(x)
    use_global <- missing_now
    x[use_global] <- global_median
    source[use_global] <- "Training_Global_Median"

    source[zero_replaced] <- "Training_Zero_Denominator_Cap"

    out[[variable]] <- x
    out[[paste0(variable, "_missing_raw")]] <- raw_missing

    row_audit[[variable]] <- tibble(
      Split_Scheme = split_scheme,
      Fold = fold,
      Horizon = horizon,
      Variable = variable,
      Training_Rows = nrow(train),
      Rows_Imputed = sum(source != "Observed"),
      Rows_Zero_Denominator_Cap = sum(source == "Training_Zero_Denominator_Cap"),
      Rows_Firm_Median = sum(source == "Training_Firm_Median"),
      Rows_Country_Median = sum(source == "Training_Country_Median"),
      Rows_Global_Median = sum(source == "Training_Global_Median"),
      Missing_After = sum(is.na(x)),
      Nonfinite_After = sum(!is.na(x) & !is.finite(x))
    )
  }

  out <- out %>%
    mutate(
      N_Missing_Model_Predictors_After_Imputation = rowSums(is.na(select(., all_of(model_predictor_vars)))),
      Any_Missing_Model_Predictor_After_Imputation = N_Missing_Model_Predictors_After_Imputation > 0L
    )

  list(data = out, audit = bind_rows(row_audit))
}

impute_by_fold <- function(data, imputation_vars, model_predictor_vars = imputation_vars) {
  grouped <- data %>%
    group_by(Split_Scheme, Fold, Horizon) %>%
    group_split()

  results <- purrr::map(grouped, function(g) {
    fit_apply_fold_imputation(
      data = g,
      split_scheme = unique(g$Split_Scheme),
      fold = unique(g$Fold),
      horizon = unique(g$Horizon),
      imputation_vars = imputation_vars,
      model_predictor_vars = model_predictor_vars
    )
  })

  list(
    data = bind_rows(purrr::map(results, "data")),
    audit = bind_rows(purrr::map(results, "audit"))
  )
}

choose_size_log_transform <- function(x, configured_transform = SIZE_LOG_TRANSFORM) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) {
    return("log")
  }
  if (configured_transform %in% c("log", "log1p")) {
    return(configured_transform)
  }
  if (!identical(configured_transform, "auto")) {
    stop("Unknown SIZE_LOG_TRANSFORM: ", configured_transform, call. = FALSE)
  }
  if (any(x == 0, na.rm = TRUE) && !any(x < 0, na.rm = TRUE)) {
    return("log1p")
  }
  "log"
}

compute_size_log_value <- function(x, transform_name) {
  case_when(
    transform_name == "log" & is.finite(x) & x > 0 ~ log(x),
    transform_name == "log1p" & is.finite(x) & x >= 0 ~ log1p(x),
    TRUE ~ NA_real_
  )
}

assign_size_buckets <- function(x, bucket_count = SIZE_BUCKET_COUNT, cutpoints = NULL) {
  if (is.null(cutpoints) || length(cutpoints) == 0L || all(is.na(cutpoints))) {
    return(factor(rep(NA_character_, length(x)), levels = c("Small", "Large")))
  }
  if (bucket_count == 2L) {
    return(factor(
      case_when(
        is.na(x) ~ NA_character_,
        x <= cutpoints[[1]] ~ "Small",
        TRUE ~ "Large"
      ),
      levels = c("Small", "Large")
    ))
  }
  if (bucket_count == 3L) {
    return(factor(
      case_when(
        is.na(x) ~ NA_character_,
        x <= cutpoints[[1]] ~ "Small",
        x <= cutpoints[[2]] ~ "Medium",
        TRUE ~ "Large"
      ),
      levels = c("Small", "Medium", "Large")
    ))
  }
  stop("SIZE_BUCKET_COUNT must be 2 or 3.", call. = FALSE)
}

add_size_bucket_to_group <- function(data) {
  if (!SIZE_VARIABLE %in% names(data)) {
    return(list(data = data, audit = tibble()))
  }

  ta_values <- data[[SIZE_VARIABLE]]
  transform_name <- choose_size_log_transform(ta_values)
  log_ta_values <- compute_size_log_value(ta_values, transform_name)
  reference_rows <- data$Fold_Role %in% SIZE_BUCKET_REFERENCE_ROLES & is.finite(log_ta_values)
  probs <- if (SIZE_BUCKET_COUNT == 2L) 0.50 else c(1 / 3, 2 / 3)
  cutpoints <- if (any(reference_rows)) {
    as.numeric(quantile(log_ta_values[reference_rows], probs = probs, na.rm = TRUE, names = FALSE))
  } else {
    rep(NA_real_, length(probs))
  }

  out <- data %>%
    mutate(
      log_ta_plain = compute_size_log_value(.data[[SIZE_VARIABLE]], "log"),
      log1p_ta = compute_size_log_value(.data[[SIZE_VARIABLE]], "log1p"),
      log_ta = log_ta_values,
      Size_Bucket = assign_size_buckets(log_ta_values, SIZE_BUCKET_COUNT, cutpoints)
    )

  audit <- tibble(
    Split_Scheme = unique(data$Split_Scheme),
    Fold = unique(data$Fold),
    Horizon = unique(data$Horizon),
    Horizon_Label = if ("Horizon_Label" %in% names(data)) unique(data$Horizon_Label)[[1]] else NA_character_,
    Size_Variable = SIZE_VARIABLE,
    Size_Log_Transform = transform_name,
    Size_Bucket_Count = SIZE_BUCKET_COUNT,
    N_Rows = nrow(data),
    N_Reference_Rows = sum(reference_rows),
    N_TA_Missing = sum(is.na(ta_values)),
    N_TA_Nonfinite = sum(!is.na(ta_values) & !is.finite(ta_values)),
    N_TA_Zero = sum(is.finite(ta_values) & ta_values == 0, na.rm = TRUE),
    N_TA_Negative = sum(is.finite(ta_values) & ta_values < 0, na.rm = TRUE),
    Min_TA = if_else(any(is.finite(ta_values)), min(ta_values[is.finite(ta_values)]), NA_real_),
    Median_TA = median_or_na(ta_values),
    Max_TA = if_else(any(is.finite(ta_values)), max(ta_values[is.finite(ta_values)]), NA_real_),
    Cutpoint_1 = cutpoints[[1]],
    Cutpoint_2 = if (length(cutpoints) >= 2L) cutpoints[[2]] else NA_real_
  )

  list(data = out, audit = audit)
}

add_size_buckets_by_split <- function(data) {
  grouped <- data %>%
    group_by(Split_Scheme, Fold, Horizon) %>%
    group_split()
  results <- purrr::map(grouped, add_size_bucket_to_group)
  list(
    data = bind_rows(purrr::map(results, "data")),
    audit = bind_rows(purrr::map(results, "audit"))
  )
}

save_plot <- function(plot, filename, width = 9, height = 5) {
  ggsave(file.path(FIGURE_DIR, filename), plot, width = width, height = height, dpi = 300)
}

panel_sheet <- "Foglio1"
panel_col_names <- names(read_excel(PANEL_FILE, sheet = panel_sheet, n_max = 0))
panel_raw <- read_excel(PANEL_FILE, sheet = panel_sheet, col_types = rep("text", length(panel_col_names)))
rating_panel_raw <- readRDS(RATING_PANEL_RDS)
macro_raw <- readRDS(MACRO_RDS)

panel_with_quarter <- panel_raw %>%
  mutate(
    Dates_original = Dates,
    Country_original = Country,
    country_code_for_join = if_else(Country == "GR", "EL", Country),
    quarter = normalise_quarter(Dates),
    quarter_end_date = quarter_to_end_date(quarter),
    Dates = quarter_end_date,
    year = as.integer(str_extract(quarter, "^\\d{4}")),
    quarter_number = as.integer(str_extract(quarter, "(?<=Q)[1-4]")),
    firm_id = coalesce(Company_Ticker, Company_Name)
  )

bad_panel_dates <- panel_with_quarter %>%
  filter(is.na(quarter) | is.na(quarter_end_date)) %>%
  distinct(Company_Name, Country, Dates_original)
save_audit(bad_panel_dates, "panel_bad_dates_v5.csv")
if (nrow(bad_panel_dates) > 0L) {
  stop("Some Panel Europe dates could not be converted to year-quarter. See Part_3_Outputs/audits/panel_bad_dates_v5.csv", call. = FALSE)
}

firm_numeric_cols <- setdiff(
  names(panel_with_quarter),
  c(
    "Company_Name",
    "Country",
    "Company_Ticker",
    "Dates",
    "Dates_original",
    "Country_original",
    "country_code_for_join",
    "quarter",
    "quarter_end_date",
    "year",
    "quarter_number",
    "firm_id"
  )
)

panel_typed <- panel_with_quarter %>%
  mutate(across(all_of(firm_numeric_cols), ~ parse_number(.x, na = c("", "NA", "N/A", "NULL"))))

duplicate_panel_keys <- panel_typed %>%
  count(Country, firm_id, Dates, name = "n") %>%
  filter(n > 1L)
save_audit(duplicate_panel_keys, "panel_duplicate_firm_quarter_keys_v5.csv")
if (nrow(duplicate_panel_keys) > 0L) {
  stop("Panel Europe has duplicate country-firm-quarter keys. See Part_3_Outputs/audits/panel_duplicate_firm_quarter_keys_v5.csv", call. = FALSE)
}

raw_financial_vars_present <- intersect(raw_financial_vars, names(panel_typed))

panel_with_financial_span <- panel_typed %>%
  mutate(
    Any_Financial_Observed = rowSums(!is.na(select(., all_of(raw_financial_vars_present)))) > 0L
  ) %>%
  group_by(Country, firm_id) %>%
  mutate(
    First_Data_Date = {
      observed_dates <- Dates[Any_Financial_Observed]
      if (length(observed_dates) == 0L) as.Date(NA) else min(observed_dates)
    },
    Last_Data_Date = {
      observed_dates <- Dates[Any_Financial_Observed]
      if (length(observed_dates) == 0L) as.Date(NA) else max(observed_dates)
    },
    Inside_Firm_Data_Span = !is.na(First_Data_Date) & Dates >= First_Data_Date & Dates <= Last_Data_Date
  ) %>%
  ungroup()

panel_eligible <- panel_with_financial_span %>%
  filter(Inside_Firm_Data_Span) %>%
  arrange(Country, firm_id, Dates)

excluded_no_financials <- panel_with_financial_span %>%
  group_by(Country, firm_id, Company_Name, Company_Ticker) %>%
  summarise(
    rows = n(),
    any_financial_observed = any(Any_Financial_Observed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!any_financial_observed)
save_audit(excluded_no_financials, "excluded_firms_no_financials_v5.csv")

rating_optional_vars <- intersect(c(rating_history_vars, rating_history_bucket_vars, seasonality_vars), names(rating_panel_raw))

rating_for_join <- rating_panel_raw %>%
  mutate(
    year = as.integer(year),
    quarter_number = as.integer(quarter_number),
    rating_rank = as.numeric(rating_rank),
    rating_number = as.numeric(rating_number),
    macro_rating = as.numeric(macro_rating),
    Downgrade = as.integer(Downgrade),
    downgrade_notches_lost = as.integer(downgrade_notches_lost),
    downgrade_count = as.integer(downgrade_count),
    upgrade_count = as.integer(upgrade_count),
    no_notch_change_count = as.integer(no_notch_change_count),
    rating_action_count = as.integer(rating_action_count)
  ) %>%
  select(
    Company_Name,
    year,
    quarter_number,
    rating_panel_company_ticker = Company_Ticker,
    rating_panel_country = Country,
    rating,
    rating_rank,
    rating_number,
    rating_group,
    macro_rating,
    rating_grade,
    Downgrade,
    downgrade_notches_lost,
    downgrade_count,
    upgrade_count,
    no_notch_change_count,
    rating_action_count,
    rating_days_in_quarter,
    quarter_days,
    rating_share_in_quarter,
    latest_rating_event_date,
    all_of(rating_optional_vars)
  )

default_rating_rank_check <- rating_for_join %>%
  filter(rating %in% c("SD", "D")) %>%
  distinct(rating, rating_rank, rating_number, macro_rating, rating_grade) %>%
  arrange(rating_rank, rating)
save_audit(default_rating_rank_check, "default_rating_rank_check_v5.csv")
if (any(default_rating_rank_check$rating == "SD" & default_rating_rank_check$rating_rank != 22, na.rm = TRUE) ||
    any(default_rating_rank_check$rating == "D" & default_rating_rank_check$rating_rank != 23, na.rm = TRUE) ||
    any(default_rating_rank_check$rating %in% c("SD", "D") & default_rating_rank_check$macro_rating != 8, na.rm = TRUE) ||
    any(default_rating_rank_check$rating %in% c("SD", "D") & default_rating_rank_check$rating_grade != "Speculative", na.rm = TRUE)) {
  stop("Default rating mapping is inconsistent with the V5 S&P severity order. Re-run Step_01_Build_Rating_Panel.R.", call. = FALSE)
}

duplicate_rating_keys <- rating_for_join %>%
  count(Company_Name, year, quarter_number, name = "n") %>%
  filter(n > 1L)
save_audit(duplicate_rating_keys, "rating_duplicate_keys_before_panel_join_v5.csv")
if (nrow(duplicate_rating_keys) > 0L) {
  stop("Rating panel has duplicate firm-quarter keys before join.", call. = FALSE)
}

panel_rating_joined <- panel_eligible %>%
  left_join(rating_for_join, by = c("Company_Name", "year", "quarter_number"))

panel_rows_without_rating_rank <- panel_rating_joined %>%
  filter(is.na(rating_rank)) %>%
  select(Company_Name, Company_Ticker, Country, firm_id, Dates, year, quarter_number, quarter)
save_audit(panel_rows_without_rating_rank, "panel_rows_without_rating_rank_v5.csv")

panel_rated <- panel_rating_joined %>%
  filter(!is.na(rating_rank)) %>%
  mutate(
    Downgrade = replace_na(Downgrade, 0L),
    downgrade_notches_lost = replace_na(downgrade_notches_lost, 0L),
    downgrade_count = replace_na(downgrade_count, 0L),
    upgrade_count = replace_na(upgrade_count, 0L),
    no_notch_change_count = replace_na(no_notch_change_count, 0L),
    rating_action_count = replace_na(rating_action_count, 0L)
  )

rating_history_vars_joined <- intersect(rating_history_vars, names(panel_rated))
if (length(rating_history_vars_joined) > 0L) {
  panel_rated <- panel_rated %>%
    mutate(across(all_of(rating_history_vars_joined), ~ replace_na(as.numeric(.x), 0)))
}

if (nrow(panel_rated) == 0L) {
  stop("No firm-quarter remains after filtering on non-missing rating_rank.", call. = FALSE)
}

macro_for_join <- macro_raw %>%
  rename(country_code_for_join = country_code) %>%
  select(-country)

duplicate_macro_keys <- macro_for_join %>%
  count(country_code_for_join, quarter, name = "n") %>%
  filter(n > 1L)
save_audit(duplicate_macro_keys, "macro_duplicate_keys_before_panel_join_v5.csv")
if (nrow(duplicate_macro_keys) > 0L) {
  stop("Macro dataset has duplicate country-quarter keys before join.", call. = FALSE)
}

unmatched_macro_keys <- panel_rated %>%
  distinct(country_code_for_join, quarter) %>%
  anti_join(macro_for_join %>% distinct(country_code_for_join, quarter), by = c("country_code_for_join", "quarter"))
save_audit(unmatched_macro_keys, "panel_country_quarter_not_matched_to_macro_v5.csv")
if (nrow(unmatched_macro_keys) > 0L) {
  stop("Some rated panel country-quarter keys do not match macro data. See Part_3_Outputs/audits/panel_country_quarter_not_matched_to_macro_v5.csv", call. = FALSE)
}

panel_final <- panel_rated %>%
  left_join(macro_for_join, by = c("country_code_for_join", "quarter")) %>%
  arrange(Country, firm_id, Dates)

if (nrow(panel_final) != nrow(panel_rated)) {
  stop("Macro join changed the number of rated panel rows.", call. = FALSE)
}
if (any(is.na(panel_final$rating_rank))) {
  stop("rating_rank has missing values after final join.", call. = FALSE)
}
if (!all(panel_final$Downgrade %in% c(0L, 1L))) {
  stop("Downgrade must be binary 0/1.", call. = FALSE)
}

final_duplicate_keys <- panel_final %>%
  count(Country, firm_id, Dates, name = "n") %>%
  filter(n > 1L)
save_audit(final_duplicate_keys, "final_panel_duplicate_keys_v5.csv")
if (nrow(final_duplicate_keys) > 0L) {
  stop("Final V5 panel has duplicate country-firm-quarter keys.", call. = FALSE)
}

panel_final <- panel_final %>%
  mutate(
    Quarter_Index = year * 4L + quarter_number,
    td_plus_mc = td + mc
  )

ratio_zero_denominator_flags <- paste0(computed_ratio_vars, "_zero_denominator")
ratio_audit <- vector("list", length(computed_ratio_vars))
for (i in seq_len(nrow(ratio_specs))) {
  spec <- ratio_specs[i, ]
  result <- compute_raw_ratio(panel_final, spec$Variable, spec$Numerator, spec$Denominator)
  panel_final[[spec$Variable]] <- result$value
  panel_final[[ratio_zero_denominator_flags[i]]] <- result$zero_denominator
  ratio_audit[[i]] <- tibble(
    Variable = spec$Variable,
    Numerator = spec$Numerator,
    Denominator = spec$Denominator,
    N_Zero_Denominator_With_Numerator = sum(result$zero_denominator),
    N_Missing_Ratio_Raw = sum(is.na(result$value)),
    N_Nonfinite_Ratio_Raw = sum(!is.na(result$value) & !is.finite(result$value)),
    Median_Raw = median_or_na(result$value)
  )
}
save_audit(bind_rows(ratio_audit), "raw_ratio_audit_v5.csv")

financial_ratio_vars_present <- intersect(financial_ratio_vars, names(panel_final))
existing_ratio_vars_present <- intersect(existing_ratio_vars_for_models, names(panel_final))
macro_vars_present <- intersect(macro_vars, names(panel_final))
systemic_vars_present <- intersect(systemic_vars, names(panel_final))
parsimonious_macro_vars_present <- intersect(parsimonious_macro_vars, names(panel_final))
change_macro_vars_present <- intersect(change_macro_vars, names(panel_final))
parsimonious_ciss_vars_present <- intersect(parsimonious_ciss_vars, names(panel_final))
parsimonious_vstoxx_vars_present <- intersect(parsimonious_vstoxx_vars, names(panel_final))
parsimonious_systemic_vars_present <- intersect(parsimonious_systemic_vars, names(panel_final))
rating_history_vars_present <- intersect(rating_history_vars, names(panel_final))
rating_history_model_vars_present <- intersect(rating_history_model_vars, names(panel_final))
rating_history_bucket_vars_present <- intersect(rating_history_bucket_vars, names(panel_final))
seasonality_vars_present <- intersect(seasonality_vars, names(panel_final))
main_financial_predictors <- intersect(c(computed_ratio_vars, existing_ratio_vars_present), names(panel_final))
main_macro_pars_stat_predictors <- parsimonious_macro_vars_present
main_macro_delta_predictors <- change_macro_vars_present
main_ciss_predictors <- parsimonious_ciss_vars_present
main_vstoxx_predictors <- parsimonious_vstoxx_vars_present
main_systemic_predictors <- unique(c(main_ciss_predictors, main_vstoxx_predictors))
main_rating_rank_predictors <- intersect("rating_rank", names(panel_final))
main_rating_group_predictors <- intersect("rating_group", names(panel_final))

# V5 is the definitive data build: model specifications use only these numeric
# predictors. Total assets is imputed separately for size-bucket diagnostics and
# is not inserted into the regressor manifest.
model_predictor_vars <- unique(c(
  main_financial_predictors,
  main_macro_pars_stat_predictors,
  main_macro_delta_predictors,
  main_systemic_predictors
))
size_imputation_vars <- intersect(SIZE_VARIABLE, names(panel_final))
fold_imputation_vars <- unique(c(model_predictor_vars, size_imputation_vars))
model_input_vars_present <- unique(c(
  raw_financial_vars_present,
  financial_ratio_vars_present,
  model_predictor_vars,
  main_rating_rank_predictors,
  "rating_number",
  "macro_rating"
))

panel_final <- panel_final %>%
  mutate(
    N_Missing_Model_Predictors_Raw = rowSums(is.na(select(., all_of(model_predictor_vars)))),
    Any_Missing_Model_Predictor_Raw = N_Missing_Model_Predictors_Raw > 0L
  )

# Build the V5 candidate manifest from controlled blocks. CISS and VSTOXX are
# treated as alternative systemic-stress blocks and are never combined in the
# same candidate specification.
macro_blocks <- list(
  list(suffix = "", predictors = character(0), has_pars_stat = FALSE, has_delta = FALSE, note = ""),
  list(suffix = "_macro_pars_stat", predictors = main_macro_pars_stat_predictors, has_pars_stat = TRUE, has_delta = FALSE, note = "parsimonious macro level/stat block"),
  list(suffix = "_macro_delta", predictors = main_macro_delta_predictors, has_pars_stat = FALSE, has_delta = TRUE, note = "macro-delta block")
)
systemic_blocks <- list(
  list(suffix = "", predictors = character(0), block = "none", has_systemic = FALSE, has_ciss = FALSE, has_vstoxx = FALSE, note = ""),
  list(suffix = "_ciss", predictors = main_ciss_predictors, block = "CISS", has_systemic = TRUE, has_ciss = TRUE, has_vstoxx = FALSE, note = "parsimonious CISS systemic-stress block"),
  list(suffix = "_vstoxx", predictors = main_vstoxx_predictors, block = "VSTOXX", has_systemic = TRUE, has_ciss = FALSE, has_vstoxx = TRUE, note = "parsimonious VSTOXX systemic-stress block")
)
rating_blocks <- list(
  list(suffix = "", predictors = character(0), encoding = "none", has_current_rating = FALSE, note = ""),
  list(suffix = "_rating_rank", predictors = main_rating_rank_predictors, encoding = "rating_rank", has_current_rating = TRUE, note = "current rating rank"),
  list(suffix = "_rating_group", predictors = main_rating_group_predictors, encoding = "rating_group", has_current_rating = TRUE, note = "current rating group")
)

regressor_rows <- list()
row_id <- 0L
for (systemic_block in systemic_blocks) {
  for (macro_block in macro_blocks) {
    for (rating_block in rating_blocks) {
      regressor_set <- paste0("financial", macro_block$suffix, systemic_block$suffix, rating_block$suffix)
      if (identical(regressor_set, "financial")) {
        regressor_set <- "financial_only"
      }
      predictors <- unique(c(
        main_financial_predictors,
        macro_block$predictors,
        systemic_block$predictors,
        rating_block$predictors
      ))
      note_parts <- c(
        "Financial variables",
        macro_block$note,
        systemic_block$note,
        rating_block$note
      )
      note_parts <- note_parts[nzchar(note_parts)]
      row_id <- row_id + 1L
      regressor_rows[[row_id]] <- tibble(
        Regressor_Set = regressor_set,
        Variables = paste(predictors, collapse = ", "),
        Role = if_else(regressor_set == "financial_only", "Benchmark", "Candidate"),
        Include_In_V5_Tuning = TRUE,
        Has_Macro_Pars_Stat = macro_block$has_pars_stat,
        Has_Macro_Delta = macro_block$has_delta,
        Has_Systemic = systemic_block$has_systemic,
        Systemic_Block = systemic_block$block,
        Has_CISS = systemic_block$has_ciss,
        Has_VSTOXX = systemic_block$has_vstoxx,
        Rating_Encoding = rating_block$encoding,
        Has_Current_Rating = rating_block$has_current_rating,
        Has_Rating_History = FALSE,
        Notes = if_else(
          regressor_set == "financial_only",
          "Financial variables only; retained as the clean benchmark.",
          paste0(paste(note_parts, collapse = " plus "), ".")
        )
      )
    }
  }
}
main_regressor_manifest <- bind_rows(regressor_rows)
if (any(main_regressor_manifest$Has_CISS & main_regressor_manifest$Has_VSTOXX)) {
  stop("Invalid V5 regressor manifest: CISS and VSTOXX cannot appear in the same specification.", call. = FALSE)
}
save_audit(main_regressor_manifest, "table_20_regressor_manifest.csv")
dir.create(file.path(V5_DIR, "Part_2_Outputs", "audits"), recursive = TRUE, showWarnings = FALSE)
write_csv(main_regressor_manifest, file.path(V5_DIR, "Part_2_Outputs", "audits", "table_20_regressor_manifest.csv"), na = "")

panel_final <- panel_final %>%
  select(
    Company_Name,
    Country,
    Country_original,
    country_code_for_join,
    Company_Ticker,
    firm_id,
    Dates,
    Dates_original,
    quarter_end_date,
    year,
    quarter_number,
    quarter,
    rating,
    rating_rank,
    rating_number,
    rating_group,
    macro_rating,
    rating_grade,
    Downgrade,
    downgrade_notches_lost,
    downgrade_count,
    upgrade_count,
    no_notch_change_count,
    rating_action_count,
    rating_days_in_quarter,
    quarter_days,
    rating_share_in_quarter,
    latest_rating_event_date,
    all_of(rating_history_vars_present),
    all_of(rating_history_bucket_vars_present),
    all_of(seasonality_vars_present),
    Quarter_Index,
    td_plus_mc,
    all_of(computed_ratio_vars),
    all_of(ratio_zero_denominator_flags),
    N_Missing_Model_Predictors_Raw,
    Any_Missing_Model_Predictor_Raw,
    everything()
  )

join_report <- tibble(
  metric = c(
    "panel_source_rows",
    "panel_rows_inside_financial_data_span",
    "final_rows_after_rating_rank_filter_and_macro_join",
    "rows_lost_because_rating_rank_missing",
    "panel_source_firms",
    "firms_inside_financial_data_span",
    "final_firms",
    "firms_lost_all_missing_rating_rank",
    "countries_original",
    "countries_join_key_after_GR_to_EL_mapping",
    "macro_variables_added",
    "downgrade_rows",
    "downgrade_count",
    "downgrade_notches_lost",
    "downgrade_rate_rows",
    "rating_group_A_rows",
    "rating_group_B_rows",
    "rating_group_C_rows"
  ),
  value = c(
    as.character(nrow(panel_raw)),
    as.character(nrow(panel_eligible)),
    as.character(nrow(panel_final)),
    as.character(nrow(panel_rows_without_rating_rank)),
    as.character(n_distinct(panel_raw$Company_Name)),
    as.character(n_distinct(panel_eligible$Company_Name)),
    as.character(n_distinct(panel_final$Company_Name)),
    as.character(length(setdiff(unique(panel_eligible$Company_Name), unique(panel_final$Company_Name)))),
    as.character(n_distinct(panel_final$Country_original)),
    as.character(n_distinct(panel_final$country_code_for_join)),
    as.character(length(macro_vars_present)),
    as.character(sum(panel_final$Downgrade == 1L, na.rm = TRUE)),
    as.character(sum(panel_final$downgrade_count, na.rm = TRUE)),
    as.character(sum(panel_final$downgrade_notches_lost, na.rm = TRUE)),
    as.character(mean(panel_final$Downgrade == 1L, na.rm = TRUE)),
    as.character(sum(panel_final$rating_group == "A", na.rm = TRUE)),
    as.character(sum(panel_final$rating_group == "B", na.rm = TRUE)),
    as.character(sum(panel_final$rating_group == "C", na.rm = TRUE))
  )
)
save_audit(join_report, "panel_v5_join_report.csv")

data_quality_tests <- tibble(
  check = c(
    "no_final_duplicate_country_firm_quarter_keys",
    "no_missing_rating_rank",
    "no_missing_rating_group",
    "no_missing_macro_rating",
    "no_missing_rating_grade",
    "downgrade_binary",
    "all_rated_panel_country_quarters_match_macro",
    "GR_panel_country_mapped_to_EL_macro_key"
  ),
  pass = c(
    nrow(final_duplicate_keys) == 0L,
    sum(is.na(panel_final$rating_rank)) == 0L,
    sum(is.na(panel_final$rating_group)) == 0L,
    sum(is.na(panel_final$macro_rating)) == 0L,
    sum(is.na(panel_final$rating_grade)) == 0L,
    all(panel_final$Downgrade %in% c(0L, 1L)),
    nrow(unmatched_macro_keys) == 0L,
    all(panel_final$country_code_for_join[panel_final$Country_original == "GR"] == "EL")
  ),
  value = c(
    as.character(nrow(final_duplicate_keys)),
    as.character(sum(is.na(panel_final$rating_rank))),
    as.character(sum(is.na(panel_final$rating_group))),
    as.character(sum(is.na(panel_final$macro_rating))),
    as.character(sum(is.na(panel_final$rating_grade))),
    paste(sort(unique(panel_final$Downgrade)), collapse = ","),
    as.character(nrow(unmatched_macro_keys)),
    as.character(sum(panel_final$Country_original == "GR", na.rm = TRUE))
  )
)
save_audit(data_quality_tests, "data_quality_tests_v5.csv")
if (!all(data_quality_tests$pass)) {
  stop("At least one data quality test failed. See Part_3_Outputs/audits/data_quality_tests_v5.csv", call. = FALSE)
}

missingness_summary <- panel_final %>%
  summarise(across(all_of(model_input_vars_present), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_values") %>%
  mutate(
    n_rows = nrow(panel_final),
    missing_rate = missing_values / n_rows
  ) %>%
  arrange(desc(missing_values), variable)
save_audit(missingness_summary, "model_input_missingness_before_imputation_v5.csv")

numeric_descriptive_stats <- panel_final %>%
  summarise(across(
    all_of(model_input_vars_present),
    list(
      n_non_missing = ~ sum(!is.na(.x)),
      mean = ~ mean(.x, na.rm = TRUE),
      sd = ~ sd(.x, na.rm = TRUE),
      p25 = ~ quantile(.x, 0.25, na.rm = TRUE, names = FALSE),
      median = ~ median(.x, na.rm = TRUE),
      p75 = ~ quantile(.x, 0.75, na.rm = TRUE, names = FALSE)
    ),
    .names = "{.col}__{.fn}"
  )) %>%
  pivot_longer(everything(), names_to = "variable_stat", values_to = "value") %>%
  separate(variable_stat, into = c("variable", "stat"), sep = "__", extra = "merge")
save_audit(numeric_descriptive_stats, "numeric_descriptive_stats_v5.csv")

numeric_descriptive_stats_by_rating_group <- panel_final %>%
  group_by(rating_group) %>%
  summarise(across(
    all_of(model_input_vars_present),
    list(
      n_non_missing = ~ sum(!is.na(.x)),
      mean = ~ mean(.x, na.rm = TRUE),
      sd = ~ sd(.x, na.rm = TRUE),
      p25 = ~ quantile(.x, 0.25, na.rm = TRUE, names = FALSE),
      median = ~ median(.x, na.rm = TRUE),
      p75 = ~ quantile(.x, 0.75, na.rm = TRUE, names = FALSE)
    ),
    .names = "{.col}__{.fn}"
  ), .groups = "drop") %>%
  pivot_longer(-rating_group, names_to = "variable_stat", values_to = "value") %>%
  separate(variable_stat, into = c("variable", "stat"), sep = "__", extra = "merge")
save_audit(numeric_descriptive_stats_by_rating_group, "numeric_descriptive_stats_by_rating_group_v5.csv")

numeric_descriptive_stats_by_downgrade <- panel_final %>%
  mutate(Downgrade_Status = if_else(Downgrade == 1L, "Downgrade_Quarter", "No_Downgrade_Quarter")) %>%
  group_by(Downgrade_Status) %>%
  summarise(across(
    all_of(model_input_vars_present),
    list(
      n_non_missing = ~ sum(!is.na(.x)),
      mean = ~ mean(.x, na.rm = TRUE),
      sd = ~ sd(.x, na.rm = TRUE),
      p25 = ~ quantile(.x, 0.25, na.rm = TRUE, names = FALSE),
      median = ~ median(.x, na.rm = TRUE),
      p75 = ~ quantile(.x, 0.75, na.rm = TRUE, names = FALSE)
    ),
    .names = "{.col}__{.fn}"
  ), .groups = "drop") %>%
  pivot_longer(-Downgrade_Status, names_to = "variable_stat", values_to = "value") %>%
  separate(variable_stat, into = c("variable", "stat"), sep = "__", extra = "merge")
save_audit(numeric_descriptive_stats_by_downgrade, "numeric_descriptive_stats_by_downgrade_status_v5.csv")

financial_persistence_variables <- intersect(c("wc_ta", "re_ta", "ebit_ta", "mc_td", "s_ta", main_financial_predictors), names(panel_final))
financial_within_firm_correlation <- bind_rows(
  within_firm_lag_correlation(panel_final, financial_persistence_variables, 1L),
  within_firm_lag_correlation(panel_final, financial_persistence_variables, 4L)
) %>%
  pivot_wider(
    names_from = Lag,
    values_from = c(Within_Firm_Correlation, N_Pairs, N_Firms, N_Countries),
    names_glue = "{.value}_Lag_{Lag}"
  ) %>%
  arrange(Variable)
save_audit(financial_within_firm_correlation, "financial_within_firm_lag_correlations_v5.csv")

financial_icc <- icc_by_variable(panel_final, financial_persistence_variables) %>%
  arrange(desc(ICC), Variable)
save_audit(financial_icc, "financial_intraclass_correlation_v5.csv")

# Outlier and winsorization audit: document tails and hypothetical 1/99
# winsorized means without transforming the modelling data.
outlier_variables <- intersect(
  c(main_financial_predictors, main_macro_pars_stat_predictors, main_macro_delta_predictors, main_systemic_predictors),
  names(panel_final)
)
outlier_winsorization_audit <- outlier_audit_table(panel_final, outlier_variables) %>%
  arrange(desc(N_Above_P99 + N_Below_P01), Variable)
save_audit(outlier_winsorization_audit, "outlier_winsorization_audit_v5.csv")

# Pairwise correlation diagnostics: describe collinearity and redundant predictor
# blocks before model tuning.
predictor_correlation_pairs <- correlation_pair_table(panel_final, model_predictor_vars)
save_audit(predictor_correlation_pairs, "predictor_correlation_pairs_v5.csv")
save_audit(
  predictor_correlation_pairs %>% filter(Abs_Correlation >= 0.80),
  "predictor_high_correlation_pairs_abs_ge_080_v5.csv"
)

correlation_plot_vars <- predictor_correlation_pairs %>%
  filter(is.finite(Abs_Correlation)) %>%
  slice_head(n = 150) %>%
  select(Variable_1, Variable_2) %>%
  pivot_longer(everything(), values_to = "Variable") %>%
  count(Variable, sort = TRUE) %>%
  slice_head(n = 25) %>%
  pull(Variable)

if (length(correlation_plot_vars) >= 2L) {
  correlation_plot_data <- correlation_pair_table(panel_final, correlation_plot_vars) %>%
    select(Variable_1, Variable_2, Correlation) %>%
    bind_rows(
      tibble(
        Variable_1 = correlation_plot_vars,
        Variable_2 = correlation_plot_vars,
        Correlation = 1
      )
    )

  save_plot(
    ggplot(correlation_plot_data, aes(x = Variable_1, y = Variable_2, fill = Correlation)) +
      geom_tile(color = "white", linewidth = 0.2) +
      scale_fill_gradient2(low = "#8a3d3d", mid = "#f7f7f2", high = "#2f6f73", midpoint = 0, limits = c(-1, 1)) +
      labs(x = NULL, y = NULL, fill = "Corr.", title = "High-correlation predictor block") +
      theme_minimal(base_size = 9) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid = element_blank()),
    "fig_08_predictor_correlation_heatmap_v5.png",
    width = 10,
    height = 9
  )
}

# Panel coverage and attrition audit: track firms, countries, missingness, and
# downgrade rates over calendar time before model estimation.
panel_coverage_by_year <- panel_final %>%
  group_by(year) %>%
  summarise(
    N_Firm_Quarters = n(),
    N_Firms = n_distinct(firm_id),
    N_Countries = n_distinct(Country),
    N_Rating_Groups = n_distinct(rating_group),
    Downgrade_Rate = mean(Downgrade == 1L, na.rm = TRUE),
    Mean_Missing_Model_Predictors_Raw = mean(N_Missing_Model_Predictors_Raw, na.rm = TRUE),
    Share_With_Any_Missing_Model_Predictor_Raw = mean(Any_Missing_Model_Predictor_Raw, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year)
save_audit(panel_coverage_by_year, "panel_coverage_attrition_by_year_v5.csv")

firm_entry_exit_audit <- panel_final %>%
  group_by(Country, firm_id, Company_Name, Company_Ticker) %>%
  summarise(
    First_Year = min(year),
    Last_Year = max(year),
    First_Quarter = min(quarter),
    Last_Quarter = max(quarter),
    N_Quarters = n(),
    N_Downgrade_Rows = sum(Downgrade == 1L, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(First_Year, Country, firm_id)
save_audit(firm_entry_exit_audit, "firm_entry_exit_audit_v5.csv")

save_plot(
  ggplot(panel_coverage_by_year, aes(x = year, y = N_Firms)) +
    geom_line(color = "#2f6f73", linewidth = 0.8) +
    geom_point(color = "#2f6f73", size = 1.8) +
    scale_y_continuous(labels = comma) +
    labs(x = "Year", y = "Firms", title = "Panel firm coverage over time") +
    theme_minimal(base_size = 11),
  "fig_09_panel_firm_coverage_by_year_v5.png"
)

# Systemic-regime diagnostics: estimate downgrade rates across low, middle, and
# high stress states.
systemic_regime_vars_present <- intersect(c("vstoxx_q_mean", "ciss_country_or_euro_q_mean"), names(panel_final))
systemic_regime_downgrade_rates <- purrr::map_dfr(systemic_regime_vars_present, function(variable) {
  panel_final %>%
    filter(is.finite(.data[[variable]])) %>%
    mutate(
      Regime_Rank = ntile(.data[[variable]], 3L),
      Regime = factor(
        case_when(
          Regime_Rank == 1L ~ "Low",
          Regime_Rank == 2L ~ "Middle",
          Regime_Rank == 3L ~ "High",
          TRUE ~ NA_character_
        ),
        levels = c("Low", "Middle", "High")
      )
    ) %>%
    group_by(Regime) %>%
    summarise(
      Variable = variable,
      N_Firm_Quarters = n(),
      N_Firms = n_distinct(firm_id),
      Mean_Regime_Value = mean(.data[[variable]], na.rm = TRUE),
      Downgrade_Rows = sum(Downgrade == 1L, na.rm = TRUE),
      Downgrade_Rate = mean(Downgrade == 1L, na.rm = TRUE),
      .groups = "drop"
    )
})
save_audit(systemic_regime_downgrade_rates, "systemic_regime_current_downgrade_rates_v5.csv")

if (nrow(systemic_regime_downgrade_rates) > 0L) {
  save_plot(
    ggplot(systemic_regime_downgrade_rates, aes(x = Regime, y = Downgrade_Rate, fill = Regime)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ Variable, scales = "free_x") +
      scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
      labs(x = "Systemic-risk regime", y = "Downgrade rate", title = "Current downgrade rate by systemic-risk regime") +
      theme_minimal(base_size = 11),
    "fig_10_systemic_regime_current_downgrade_rates_v5.png",
    width = 9,
    height = 5
  )
}

# Naive baseline rates: compute simple conditional downgrade frequencies as
# benchmarks before statistical or machine-learning models.
baseline_rate_global <- panel_final %>%
  summarise(
    Baseline = "global",
    Group = "All",
    N_Firm_Quarters = n(),
    N_Downgrades = sum(Downgrade == 1L, na.rm = TRUE),
    Downgrade_Rate = mean(Downgrade == 1L, na.rm = TRUE)
  )

baseline_rate_by_rating_group <- panel_final %>%
  group_by(rating_group) %>%
  summarise(
    Baseline = "rating_group",
    Group = as.character(first(rating_group)),
    N_Firm_Quarters = n(),
    N_Downgrades = sum(Downgrade == 1L, na.rm = TRUE),
    Downgrade_Rate = mean(Downgrade == 1L, na.rm = TRUE),
    .groups = "drop"
  )

baseline_rate_by_rating_grade <- panel_final %>%
  group_by(rating_grade) %>%
  summarise(
    Baseline = "rating_grade",
    Group = as.character(first(rating_grade)),
    N_Firm_Quarters = n(),
    N_Downgrades = sum(Downgrade == 1L, na.rm = TRUE),
    Downgrade_Rate = mean(Downgrade == 1L, na.rm = TRUE),
    .groups = "drop"
  )

baseline_rate_by_history <- if ("N_Downgrades_Previous_4Q_Bucket" %in% names(panel_final)) {
  panel_final %>%
    group_by(N_Downgrades_Previous_4Q_Bucket) %>%
    summarise(
      Baseline = "previous_4q_downgrade_bucket",
      Group = as.character(first(N_Downgrades_Previous_4Q_Bucket)),
      N_Firm_Quarters = n(),
      N_Downgrades = sum(Downgrade == 1L, na.rm = TRUE),
      Downgrade_Rate = mean(Downgrade == 1L, na.rm = TRUE),
      .groups = "drop"
    )
} else {
  tibble()
}

baseline_naive_rates <- bind_rows(
  baseline_rate_global,
  baseline_rate_by_rating_group,
  baseline_rate_by_rating_grade,
  baseline_rate_by_history
) %>%
  arrange(Baseline, Group)
save_audit(baseline_naive_rates, "baseline_naive_current_downgrade_rates_v5.csv")

firm_summary <- panel_final %>%
  group_by(Company_Name, Company_Ticker, Country, firm_id) %>%
  summarise(
    n_quarters = n(),
    first_quarter = min(quarter),
    last_quarter = max(quarter),
    downgrade_rows = sum(Downgrade == 1L, na.rm = TRUE),
    downgrade_count = sum(downgrade_count, na.rm = TRUE),
    main_rating_group = names(sort(table(rating_group), decreasing = TRUE))[1],
    .groups = "drop"
  ) %>%
  arrange(desc(downgrade_count), Company_Name)
save_audit(firm_summary, "firm_summary_v5.csv")

country_summary <- panel_final %>%
  group_by(Country, country_code_for_join) %>%
  summarise(
    n_rows = n(),
    n_firms = n_distinct(Company_Name),
    downgrade_rows = sum(Downgrade == 1L, na.rm = TRUE),
    downgrade_count = sum(downgrade_count, na.rm = TRUE),
    downgrade_rate = mean(Downgrade == 1L, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_rows), Country)
save_audit(country_summary, "country_summary_v5.csv")

year_summary <- panel_final %>%
  group_by(year) %>%
  summarise(
    n_rows = n(),
    n_firms = n_distinct(Company_Name),
    downgrade_rows = sum(Downgrade == 1L, na.rm = TRUE),
    downgrade_count = sum(downgrade_count, na.rm = TRUE),
    downgrade_rate = mean(Downgrade == 1L, na.rm = TRUE),
    .groups = "drop"
  )
save_audit(year_summary, "downgrade_distribution_by_year_v5.csv")

quarter_summary <- panel_final %>%
  group_by(year, quarter_number, quarter) %>%
  summarise(
    n_rows = n(),
    n_firms = n_distinct(Company_Name),
    downgrade_rows = sum(Downgrade == 1L, na.rm = TRUE),
    downgrade_count = sum(downgrade_count, na.rm = TRUE),
    downgrade_rate = mean(Downgrade == 1L, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year, quarter_number) %>%
  mutate(quarter_date = as.Date(sprintf("%04d-%02d-01", year, (quarter_number - 1L) * 3L + 1L)))
save_audit(quarter_summary, "downgrade_distribution_by_quarter_v5.csv")

final_panel_time_clustering_summary <- quarter_summary %>%
  summarise(
    Period = paste0(min(year, na.rm = TRUE), "-", max(year, na.rm = TRUE)),
    N_Quarters = n(),
    Mean_Downgrade_Rate = mean(downgrade_rate, na.rm = TRUE),
    Median_Downgrade_Rate = median(downgrade_rate, na.rm = TRUE),
    SD_Downgrade_Rate = sd(downgrade_rate, na.rm = TRUE),
    Min_Downgrade_Rate = min(downgrade_rate, na.rm = TRUE),
    Max_Downgrade_Rate = max(downgrade_rate, na.rm = TRUE)
  )
save_audit(final_panel_time_clustering_summary, "final_panel_time_clustering_summary_v5.csv")

final_panel_seasonality_by_quarter <- panel_final %>%
  group_by(quarter_number) %>%
  summarise(
    QuarterOfYear = paste0("Q", first(quarter_number)),
    N_Firm_Quarters = n(),
    N_Firms = n_distinct(firm_id),
    Downgrade_Rows = sum(Downgrade == 1L, na.rm = TRUE),
    P_Downgrade = Downgrade_Rows / N_Firm_Quarters,
    .groups = "drop"
  ) %>%
  arrange(quarter_number)
save_audit(final_panel_seasonality_by_quarter, "final_panel_seasonality_downgrade_probability_by_quarter_of_year_v5.csv")

final_panel_seasonality_table <- table(panel_final$quarter_number, panel_final$Downgrade)
final_panel_seasonality_test <- suppressWarnings(chisq.test(final_panel_seasonality_table))
final_panel_seasonality_cramers_v <- sqrt(
  unname(final_panel_seasonality_test$statistic) /
    (sum(final_panel_seasonality_table) * (min(dim(final_panel_seasonality_table)) - 1L))
)
final_panel_seasonality_test_audit <- tibble(
  Test = "Pearson_chisq_quarter_of_year_vs_downgrade",
  Statistic = unname(final_panel_seasonality_test$statistic),
  DF = unname(final_panel_seasonality_test$parameter),
  P_Value = final_panel_seasonality_test$p.value,
  Cramers_V = final_panel_seasonality_cramers_v,
  Include_QuarterOfYear_In_V5 = FALSE,
  Decision_Note = "Quarter-of-year is not carried into V5 model specifications by design."
)
save_audit(final_panel_seasonality_test_audit, "final_panel_seasonality_test_v5.csv")

final_panel_issuer_persistence <- panel_final %>%
  group_by(Company_Name, Company_Ticker, Country, firm_id) %>%
  summarise(
    T_i = n(),
    N_i_down = sum(Downgrade == 1L, na.rm = TRUE),
    downgrade_events_i = sum(downgrade_count, na.rm = TRUE),
    notches_lost_i = sum(downgrade_notches_lost, na.rm = TRUE),
    N_i_down_over_T_i = N_i_down / T_i,
    first_quarter = min(quarter),
    last_quarter = max(quarter),
    .groups = "drop"
  ) %>%
  mutate(
    Downgrade_Frequency_Bucket = case_when(
      N_i_down_over_T_i == 0 ~ "0%",
      N_i_down_over_T_i > 0 & N_i_down_over_T_i <= 0.01 ~ "(0%, 1%]",
      N_i_down_over_T_i > 0.01 & N_i_down_over_T_i <= 0.05 ~ "(1%, 5%]",
      N_i_down_over_T_i > 0.05 & N_i_down_over_T_i <= 0.10 ~ "(5%, 10%]",
      N_i_down_over_T_i > 0.10 ~ ">10%",
      TRUE ~ NA_character_
    ),
    Downgrade_Frequency_Bucket = factor(
      Downgrade_Frequency_Bucket,
      levels = c("0%", "(0%, 1%]", "(1%, 5%]", "(5%, 10%]", ">10%")
    )
  ) %>%
  arrange(desc(N_i_down_over_T_i), desc(N_i_down), Company_Name)
save_audit(final_panel_issuer_persistence, "final_panel_issuer_persistence_v5.csv")
save_audit(
  final_panel_issuer_persistence %>% filter(N_i_down_over_T_i > 0.10),
  "final_panel_issuer_persistence_gt_10pct_v5.csv"
)

final_panel_issuer_persistence_bucket_distribution <- final_panel_issuer_persistence %>%
  count(Downgrade_Frequency_Bucket, name = "N_Issuers") %>%
  mutate(Share_Issuers = N_Issuers / sum(N_Issuers)) %>%
  arrange(Downgrade_Frequency_Bucket)
save_audit(final_panel_issuer_persistence_bucket_distribution, "final_panel_issuer_persistence_frequency_buckets_v5.csv")

final_panel_previous_downgrade_distribution <- if ("N_Downgrades_Previous_4Q_Bucket" %in% names(panel_final)) {
  panel_final %>%
    count(N_Downgrades_Previous_4Q_Bucket, name = "N_Firm_Quarters") %>%
    mutate(Share = N_Firm_Quarters / sum(N_Firm_Quarters)) %>%
    arrange(factor(N_Downgrades_Previous_4Q_Bucket, levels = c("0", "1", "2", "3", "4")))
} else {
  tibble()
}
save_audit(final_panel_previous_downgrade_distribution, "final_panel_previous_4q_downgrade_count_distribution_v5.csv")

final_panel_previous_history_next_downgrade_rate <- if ("N_Downgrades_Previous_4Q_Bucket" %in% names(panel_final)) {
  panel_final %>%
    group_by(N_Downgrades_Previous_4Q_Bucket) %>%
    summarise(
      N_Firm_Quarters = n(),
      Downgrade_Rows = sum(Downgrade == 1L, na.rm = TRUE),
      Downgrade_Rate = mean(Downgrade == 1L, na.rm = TRUE),
      Mean_Notches_Lost_Current_Quarter = mean(downgrade_notches_lost, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(factor(N_Downgrades_Previous_4Q_Bucket, levels = c("0", "1", "2", "3", "4")))
} else {
  tibble()
}
save_audit(final_panel_previous_history_next_downgrade_rate, "final_panel_previous_4q_history_current_downgrade_rate_v5.csv")

final_panel_previous_event_history <- if ("N_Downgrade_Events_Previous_4Q" %in% names(panel_final)) {
  panel_final %>%
    mutate(
      N_Downgrade_Events_Previous_4Q_Bucket = case_when(
        N_Downgrade_Events_Previous_4Q >= 4L ~ ">=4",
        TRUE ~ as.character(N_Downgrade_Events_Previous_4Q)
      ),
      N_Downgrade_Events_Previous_4Q_Bucket = factor(
        N_Downgrade_Events_Previous_4Q_Bucket,
        levels = c("0", "1", "2", "3", ">=4")
      )
    )
} else {
  tibble()
}

final_panel_previous_event_distribution <- if (nrow(final_panel_previous_event_history) > 0L) {
  final_panel_previous_event_history %>%
    count(N_Downgrade_Events_Previous_4Q_Bucket, name = "N_Firm_Quarters") %>%
    mutate(Share = N_Firm_Quarters / sum(N_Firm_Quarters)) %>%
    arrange(N_Downgrade_Events_Previous_4Q_Bucket)
} else {
  tibble()
}
save_audit(final_panel_previous_event_distribution, "final_panel_previous_4q_downgrade_event_distribution_v5.csv")

final_panel_previous_event_history_next_downgrade_rate <- if (nrow(final_panel_previous_event_history) > 0L) {
  final_panel_previous_event_history %>%
    group_by(N_Downgrade_Events_Previous_4Q_Bucket) %>%
    summarise(
      N_Firm_Quarters = n(),
      Downgrade_Rows = sum(Downgrade == 1L, na.rm = TRUE),
      Downgrade_Rate = mean(Downgrade == 1L, na.rm = TRUE),
      Mean_Notches_Lost_Current_Quarter = mean(downgrade_notches_lost, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(N_Downgrade_Events_Previous_4Q_Bucket)
} else {
  tibble()
}
save_audit(final_panel_previous_event_history_next_downgrade_rate, "final_panel_previous_4q_event_history_current_downgrade_rate_v5.csv")

rating_group_summary <- panel_final %>%
  group_by(rating_group) %>%
  summarise(
    n_rows = n(),
    n_firms = n_distinct(Company_Name),
    downgrade_rows = sum(Downgrade == 1L, na.rm = TRUE),
    downgrade_count = sum(downgrade_count, na.rm = TRUE),
    downgrade_rate = mean(Downgrade == 1L, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(rating_group)
save_audit(rating_group_summary, "downgrade_distribution_by_rating_group_v5.csv")

firm_continuity <- panel_final %>%
  mutate(quarter_index = year * 4L + quarter_number) %>%
  group_by(Company_Name, Company_Ticker, Country, firm_id) %>%
  summarise(
    n_quarters = n(),
    first_quarter = min(quarter),
    last_quarter = max(quarter),
    min_quarter_gap = if (n() > 1L) min(diff(quarter_index)) else NA_integer_,
    max_quarter_gap = if (n() > 1L) max(diff(quarter_index)) else NA_integer_,
    has_quarterly_gap_after_rating_filter = if (n() > 1L) any(diff(quarter_index) != 1L) else FALSE,
    .groups = "drop"
  )
save_audit(firm_continuity, "firm_continuity_after_rating_filter_v5.csv")

# Size diagnostics: impute total assets in a final-panel descriptive copy, then
# compute log-size buckets for audit plots. These variables are not added to any
# V5 model specification.
panel_final_size_input <- panel_final %>%
  mutate(
    Split_Scheme = "Final_Panel",
    Fold = "Full_Sample",
    Horizon = 0L,
    Horizon_Label = "single_horizon",
    Fold_Role = "Training"
  )

panel_final_size <- panel_final
if (length(size_imputation_vars) > 0L) {
  panel_final_size_imputed_result <- fit_apply_fold_imputation(
    data = panel_final_size_input,
    split_scheme = "Final_Panel",
    fold = "Full_Sample",
    horizon = 0L,
    imputation_vars = size_imputation_vars,
    model_predictor_vars = character(0)
  )
  save_audit(panel_final_size_imputed_result$audit, "final_panel_size_imputation_audit_v5.csv")

  panel_final_size_result <- add_size_buckets_by_split(panel_final_size_imputed_result$data)
  panel_final_size <- panel_final_size_result$data %>%
    select(-Split_Scheme, -Fold, -Horizon, -Horizon_Label, -Fold_Role)
  save_audit(panel_final_size_result$audit, "final_panel_size_transform_and_bucket_audit_v5.csv")
  write_csv(
    panel_final_size %>%
      select(
        Company_Name,
        Company_Ticker,
        Country,
        firm_id,
        Dates,
        quarter,
        ta,
        log_ta_plain,
        log1p_ta,
        log_ta,
        Size_Bucket,
        Downgrade,
        downgrade_count
      ),
    file.path(DATA_DIR, "panel_europe_v5_final_panel_size_buckets.csv"),
    na = ""
  )
}

final_panel_size_bucket_summary <- if ("Size_Bucket" %in% names(panel_final_size)) {
  panel_final_size %>%
    filter(!is.na(Size_Bucket)) %>%
    group_by(Size_Bucket) %>%
    summarise(
      N_Firm_Quarters = n(),
      N_Firms = n_distinct(firm_id),
      Downgrade_Rows = sum(Downgrade == 1L, na.rm = TRUE),
      Downgrade_Count = sum(downgrade_count, na.rm = TRUE),
      Downgrade_Rate = mean(Downgrade == 1L, na.rm = TRUE),
      Median_TA = median_or_na(ta),
      Median_Log_TA = median_or_na(log_ta),
      .groups = "drop"
    ) %>%
    arrange(Size_Bucket)
} else {
  tibble()
}
save_audit(final_panel_size_bucket_summary, "final_panel_size_bucket_summary_v5.csv")

save_plot(
  ggplot(quarter_summary, aes(x = as.Date(sprintf("%04d-%02d-01", year, (quarter_number - 1L) * 3L + 1L)))) +
    geom_col(aes(y = downgrade_count), fill = main_plot_colours[["blue"]]) +
    scale_y_continuous(labels = comma) +
    labs(x = "Quarter", y = "Downgrade count", title = "Quarterly downgrade counts") +
    theme_minimal(base_size = 11),
  "fig_01_quarterly_downgrade_count_v5.png",
  width = 10,
  height = 5
)

save_plot(
  ggplot(year_summary, aes(x = year, y = downgrade_rate)) +
    geom_line(color = main_plot_colours[["blue"]], linewidth = 0.8) +
    geom_point(color = main_plot_colours[["blue"]], size = 1.8) +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    labs(x = "Year", y = "Downgrade rate", title = "Downgrade rate by year") +
    theme_minimal(base_size = 11),
  "fig_02_downgrade_rate_by_year_v5.png"
)

save_plot(
  ggplot(rating_group_summary, aes(x = rating_group, y = n_rows, fill = rating_group)) +
    geom_col(show.legend = FALSE) +
    scale_fill_manual(values = main_plot_palette(n_distinct(rating_group_summary$rating_group))) +
    scale_y_continuous(labels = comma) +
    labs(x = "Rating group", y = "Firm-quarter rows", title = "Final panel rating group distribution") +
    theme_minimal(base_size = 11),
  "fig_03_rating_group_distribution_final_panel_v5.png"
)

save_plot(
  ggplot(rating_group_summary, aes(x = rating_group, y = downgrade_rate, fill = rating_group)) +
    geom_col(show.legend = FALSE) +
    scale_fill_manual(values = main_plot_palette(n_distinct(rating_group_summary$rating_group))) +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    labs(x = "Rating group", y = "Downgrade rate", title = "Final panel downgrade rate by rating group") +
    theme_minimal(base_size = 11),
  "fig_04_downgrade_rate_by_rating_group_final_panel_v5.png"
)

if (nrow(final_panel_size_bucket_summary) > 0L) {
  save_plot(
    ggplot(final_panel_size_bucket_summary, aes(x = Size_Bucket, y = N_Firms, fill = Size_Bucket)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = comma(N_Firms)), vjust = -0.25, size = 3.3) +
      scale_fill_manual(values = main_plot_palette(n_distinct(final_panel_size_bucket_summary$Size_Bucket))) +
      scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.08))) +
      labs(x = "Size bucket", y = "Firms", title = "Final panel firm distribution by total-assets size bucket") +
      theme_minimal(base_size = 11),
    "fig_size_bucket_firm_distribution_final_panel_v5.png"
  )

  save_plot(
    ggplot(final_panel_size_bucket_summary, aes(x = Size_Bucket, y = Downgrade_Rate, fill = Size_Bucket)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = paste0("N=", comma(N_Firm_Quarters))), vjust = -0.25, size = 3.2) +
      scale_fill_manual(values = main_plot_palette(n_distinct(final_panel_size_bucket_summary$Size_Bucket))) +
      scale_y_continuous(labels = percent_format(accuracy = 0.1), expand = expansion(mult = c(0, 0.10))) +
      labs(x = "Size bucket", y = "Downgrade rate", title = "Final panel downgrade rate by total-assets size bucket") +
      theme_minimal(base_size = 11),
    "fig_size_bucket_downgrade_rate_final_panel_v5.png"
  )
}

save_plot(
  ggplot(quarter_summary, aes(x = quarter_date, y = downgrade_rate)) +
    geom_hline(
      data = final_panel_time_clustering_summary,
      aes(yintercept = Mean_Downgrade_Rate, linetype = "Full-sample mean"),
      color = "#6f6f6f",
      linewidth = 0.4,
      inherit.aes = FALSE
    ) +
    geom_line(aes(color = "Quarterly %"), linewidth = 0.55) +
    geom_point(aes(color = "Quarterly %"), size = 1.3) +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    scale_color_manual(values = c("Quarterly %" = main_plot_colours[["blue"]])) +
    scale_linetype_manual(values = c("Full-sample mean" = "dashed")) +
    labs(x = "Quarter", y = "n. of Downgrades / n. of firms per quarter (in %)", color = NULL, linetype = NULL, title = "Final panel quarterly downgrade percentage (%)") +
    theme_minimal(base_size = 11),
  "fig_pre_model_quarterly_downgrade_rate_final_panel_v5.png",
  width = 10,
  height = 5
)

save_plot(
  ggplot(final_panel_seasonality_by_quarter, aes(x = QuarterOfYear, y = P_Downgrade)) +
    geom_col(fill = main_plot_colours[["blue"]]) +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    labs(x = "Quarter of year", y = "P(Downgrade)", title = "Final panel downgrade seasonality") +
    theme_minimal(base_size = 11),
  "fig_pre_model_seasonality_final_panel_v5.png"
)

save_plot(
  ggplot(final_panel_issuer_persistence_bucket_distribution, aes(x = Downgrade_Frequency_Bucket, y = N_Issuers)) +
    geom_col(fill = main_plot_colours[["blue"]]) +
    geom_text(aes(label = comma(N_Issuers)), vjust = -0.25, size = 3.3) +
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.08))) +
    labs(x = expression("Issuer downgrade frequency bucket (" * N[i]^down / T[i] * ")"), y = "Issuers", title = "Final panel issuer persistence in downgrades") +
    theme_minimal(base_size = 11),
  "fig_pre_model_issuer_persistence_distribution_final_panel_v5.png"
)

save_plot(
  ggplot(final_panel_issuer_persistence, aes(x = N_i_down_over_T_i)) +
    geom_histogram(bins = 30, fill = main_plot_colours[["blue"]], color = "white", linewidth = 0.2) +
    scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
    scale_y_continuous(labels = comma) +
    labs(x = expression("Issuer downgrade frequency (" * N[i]^down / T[i] * ")"), y = "Issuers", title = "Final panel issuer persistence in downgrades") +
    theme_minimal(base_size = 11),
  "fig_pre_model_issuer_persistence_histogram_final_panel_v5.png"
)

if (nrow(final_panel_previous_history_next_downgrade_rate) > 0L) {
  save_plot(
    ggplot(final_panel_previous_history_next_downgrade_rate, aes(x = factor(N_Downgrades_Previous_4Q_Bucket, levels = c("0", "1", "2", "3", "4")), y = Downgrade_Rate)) +
      geom_col(fill = main_plot_colours[["blue"]]) +
      geom_text(aes(label = paste0("N=", comma(N_Firm_Quarters))), vjust = -0.3, size = 3.2) +
      scale_y_continuous(labels = percent_format(accuracy = 0.1), expand = expansion(mult = c(0, 0.12))) +
      labs(x = "Downgrade quarters in previous 4 quarters", y = "Current-quarter downgrade rate", title = "Final panel downgrade frequency by recent rating history") +
      theme_minimal(base_size = 11),
    "fig_pre_model_previous_4q_history_current_downgrade_rate_final_panel_v5.png"
  )
}

if (nrow(final_panel_previous_event_history_next_downgrade_rate) > 0L) {
  save_plot(
    ggplot(final_panel_previous_event_history_next_downgrade_rate, aes(x = N_Downgrade_Events_Previous_4Q_Bucket, y = Downgrade_Rate)) +
      geom_col(fill = main_plot_colours[["blue"]]) +
      geom_text(aes(label = paste0("N=", comma(N_Firm_Quarters))), vjust = -0.3, size = 3.2) +
      scale_y_continuous(labels = percent_format(accuracy = 0.1), expand = expansion(mult = c(0, 0.12))) +
      labs(x = "Downgrade events in previous 4 quarters", y = "Current-quarter downgrade rate", title = "Final panel downgrade frequency by recent rating-event history") +
      theme_minimal(base_size = 11),
    "fig_pre_model_previous_4q_event_history_current_downgrade_rate_final_panel_v5.png"
  )
}

save_plot(
  ggplot(missingness_summary %>% filter(missing_values > 0L), aes(x = reorder(variable, missing_rate), y = missing_rate)) +
    geom_col(fill = main_plot_colours[["blue"]]) +
    coord_flip() +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    labs(x = NULL, y = "Missing rate", title = "Model input missingness before imputation") +
    theme_minimal(base_size = 11),
  "fig_05_model_input_missingness_before_imputation_v5.png",
  width = 9,
  height = 7
)

financial_correlation_plot_data <- financial_within_firm_correlation %>%
  select(Variable, starts_with("Within_Firm_Correlation_Lag_")) %>%
  pivot_longer(-Variable, names_to = "Lag", values_to = "Within_Firm_Correlation") %>%
  mutate(Lag = str_replace(Lag, "Within_Firm_Correlation_Lag_", "Lag "))

save_plot(
  ggplot(financial_correlation_plot_data, aes(x = reorder(Variable, Within_Firm_Correlation), y = Within_Firm_Correlation, fill = Lag)) +
    geom_col(position = "dodge") +
    coord_flip() +
    scale_fill_manual(values = main_plot_palette(n_distinct(financial_correlation_plot_data$Lag))) +
    scale_y_continuous(labels = number_format(accuracy = 0.01), limits = c(-1, 1)) +
    labs(x = NULL, y = "Within-firm correlation", title = "Financial variable persistence") +
    theme_minimal(base_size = 11),
  "fig_06_financial_within_firm_lag_correlations_v5.png",
  width = 9,
  height = 7
)

save_plot(
  ggplot(financial_icc, aes(x = reorder(Variable, ICC), y = ICC)) +
    geom_col(fill = main_plot_colours[["blue"]]) +
    coord_flip() +
    scale_y_continuous(labels = number_format(accuracy = 0.01), limits = c(0, 1)) +
    labs(x = NULL, y = "ICC", title = "Within-vs-between firm variance") +
    theme_minimal(base_size = 11),
  "fig_07_financial_intraclass_correlation_v5.png",
  width = 9,
  height = 7
)

validation_calendar <- create_validation_calendar(VALIDATION_YEARS)

target_lookup <- panel_final %>%
  transmute(
    Country,
    firm_id,
    Target_Quarter_Index = Quarter_Index,
    Target_Date = Dates,
    Outcome_Num = Downgrade,
    Target_Downgrade_Count = downgrade_count,
    Target_Downgrade_Notches_Lost = downgrade_notches_lost,
    Target_Rating_Migration_Count = rating_action_count
  )

firm_last_quarter <- panel_final %>%
  group_by(Country, firm_id) %>%
  summarise(Last_Firm_Quarter_Index = max(Quarter_Index), .groups = "drop")

# Target construction: use quarter indices rather than date arithmetic to avoid
# month-end inconsistencies when building t+h labels.
target_candidates <- panel_final %>%
  tidyr::crossing(Horizon = HORIZONS) %>%
  mutate(
    Horizon_Label = paste0("t+", Horizon),
    Predictor_Date = Dates,
    Predictor_Quarter_Index = Quarter_Index,
    Target_Quarter_Index = Predictor_Quarter_Index + Horizon
  ) %>%
  left_join(target_lookup, by = c("Country", "firm_id", "Target_Quarter_Index")) %>%
  left_join(firm_last_quarter, by = c("Country", "firm_id")) %>%
  mutate(
    Target_Available = !is.na(Target_Date) & !is.na(Outcome_Num),
    Missing_Internal_Target = !Target_Available & Target_Quarter_Index <= Last_Firm_Quarter_Index
  )

target_availability_audit <- target_candidates %>%
  group_by(Horizon, Horizon_Label) %>%
  summarise(
    N_Predictor_Observations = n(),
    N_Targets_Available = sum(Target_Available),
    N_Targets_Unavailable = sum(!Target_Available),
    N_Internal_Target_Gaps = sum(Missing_Internal_Target),
    N_Right_Censored = sum(!Target_Available & Target_Quarter_Index > Last_Firm_Quarter_Index),
    .groups = "drop"
  )
save_audit(target_availability_audit, "target_availability_by_horizon_v5.csv")

Panel_Europe_multihorizon_raw <- target_candidates %>%
  filter(Target_Available) %>%
  mutate(
    Outcome_Num = as.integer(Outcome_Num),
    Outcome = factor(if_else(Outcome_Num == 1L, "Yes", "No"), levels = c("No", "Yes")),
    Observation_ID = paste(
      Country,
      firm_id,
      format(Predictor_Date, "%Y-%m-%d"),
      paste0("h", Horizon),
      sep = "__"
    ),
    Downgrade_At_Predictor_Date = Downgrade,
    Temporal_Sample = case_when(
      Target_Date < TEST_START ~ "Pre_Test",
      Target_Date >= TEST_START ~ "Test",
      TRUE ~ "Unassigned"
    )
  ) %>%
  select(
    Observation_ID,
    Country,
    Company_Name,
    Company_Ticker,
    firm_id,
    Predictor_Date,
    Target_Date,
    Horizon,
    Horizon_Label,
    Temporal_Sample,
    Outcome,
    Outcome_Num,
    Target_Downgrade_Count,
    Target_Downgrade_Notches_Lost,
    Target_Rating_Migration_Count,
    Downgrade_At_Predictor_Date,
    all_of(rating_vars_for_carry),
    all_of(rating_history_vars_present),
    all_of(rating_history_bucket_vars_present),
    all_of(seasonality_vars_present),
    all_of(raw_financial_vars_present),
    all_of(computed_ratio_vars),
    all_of(existing_ratio_vars_present),
    all_of(macro_vars_present),
    all_of(ratio_zero_denominator_flags),
    N_Missing_Model_Predictors_Raw,
    Any_Missing_Model_Predictor_Raw,
    Predictor_Quarter_Index,
    Target_Quarter_Index
  ) %>%
  arrange(Horizon, Target_Date, Country, firm_id)

target_timing_audit <- Panel_Europe_multihorizon_raw %>%
  mutate(
    Realised_Horizon = Target_Quarter_Index - Predictor_Quarter_Index,
    Correct_Horizon = Realised_Horizon == Horizon
  ) %>%
  count(Horizon, Horizon_Label, Realised_Horizon, Correct_Horizon, name = "N_Observations")
save_audit(target_timing_audit, "target_timing_audit_v5.csv")

duplicate_observation_ids <- Panel_Europe_multihorizon_raw %>%
  count(Observation_ID, name = "N") %>%
  filter(N > 1L)
save_audit(duplicate_observation_ids, "duplicate_observation_id_audit_v5.csv")
if (nrow(duplicate_observation_ids) > 0L || !all(target_timing_audit$Correct_Horizon)) {
  stop("Multi-horizon target timing or observation-id checks failed.", call. = FALSE)
}

pre_model_screening_vars <- intersect(
  unique(c(
    main_financial_predictors,
    main_macro_pars_stat_predictors,
    main_macro_delta_predictors,
    main_systemic_predictors,
    "rating_rank",
    "macro_rating"
  )),
  names(Panel_Europe_multihorizon_raw)
)

# Distribution-shift diagnostics: compare pre-test and locked-test covariates by
# horizon because temporal splits are defined on target dates.
distribution_shift_pretest_test <- distribution_shift_table(
  Panel_Europe_multihorizon_raw,
  pre_model_screening_vars,
  group_var = "Temporal_Sample",
  reference_level = "Pre_Test",
  comparison_level = "Test",
  by_vars = c("Horizon", "Horizon_Label")
) %>%
  arrange(desc(abs(Standardized_Difference)), Horizon, Variable)
save_audit(distribution_shift_pretest_test, "distribution_shift_pretest_vs_test_by_horizon_v5.csv")

save_plot(
  ggplot(distribution_shift_pretest_test %>% filter(is.finite(Standardized_Difference)) %>% group_by(Horizon_Label) %>% slice_max(abs(Standardized_Difference), n = 12, with_ties = FALSE) %>% ungroup(),
         aes(x = reorder(Variable, Standardized_Difference), y = Standardized_Difference, fill = Standardized_Difference > 0)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    facet_wrap(~ Horizon_Label, scales = "free_y") +
    labs(x = NULL, y = "Standardized difference: Test - Pre-test", title = "Largest pre-test/test distribution shifts") +
    theme_minimal(base_size = 10),
  "fig_11_distribution_shift_pretest_vs_test_v5.png",
  width = 12,
  height = 8
)

# Univariate screening: compare predictor-origin covariates between future
# downgrade and no-downgrade observations without fitting a model.
univariate_future_downgrade_screening <- purrr::map_dfr(pre_model_screening_vars, function(variable) {
  Panel_Europe_multihorizon_raw %>%
    group_by(Horizon, Horizon_Label, Outcome_Num) %>%
    summarise(
      Variable = variable,
      N = sum(is.finite(.data[[variable]])),
      Mean = safe_mean(.data[[variable]]),
      SD = safe_sd_value(.data[[variable]]),
      P25 = safe_quantile_value(.data[[variable]], 0.25),
      Median = safe_quantile_value(.data[[variable]], 0.50),
      P75 = safe_quantile_value(.data[[variable]], 0.75),
      .groups = "drop"
    )
}) %>%
  mutate(Outcome = if_else(Outcome_Num == 1L, "Future_Downgrade", "No_Future_Downgrade")) %>%
  select(-Outcome_Num) %>%
  arrange(Horizon, Variable, Outcome)
save_audit(univariate_future_downgrade_screening, "univariate_future_downgrade_screening_v5.csv")

univariate_future_downgrade_contrast <- univariate_future_downgrade_screening %>%
  select(Horizon, Horizon_Label, Variable, Outcome, N, Mean, Median) %>%
  pivot_wider(
    names_from = Outcome,
    values_from = c(N, Mean, Median)
  ) %>%
  mutate(
    Mean_Difference_Downgrade_Minus_No = Mean_Future_Downgrade - Mean_No_Future_Downgrade,
    Median_Difference_Downgrade_Minus_No = Median_Future_Downgrade - Median_No_Future_Downgrade
  ) %>%
  arrange(Horizon, desc(abs(Mean_Difference_Downgrade_Minus_No)))
save_audit(univariate_future_downgrade_contrast, "univariate_future_downgrade_contrasts_v5.csv")

save_plot(
  ggplot(univariate_future_downgrade_contrast %>% filter(is.finite(Mean_Difference_Downgrade_Minus_No)) %>% group_by(Horizon_Label) %>% slice_max(abs(Mean_Difference_Downgrade_Minus_No), n = 12, with_ties = FALSE) %>% ungroup(),
         aes(x = reorder(Variable, Mean_Difference_Downgrade_Minus_No), y = Mean_Difference_Downgrade_Minus_No, fill = Mean_Difference_Downgrade_Minus_No > 0)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    facet_wrap(~ Horizon_Label, scales = "free_y") +
    labs(x = NULL, y = "Mean difference: future downgrade - no future downgrade", title = "Largest univariate future-downgrade contrasts") +
    theme_minimal(base_size = 10),
  "fig_12_univariate_future_downgrade_contrasts_v5.png",
  width = 12,
  height = 8
)

future_baseline_global <- Panel_Europe_multihorizon_raw %>%
  group_by(Horizon, Horizon_Label) %>%
  summarise(
    Baseline = "global",
    Group = "All",
    N_Observations = n(),
    N_Downgrades = sum(Outcome_Num == 1L, na.rm = TRUE),
    Downgrade_Rate = mean(Outcome_Num == 1L, na.rm = TRUE),
    .groups = "drop"
  )

future_baseline_rating_group <- Panel_Europe_multihorizon_raw %>%
  group_by(Horizon, Horizon_Label, rating_group) %>%
  summarise(
    Baseline = "rating_group",
    Group = as.character(first(rating_group)),
    N_Observations = n(),
    N_Downgrades = sum(Outcome_Num == 1L, na.rm = TRUE),
    Downgrade_Rate = mean(Outcome_Num == 1L, na.rm = TRUE),
    .groups = "drop"
  )

future_baseline_rating_grade <- Panel_Europe_multihorizon_raw %>%
  group_by(Horizon, Horizon_Label, rating_grade) %>%
  summarise(
    Baseline = "rating_grade",
    Group = as.character(first(rating_grade)),
    N_Observations = n(),
    N_Downgrades = sum(Outcome_Num == 1L, na.rm = TRUE),
    Downgrade_Rate = mean(Outcome_Num == 1L, na.rm = TRUE),
    .groups = "drop"
  )

future_baseline_history <- if ("N_Downgrades_Previous_4Q_Bucket" %in% names(Panel_Europe_multihorizon_raw)) {
  Panel_Europe_multihorizon_raw %>%
    group_by(Horizon, Horizon_Label, N_Downgrades_Previous_4Q_Bucket) %>%
    summarise(
      Baseline = "previous_4q_downgrade_bucket",
      Group = as.character(first(N_Downgrades_Previous_4Q_Bucket)),
      N_Observations = n(),
      N_Downgrades = sum(Outcome_Num == 1L, na.rm = TRUE),
      Downgrade_Rate = mean(Outcome_Num == 1L, na.rm = TRUE),
      .groups = "drop"
    )
} else {
  tibble()
}

future_baseline_naive_rates <- bind_rows(
  future_baseline_global,
  future_baseline_rating_group,
  future_baseline_rating_grade,
  future_baseline_history
) %>%
  arrange(Horizon, Baseline, Group)
save_audit(future_baseline_naive_rates, "baseline_naive_future_downgrade_rates_by_horizon_v5.csv")

future_systemic_regime_rates <- purrr::map_dfr(systemic_regime_vars_present, function(variable) {
  if (!variable %in% names(Panel_Europe_multihorizon_raw)) {
    return(tibble())
  }
  Panel_Europe_multihorizon_raw %>%
    filter(is.finite(.data[[variable]])) %>%
    group_by(Horizon, Horizon_Label) %>%
    mutate(
      Regime_Rank = ntile(.data[[variable]], 3L),
      Regime = factor(
        case_when(
          Regime_Rank == 1L ~ "Low",
          Regime_Rank == 2L ~ "Middle",
          Regime_Rank == 3L ~ "High",
          TRUE ~ NA_character_
        ),
        levels = c("Low", "Middle", "High")
      )
    ) %>%
    ungroup() %>%
    group_by(Horizon, Horizon_Label, Regime) %>%
    summarise(
      Variable = variable,
      N_Observations = n(),
      N_Firms = n_distinct(firm_id),
      Mean_Regime_Value = mean(.data[[variable]], na.rm = TRUE),
      N_Downgrades = sum(Outcome_Num == 1L, na.rm = TRUE),
      Downgrade_Rate = mean(Outcome_Num == 1L, na.rm = TRUE),
      .groups = "drop"
    )
})
save_audit(future_systemic_regime_rates, "systemic_regime_future_downgrade_rates_by_horizon_v5.csv")

if (nrow(future_systemic_regime_rates) > 0L) {
  save_plot(
    ggplot(future_systemic_regime_rates, aes(x = Regime, y = Downgrade_Rate, fill = Regime)) +
      geom_col(show.legend = FALSE) +
      facet_grid(Variable ~ Horizon_Label) +
      scale_fill_manual(values = main_plot_palette(n_distinct(future_systemic_regime_rates$Regime))) +
      scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
      labs(x = "Systemic-risk regime", y = "Future downgrade rate", title = "Future downgrade rate by systemic-risk regime and horizon") +
      theme_minimal(base_size = 10),
    "fig_13_systemic_regime_future_downgrade_rates_v5.png",
    width = 12,
    height = 7
  )
}

expanding_folds_raw <- purrr::pmap_dfr(
  validation_calendar,
  function(Fold, Validation_Start, Validation_End) {
    create_expanding_partition(Panel_Europe_multihorizon_raw, Fold, Validation_Start, Validation_End)
  }
) %>%
  filter(Fold_Role %in% c("Training", "Validation")) %>%
  arrange(Horizon, Fold, Fold_Role, Target_Date, Country, firm_id)

rolling_folds_raw <- purrr::pmap_dfr(
  validation_calendar,
  function(Fold, Validation_Start, Validation_End) {
    create_rolling_partition(Panel_Europe_multihorizon_raw, Fold, Validation_Start, Validation_End)
  }
) %>%
  filter(Fold_Role %in% c("Training", "Validation")) %>%
  arrange(Horizon, Fold, Fold_Role, Target_Date, Country, firm_id)

locked_test_raw <- Panel_Europe_multihorizon_raw %>%
  mutate(
    Split_Scheme = "Locked_Test",
    Fold = "PreTest_to_Test",
    Validation_Start = TEST_START,
    Validation_End = as.Date(NA),
    Fold_Role = if_else(Temporal_Sample == "Pre_Test", "Training", "Test")
  ) %>%
  arrange(Horizon, Fold_Role, Target_Date, Country, firm_id)

raw_split_summary <- bind_rows(expanding_folds_raw, rolling_folds_raw, locked_test_raw) %>%
  group_by(Split_Scheme, Horizon, Horizon_Label, Fold_Role) %>%
  summarise(
    N_Observations = n(),
    N_Firms = n_distinct(firm_id),
    N_Countries = n_distinct(Country),
    First_Target_Date = min(Target_Date),
    Last_Target_Date = max(Target_Date),
    N_Downgrades = sum(Outcome_Num),
    Downgrade_Rate_Pct = 100 * mean(Outcome_Num),
    N_With_Missing_Raw = sum(Any_Missing_Model_Predictor_Raw),
    .groups = "drop"
  )
save_audit(raw_split_summary, "raw_split_summary_v5.csv")

write_dataset(panel_final, RAW_DIR, "Panel_Europe_macro_single_horizon_raw")
write_dataset(Panel_Europe_multihorizon_raw, RAW_DIR, "Panel_Europe_macro_multihorizon_raw")
write_dataset(expanding_folds_raw, RAW_DIR, "Panel_Europe_macro_expanding_window_folds_raw")
write_dataset(rolling_folds_raw, RAW_DIR, "Panel_Europe_macro_rolling_window_folds_raw")
write_dataset(locked_test_raw, RAW_DIR, "Panel_Europe_macro_locked_test_raw")

raw_missingness <- Panel_Europe_multihorizon_raw %>%
  summarise(across(all_of(model_predictor_vars), ~ mean(is.na(.x)) * 100)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Missing_Pct_Raw") %>%
  arrange(desc(Missing_Pct_Raw))
save_audit(raw_missingness, "raw_multihorizon_missingness_v5.csv")

raw_size_missingness <- if (length(size_imputation_vars) > 0L) {
  Panel_Europe_multihorizon_raw %>%
    summarise(across(all_of(size_imputation_vars), ~ mean(is.na(.x) | !is.finite(.x)) * 100)) %>%
    pivot_longer(everything(), names_to = "Variable", values_to = "Missing_Or_Nonfinite_Pct_Raw") %>%
    arrange(desc(Missing_Or_Nonfinite_Pct_Raw))
} else {
  tibble()
}
save_audit(raw_size_missingness, "raw_multihorizon_size_missingness_v5.csv")

expanding_imputed_result <- impute_by_fold(expanding_folds_raw, fold_imputation_vars, model_predictor_vars)
rolling_imputed_result <- impute_by_fold(rolling_folds_raw, fold_imputation_vars, model_predictor_vars)
locked_test_imputed_result <- impute_by_fold(locked_test_raw, fold_imputation_vars, model_predictor_vars)

expanding_folds_imputed <- expanding_imputed_result$data
rolling_folds_imputed <- rolling_imputed_result$data
locked_test_imputed <- locked_test_imputed_result$data
imputation_audit <- bind_rows(expanding_imputed_result$audit, rolling_imputed_result$audit, locked_test_imputed_result$audit)
save_audit(imputation_audit, "fold_specific_imputation_audit_v5.csv")

if (length(size_imputation_vars) > 0L) {
  expanding_size_result <- add_size_buckets_by_split(expanding_folds_imputed)
  rolling_size_result <- add_size_buckets_by_split(rolling_folds_imputed)
  locked_test_size_result <- add_size_buckets_by_split(locked_test_imputed)
  expanding_folds_imputed <- expanding_size_result$data
  rolling_folds_imputed <- rolling_size_result$data
  locked_test_imputed <- locked_test_size_result$data
  save_audit(
    bind_rows(expanding_size_result$audit, rolling_size_result$audit, locked_test_size_result$audit),
    "fold_size_transform_and_bucket_audit_v5.csv"
  )
}

imputation_summary <- imputation_audit %>%
  group_by(Split_Scheme, Variable) %>%
  summarise(
    Total_Rows_Imputed_Across_Folds = sum(Rows_Imputed),
    Total_Zero_Denominator_Caps = sum(Rows_Zero_Denominator_Cap),
    Total_Firm_Median = sum(Rows_Firm_Median),
    Total_Country_Median = sum(Rows_Country_Median),
    Total_Global_Median = sum(Rows_Global_Median),
    Total_Missing_After = sum(Missing_After),
    Total_Nonfinite_After = sum(Nonfinite_After),
    .groups = "drop"
  )
save_audit(imputation_summary, "imputation_summary_by_scheme_variable_v5.csv")

imputed_split_summary <- bind_rows(expanding_folds_imputed, rolling_folds_imputed, locked_test_imputed) %>%
  group_by(Split_Scheme, Horizon, Horizon_Label, Fold_Role) %>%
  summarise(
    N_Observations = n(),
    N_Firms = n_distinct(firm_id),
    N_Countries = n_distinct(Country),
    First_Target_Date = min(Target_Date),
    Last_Target_Date = max(Target_Date),
    N_Downgrades = sum(Outcome_Num),
    Downgrade_Rate_Pct = 100 * mean(Outcome_Num),
    N_With_Missing_Raw = sum(Any_Missing_Model_Predictor_Raw),
    N_With_Missing_After = sum(Any_Missing_Model_Predictor_After_Imputation),
    .groups = "drop"
  )
save_audit(imputed_split_summary, "imputed_split_summary_v5.csv")

if (
  !all(imputation_audit$Missing_After == 0L) ||
    !all(imputation_audit$Nonfinite_After == 0L) ||
    !all(expanding_folds_imputed$N_Missing_Model_Predictors_After_Imputation == 0L) ||
    !all(rolling_folds_imputed$N_Missing_Model_Predictors_After_Imputation == 0L) ||
    !all(locked_test_imputed$N_Missing_Model_Predictors_After_Imputation == 0L)
) {
  stop("Post-imputation missing/non-finite checks failed.", call. = FALSE)
}

# Save model-ready imputed datasets in both R-native and CSV formats.
write_dataset(expanding_folds_imputed, IMPUTED_DIR, "Panel_Europe_macro_expanding_window_folds_imputed", csv = TRUE)
write_dataset(rolling_folds_imputed, IMPUTED_DIR, "Panel_Europe_macro_rolling_window_folds_imputed", csv = TRUE)
write_dataset(locked_test_imputed, IMPUTED_DIR, "Panel_Europe_macro_locked_test_imputed", csv = TRUE)

part_3_modeling_manifest <- tibble(
  Setting = c(
    "Multi-horizon split location",
    "Imputation location",
    "Target construction",
    "Target-date split rule",
    "Test target start",
    "Validation target years",
    "Rolling training years",
    "Horizons",
    "Model predictors imputed",
    "Rating variables carried without imputation",
    "Rating-history variables in V5 regressor manifest",
    "Rating-history variables carried for diagnostics",
    "V5 regressor sets",
    "Systemic-risk variables",
    "Seasonality variables"
  ),
  Value = c(
    "Part_3_V5",
    "Part_3_V5",
    "Only rated firm-quarters are used as predictors and eligible targets",
    "Splits are based on Target_Date = t + h",
    as.character(TEST_START),
    paste(VALIDATION_YEARS, collapse = ", "),
    as.character(ROLLING_TRAIN_YEARS),
    paste(paste0("t+", HORIZONS), collapse = ", "),
    paste(model_predictor_vars, collapse = ", "),
    paste(rating_vars_for_carry, collapse = ", "),
    "None",
    paste(rating_history_vars_present, collapse = ", "),
    paste(main_regressor_manifest$Regressor_Set, collapse = ", "),
    paste(systemic_vars_present, collapse = ", "),
    "Quarter-of-year is not included in V5 model specifications."
  )
)
save_audit(part_3_modeling_manifest, "part_3_v5_modeling_manifest.csv")

write_csv(panel_final, file.path(DATA_DIR, "panel_europe_v5_rating_macro_with_downgrade.csv"), na = "")
saveRDS(panel_final, file.path(DATA_DIR, "panel_europe_v5_rating_macro_with_downgrade.rds"))

writeLines(
  kable(join_report, format = "latex", booktabs = TRUE),
  file.path(TABLE_DIR, "panel_v5_join_report.tex")
)
writeLines(
  kable(rating_group_summary, format = "latex", booktabs = TRUE),
  file.path(TABLE_DIR, "rating_group_summary_v5.tex")
)
writeLines(
  kable(year_summary, format = "latex", booktabs = TRUE),
  file.path(TABLE_DIR, "downgrade_distribution_by_year_v5.tex")
)

message("Part 3 V5 completed.")
message("Final rows: ", nrow(panel_final))
message("Final firms: ", n_distinct(panel_final$Company_Name))
message("Downgrade rows: ", sum(panel_final$Downgrade == 1L, na.rm = TRUE))
message("Downgrade count: ", sum(panel_final$downgrade_count, na.rm = TRUE))
message("Downgrade rate: ", percent(mean(panel_final$Downgrade == 1L, na.rm = TRUE), accuracy = 0.01))



