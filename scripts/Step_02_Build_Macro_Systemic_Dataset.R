# Step 02 - Build the country-quarter macroeconomic and systemic-risk dataset.
# GitHub-ready copy: paths are repository-relative and data files are intentionally excluded.

## Systemic stress is retained for the definitive no-history main specification.

required_packages <- c("xml2", "readxl", "dplyr", "tidyr", "stringr", "lubridate", "readr", "purrr", "tibble", "ggplot2", "scales", "plm", "knitr")
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0L) {
  stop("Install missing package(s): ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

suppressPackageStartupMessages({
  library(xml2)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(readr)
  library(purrr)
  library(tibble)
  library(ggplot2)
  library(scales)
  library(plm)
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

SCRIPT_DIR <- get_script_dir()
V5_DIR <- normalizePath(file.path(SCRIPT_DIR, ".."), winslash = "/", mustWork = TRUE)
INPUT_DIR <- file.path(V5_DIR, "input")

OUTPUT_DIR <- file.path(V5_DIR, "Part_2_Outputs")
DATA_DIR <- file.path(OUTPUT_DIR, "data")
AUDIT_DIR <- file.path(OUTPUT_DIR, "audits")
FIGURE_DIR <- file.path(OUTPUT_DIR, "figures")
dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(AUDIT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIGURE_DIR, recursive = TRUE, showWarnings = FALSE)

macro_files <- list(
  gdp = file.path(INPUT_DIR, "namq_10_gdp__custom_22184615_page_spreadsheet.xlsx"),
  industrial_production = file.path(INPUT_DIR, "sts_inpr_m__custom_22170566_page_spreadsheet.xlsx"),
  unemployment = file.path(INPUT_DIR, "une_rt_m__custom_22171523_page_spreadsheet.xlsx"),
  hicp = file.path(INPUT_DIR, "prc_hicp_minr__custom_22174938_page_spreadsheet.xlsx"),
  long_term_gov_yield = file.path(INPUT_DIR, "irt_lt_mcby_m__custom_22175193_page_spreadsheet.xlsx"),
  public_debt = file.path(INPUT_DIR, "gov_10q_ggdebt__custom_22179584_page_spreadsheet.xlsx")
)

systemic_files <- list(
  ciss_euro_area = file.path(INPUT_DIR, "systemic_variables", "from_2005", "CISS_Euro_Area_from_2005.xlsx"),
  ciss_country = file.path(INPUT_DIR, "systemic_variables", "from_2005", "CISS_Specific_Countries_from_2005.xlsx"),
  vstoxx = file.path(INPUT_DIR, "systemic_variables", "from_2005", "VSTOXX_from_2005.xlsx")
)

# VSTOXX high-stress threshold: retain the V3 systemic-extension cutoff to keep
# the V5 audit comparable with the previous diagnostic.
HIGH_VSTOXX_THRESHOLD <- 31.7
PANEL_START_DATE <- as.Date("2006-01-01")
SYSTEMIC_SUPPORT_START_DATE <- as.Date("2005-01-01")

missing_inputs <- names(macro_files)[!file.exists(unlist(macro_files))]
if (length(missing_inputs) > 0L) {
  stop("Missing macro input file(s): ", paste(missing_inputs, collapse = ", "), call. = FALSE)
}

missing_systemic_inputs <- names(systemic_files)[!file.exists(unlist(systemic_files))]
if (length(missing_systemic_inputs) > 0L) {
  stop("Missing systemic-risk input file(s): ", paste(missing_systemic_inputs, collapse = ", "), call. = FALSE)
}

excel_col_to_num <- function(col_ref) {
  letters <- strsplit(col_ref, "", fixed = TRUE)[[1]]
  Reduce(function(total, letter) total * 26 + match(letter, LETTERS), letters, init = 0)
}

cell_ref_to_position <- function(cell_ref) {
  col_ref <- gsub("[0-9]", "", cell_ref)
  row_ref <- as.integer(gsub("[A-Z]", "", cell_ref))
  c(row = row_ref, col = excel_col_to_num(col_ref))
}

read_xlsx_sheet_xml <- function(path, sheet_name = "Sheet 1") {
  tmp_dir <- tempfile("xlsx_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  utils::unzip(path, exdir = tmp_dir)

  workbook <- read_xml(file.path(tmp_dir, "xl", "workbook.xml"))
  xml_ns_strip(workbook)
  sheets <- xml_find_all(workbook, ".//sheet")
  sheet_names <- xml_attr(sheets, "name")
  sheet_ids <- xml_attr(sheets, "id")

  if (!sheet_name %in% sheet_names) {
    stop("Sheet '", sheet_name, "' not found in ", basename(path), call. = FALSE)
  }

  rels <- read_xml(file.path(tmp_dir, "xl", "_rels", "workbook.xml.rels"))
  xml_ns_strip(rels)
  rel_nodes <- xml_find_all(rels, ".//Relationship")
  rel_map <- setNames(xml_attr(rel_nodes, "Target"), xml_attr(rel_nodes, "Id"))
  sheet_target <- rel_map[[sheet_ids[match(sheet_name, sheet_names)]]]
  sheet_file <- file.path(tmp_dir, "xl", gsub("/", .Platform$file.sep, sheet_target))

  shared_strings_file <- file.path(tmp_dir, "xl", "sharedStrings.xml")
  shared_strings <- character()
  if (file.exists(shared_strings_file)) {
    shared_xml <- read_xml(shared_strings_file)
    xml_ns_strip(shared_xml)
    shared_strings <- xml_text(xml_find_all(shared_xml, ".//si"))
  }

  sheet_xml <- read_xml(sheet_file)
  xml_ns_strip(sheet_xml)
  cells <- xml_find_all(sheet_xml, ".//c")
  if (length(cells) == 0L) {
    return(matrix(NA_character_, nrow = 0L, ncol = 0L))
  }

  refs <- xml_attr(cells, "r")
  positions <- t(vapply(refs, cell_ref_to_position, numeric(2)))
  values <- character(length(cells))

  for (i in seq_along(cells)) {
    cell_type <- xml_attr(cells[[i]], "t")
    raw_value <- xml_text(xml_find_first(cells[[i]], ".//v"))
    values[[i]] <- if (is.na(raw_value) || identical(raw_value, "")) {
      if (identical(cell_type, "inlineStr")) xml_text(xml_find_first(cells[[i]], ".//is")) else NA_character_
    } else if (identical(cell_type, "s")) {
      shared_strings[as.integer(raw_value) + 1L]
    } else {
      raw_value
    }
  }

  out <- matrix(NA_character_, nrow = max(positions[, "row"]), ncol = max(positions[, "col"]))
  out[positions] <- values
  out
}

is_blank <- function(x) {
  is.na(x) | trimws(as.character(x)) == ""
}

read_eurostat_long <- function(path) {
  mat <- read_xlsx_sheet_xml(path, sheet_name = "Sheet 1")

  keep_rows <- rowSums(!is_blank(mat)) > 0L
  keep_cols <- colSums(!is_blank(mat)) > 0L
  mat <- mat[keep_rows, keep_cols, drop = FALSE]

  time_row <- which(mat[, 1] == "TIME" & mat[, 2] == "TIME")[1]
  if (is.na(time_row)) {
    stop("Could not find TIME row in ", basename(path), call. = FALSE)
  }

  periods <- mat[time_row, -(1:2)]
  period_cols <- which(!is_blank(periods)) + 2L
  periods <- periods[!is_blank(periods)]

  country_rows <- which(str_detect(mat[, 1], "^[A-Z]{2}$") & !is_blank(mat[, 2]))
  values <- mat[country_rows, period_cols, drop = FALSE]
  colnames(values) <- periods

  tibble(
    country_code = mat[country_rows, 1],
    country = mat[country_rows, 2]
  ) %>%
    bind_cols(as_tibble(values, .name_repair = "minimal")) %>%
    pivot_longer(
      cols = -c(country_code, country),
      names_to = "period",
      values_to = "value"
    ) %>%
    mutate(value = suppressWarnings(as.numeric(value)))
}

quarter_label_to_date <- function(quarter_label) {
  year_value <- as.integer(str_extract(quarter_label, "^\\d{4}"))
  qtr_value <- as.integer(str_extract(quarter_label, "(?<=Q)[1-4]"))
  as.Date(sprintf("%04d-%02d-01", year_value, (qtr_value - 1L) * 3L + 1L))
}

to_quarterly <- function(path, frequency, value_name) {
  long <- read_eurostat_long(path)

  if (identical(frequency, "quarterly")) {
    return(
      long %>%
        transmute(
          country_code,
          country,
          quarter = str_replace(period, "-Q", " Q"),
          quarter_date = quarter_label_to_date(quarter),
          !!value_name := value
        )
    )
  }

  if (!identical(frequency, "monthly")) {
    stop("Unknown frequency: ", frequency, call. = FALSE)
  }

  # Monthly-to-quarter aggregation: average monthly observations only when all
  # three months are available; partial quarters remain missing for later handling.
  long %>%
    mutate(
      month_date = ym(period),
      year = year(month_date),
      qtr = quarter(month_date),
      quarter = sprintf("%d Q%d", year, qtr),
      quarter_date = as.Date(sprintf("%04d-%02d-01", year, (qtr - 1L) * 3L + 1L))
    ) %>%
    group_by(country_code, country, quarter, quarter_date) %>%
    summarise(
      n_obs = sum(!is.na(value)),
      q_average = mean(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(!!value_name := if_else(n_obs == 3L, q_average, NA_real_)) %>%
    select(country_code, country, quarter, quarter_date, all_of(value_name))
}

add_changes <- function(data, value_name, qoq_name, yoy_name) {
  data %>%
    arrange(country_code, quarter_date) %>%
    group_by(country_code, country) %>%
    mutate(
      !!qoq_name := .data[[value_name]] - dplyr::lag(.data[[value_name]], 1L),
      !!yoy_name := .data[[value_name]] - dplyr::lag(.data[[value_name]], 4L)
    ) %>%
    ungroup()
}

quarter_end_from_label <- function(x) {
  x <- str_squish(as.character(x))
  qtr_value <- suppressWarnings(as.integer(str_extract(x, "(?i)(?<=Q)[1-4]")))
  year_value <- suppressWarnings(as.integer(str_extract(x, "\\d{4}")))
  make_date(year_value, (qtr_value - 1L) * 3L + 1L, 1L) %m+% months(3L) - days(1L)
}

parse_excel_or_iso_date <- function(x) {
  x_chr <- as.character(x)
  out <- rep(as.Date(NA), length(x_chr))
  looks_iso <- str_detect(x_chr, "^\\d{4}-\\d{2}-\\d{2}$")
  out[looks_iso] <- suppressWarnings(as.Date(x_chr[looks_iso], format = "%Y-%m-%d"))
  needs_serial <- is.na(out) & str_detect(x_chr, "^\\d+(\\.\\d+)?$")
  out[needs_serial] <- as.Date(suppressWarnings(as.numeric(x_chr[needs_serial])), origin = "1899-12-30")
  out
}

safe_quantile <- function(x, prob) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) NA_real_ else as.numeric(quantile(x, prob, na.rm = TRUE, names = FALSE))
}

safe_sd <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) <= 1L) NA_real_ else sd(x)
}

rolling_apply <- function(x, width, fun) {
  vapply(
    seq_along(x),
    function(i) {
      window <- x[max(1L, i - width + 1L):i]
      value <- suppressWarnings(fun(window))
      if (length(value) == 0L || is.nan(value) || is.infinite(value)) NA_real_ else as.numeric(value)
    },
    numeric(1)
  )
}

first_non_missing_or_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) NA_character_ else as.character(x[[1]])
}

last_non_missing_or_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) NA_character_ else as.character(x[[length(x)]])
}

choose_existing_column <- function(data, candidates, label) {
  existing <- candidates[candidates %in% names(data)]
  if (length(existing) == 0L) {
    stop(
      "Could not find a valid ",
      label,
      " column. Tried: ",
      paste(candidates, collapse = ", "),
      call. = FALSE
    )
  }
  existing[[1]]
}

min_time_index_or_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) NA_integer_ else as.integer(min(x))
}

max_time_index_or_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) NA_integer_ else as.integer(max(x))
}

quarterise_daily_systemic <- function(data, value_col, value_name, country_col = NULL) {
  out <- data %>%
    mutate(
      Date = parse_excel_or_iso_date(.data$Date),
      quarter_end = quarter_end_from_label(.data$Quarter),
      quarter_date = floor_date(quarter_end, "quarter"),
      quarter = sprintf("%d Q%d", year(quarter_date), quarter(quarter_date)),
      Value_Num = suppressWarnings(as.numeric(.data[[value_col]]))
    ) %>%
    filter(!is.na(quarter_date))

  groups <- c("quarter", "quarter_date")
  if (!is.null(country_col)) groups <- c(country_col, groups)

  out %>%
    group_by(across(all_of(groups))) %>%
    summarise(
      "{value_name}_mean" := mean(Value_Num, na.rm = TRUE),
      "{value_name}_max" := max(Value_Num, na.rm = TRUE),
      "{value_name}_sd" := safe_sd(Value_Num),
      "{value_name}_p90" := safe_quantile(Value_Num, 0.90),
      "{value_name}_n_daily_obs" := sum(!is.na(Value_Num)),
      .groups = "drop"
    ) %>%
    mutate(
      across(ends_with(c("_mean", "_max", "_sd", "_p90")), ~ if_else(is.infinite(.x), NA_real_, .x))
    )
}

gdp <- to_quarterly(macro_files$gdp, "quarterly", "gdp_real_yoy") %>%
  add_changes("gdp_real_yoy", "d_gdp_real_yoy_qoq_pp", "d_gdp_real_yoy_yoy_pp")

industrial_production <- to_quarterly(macro_files$industrial_production, "monthly", "industrial_production_index_q") %>%
  arrange(country_code, quarter_date) %>%
  group_by(country_code, country) %>%
  mutate(
    industrial_production_growth_yoy = 100 * (industrial_production_index_q / dplyr::lag(industrial_production_index_q, 4L) - 1),
    industrial_production_growth_qoq = 100 * (industrial_production_index_q / dplyr::lag(industrial_production_index_q, 1L) - 1),
    d_industrial_production_growth_qoq_pp = industrial_production_growth_yoy - dplyr::lag(industrial_production_growth_yoy, 1L)
  ) %>%
  ungroup()

unemployment <- to_quarterly(macro_files$unemployment, "monthly", "unemployment_rate_q") %>%
  add_changes("unemployment_rate_q", "d_unemployment_qoq_pp", "d_unemployment_yoy_pp")

hicp <- to_quarterly(macro_files$hicp, "monthly", "hicp_inflation_yoy_q") %>%
  add_changes("hicp_inflation_yoy_q", "d_hicp_inflation_qoq_pp", "d_hicp_inflation_yoy_pp")

long_term_gov_yield <- to_quarterly(macro_files$long_term_gov_yield, "monthly", "long_term_gov_yield_q") %>%
  arrange(country_code, quarter_date) %>%
  group_by(country_code, country) %>%
  mutate(
    d_long_term_gov_yield_qoq_pp = long_term_gov_yield_q - dplyr::lag(long_term_gov_yield_q, 1L),
    d_long_term_gov_yield_yoy_pp = long_term_gov_yield_q - dplyr::lag(long_term_gov_yield_q, 4L)
  ) %>%
  ungroup()

public_debt <- to_quarterly(macro_files$public_debt, "quarterly", "public_debt_gdp_q") %>%
  add_changes("public_debt_gdp_q", "d_public_debt_gdp_qoq_pp", "d_public_debt_gdp_yoy_pp")

# Systemic-risk quarterly construction: aggregate daily CISS/VSTOXX inputs into
# levels, within-quarter moments, changes, and rolling stress measures.
ciss_euro_raw <- read_excel(systemic_files$ciss_euro_area, sheet = "Final", col_types = "text")
ciss_country_raw <- read_excel(systemic_files$ciss_country, sheet = "Final", col_types = "text")
vstoxx_raw <- read_excel(systemic_files$vstoxx, sheet = "Foglio1", col_types = "text")
vstoxx_value_col <- choose_existing_column(vstoxx_raw, c("Indexvalue", "Index_Value", "Index Value"), "VSTOXX value")

ciss_euro_q <- quarterise_daily_systemic(ciss_euro_raw, "Value", "ciss_euro_area_q")

ciss_country_q <- quarterise_daily_systemic(
  ciss_country_raw,
  "Value",
  "ciss_country_q",
  country_col = "Country_Acronym"
) %>%
  rename(country_code = Country_Acronym)

vstoxx_q <- quarterise_daily_systemic(vstoxx_raw, vstoxx_value_col, "vstoxx_q") %>%
  arrange(quarter_date) %>%
  mutate(
    d_vstoxx_qoq = vstoxx_q_mean - dplyr::lag(vstoxx_q_mean, 1L),
    d_vstoxx_yoy = vstoxx_q_mean - dplyr::lag(vstoxx_q_mean, 4L),
    vstoxx_q_mean_lag1 = dplyr::lag(vstoxx_q_mean, 1L),
    vstoxx_q_mean_lag2 = dplyr::lag(vstoxx_q_mean, 2L),
    high_vstoxx_90 = as.integer(!is.na(vstoxx_q_mean) & vstoxx_q_mean >= HIGH_VSTOXX_THRESHOLD),
    mean_vstoxx_last_4q = rolling_apply(vstoxx_q_mean, 4L, function(x) mean(x, na.rm = TRUE)),
    max_vstoxx_last_4q = rolling_apply(vstoxx_q_mean, 4L, function(x) max(x, na.rm = TRUE)),
    p90_vstoxx_last_4q = rolling_apply(vstoxx_q_p90, 4L, function(x) max(x, na.rm = TRUE)),
    n_high_vstoxx_last_4q = rolling_apply(high_vstoxx_90, 4L, function(x) sum(x == 1L, na.rm = TRUE))
  )

countries <- read_eurostat_long(macro_files$gdp) %>%
  distinct(country_code, country) %>%
  mutate(country_order = row_number())

quarters <- tibble(
  quarter_date = seq(PANEL_START_DATE, as.Date("2025-10-01"), by = "3 months")
) %>%
  mutate(
    quarter = sprintf("%d Q%d", year(quarter_date), quarter(quarter_date)),
    quarter_order = row_number()
  )

macro_panel <- tidyr::expand_grid(countries, quarters)
systemic_support_quarters <- tibble(
  quarter_date = seq(SYSTEMIC_SUPPORT_START_DATE, max(quarters$quarter_date), by = "3 months")
) %>%
  mutate(
    quarter = sprintf("%d Q%d", year(quarter_date), quarter(quarter_date))
  )
systemic_support_panel <- tidyr::expand_grid(countries, systemic_support_quarters)
datasets <- list(gdp, industrial_production, unemployment, hicp, long_term_gov_yield, public_debt)

systemic_quarterly_by_country_support <- systemic_support_panel %>%
  select(country, country_code, quarter, quarter_date) %>%
  left_join(ciss_country_q, by = c("country_code", "quarter", "quarter_date")) %>%
  left_join(ciss_euro_q, by = c("quarter", "quarter_date")) %>%
  left_join(vstoxx_q, by = c("quarter", "quarter_date")) %>%
  arrange(country_code, quarter_date) %>%
  group_by(country_code, country) %>%
  mutate(
    ciss_country_or_euro_q_mean = coalesce(ciss_country_q_mean, ciss_euro_area_q_mean),
    ciss_country_or_euro_q_max = coalesce(ciss_country_q_max, ciss_euro_area_q_max),
    ciss_country_or_euro_q_sd = coalesce(ciss_country_q_sd, ciss_euro_area_q_sd),
    ciss_country_or_euro_q_p90 = coalesce(ciss_country_q_p90, ciss_euro_area_q_p90),
    ciss_country_or_euro_q_n_daily_obs = coalesce(ciss_country_q_n_daily_obs, ciss_euro_area_q_n_daily_obs),
    d_ciss_country_or_euro_qoq = ciss_country_or_euro_q_mean - dplyr::lag(ciss_country_or_euro_q_mean, 1L),
    d_ciss_country_or_euro_yoy = ciss_country_or_euro_q_mean - dplyr::lag(ciss_country_or_euro_q_mean, 4L),
    ciss_country_or_euro_q_mean_lag1 = dplyr::lag(ciss_country_or_euro_q_mean, 1L),
    ciss_country_or_euro_q_mean_lag2 = dplyr::lag(ciss_country_or_euro_q_mean, 2L),
    mean_ciss_country_or_euro_last_4q = rolling_apply(ciss_country_or_euro_q_mean, 4L, function(x) mean(x, na.rm = TRUE)),
    max_ciss_country_or_euro_last_4q = rolling_apply(ciss_country_or_euro_q_mean, 4L, function(x) max(x, na.rm = TRUE)),
    ciss_source = case_when(
      !is.na(ciss_country_q_mean) ~ "Country_CISS",
      !is.na(ciss_euro_area_q_mean) ~ "Euro_Area_CISS_Fallback",
      TRUE ~ "Missing"
    )
  ) %>%
  ungroup()

systemic_quarterly_by_country <- systemic_quarterly_by_country_support %>%
  filter(quarter_date >= PANEL_START_DATE)

systemic_vars <- setdiff(names(systemic_quarterly_by_country), c("country", "country_code", "quarter", "quarter_date", "ciss_source"))

final_macro_dataset <- Reduce(
  function(left, right) {
    left_join(left, right, by = c("country_code", "country", "quarter", "quarter_date"))
  },
  datasets,
  init = macro_panel
) %>%
  arrange(country_order, quarter_order) %>%
  select(
    country,
    country_code,
    quarter,
    gdp_real_yoy,
    d_gdp_real_yoy_qoq_pp,
    d_gdp_real_yoy_yoy_pp,
    industrial_production_index_q,
    industrial_production_growth_yoy,
    industrial_production_growth_qoq,
    d_industrial_production_growth_qoq_pp,
    unemployment_rate_q,
    d_unemployment_qoq_pp,
    d_unemployment_yoy_pp,
    hicp_inflation_yoy_q,
    d_hicp_inflation_qoq_pp,
    d_hicp_inflation_yoy_pp,
    long_term_gov_yield_q,
    d_long_term_gov_yield_qoq_pp,
    d_long_term_gov_yield_yoy_pp,
    public_debt_gdp_q,
    d_public_debt_gdp_qoq_pp,
    d_public_debt_gdp_yoy_pp
  ) %>%
  left_join(
    systemic_quarterly_by_country %>%
      select(country_code, quarter, all_of(systemic_vars), ciss_source),
    by = c("country_code", "quarter")
  )

expected_rows <- 22L * 80L
if (nrow(final_macro_dataset) != expected_rows) {
  stop("Unexpected macro row count: ", nrow(final_macro_dataset), " instead of ", expected_rows, call. = FALSE)
}
if (n_distinct(final_macro_dataset$country_code) != 22L) {
  stop("Unexpected number of macro countries.", call. = FALSE)
}

duplicate_macro_keys <- final_macro_dataset %>%
  count(country_code, quarter, name = "n") %>%
  filter(n > 1L)
write_csv(duplicate_macro_keys, file.path(AUDIT_DIR, "macro_duplicate_keys_v5.csv"), na = "")
if (nrow(duplicate_macro_keys) > 0L) {
  stop("Macro dataset has duplicate country-quarter keys.", call. = FALSE)
}

missing_summary <- final_macro_dataset %>%
  summarise(across(-c(country, country_code, quarter), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_values") %>%
  mutate(
    n_rows = nrow(final_macro_dataset),
    missing_rate = missing_values / n_rows
  ) %>%
  arrange(desc(missing_values), variable)

macro_value_vars <- setdiff(names(final_macro_dataset), c("country", "country_code", "quarter"))
country_coverage <- final_macro_dataset %>%
  mutate(row_missing_macro_values = rowSums(is.na(pick(all_of(macro_value_vars))))) %>%
  group_by(country_code, country) %>%
  summarise(
    n_quarters = n(),
    first_quarter = min(quarter),
    last_quarter = max(quarter),
    total_missing_macro_values = sum(row_missing_macro_values),
    .groups = "drop"
  )

write_csv(missing_summary, file.path(AUDIT_DIR, "macro_missing_summary_v5.csv"), na = "")
write_csv(country_coverage, file.path(AUDIT_DIR, "macro_country_coverage_v5.csv"), na = "")

systemic_missing_summary <- final_macro_dataset %>%
  summarise(across(all_of(systemic_vars), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_values") %>%
  mutate(
    n_rows = nrow(final_macro_dataset),
    missing_rate = missing_values / n_rows
  ) %>%
  arrange(desc(missing_values), variable)

systemic_source_coverage <- final_macro_dataset %>%
  count(country_code, country, ciss_source, name = "n_country_quarters") %>%
  arrange(country_code, ciss_source)

systemic_input_manifest <- tibble(
  Input = names(systemic_files),
  Path = unlist(systemic_files, use.names = FALSE),
  File = basename(Path),
  Sheet = c("Final", "Final", "Foglio1"),
  Value_Column = c("Value", "Value", vstoxx_value_col),
  Support_Start = as.character(SYSTEMIC_SUPPORT_START_DATE),
  Panel_Start = as.character(PANEL_START_DATE),
  Note = "Systemic inputs include pre-sample 2005 observations only to compute lags and changes for the 2006-onward panel."
)

write_csv(systemic_quarterly_by_country, file.path(DATA_DIR, "systemic_stress_quarterly_by_country_v5.csv"), na = "")
saveRDS(systemic_quarterly_by_country, file.path(DATA_DIR, "systemic_stress_quarterly_by_country_v5.rds"))
write_csv(systemic_quarterly_by_country_support, file.path(DATA_DIR, "systemic_stress_quarterly_by_country_support_from_2005_v5.csv"), na = "")
saveRDS(systemic_quarterly_by_country_support, file.path(DATA_DIR, "systemic_stress_quarterly_by_country_support_from_2005_v5.rds"))
write_csv(systemic_input_manifest, file.path(AUDIT_DIR, "systemic_input_manifest_v5.csv"), na = "")
write_csv(systemic_missing_summary, file.path(AUDIT_DIR, "systemic_missing_summary_v5.csv"), na = "")
write_csv(systemic_source_coverage, file.path(AUDIT_DIR, "systemic_ciss_source_coverage_v5.csv"), na = "")

ggsave(
  file.path(FIGURE_DIR, "fig_01_macro_missingness_v5.png"),
  ggplot(missing_summary, aes(x = reorder(variable, missing_values), y = missing_values)) +
    geom_col(fill = "#3b6ea8") +
    coord_flip() +
    scale_y_continuous(labels = comma) +
    labs(x = NULL, y = "Missing values", title = "Macro variable missingness") +
    theme_minimal(base_size = 11),
  width = 9,
  height = 6,
  dpi = 300
)

write_csv(final_macro_dataset, file.path(DATA_DIR, "final_macro_dataset_v5.csv"), na = "")
saveRDS(final_macro_dataset, file.path(DATA_DIR, "final_macro_dataset_v5.rds"))

# Unit-root diagnostics: test macro and systemic variables up to second differences.
DIAGNOSTICS_DIR <- file.path(OUTPUT_DIR, "unit_root_tests")
UNIT_AUDIT_DIR <- file.path(DIAGNOSTICS_DIR, "audits")
UNIT_DATA_DIR <- file.path(DIAGNOSTICS_DIR, "data")
UNIT_TABLE_DIR <- file.path(DIAGNOSTICS_DIR, "tables_latex")
UNIT_NOTES_DIR <- file.path(DIAGNOSTICS_DIR, "notes")
dir.create(UNIT_AUDIT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(UNIT_DATA_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(UNIT_TABLE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(UNIT_NOTES_DIR, recursive = TRUE, showWarnings = FALSE)

write_diagnostic_table <- function(x, name, caption = NULL, digits = 4) {
  write_csv(x, file.path(UNIT_AUDIT_DIR, paste0(name, ".csv")), na = "")
  saveRDS(x, file.path(UNIT_DATA_DIR, paste0(name, ".rds")))
  latex_table <- knitr::kable(
    x,
    format = "latex",
    booktabs = TRUE,
    longtable = nrow(x) > 30L,
    caption = caption,
    digits = digits,
    escape = TRUE
  )
  writeLines(as.character(latex_table), file.path(UNIT_TABLE_DIR, paste0(name, ".tex")))
  invisible(x)
}

quarter_to_time_id <- function(quarter_chr) {
  year_value <- as.integer(substr(as.character(quarter_chr), 1L, 4L))
  qtr_value <- as.integer(str_extract(as.character(quarter_chr), "(?<=Q)[1-4]"))
  year_value * 4L + qtr_value
}

make_difference_name <- function(variable, difference_order) {
  if (difference_order == 0L) variable else paste0("diff", difference_order, "_", variable)
}

unit_root_data <- final_macro_dataset %>%
  arrange(country_code, quarter) %>%
  mutate(
    time_id = quarter_to_time_id(quarter),
    time_index = dense_rank(time_id)
  )

unit_root_variables <- unit_root_data %>%
  select(where(is.numeric)) %>%
  select(-any_of(c("time_id", "time_index")), -matches("_n_daily_obs$")) %>%
  names()

variable_manifest <- tibble(
  Variable = unit_root_variables,
  Transformation_Class = case_when(
    Variable %in% systemic_vars ~ "systemic_risk",
    str_starts(Variable, "d_") ~ "difference_or_change",
    str_detect(Variable, regex("growth|yoy|qoq|inflation", ignore_case = TRUE)) ~ "rate_or_growth",
    TRUE ~ "level_or_ratio"
  ),
  N_Total = map_int(Variable, ~ sum(!is.na(unit_root_data[[.x]]))),
  N_Countries = map_int(Variable, ~ n_distinct(unit_root_data$country_code[!is.na(unit_root_data[[.x]])])),
  Start_Quarter = map_chr(Variable, ~ first_non_missing_or_na(unit_root_data$quarter[!is.na(unit_root_data[[.x]])])),
  End_Quarter = map_chr(Variable, ~ last_non_missing_or_na(unit_root_data$quarter[!is.na(unit_root_data[[.x]])]))
)

test_series_data <- unit_root_data %>%
  select(country_code, time_index, all_of(unit_root_variables))

test_series_manifest <- tidyr::expand_grid(
  Variable = unit_root_variables,
  Difference_Order = 0:2
) %>%
  mutate(
    Test_Series = map2_chr(Variable, Difference_Order, make_difference_name),
    Transformation_Class = variable_manifest$Transformation_Class[match(Variable, variable_manifest$Variable)]
  )

for (variable in unit_root_variables) {
  diff1_name <- make_difference_name(variable, 1L)
  diff2_name <- make_difference_name(variable, 2L)
  test_series_data <- test_series_data %>%
    group_by(country_code) %>%
    arrange(time_index, .by_group = TRUE) %>%
    mutate(
      "{diff1_name}" := .data[[variable]] - dplyr::lag(.data[[variable]], 1L),
      "{diff2_name}" := .data[[diff1_name]] - dplyr::lag(.data[[diff1_name]], 1L)
    ) %>%
    ungroup()
}

test_series_manifest <- test_series_manifest %>%
  mutate(
    N_Total = map_int(Test_Series, ~ sum(!is.na(test_series_data[[.x]]))),
    N_Countries = map_int(Test_Series, ~ n_distinct(test_series_data$country_code[!is.na(test_series_data[[.x]])])),
    Start_Time_Index = map_int(Test_Series, ~ min_time_index_or_na(test_series_data$time_index[!is.na(test_series_data[[.x]])])),
    End_Time_Index = map_int(Test_Series, ~ max_time_index_or_na(test_series_data$time_index[!is.na(test_series_data[[.x]])]))
  )

extract_panel_test <- function(variable, test_series, difference_order, test_name, exo_name) {
  local_data <- test_series_data %>%
    select(country_code, time_index, all_of(test_series)) %>%
    filter(!is.na(.data[[test_series]]))

  if (n_distinct(local_data$country_code) < 2L || nrow(local_data) < 20L) {
    return(tibble(
      Variable = variable,
      Test_Series = test_series,
      Difference_Order = difference_order,
      Test = test_name,
      Exogenous = exo_name,
      Statistic_Name = NA_character_,
      Statistic = NA_real_,
      P_Value = NA_real_,
      Reject_Unit_Root_5pct = NA,
      Status = "Failed",
      Message = "Insufficient panel coverage for unit-root test."
    ))
  }

  local_panel_data <- pdata.frame(local_data, index = c("country_code", "time_index"), drop.index = FALSE, row.names = TRUE)
  result <- tryCatch(
    plm::purtest(local_panel_data[[test_series]], test = test_name, exo = exo_name, lags = "AIC", pmax = 4),
    error = function(e) e
  )

  if (inherits(result, "error")) {
    return(tibble(
      Variable = variable,
      Test_Series = test_series,
      Difference_Order = difference_order,
      Test = test_name,
      Exogenous = exo_name,
      Statistic_Name = NA_character_,
      Statistic = NA_real_,
      P_Value = NA_real_,
      Reject_Unit_Root_5pct = NA,
      Status = "Failed",
      Message = conditionMessage(result)
    ))
  }

  statistic_object <- result$statistic
  statistic_name <- names(statistic_object$statistic)[[1]]
  statistic_value <- as.numeric(statistic_object$statistic[[1]])
  p_value <- as.numeric(statistic_object$p.value[[1]])

  tibble(
    Variable = variable,
    Test_Series = test_series,
    Difference_Order = difference_order,
    Test = test_name,
    Exogenous = exo_name,
    Statistic_Name = statistic_name,
    Statistic = statistic_value,
    P_Value = p_value,
    Reject_Unit_Root_5pct = if_else(is.na(p_value) | is.nan(p_value), NA, p_value < 0.05),
    Status = if_else(is.na(p_value) | is.nan(p_value), "Failed", "OK"),
    Message = if_else(is.na(p_value) | is.nan(p_value), "Panel unit-root test returned a missing p-value.", NA_character_)
  )
}

test_grid <- tidyr::expand_grid(
  test_series_manifest %>% select(Variable, Test_Series, Difference_Order),
  Test = c("ips", "madwu"),
  Exogenous = c("intercept", "trend")
)

panel_summary <- pmap_dfr(
  test_grid,
  function(Variable, Test_Series, Difference_Order, Test, Exogenous) {
    extract_panel_test(Variable, Test_Series, Difference_Order, Test, Exogenous)
  }
) %>%
  left_join(variable_manifest, by = "Variable") %>%
  relocate(Transformation_Class, .after = Variable) %>%
  arrange(Transformation_Class, Variable, Test, Exogenous)

compact_summary <- panel_summary %>%
  group_by(Transformation_Class, Difference_Order, Test, Exogenous) %>%
  summarise(
    N_Variables = n(),
    N_OK = sum(Status == "OK"),
    N_Reject_Unit_Root_5pct = sum(Reject_Unit_Root_5pct %in% TRUE, na.rm = TRUE),
    Share_Reject_Unit_Root_5pct = N_Reject_Unit_Root_5pct / N_OK,
    .groups = "drop"
  )

integration_order_summary <- panel_summary %>%
  filter(Exogenous == "intercept") %>%
  select(Variable, Transformation_Class, Difference_Order, Test, Reject_Unit_Root_5pct, P_Value) %>%
  pivot_wider(
    names_from = c(Difference_Order, Test),
    values_from = c(Reject_Unit_Root_5pct, P_Value),
    names_glue = "{.value}_d{Difference_Order}_{Test}"
  ) %>%
  mutate(
    Reject_Level_Both = Reject_Unit_Root_5pct_d0_ips %in% TRUE & Reject_Unit_Root_5pct_d0_madwu %in% TRUE,
    Reject_Diff1_Both = Reject_Unit_Root_5pct_d1_ips %in% TRUE & Reject_Unit_Root_5pct_d1_madwu %in% TRUE,
    Reject_Diff2_Both = Reject_Unit_Root_5pct_d2_ips %in% TRUE & Reject_Unit_Root_5pct_d2_madwu %in% TRUE,
    Suggested_Integration_Order = case_when(
      Reject_Level_Both ~ "I(0)",
      Reject_Diff1_Both ~ "I(1)",
      Reject_Diff2_Both ~ "I(2)",
      TRUE ~ "Unclear"
    )
  ) %>%
  arrange(factor(Suggested_Integration_Order, levels = c("I(0)", "I(1)", "I(2)", "Unclear")), Transformation_Class, Variable)

integration_order_compact <- integration_order_summary %>%
  count(Suggested_Integration_Order, name = "N_Variables") %>%
  arrange(factor(Suggested_Integration_Order, levels = c("I(0)", "I(1)", "I(2)", "Unclear")))

write_diagnostic_table(variable_manifest, "table_00_variable_manifest", "V5 macro and systemic variables tested for unit roots")
write_diagnostic_table(test_series_manifest, "table_00b_unit_root_test_series_manifest", "V5 unit-root test series, including first and second differences")
write_diagnostic_table(panel_summary, "table_01_panel_unit_root_tests", "Panel unit-root tests on V5 macro and systemic variables")
write_diagnostic_table(compact_summary, "table_02_unit_root_compact_summary", "Compact summary of V5 unit-root diagnostics")
write_diagnostic_table(integration_order_summary, "table_03_integration_order_summary", "Suggested V5 integration order up to second differences")
write_diagnostic_table(integration_order_compact, "table_04_integration_order_compact", "Compact suggested V5 integration order summary")

writeLines(
  c(
    "V5 macro and systemic unit-root diagnostics",
    "",
    paste0("Variables tested: ", length(unit_root_variables)),
    paste0("Countries: ", n_distinct(final_macro_dataset$country_code)),
    paste0("Quarters: ", n_distinct(final_macro_dataset$quarter)),
    "",
    "Tests: IPS and Maddala-Wu panel unit-root tests with intercept and trend specifications.",
    "Interpretation: p-value < 0.05 rejects the unit-root null. Suggested integration order uses the intercept specification and requires both IPS and Maddala-Wu to reject at the same differencing order.",
    "Daily-observation count fields are excluded from unit-root testing because they are coverage metadata, not economic variables."
  ),
  file.path(UNIT_NOTES_DIR, "unit_root_tests_notes.txt")
)

message("Part 2 V5 completed.")
message("Macro/systemic rows: ", nrow(final_macro_dataset))
message("Macro/systemic countries: ", n_distinct(final_macro_dataset$country_code))
message("Numeric variables tested for unit roots: ", length(unit_root_variables))



