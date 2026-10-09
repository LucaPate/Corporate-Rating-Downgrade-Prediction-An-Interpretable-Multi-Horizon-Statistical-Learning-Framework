# Step 01 - Build the firm-quarter rating panel from S&P rating actions.
# GitHub-ready copy: paths are repository-relative and data files are intentionally excluded.

## This definitive version keeps the corrected S&P severity order, with D below SD.

required_packages <- c("readxl", "dplyr", "stringr", "lubridate", "readr", "tidyr", "purrr", "ggplot2", "scales")
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

first_existing_optional <- function(paths) {
  existing <- paths[file.exists(paths)]
  if (length(existing) == 0L) {
    return(NA_character_)
  }
  existing[[1]]
}

SCRIPT_DIR <- get_script_dir()
V5_DIR <- normalizePath(file.path(SCRIPT_DIR, ".."), winslash = "/", mustWork = TRUE)
INPUT_DIR <- file.path(V5_DIR, "input")

RATING_FILE <- first_existing(file.path(INPUT_DIR, "Rating Change Listed Quarterly_V2.xlsx"))
RATING_HISTORY_SUPPORT_FILE <- first_existing_optional(file.path(INPUT_DIR, "Rating Changes 2005.xlsx"))

OUTPUT_DIR <- file.path(V5_DIR, "Part_1_Outputs")
DATA_DIR <- file.path(OUTPUT_DIR, "data")
AUDIT_DIR <- file.path(OUTPUT_DIR, "audits")
FIGURE_DIR <- file.path(OUTPUT_DIR, "figures")
dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(AUDIT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIGURE_DIR, recursive = TRUE, showWarnings = FALSE)

main_plot_colours <- c(
  blue = "#4472C4",
  navy = "#182642",
  orange = "#ED7D31"
)

main_plot_palette <- function(n) {
  unname(rep(main_plot_colours, length.out = n))
}

HISTORY_SUPPORT_START <- as.Date("2005-01-01")
PANEL_START <- as.Date("2006-01-01")
PANEL_END <- as.Date("2025-12-31")

rating_order <- c(
  "AAA",
  "AA+", "AA", "AA-",
  "A+", "A", "A-",
  "BBB+", "BBB", "BBB-",
  "BB+", "BB", "BB-",
  "B+", "B", "B-",
  "CCC+", "CCC", "CCC-",
  "CC",
  "C",
  # S&P: SD is selective default, D is general default; keep D as the worst notch.
  "SD",
  "D"
)

rating_rank_map <- setNames(seq_along(rating_order), rating_order)
stopifnot(unname(rating_rank_map["SD"]) == 22L)
stopifnot(unname(rating_rank_map["D"]) == 23L)

rating_group_from_rating <- function(rating) {
  case_when(
    rating %in% c("AAA", "AA+", "AA", "AA-", "A+", "A", "A-") ~ "A",
    rating %in% c("BBB+", "BBB", "BBB-", "BB+", "BB", "BB-", "B+", "B", "B-") ~ "B",
    rating %in% c("CCC+", "CCC", "CCC-", "CC", "C", "SD", "D") ~ "C",
    TRUE ~ NA_character_
  )
}

macro_rating_from_rating <- function(rating) {
  case_when(
    rating %in% c("AAA") ~ 1L,
    rating %in% c("AA+", "AA", "AA-") ~ 2L,
    rating %in% c("A+", "A", "A-") ~ 3L,
    rating %in% c("BBB+", "BBB", "BBB-") ~ 4L,
    rating %in% c("BB+", "BB", "BB-") ~ 5L,
    rating %in% c("B+", "B", "B-") ~ 6L,
    rating %in% c("CCC+", "CCC", "CCC-") ~ 7L,
    rating %in% c("CC", "C", "SD", "D") ~ 8L,
    TRUE ~ NA_integer_
  )
}

rating_grade_from_rating <- function(rating) {
  case_when(
    rating %in% c("AAA", "AA+", "AA", "AA-", "A+", "A", "A-", "BBB+", "BBB", "BBB-") ~ "Investment",
    rating %in% c("BB+", "BB", "BB-", "B+", "B", "B-", "CCC+", "CCC", "CCC-", "CC", "C", "SD", "D") ~ "Speculative",
    TRUE ~ NA_character_
  )
}

clean_rating <- function(x) {
  x %>%
    as.character() %>%
    str_to_upper() %>%
    str_squish() %>%
    na_if("") %>%
    # Keep true +/- notches, but remove watch/outlook markers after "*".
    # Examples: "BBB *-" -> "BBB"; "BB**+" -> "BB".
    str_remove("\\s*\\*.*$") %>%
    str_remove_all("[^A-Z+\\-]") %>%
    na_if("NR") %>%
    na_if("")
}

parse_rating_date <- function(x) {
  if (inherits(x, "Date")) {
    return(as.Date(x))
  }
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
    return(as.Date(x))
  }

  x_chr <- str_squish(as.character(x))
  x_chr[x_chr == ""] <- NA_character_

  parsed <- as.Date(rep(NA_real_, length(x_chr)), origin = "1970-01-01")

  numeric_dates <- suppressWarnings(as.numeric(x_chr))
  excel_date_rows <- !is.na(numeric_dates) & numeric_dates > 20000 & numeric_dates < 60000
  parsed[excel_date_rows] <- as.Date(numeric_dates[excel_date_rows], origin = "1899-12-30")

  text_rows <- !excel_date_rows & !is.na(x_chr)
  if (any(text_rows)) {
    first_number <- suppressWarnings(as.integer(str_extract(x_chr[text_rows], "^\\d{1,2}")))
    orders <- if_else(!is.na(first_number) & first_number > 12L, "dmy", "mdy")

    mdy_rows <- which(text_rows)[orders == "mdy"]
    dmy_rows <- which(text_rows)[orders == "dmy"]

    parsed[mdy_rows] <- suppressWarnings(as.Date(parse_date_time(x_chr[mdy_rows], orders = "mdy", tz = "UTC")))
    parsed[dmy_rows] <- suppressWarnings(as.Date(parse_date_time(x_chr[dmy_rows], orders = "dmy", tz = "UTC")))

    still_missing <- is.na(parsed) & !is.na(x_chr)
    if (any(still_missing)) {
      parsed[still_missing] <- suppressWarnings(as.Date(parse_date_time(
        x_chr[still_missing],
        orders = c("ymd", "dmy", "mdy"),
        tz = "UTC"
      )))
    }
  }

  parsed
}

date_to_quarter <- function(date_value) {
  sprintf("%d Q%d", year(date_value), quarter(date_value))
}

make_quarter_calendar <- function(start_date, end_date) {
  tibble(quarter_start = seq(floor_date(start_date, "quarter"), floor_date(end_date, "quarter"), by = "quarter")) %>%
    mutate(
      year = year(quarter_start),
      quarter_number = quarter(quarter_start),
      quarter = sprintf("%d Q%d", year, quarter_number),
      quarter_end = ceiling_date(quarter_start, "quarter") - days(1),
      quarter_days = as.integer(quarter_end - quarter_start) + 1L
    )
}

last_or_na <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0L) NA_character_ else x[[length(x)]]
}

save_audit <- function(x, filename) {
  write_csv(x, file.path(AUDIT_DIR, filename), na = "")
}

save_plot <- function(plot, filename, width = 9, height = 5) {
  ggsave(file.path(FIGURE_DIR, filename), plot, width = width, height = height, dpi = 300)
}

normalise_match_key <- function(x) {
  x %>%
    as.character() %>%
    str_to_upper() %>%
    str_squish() %>%
    na_if("")
}

standardise_rating_input <- function(x, source_label, source_sheet = NA_character_) {
  alternate_names <- c(
    "Company Name" = "Company_Name",
    "Data" = "Date",
    "Tipo rating" = "Rating_Type",
    "Agenzia" = "Agency",
    "Curr Rtg" = "Curr_Rtg",
    "Last Rtg" = "Last_Rtg",
    "Paes/Reg" = "Country",
    "Industry Type" = "Industry",
    "Security Name" = "Company_Ticker"
  )

  for (old_name in names(alternate_names)) {
    new_name <- unname(alternate_names[[old_name]])
    if (old_name %in% names(x) && !new_name %in% names(x)) {
      names(x)[names(x) == old_name] <- new_name
    }
  }

  x %>%
    mutate(
      Input_Source = source_label,
      Input_Sheet = source_sheet
    )
}

save_transition_stock_plot <- function(data, from_var, to_var, levels, prefix, title) {
  label_min_share <- if_else(length(levels) > 10L, 0.05, 0.02)

  stock_data <- bind_rows(
    data %>%
      filter(!is.na(.data[[from_var]])) %>%
      count(State = .data[[from_var]], name = "N") %>%
      mutate(Stage = "Previous"),
    data %>%
      filter(!is.na(.data[[to_var]])) %>%
      count(State = .data[[to_var]], name = "N") %>%
      mutate(Stage = "Current")
  ) %>%
    mutate(
      Stage = factor(Stage, levels = c("Previous", "Current")),
      State = factor(State, levels = levels)
    ) %>%
    group_by(Stage) %>%
    mutate(
      Share = N / sum(N),
      Segment_Label = if_else(Share >= label_min_share, paste0(comma(N), "\n", percent(Share, accuracy = 0.1)), "")
    ) %>%
    ungroup()

  save_audit(stock_data, paste0(prefix, "_transition_previous_current_stocks_v5.csv"))

  save_plot(
    ggplot(stock_data, aes(x = Stage, y = N, fill = State)) +
      geom_col(width = 0.55, color = "white", linewidth = 0.15, position = position_stack(reverse = TRUE)) +
      geom_text(
        aes(label = Segment_Label),
        position = position_stack(vjust = 0.5, reverse = TRUE),
        size = if_else(length(levels) > 10L, 2.2, 3.1),
        color = "white",
        lineheight = 0.9
      ) +
      scale_fill_manual(values = main_plot_palette(length(levels))) +
      scale_y_continuous(labels = comma) +
      labs(x = NULL, y = "Rating actions", fill = "Rating", title = title) +
      theme_minimal(base_size = 10) +
      theme(panel.grid.major.x = element_blank()),
    paste0("fig_transition_stock_", prefix, "_v5.png"),
    width = 8,
    height = 7
  )

  invisible(stock_data)
}

save_transition_outputs <- function(data, from_var, to_var, levels, prefix, title, x_label = "To", y_label = "From") {
  observed <- data %>%
    filter(!is.na(.data[[from_var]]), !is.na(.data[[to_var]])) %>%
    mutate(
      From = factor(.data[[from_var]], levels = levels),
      To = factor(.data[[to_var]], levels = levels)
    ) %>%
    count(From, To, name = "N_Transitions")

  matrix_long <- tidyr::expand_grid(
    From = factor(levels, levels = levels),
    To = factor(levels, levels = levels)
  ) %>%
    left_join(observed, by = c("From", "To")) %>%
    mutate(N_Transitions = replace_na(N_Transitions, 0L)) %>%
    group_by(From) %>%
    mutate(
      N_From = sum(N_Transitions),
      Row_Share = if_else(N_From > 0L, N_Transitions / N_From, NA_real_)
    ) %>%
    ungroup()

  save_audit(matrix_long, paste0(prefix, "_transition_matrix_long_v5.csv"))

  compact <- observed %>%
    group_by(From) %>%
    mutate(
      N_From = sum(N_Transitions),
      Row_Share = N_Transitions / N_From
    ) %>%
    ungroup() %>%
    arrange(From, To)
  save_audit(compact, paste0(prefix, "_transition_matrix_nonzero_v5.csv"))

  # Heatmap of transitions: for visualising migration mass and direction.
  save_plot(
    ggplot(matrix_long, aes(x = To, y = From, fill = N_Transitions)) +
      geom_tile(color = "white", linewidth = 0.25) +
      geom_text(aes(label = if_else(N_Transitions > 0L, as.character(N_Transitions), "")), size = 2.4) +
      scale_fill_gradient(low = "#f7f7f2", high = "#2f6f73", labels = comma) +
      scale_y_discrete(limits = rev(levels)) +
      labs(x = x_label, y = y_label, fill = "N", title = title) +
      theme_minimal(base_size = 10) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank()
      ),
    paste0("fig_transition_heatmap_", prefix, "_v5.png"),
    width = if_else(length(levels) > 10L, 12, 8),
    height = if_else(length(levels) > 10L, 10, 6)
  )

  build_flow_plot_data <- function(flow_data, max_flows = NULL) {
    out <- flow_data %>%
      filter(N_Transitions > 0L) %>%
      arrange(desc(N_Transitions))

    if (!is.null(max_flows)) {
      out <- out %>% slice_head(n = max_flows)
    }

    out %>%
      mutate(
        From_Position = length(levels) - match(as.character(From), levels) + 1L,
        To_Position = length(levels) - match(as.character(To), levels) + 1L,
        Label_X = 1.5,
        Label_Y = (From_Position + To_Position) / 2 + case_when(
          From_Position < To_Position ~ 0.16,
          From_Position > To_Position ~ -0.16,
          TRUE ~ 0
        ),
        Flow_Label = comma(N_Transitions)
      )
  }

  save_flow_plot <- function(flow_plot_data, filename_suffix, plot_title, label_counts = length(levels) <= 3L) {
    if (nrow(flow_plot_data) == 0L) {
      return(invisible(NULL))
    }

    label_data <- bind_rows(
      tibble(x = 1, y = rev(seq_along(levels)), label = levels),
      tibble(x = 2, y = rev(seq_along(levels)), label = levels)
    )

    save_plot(
      ggplot(flow_plot_data) +
        geom_segment(
          aes(x = 1.12, xend = 1.88, y = From_Position, yend = To_Position, linewidth = N_Transitions, color = N_Transitions),
          lineend = "round",
          alpha = 0.78
        ) +
        {if (label_counts) geom_label(
          aes(x = Label_X, y = Label_Y, label = Flow_Label),
          size = 3,
          fill = "white",
          linewidth = 0.15,
          alpha = 0.9
        )} +
        geom_text(data = label_data, aes(x = x, y = y, label = label), size = if_else(length(levels) > 10L, 2.6, 3.4)) +
        scale_x_continuous(limits = c(0.75, 2.25), breaks = c(1, 2), labels = c("Previous", "Current")) +
        scale_y_continuous(breaks = seq_along(levels), labels = rev(levels)) +
        scale_linewidth_continuous(range = c(0.2, 3), labels = comma) +
        scale_color_gradient(low = "#9fb7a9", high = "#2f4f57", labels = comma) +
        labs(x = NULL, y = NULL, linewidth = "N", color = "N", title = plot_title) +
        theme_minimal(base_size = 10) +
        theme(
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
        ),
      paste0("fig_transition_flow_", prefix, filename_suffix, "_v5.png"),
      width = if_else(length(levels) > 10L, 11, 8),
      height = if_else(length(levels) > 10L, 10, 5.5)
    )
  }

  main_flow_plot_data <- build_flow_plot_data(compact, max_flows = if_else(length(levels) > 10L, 60L, 50L))
  all_flow_plot_data <- build_flow_plot_data(compact, max_flows = NULL)

  save_flow_plot(main_flow_plot_data, "", paste0(title, ": main flows"))
  save_flow_plot(all_flow_plot_data, "_all_flows", title)

  invisible(matrix_long)
}

lag_sum <- function(x, lags = 1:4) {
  pieces <- lapply(lags, function(k) dplyr::lag(x, k, default = 0L))
  Reduce(`+`, pieces)
}

rating_rank_mapping <- tibble(
  rating = names(rating_rank_map),
  rating_rank = as.integer(rating_rank_map),
  rating_group = rating_group_from_rating(rating),
  macro_rating = macro_rating_from_rating(rating),
  rating_grade = rating_grade_from_rating(rating)
)
save_audit(rating_rank_mapping, "rating_rank_mapping_v5.csv")

rating_main_raw <- read_excel(RATING_FILE, col_types = "text", .name_repair = "unique_quiet") %>%
  standardise_rating_input("main_rating_file", NA_character_) %>%
  mutate(History_Support_Only = FALSE)

required_columns <- c("Company_Name", "Date", "Curr_Rtg", "Last_Rtg")
missing_columns <- setdiff(required_columns, names(rating_main_raw))
if (length(missing_columns) > 0L) {
  stop("Missing required rating column(s): ", paste(missing_columns, collapse = ", "), call. = FALSE)
}

optional_columns <- c("Company_Ticker", "Country", "Agency", "Rating_Type", "Industry", "Frequency")
for (col in optional_columns) {
  if (!col %in% names(rating_main_raw)) {
    rating_main_raw[[col]] <- NA_character_
  }
}

main_firm_universe <- rating_main_raw %>%
  transmute(
    Company_Name_Key = normalise_match_key(Company_Name),
    Company_Ticker_Key = normalise_match_key(Company_Ticker)
  ) %>%
  distinct()
main_company_name_keys <- main_firm_universe$Company_Name_Key[!is.na(main_firm_universe$Company_Name_Key)]
main_company_ticker_keys <- main_firm_universe$Company_Ticker_Key[!is.na(main_firm_universe$Company_Ticker_Key)]
main_ticker_lookup <- rating_main_raw %>%
  mutate(
    Company_Name_Key = normalise_match_key(Company_Name),
    Company_Ticker_Key = normalise_match_key(Company_Ticker)
  ) %>%
  filter(!is.na(Company_Ticker_Key)) %>%
  group_by(Company_Ticker_Key) %>%
  summarise(
    Canonical_Company_Name = last_or_na(Company_Name),
    Canonical_Company_Ticker = last_or_na(Company_Ticker),
    Canonical_Country = last_or_na(Country),
    .groups = "drop"
  )
main_name_lookup <- rating_main_raw %>%
  mutate(
    Company_Name_Key = normalise_match_key(Company_Name),
    Company_Ticker_Key = normalise_match_key(Company_Ticker)
  ) %>%
  filter(!is.na(Company_Name_Key)) %>%
  group_by(Company_Name_Key) %>%
  summarise(
    Canonical_Company_Name = last_or_na(Company_Name),
    Canonical_Company_Ticker = last_or_na(Company_Ticker),
    Canonical_Country = last_or_na(Country),
    .groups = "drop"
  )

rating_support_raw <- tibble()
rating_support_match_audit <- tibble()
if (!is.na(RATING_HISTORY_SUPPORT_FILE)) {
  rating_support_raw_all <- read_excel(
    RATING_HISTORY_SUPPORT_FILE,
    sheet = "Listed",
    col_types = "text",
    .name_repair = "unique_quiet"
  ) %>%
    standardise_rating_input("history_support_2005", "Listed") %>%
    mutate(
      History_Support_Only = TRUE,
      support_rating_date = parse_rating_date(Date),
      Support_Company_Name_Raw = Company_Name,
      Support_Company_Ticker_Raw = Company_Ticker,
      Company_Name_Key = normalise_match_key(Company_Name),
      Company_Ticker_Key = normalise_match_key(Company_Ticker),
      Matched_By_Ticker = !is.na(Company_Ticker_Key) & Company_Ticker_Key %in% main_company_ticker_keys,
      Matched_By_Name = !is.na(Company_Name_Key) & Company_Name_Key %in% main_company_name_keys,
      Matched_Main_Rating_Universe = Matched_By_Ticker | Matched_By_Name,
      In_History_Support_Window = support_rating_date >= HISTORY_SUPPORT_START & support_rating_date < PANEL_START
    ) %>%
    left_join(main_ticker_lookup, by = "Company_Ticker_Key") %>%
    left_join(main_name_lookup, by = "Company_Name_Key", suffix = c("_Ticker", "_Name")) %>%
    mutate(
      Company_Name = coalesce(Canonical_Company_Name_Ticker, Canonical_Company_Name_Name, Company_Name),
      Company_Ticker = coalesce(Canonical_Company_Ticker_Ticker, Canonical_Company_Ticker_Name, Company_Ticker),
      Country = coalesce(Canonical_Country_Ticker, Canonical_Country_Name, Country)
    )

  rating_support_match_audit <- rating_support_raw_all %>%
    count(
      Matched_Main_Rating_Universe,
      Matched_By_Ticker,
      Matched_By_Name,
      In_History_Support_Window,
      name = "N_Rows"
    ) %>%
    arrange(desc(Matched_Main_Rating_Universe), desc(In_History_Support_Window))
  save_audit(rating_support_match_audit, "rating_2005_history_support_match_summary_v5.csv")

  rating_support_unmatched <- rating_support_raw_all %>%
    filter(!Matched_Main_Rating_Universe | !In_History_Support_Window) %>%
    select(
      Company_Name,
      Company_Ticker,
      Support_Company_Name_Raw,
      Support_Company_Ticker_Raw,
      Date,
      support_rating_date,
      Curr_Rtg,
      Last_Rtg,
      Country,
      Matched_By_Ticker,
      Matched_By_Name,
      Matched_Main_Rating_Universe,
      In_History_Support_Window
    )
  save_audit(rating_support_unmatched, "rating_2005_history_support_excluded_rows_v5.csv")

  rating_support_raw <- rating_support_raw_all %>%
    filter(Matched_Main_Rating_Universe, In_History_Support_Window) %>%
    select(
      -support_rating_date,
      -Company_Name_Key,
      -Company_Ticker_Key,
      -starts_with("Canonical_")
    )

  for (col in optional_columns) {
    if (!col %in% names(rating_support_raw)) {
      rating_support_raw[[col]] <- NA_character_
    }
  }
}

rating_raw <- bind_rows(
  rating_main_raw,
  rating_support_raw %>%
    select(any_of(names(rating_main_raw)), everything())
) %>%
  mutate(
    History_Support_Only = replace_na(History_Support_Only, FALSE)
  )

rating_actions <- rating_raw %>%
  mutate(
    source_row = row_number(),
    rating_date = parse_rating_date(Date),
    Curr_Rtg_raw = Curr_Rtg,
    Last_Rtg_raw = Last_Rtg,
    Curr_Rtg_clean = clean_rating(Curr_Rtg),
    Last_Rtg_clean = clean_rating(Last_Rtg),
    curr_rank = unname(rating_rank_map[Curr_Rtg_clean]),
    last_rank = unname(rating_rank_map[Last_Rtg_clean]),
    rating_notch_change = curr_rank - last_rank,
    is_downgrade = !is.na(rating_notch_change) & rating_notch_change > 0L,
    is_upgrade = !is.na(rating_notch_change) & rating_notch_change < 0L,
    notches_lost = if_else(is_downgrade, as.integer(rating_notch_change), 0L),
    year = year(rating_date),
    quarter_number = quarter(rating_date),
    quarter = date_to_quarter(rating_date)
  )

bad_dates <- rating_actions %>%
  filter(is.na(rating_date)) %>%
  select(Company_Name, Date, Curr_Rtg_raw, Last_Rtg_raw)
save_audit(bad_dates, "rating_actions_bad_dates_v5.csv")
if (nrow(bad_dates) > 0L) {
  stop("Some rating dates could not be parsed. See Part_1_Outputs/audits/rating_actions_bad_dates_v5.csv", call. = FALSE)
}

unknown_ratings <- rating_actions %>%
  filter(
    (!is.na(Curr_Rtg_clean) & is.na(curr_rank)) |
      (!is.na(Last_Rtg_clean) & is.na(last_rank))
  ) %>%
  distinct(Curr_Rtg_raw, Curr_Rtg_clean, Last_Rtg_raw, Last_Rtg_clean)
save_audit(unknown_ratings, "rating_unknown_values_v5.csv")
if (nrow(unknown_ratings) > 0L) {
  stop("Unknown rating value(s) after cleaning. See Part_1_Outputs/audits/rating_unknown_values_v5.csv", call. = FALSE)
}

rating_transition_actions <- rating_actions %>%
  filter(rating_date >= PANEL_START, rating_date <= PANEL_END) %>%
  filter(!is.na(Last_Rtg_clean), !is.na(Curr_Rtg_clean), !is.na(last_rank), !is.na(curr_rank)) %>%
  mutate(
    From_Rating = Last_Rtg_clean,
    To_Rating = Curr_Rtg_clean,
    From_Rating_Group = rating_group_from_rating(From_Rating),
    To_Rating_Group = rating_group_from_rating(To_Rating),
    From_Rating_Grade = rating_grade_from_rating(From_Rating),
    To_Rating_Grade = rating_grade_from_rating(To_Rating),
    Transition_Direction = case_when(
      curr_rank > last_rank ~ "Downgrade",
      curr_rank < last_rank ~ "Upgrade",
      curr_rank == last_rank ~ "No_Notch_Change",
      TRUE ~ "Unknown"
    ),
    Notches_Changed = curr_rank - last_rank
  )

save_audit(
  rating_transition_actions %>%
    select(
      Company_Name,
      Company_Ticker,
      Country,
      rating_date,
      From_Rating,
      To_Rating,
      From_Rating_Group,
      To_Rating_Group,
      From_Rating_Grade,
      To_Rating_Grade,
      Transition_Direction,
      Notches_Changed
    ),
  "rating_transition_actions_v5.csv"
)

transition_summary <- rating_transition_actions %>%
  count(Transition_Direction, name = "N_Transitions") %>%
  mutate(Share = N_Transitions / sum(N_Transitions))
save_audit(transition_summary, "rating_transition_direction_summary_v5.csv")

save_transition_outputs(
  rating_transition_actions,
  "From_Rating",
  "To_Rating",
  rating_order,
  "rating_notch",
  "Rating transitions",
  x_label = "Current rating",
  y_label = "Previous rating"
)

save_transition_stock_plot(
  rating_transition_actions,
  "From_Rating",
  "To_Rating",
  rating_order,
  "rating_notch",
  "Previous and current rating stocks"
)

# Default-exit audit: these are upgrades out of D/SD, not cleaning errors.
default_exit_transitions <- rating_transition_actions %>%
  filter(
    From_Rating %in% c("SD", "D"),
    !To_Rating %in% c("SD", "D"),
    Transition_Direction == "Upgrade"
  ) %>%
  arrange(Company_Name, rating_date, source_row)

save_audit(
  default_exit_transitions %>%
    select(
      Company_Name,
      Company_Ticker,
      Country,
      rating_date,
      From_Rating,
      To_Rating,
      Transition_Direction,
      Notches_Changed,
      source_row
    ),
  "rating_default_exit_transition_actions_v5.csv"
)

rating_transition_actions_ex_default_exit <- rating_transition_actions %>%
  anti_join(default_exit_transitions %>% distinct(source_row), by = "source_row")

save_transition_outputs(
  rating_transition_actions_ex_default_exit,
  "From_Rating",
  "To_Rating",
  rating_order,
  "rating_notch_excluding_default_exits",
  "Rating transitions excluding default exits",
  x_label = "Current rating",
  y_label = "Previous rating"
)

save_transition_stock_plot(
  rating_transition_actions_ex_default_exit,
  "From_Rating",
  "To_Rating",
  rating_order,
  "rating_notch_excluding_default_exits",
  "Previous and current rating stocks excluding default exits"
)

save_transition_outputs(
  rating_transition_actions,
  "From_Rating_Group",
  "To_Rating_Group",
  c("A", "B", "C"),
  "rating_group",
  "Rating-group transitions",
  x_label = "Current rating group",
  y_label = "Previous rating group"
)

save_transition_stock_plot(
  rating_transition_actions,
  "From_Rating_Group",
  "To_Rating_Group",
  c("A", "B", "C"),
  "rating_group",
  "Previous and current rating-group stocks"
)

save_transition_outputs(
  rating_transition_actions,
  "From_Rating_Grade",
  "To_Rating_Grade",
  c("Investment", "Speculative"),
  "investment_speculative",
  "Investment-grade and speculative-grade transitions",
  x_label = "Current grade",
  y_label = "Previous grade"
)

save_transition_stock_plot(
  rating_transition_actions,
  "From_Rating_Grade",
  "To_Rating_Grade",
  c("Investment", "Speculative"),
  "investment_speculative",
  "Previous and current investment/speculative stocks"
)

same_day_actions <- rating_actions %>%
  count(Company_Name, rating_date, name = "actions_same_day") %>%
  filter(actions_same_day > 1L) %>%
  left_join(
    rating_actions %>%
      select(Company_Name, rating_date, source_row, Curr_Rtg_raw, Last_Rtg_raw, Curr_Rtg_clean, Last_Rtg_clean),
    by = c("Company_Name", "rating_date")
  ) %>%
  arrange(Company_Name, rating_date, source_row)
save_audit(same_day_actions, "rating_same_day_actions_v5.csv")

# Same-day action handling: collapse to the last row of the day to define the
# end-of-day current rating used in the carry-forward panel.
daily_actions <- rating_actions %>%
  filter(!is.na(Curr_Rtg_clean), !is.na(curr_rank)) %>%
  arrange(Company_Name, rating_date, source_row) %>%
  group_by(Company_Name, rating_date) %>%
  slice_tail(n = 1L) %>%
  ungroup()

# Initial-rating backfill: the first observed Last_Rtg identifies the rating in
# force before the first action, allowing pre-first-action quarters to be retained.
first_actions <- rating_actions %>%
  arrange(Company_Name, rating_date, source_row) %>%
  group_by(Company_Name) %>%
  slice(1L) %>%
  ungroup() %>%
  filter(!is.na(Last_Rtg_clean), !is.na(last_rank)) %>%
  transmute(
    Company_Name,
    Company_Ticker,
    Country,
    Agency,
    Rating_Type,
    Industry,
    Frequency,
    interval_start = as.Date("1900-01-01"),
    event_date = rating_date,
    rating = Last_Rtg_clean,
    rating_rank = as.integer(last_rank),
    interval_source = "first_Last_Rtg_backfill"
  )

current_actions <- daily_actions %>%
  transmute(
    Company_Name,
    Company_Ticker,
    Country,
    Agency,
    Rating_Type,
    Industry,
    Frequency,
    interval_start = rating_date,
    event_date = rating_date,
    rating = Curr_Rtg_clean,
    rating_rank = as.integer(curr_rank),
    interval_source = "Curr_Rtg_carry_forward"
  )

rating_intervals <- bind_rows(first_actions, current_actions) %>%
  arrange(Company_Name, interval_start, event_date) %>%
  group_by(Company_Name) %>%
  mutate(
    interval_end = lead(interval_start) - days(1),
    interval_end = coalesce(interval_end, as.Date("2099-12-31")),
    interval_start = pmax(interval_start, HISTORY_SUPPORT_START),
    interval_end = pmin(interval_end, PANEL_END)
  ) %>%
  ungroup() %>%
  filter(interval_start <= interval_end)

quarter_calendar <- make_quarter_calendar(HISTORY_SUPPORT_START, PANEL_END)

# Firm-quarter rating assignment: intersect rating intervals with calendar
# quarters and retain the rating with the largest within-quarter day coverage.
rating_quarter_days <- rating_intervals %>%
  mutate(.join_key = 1L) %>%
  inner_join(quarter_calendar %>% mutate(.join_key = 1L), by = ".join_key", relationship = "many-to-many") %>%
  select(-.join_key) %>%
  mutate(
    overlap_start = pmax(interval_start, quarter_start),
    overlap_end = pmin(interval_end, quarter_end),
    rating_days = as.integer(overlap_end - overlap_start) + 1L
  ) %>%
  filter(rating_days > 0L) %>%
  group_by(Company_Name, year, quarter_number, quarter, rating, rating_rank) %>%
  summarise(
    rating_days_in_quarter = sum(rating_days),
    latest_rating_event_date = max(event_date, na.rm = TRUE),
    quarter_start = first(quarter_start),
    quarter_end = first(quarter_end),
    quarter_days = first(quarter_days),
    Company_Ticker = last_or_na(Company_Ticker),
    Country = last_or_na(Country),
    Agency = last_or_na(Agency),
    Rating_Type = last_or_na(Rating_Type),
    Industry = last_or_na(Industry),
    Frequency = last_or_na(Frequency),
    .groups = "drop"
  )

rating_panel_base <- rating_quarter_days %>%
  group_by(Company_Name, year, quarter_number, quarter) %>%
  arrange(desc(rating_days_in_quarter), desc(latest_rating_event_date), .by_group = TRUE) %>%
  slice(1L) %>%
  ungroup() %>%
  mutate(
    rating_number = rating_rank,
    rating_group = rating_group_from_rating(rating),
    macro_rating = macro_rating_from_rating(rating),
    rating_grade = rating_grade_from_rating(rating),
    rating_share_in_quarter = rating_days_in_quarter / quarter_days
  )

downgrade_by_firm_quarter <- rating_actions %>%
  filter(rating_date >= HISTORY_SUPPORT_START, rating_date <= PANEL_END) %>%
  group_by(Company_Name, year, quarter_number, quarter) %>%
  summarise(
    Downgrade = as.integer(any(is_downgrade, na.rm = TRUE)),
    downgrade_notches_lost = sum(notches_lost, na.rm = TRUE),
    downgrade_count = sum(is_downgrade, na.rm = TRUE),
    upgrade_count = sum(is_upgrade, na.rm = TRUE),
    no_notch_change_count = sum(!is.na(rating_notch_change) & rating_notch_change == 0L, na.rm = TRUE),
    rating_action_count = n(),
    .groups = "drop"
  )

rating_panel_with_history_support <- rating_panel_base %>%
  left_join(downgrade_by_firm_quarter, by = c("Company_Name", "year", "quarter_number", "quarter")) %>%
  mutate(
    Downgrade = replace_na(Downgrade, 0L),
    downgrade_notches_lost = replace_na(downgrade_notches_lost, 0L),
    downgrade_count = replace_na(downgrade_count, 0L),
    upgrade_count = replace_na(upgrade_count, 0L),
    no_notch_change_count = replace_na(no_notch_change_count, 0L),
    rating_action_count = replace_na(rating_action_count, 0L)
  ) %>%
  select(
    Company_Name,
    Company_Ticker,
    Country,
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
    Agency,
    Rating_Type,
    Industry,
    Frequency
  ) %>%
  arrange(Company_Name, year, quarter_number)

duplicate_keys <- rating_panel_with_history_support %>%
  count(Company_Name, year, quarter_number, name = "n") %>%
  filter(n > 1L)
save_audit(duplicate_keys, "rating_panel_duplicate_keys_v5.csv")
if (nrow(duplicate_keys) > 0L) {
  stop("Rating panel has duplicate firm-quarter keys. See Part_1_Outputs/audits/rating_panel_duplicate_keys_v5.csv", call. = FALSE)
}

# Rating-history predictors: construct previous-four-quarter downgrade and
# migration measures excluding the current quarter to avoid target leakage.
rating_panel_with_history_support <- rating_panel_with_history_support %>%
  mutate(Quarter_Index = year * 4L + quarter_number) %>%
  arrange(Company_Name, Quarter_Index) %>%
  group_by(Company_Name) %>%
  mutate(
    N_Downgrade_Quarters_Previous_4Q = lag_sum(Downgrade, 1:4),
    N_Downgrades_Previous_4Q = N_Downgrade_Quarters_Previous_4Q,
    Downgrade_Previous_4Q = as.integer(N_Downgrade_Quarters_Previous_4Q > 0L),
    N_Downgrade_Events_Previous_4Q = lag_sum(downgrade_count, 1:4),
    Notches_Lost_Previous_4Q = lag_sum(downgrade_notches_lost, 1:4),
    N_Rating_Actions_Previous_4Q = lag_sum(rating_action_count, 1:4),
    N_Upgrades_Previous_4Q = lag_sum(upgrade_count, 1:4),
    N_Downgrades_Previous_4Q_Bucket = as.character(N_Downgrade_Quarters_Previous_4Q)
  ) %>%
  ungroup()

rating_history_support_2006_audit <- rating_panel_with_history_support %>%
  filter(year == 2006L) %>%
  select(
    Company_Name,
    Company_Ticker,
    Country,
    year,
    quarter_number,
    quarter,
    Downgrade_Previous_4Q,
    N_Downgrade_Quarters_Previous_4Q,
    N_Downgrade_Events_Previous_4Q,
    Notches_Lost_Previous_4Q,
    N_Rating_Actions_Previous_4Q,
    N_Upgrades_Previous_4Q
  )
save_audit(rating_history_support_2006_audit, "rating_history_support_2006_lagged_variables_v5.csv")

rating_history_support_2006_summary <- rating_history_support_2006_audit %>%
  group_by(year, quarter_number, quarter) %>%
  summarise(
    N_Firm_Quarters = n(),
    N_With_Previous_4Q_Downgrade = sum(Downgrade_Previous_4Q == 1L, na.rm = TRUE),
    N_Previous_4Q_Downgrade_Events = sum(N_Downgrade_Events_Previous_4Q, na.rm = TRUE),
    N_Previous_4Q_Rating_Actions = sum(N_Rating_Actions_Previous_4Q, na.rm = TRUE),
    .groups = "drop"
  )
save_audit(rating_history_support_2006_summary, "rating_history_support_2006_summary_v5.csv")

rating_panel <- rating_panel_with_history_support %>%
  filter(year >= lubridate::year(PANEL_START))

time_clustering_quarter <- rating_panel %>%
  group_by(year, quarter_number, quarter) %>%
  summarise(
    N_t = n(),
    N_firms_t = n_distinct(Company_Name),
    downgrades_t = sum(Downgrade == 1L, na.rm = TRUE),
    downgrade_events_t = sum(downgrade_count, na.rm = TRUE),
    downgrade_rate_t = downgrades_t / N_t,
    downgrade_events_per_firm_quarter = downgrade_events_t / N_t,
    .groups = "drop"
  ) %>%
  arrange(year, quarter_number) %>%
  mutate(
    quarter_date = as.Date(sprintf("%04d-%02d-01", year, (quarter_number - 1L) * 3L + 1L))
  )
save_audit(time_clustering_quarter, "time_clustering_downgrade_rate_by_quarter_v5.csv")

time_clustering_full_sample_summary <- time_clustering_quarter %>%
  summarise(
    Period = paste0(min(year, na.rm = TRUE), "-", max(year, na.rm = TRUE)),
    N_Quarters = n(),
    Mean_Downgrade_Rate = mean(downgrade_rate_t, na.rm = TRUE),
    Median_Downgrade_Rate = median(downgrade_rate_t, na.rm = TRUE),
    SD_Downgrade_Rate = sd(downgrade_rate_t, na.rm = TRUE),
    Min_Downgrade_Rate = min(downgrade_rate_t, na.rm = TRUE),
    Max_Downgrade_Rate = max(downgrade_rate_t, na.rm = TRUE)
  )
save_audit(time_clustering_full_sample_summary, "time_clustering_full_sample_summary_v5.csv")

time_clustering_year <- rating_panel %>%
  group_by(year) %>%
  summarise(
    N_t = n(),
    N_firms_t = n_distinct(Company_Name),
    downgrades_t = sum(Downgrade == 1L, na.rm = TRUE),
    downgrade_events_t = sum(downgrade_count, na.rm = TRUE),
    downgrade_rate_t = downgrades_t / N_t,
    downgrade_events_per_firm_quarter = downgrade_events_t / N_t,
    .groups = "drop"
  ) %>%
  arrange(year)
save_audit(time_clustering_year, "time_clustering_downgrade_rate_by_year_v5.csv")

seasonality_by_quarter <- rating_panel %>%
  group_by(quarter_number) %>%
  summarise(
    QuarterOfYear = paste0("Q", first(quarter_number)),
    N_t = n(),
    downgrades_t = sum(Downgrade == 1L, na.rm = TRUE),
    P_Downgrade = downgrades_t / N_t,
    .groups = "drop"
  ) %>%
  arrange(quarter_number)
save_audit(seasonality_by_quarter, "seasonality_downgrade_probability_by_quarter_of_year_v5.csv")

seasonality_table <- table(rating_panel$quarter_number, rating_panel$Downgrade)
seasonality_test <- suppressWarnings(chisq.test(seasonality_table))
seasonality_cramers_v <- sqrt(unname(seasonality_test$statistic) / (sum(seasonality_table) * (min(dim(seasonality_table)) - 1L)))
seasonality_test_audit <- tibble(
  Test = "Pearson_chisq_quarter_of_year_vs_downgrade",
  Statistic = unname(seasonality_test$statistic),
  DF = unname(seasonality_test$parameter),
  P_Value = seasonality_test$p.value,
  Cramers_V = seasonality_cramers_v,
  P_Value_Threshold = 0.05,
  Cramers_V_Practical_Threshold = 0.05,
  Statistically_Significant_5pct = seasonality_test$p.value < 0.05,
  Practically_Meaningful = seasonality_cramers_v >= 0.05,
  Include_QuarterOfYear_In_V5 = FALSE,
  Decision_Note = "Quarter-of-year is not carried into V5 model specifications by design."
)
save_audit(seasonality_test_audit, "seasonality_test_v5.csv")

issuer_persistence <- rating_panel %>%
  group_by(Company_Name, Company_Ticker, Country) %>%
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
save_audit(issuer_persistence, "issuer_persistence_v5.csv")

issuer_persistence_gt_10pct <- issuer_persistence %>%
  filter(N_i_down_over_T_i > 0.10) %>%
  arrange(desc(N_i_down_over_T_i), desc(N_i_down), Company_Name)
save_audit(issuer_persistence_gt_10pct, "issuer_persistence_gt_10pct_v5.csv")

issuer_persistence_bucket_distribution <- issuer_persistence %>%
  count(Downgrade_Frequency_Bucket, name = "N_Issuers") %>%
  mutate(Share_Issuers = N_Issuers / sum(N_Issuers)) %>%
  arrange(Downgrade_Frequency_Bucket)
save_audit(issuer_persistence_bucket_distribution, "issuer_persistence_frequency_buckets_v5.csv")

previous_downgrade_distribution <- rating_panel %>%
  count(N_Downgrades_Previous_4Q_Bucket, name = "N_Firm_Quarters") %>%
  mutate(Share = N_Firm_Quarters / sum(N_Firm_Quarters)) %>%
  arrange(factor(N_Downgrades_Previous_4Q_Bucket, levels = c("0", "1", "2", "3", "4")))
save_audit(previous_downgrade_distribution, "previous_4q_downgrade_count_distribution_v5.csv")

previous_history_next_downgrade_rate <- rating_panel %>%
  group_by(N_Downgrades_Previous_4Q_Bucket) %>%
  summarise(
    N_Firm_Quarters = n(),
    Downgrade_Rows = sum(Downgrade == 1L, na.rm = TRUE),
    Downgrade_Rate = mean(Downgrade == 1L, na.rm = TRUE),
    Mean_Notches_Lost_Current_Quarter = mean(downgrade_notches_lost, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(factor(N_Downgrades_Previous_4Q_Bucket, levels = c("0", "1", "2", "3", "4")))
save_audit(previous_history_next_downgrade_rate, "previous_4q_history_current_downgrade_rate_v5.csv")

previous_event_history <- rating_panel %>%
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

previous_event_distribution <- previous_event_history %>%
  count(N_Downgrade_Events_Previous_4Q_Bucket, name = "N_Firm_Quarters") %>%
  mutate(Share = N_Firm_Quarters / sum(N_Firm_Quarters)) %>%
  arrange(N_Downgrade_Events_Previous_4Q_Bucket)
save_audit(previous_event_distribution, "previous_4q_downgrade_event_distribution_v5.csv")

previous_event_history_next_downgrade_rate <- previous_event_history %>%
  group_by(N_Downgrade_Events_Previous_4Q_Bucket) %>%
  summarise(
    N_Firm_Quarters = n(),
    Downgrade_Rows = sum(Downgrade == 1L, na.rm = TRUE),
    Downgrade_Rate = mean(Downgrade == 1L, na.rm = TRUE),
    Mean_Notches_Lost_Current_Quarter = mean(downgrade_notches_lost, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(N_Downgrade_Events_Previous_4Q_Bucket)
save_audit(previous_event_history_next_downgrade_rate, "previous_4q_event_history_current_downgrade_rate_v5.csv")

rating_actions_not_in_panel <- rating_actions %>%
  filter(rating_date >= PANEL_START, rating_date <= PANEL_END) %>%
  anti_join(
    rating_panel %>% distinct(Company_Name, year, quarter_number),
    by = c("Company_Name", "year", "quarter_number")
  ) %>%
  select(Company_Name, Company_Ticker, Country, rating_date, quarter, Curr_Rtg_raw, Last_Rtg_raw, Curr_Rtg_clean, Last_Rtg_clean)
save_audit(rating_actions_not_in_panel, "rating_actions_not_represented_in_rating_panel_v5.csv")

rating_audit <- tibble(
  metric = c(
    "main_input_rating_action_rows",
    "history_support_2005_file_available",
    "history_support_2005_rows_used",
    "input_rating_action_rows_including_history_support",
    "companies_in_rating_action_file",
    "output_company_quarter_rows",
    "output_companies",
    "first_output_quarter",
    "last_output_quarter",
    "rating_group_A_rows",
    "rating_group_B_rows",
    "rating_group_C_rows",
    "rows_selected_with_less_than_full_quarter_rating_coverage",
    "downgrade_rows",
    "downgrade_count",
    "upgrade_count",
    "rating_action_rows_not_represented_in_panel",
    "unknown_cleaned_rating_values",
    "company_date_combinations_with_multiple_actions"
  ),
  value = c(
    as.character(nrow(rating_main_raw)),
    as.character(!is.na(RATING_HISTORY_SUPPORT_FILE)),
    as.character(nrow(rating_support_raw)),
    as.character(nrow(rating_raw)),
    as.character(n_distinct(rating_actions$Company_Name)),
    as.character(nrow(rating_panel)),
    as.character(n_distinct(rating_panel$Company_Name)),
    min(rating_panel$quarter),
    max(rating_panel$quarter),
    as.character(sum(rating_panel$rating_group == "A", na.rm = TRUE)),
    as.character(sum(rating_panel$rating_group == "B", na.rm = TRUE)),
    as.character(sum(rating_panel$rating_group == "C", na.rm = TRUE)),
    as.character(sum(rating_panel$rating_days_in_quarter < rating_panel$quarter_days, na.rm = TRUE)),
    as.character(sum(rating_panel$Downgrade == 1L, na.rm = TRUE)),
    as.character(sum(rating_panel$downgrade_count, na.rm = TRUE)),
    as.character(sum(rating_panel$upgrade_count, na.rm = TRUE)),
    as.character(nrow(rating_actions_not_in_panel)),
    as.character(nrow(unknown_ratings)),
    as.character(n_distinct(paste(same_day_actions$Company_Name, same_day_actions$rating_date)))
  )
)
save_audit(rating_audit, "rating_panel_audit_v5.csv")

rating_group_distribution <- rating_panel %>%
  count(rating_group, name = "n_rows") %>%
  mutate(row_share = n_rows / sum(n_rows))
save_audit(rating_group_distribution, "rating_group_distribution_v5.csv")

macro_rating_distribution <- rating_panel %>%
  count(macro_rating, name = "n_rows") %>%
  mutate(row_share = n_rows / sum(n_rows))
save_audit(macro_rating_distribution, "macro_rating_distribution_v5.csv")

rating_grade_distribution <- rating_panel %>%
  count(rating_grade, name = "n_rows") %>%
  mutate(row_share = n_rows / sum(n_rows))
save_audit(rating_grade_distribution, "rating_grade_distribution_v5.csv")

downgrade_by_year <- rating_panel %>%
  group_by(year) %>%
  summarise(
    n_rated_firm_quarters = n(),
    n_firms = n_distinct(Company_Name),
    downgrade_rows = sum(Downgrade == 1L, na.rm = TRUE),
    downgrade_count = sum(downgrade_count, na.rm = TRUE),
    downgrade_rate = mean(Downgrade == 1L, na.rm = TRUE),
    .groups = "drop"
  )
save_audit(downgrade_by_year, "downgrade_distribution_by_year_v5.csv")

downgrade_by_group <- rating_panel %>%
  group_by(rating_group) %>%
  summarise(
    n_rated_firm_quarters = n(),
    n_firms = n_distinct(Company_Name),
    downgrade_rows = sum(Downgrade == 1L, na.rm = TRUE),
    downgrade_count = sum(downgrade_count, na.rm = TRUE),
    downgrade_rate = mean(Downgrade == 1L, na.rm = TRUE),
    .groups = "drop"
  )
save_audit(downgrade_by_group, "downgrade_distribution_by_rating_group_v5.csv")

  save_plot(
    ggplot(rating_group_distribution, aes(x = rating_group, y = n_rows, fill = rating_group)) +
      geom_col(show.legend = FALSE) +
    scale_fill_manual(values = main_plot_palette(n_distinct(rating_group_distribution$rating_group))) +
    scale_y_continuous(labels = comma) +
    labs(x = "Rating group", y = "Firm-quarter rows", title = "Rating group distribution") +
    theme_minimal(base_size = 11),
  "fig_01_rating_group_distribution_v5.png"
)

save_plot(
  ggplot(downgrade_by_year, aes(x = year, y = downgrade_count)) +
    geom_col(fill = main_plot_colours[["blue"]]) +
    scale_y_continuous(labels = comma) +
    labs(x = "Year", y = "Downgrade count", title = "Downgrades by year") +
    theme_minimal(base_size = 11),
  "fig_02_downgrade_count_by_year_v5.png"
)

save_plot(
  ggplot(downgrade_by_group, aes(x = rating_group, y = downgrade_rate, fill = rating_group)) +
    geom_col(show.legend = FALSE) +
    scale_fill_manual(values = main_plot_palette(n_distinct(downgrade_by_group$rating_group))) +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    labs(x = "Rating group", y = "Downgrade rate", title = "Downgrade rate by rating group") +
    theme_minimal(base_size = 11),
  "fig_03_downgrade_rate_by_rating_group_v5.png"
)

write_csv(rating_panel, file.path(DATA_DIR, "company_quarter_rating_panel_v5.csv"), na = "")
saveRDS(rating_panel, file.path(DATA_DIR, "company_quarter_rating_panel_v5.rds"))
write_csv(rating_actions, file.path(DATA_DIR, "rating_actions_clean_v5.csv"), na = "")
saveRDS(rating_actions, file.path(DATA_DIR, "rating_actions_clean_v5.rds"))

message("Part 1 V5 completed.")
message("Rating panel rows: ", nrow(rating_panel))
message("Rating panel firms: ", n_distinct(rating_panel$Company_Name))
message("Downgrade rows: ", sum(rating_panel$Downgrade == 1L, na.rm = TRUE))
message("Downgrade count: ", sum(rating_panel$downgrade_count, na.rm = TRUE))



