# Step 09 - Compute ALE and RGE explainability summaries.
# GitHub-ready copy: paths are repository-relative and data files are intentionally excluded.

# Main explainability deliberately excludes SHAP, RF impurity importance,
# XGBoost gain, and permutation importance. The common framework is based on
# first-order ALE profiles, ALE-derived global importance, and block-level RGE.

set.seed(20260724)

required_packages <- c(
  "tidyverse", "glmnet", "mgcv", "ranger", "xgboost",
  "knitr", "scales", "digest"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop("Missing required package(s): ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
})

SEED <- 20260724
PART7_SPEC_BLOCK <- tolower(trimws(Sys.getenv("PART7_SPEC_BLOCK", Sys.getenv("PART4_SPEC_BLOCK", "all"))))
PART7_SELECTION_RULE <- tolower(trimws(Sys.getenv("PART7_SELECTION_RULE", Sys.getenv("PART5_SELECTION_RULE", "near_best_calibrated"))))
PART7_ALE_SCOPE <- tolower(trimws(Sys.getenv("PART7_ALE_SCOPE", "all")))
PART7_RGE_SCOPE <- tolower(trimws(Sys.getenv("PART7_RGE_SCOPE", "all")))
PART7_MAX_EXPLAIN_ROWS <- as.integer(Sys.getenv("PART7_MAX_EXPLAIN_ROWS", "500"))
PART7_MAX_EXPLAIN_ROWS <- if_else(is.na(PART7_MAX_EXPLAIN_ROWS) | PART7_MAX_EXPLAIN_ROWS < 1L, 500L, PART7_MAX_EXPLAIN_ROWS)
PART7_ALE_GRID <- as.integer(Sys.getenv("PART7_ALE_GRID", "10"))
PART7_ALE_GRID <- if_else(is.na(PART7_ALE_GRID) | PART7_ALE_GRID < 2L, 10L, PART7_ALE_GRID)
PART7_RUN_ALE <- tolower(Sys.getenv("PART7_RUN_ALE", "TRUE")) %in% c("true", "t", "1", "yes")
PART7_RUN_RGE <- tolower(Sys.getenv("PART7_RUN_RGE", "TRUE")) %in% c("true", "t", "1", "yes")
PART7_OVERWRITE_OUTPUTS <- tolower(Sys.getenv("PART7_OVERWRITE_OUTPUTS", "FALSE")) %in% c("true", "t", "1", "yes")
PART7_OUTPUT_TAG <- trimws(Sys.getenv("PART7_OUTPUT_TAG", ""))
default_figure_mode <- if_else(PART7_ALE_SCOPE == "all" || PART7_RGE_SCOPE == "all", "summary", "selected")
PART7_FIGURE_MODE <- tolower(trimws(Sys.getenv("PART7_FIGURE_MODE", default_figure_mode)))

allowed_selection_rules <- c("max_pr_auc", "near_best_parsimony", "near_best_calibrated")
if (!PART7_SELECTION_RULE %in% allowed_selection_rules) {
  stop("PART7_SELECTION_RULE must be one of: ", paste(allowed_selection_rules, collapse = ", "), call. = FALSE)
}
if (!PART7_ALE_SCOPE %in% c("best_by_horizon_pr_auc", "best_by_horizon_mcc", "all")) {
  stop("PART7_ALE_SCOPE must be one of: best_by_horizon_pr_auc, best_by_horizon_mcc, all.", call. = FALSE)
}
if (!PART7_RGE_SCOPE %in% c("all", "best_by_horizon_pr_auc", "best_by_horizon_mcc")) {
  stop("PART7_RGE_SCOPE must be one of: all, best_by_horizon_pr_auc, best_by_horizon_mcc.", call. = FALSE)
}
if (!PART7_FIGURE_MODE %in% c("none", "summary", "selected", "all")) {
  stop("PART7_FIGURE_MODE must be one of: none, summary, selected, all.", call. = FALSE)
}

parse_filter <- function(value) {
  value <- trimws(value)
  if (!nzchar(value)) return(character())
  strsplit(value, ",", fixed = TRUE)[[1]] %>%
    trimws() %>%
    purrr::discard(~ !nzchar(.x))
}

selected_models_filter <- parse_filter(Sys.getenv("PART7_SELECTED_MODELS", ""))
selected_sets_filter <- parse_filter(Sys.getenv("PART7_SELECTED_REGRESSOR_SETS", ""))
selected_horizons_filter <- parse_filter(Sys.getenv("PART7_SELECTED_HORIZONS", ""))
ale_models_filter <- parse_filter(Sys.getenv("PART7_ALE_MODELS", Sys.getenv("PART7_SELECTED_MODELS", "")))
ale_sets_filter <- parse_filter(Sys.getenv("PART7_ALE_REGRESSOR_SETS", Sys.getenv("PART7_SELECTED_REGRESSOR_SETS", "")))
ale_horizons_filter <- parse_filter(Sys.getenv("PART7_ALE_HORIZONS", Sys.getenv("PART7_SELECTED_HORIZONS", "")))

clean_file_id <- function(x) {
  x <- gsub("[^A-Za-z0-9_\\-]+", "_", x)
  x <- gsub("_+", "_", x)
  gsub("^_|_$", "", x)
}

part_suffix <- function(spec) {
  if (spec %in% c("", "none")) "" else paste0("_", clean_file_id(spec))
}

selection_suffix <- function(rule) {
  paste0("_", clean_file_id(rule))
}

get_script_dir <- function() {
  file_arg <- "--file="
  args <- commandArgs(trailingOnly = FALSE)
  script_arg <- args[startsWith(args, file_arg)]
  if (length(script_arg) > 0L) {
    return(dirname(normalizePath(sub(file_arg, "", script_arg[[1]]), winslash = "/", mustWork = TRUE)))
  }
  frame_files <- unlist(lapply(sys.frames(), function(frame) {
    if (!is.null(frame$ofile)) frame$ofile else character()
  }))
  frame_files <- frame_files[nzchar(frame_files)]
  if (length(frame_files) > 0L) {
    for (source_file in rev(frame_files)) {
      candidates <- c(source_file, file.path(getwd(), source_file), basename(source_file))
      candidates <- candidates[file.exists(candidates)]
      if (length(candidates) > 0L) {
        return(dirname(normalizePath(candidates[[1]], winslash = "/", mustWork = TRUE)))
      }
    }
  }
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

SCRIPT_DIR <- get_script_dir()
V5_DIR <- normalizePath(file.path(SCRIPT_DIR, ".."), winslash = "/", mustWork = TRUE)
PART5_OUTPUT_DIR <- file.path(V5_DIR, paste0("Part_5", part_suffix(PART7_SPEC_BLOCK), "_Outputs", selection_suffix(PART7_SELECTION_RULE)))
LOCKED_TEST_RDS <- file.path(V5_DIR, "Part_3_Outputs", "data_imputed", "Panel_Europe_macro_locked_test_imputed.rds")
REGRESSOR_MANIFEST_CSV <- file.path(V5_DIR, "Part_3_Outputs", "audits", "table_20_regressor_manifest.csv")
OUTPUT_DIR <- file.path(V5_DIR, paste0("Part_7_ALE_RGE", part_suffix(PART7_SPEC_BLOCK), "_Outputs", selection_suffix(PART7_SELECTION_RULE)))
if (nzchar(clean_file_id(PART7_OUTPUT_TAG))) {
  OUTPUT_DIR <- paste0(OUTPUT_DIR, "_", clean_file_id(PART7_OUTPUT_TAG))
}
DATA_DIR <- file.path(OUTPUT_DIR, "data")
AUDIT_DIR <- file.path(OUTPUT_DIR, "audits")
TABLE_DIR <- file.path(OUTPUT_DIR, "tables_latex")
FIGURE_DIR <- file.path(OUTPUT_DIR, "figures")

dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(AUDIT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TABLE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIGURE_DIR, recursive = TRUE, showWarnings = FALSE)

assert_new_file <- function(path) {
  if (file.exists(path) && !PART7_OVERWRITE_OUTPUTS) {
    stop("Refusing to overwrite existing Part 7 V5 output: ", path,
         ". Set PART7_OVERWRITE_OUTPUTS=TRUE deliberately to replace it.", call. = FALSE)
  }
  invisible(path)
}

write_audit <- function(x, name, caption = NULL, digits = 4) {
  csv_path <- file.path(AUDIT_DIR, paste0(name, ".csv"))
  tex_path <- file.path(TABLE_DIR, paste0(name, ".tex"))
  assert_new_file(csv_path)
  assert_new_file(tex_path)
  readr::write_csv(x, csv_path, na = "")
  latex_table <- knitr::kable(x, format = "latex", booktabs = TRUE, digits = digits, caption = caption)
  writeLines(latex_table, tex_path)
  invisible(x)
}

save_data <- function(x, name) {
  path <- file.path(DATA_DIR, paste0(name, ".rds"))
  assert_new_file(path)
  saveRDS(x, path)
  invisible(x)
}

save_plot <- function(plot, name, width = 10, height = 6) {
  path <- file.path(FIGURE_DIR, paste0(name, ".png"))
  assert_new_file(path)
  ggplot2::ggsave(path, plot = plot, width = width, height = height, dpi = 300)
  invisible(path)
}

latest_matching_file <- function(dir, pattern) {
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0L) return(NA_character_)
  files[which.max(file.info(files)$mtime)]
}

stable_seed <- function(...) {
  key <- paste(..., collapse = "|")
  as.integer(strtoi(substr(digest::digest(key, algo = "xxhash32"), 1, 7), base = 16L)) %% .Machine$integer.max
}

SIGNED_LOG_EXCLUDE <- c("rating_rank", "rating_number", "macro_rating", "rating_group_B", "rating_group_C")

signed_log <- function(x) {
  sign(x) * log1p(abs(x))
}

apply_preprocessor <- function(data, prep) {
  x <- data %>%
    select(all_of(prep$predictors)) %>%
    mutate(across(everything(), as.numeric))
  if (prep$use_signed_log) {
    signed_log_vars <- setdiff(names(x), SIGNED_LOG_EXCLUDE)
    if (length(signed_log_vars) > 0L) {
      x <- x %>% mutate(across(all_of(signed_log_vars), signed_log))
    }
  }
  x <- sweep(as.matrix(x), 2, prep$centers, "-")
  x <- sweep(x, 2, prep$scales, "/")
  as_tibble(x, .name_repair = "minimal") %>% setNames(prep$predictors)
}

prepare_model_data <- function(data) {
  data %>%
    mutate(
      rating_rank = as.numeric(rating_rank),
      rating_number = if ("rating_number" %in% names(.)) as.numeric(rating_number) else rating_rank,
      macro_rating = if ("macro_rating" %in% names(.)) as.numeric(macro_rating) else NA_real_,
      rating_group_B = as.integer(rating_group == "B"),
      rating_group_C = as.integer(rating_group == "C")
    )
}

predict_v5_model <- function(model_object, newdata) {
  model_name <- model_object$candidate$Model
  predictors <- model_object$predictors
  missing_predictors <- setdiff(predictors, names(newdata))
  if (length(missing_predictors) > 0L) {
    stop("Missing predictors for Part 7 prediction: ", paste(missing_predictors, collapse = ", "), call. = FALSE)
  }
  fit <- model_object$model
  if (model_name %in% c("Logit", "Probit", "GAM")) {
    x <- apply_preprocessor(newdata, model_object$preprocessor)
    model_data <- bind_cols(tibble(Outcome_Num = 0), x)
    return(as.numeric(predict(fit, newdata = model_data, type = "response")))
  }
  if (model_name == "Elastic_Net") {
    x <- as.matrix(apply_preprocessor(newdata, model_object$preprocessor))
    return(as.numeric(predict(fit, newx = x, s = model_object$candidate$lambda, type = "response")))
  }
  if (model_name == "Random_Forest") {
    x <- newdata %>% select(all_of(predictors))
    pred <- predict(fit, data = x)$predictions
    return(as.numeric(pred[, "Downgrade"]))
  }
  if (model_name == "XGBoost") {
    x <- as.matrix(newdata %>% select(all_of(predictors)))
    return(as.numeric(predict(fit, xgboost::xgb.DMatrix(data = x))))
  }
  stop("Unsupported model for Part 7 prediction: ", model_name, call. = FALSE)
}

load_part5_inputs <- function() {
  if (!dir.exists(PART5_OUTPUT_DIR)) {
    stop("Missing Part 5 output directory: ", PART5_OUTPUT_DIR, call. = FALSE)
  }
  prediction_index_file <- latest_matching_file(file.path(PART5_OUTPUT_DIR, "audits"), "^table_15_locked_test_prediction_file_index_.*\\.csv$")
  model_index_file <- latest_matching_file(file.path(PART5_OUTPUT_DIR, "audits"), "^table_11_locked_test_model_object_index_.*\\.csv$")
  if (is.na(prediction_index_file) || is.na(model_index_file)) {
    stop("Missing Part 5 prediction or model-object index in: ", file.path(PART5_OUTPUT_DIR, "audits"), call. = FALSE)
  }
  list(
    prediction_index_file = prediction_index_file,
    model_index_file = model_index_file,
    prediction_index = readr::read_csv(prediction_index_file, show_col_types = FALSE),
    model_index = readr::read_csv(model_index_file, show_col_types = FALSE)
  )
}

apply_common_filters <- function(x, models = selected_models_filter, sets = selected_sets_filter, horizons = selected_horizons_filter) {
  out <- x
  if (length(models) > 0L) out <- out %>% filter(Model %in% models)
  if (length(sets) > 0L) out <- out %>% filter(Predictor_Set %in% sets)
  if (length(horizons) > 0L) {
    horizon_labels <- ifelse(grepl("^t\\+", horizons), horizons, paste0("t+", horizons))
    out <- out %>% filter(Horizon_Label %in% horizon_labels | as.character(Horizon) %in% horizons)
  }
  out
}

scope_metric_file <- function(scope) {
  metric_file <- latest_matching_file(
    file.path(PART5_OUTPUT_DIR, "audits"),
    if (scope == "best_by_horizon_pr_auc") "^table_07_locked_test_best_by_horizon_pr_auc_.*\\.csv$" else "^table_08_locked_test_best_by_horizon_mcc_.*\\.csv$"
  )
  if (is.na(metric_file)) {
    stop("Missing Part 5 best-by-horizon table for scope=", scope, call. = FALSE)
  }
  metric_file
}

selected_by_scope <- function(scope) {
  if (scope == "all") return(tibble())
  readr::read_csv(scope_metric_file(scope), show_col_types = FALSE) %>%
    distinct(Predictor_Set, Model, Horizon, Horizon_Label)
}

filter_by_scope <- function(index, scope) {
  out <- apply_common_filters(index)
  if (scope == "all") return(out)
  selected <- selected_by_scope(scope)
  out %>% inner_join(selected, by = c("Predictor_Set", "Model", "Horizon", "Horizon_Label"))
}

filter_ale_tasks <- function(model_index) {
  out <- model_index %>% filter(Fit_Status == "OK")
  out <- filter_by_scope(out, PART7_ALE_SCOPE)
  apply_common_filters(out, models = ale_models_filter, sets = ale_sets_filter, horizons = ale_horizons_filter)
}

load_prediction_files <- function(index) {
  missing_files <- index %>% filter(!file.exists(File_Path))
  if (nrow(missing_files) > 0L) {
    stop("Some Part 5 prediction files are missing. First missing file: ", missing_files$File_Path[[1]], call. = FALSE)
  }
  purrr::map_dfr(index$File_Path, readRDS)
}

sample_explain_rows <- function(data, max_rows, seed) {
  if (nrow(data) <= max_rows) return(data)
  set.seed(seed)
  pos <- data %>% filter(Outcome_Num == 1L)
  neg <- data %>% filter(Outcome_Num != 1L)
  n_pos <- min(nrow(pos), ceiling(max_rows * 0.5))
  n_neg <- max_rows - n_pos
  bind_rows(
    if (n_pos > 0L) slice_sample(pos, n = n_pos) else pos,
    if (n_neg > 0L && nrow(neg) > 0L) slice_sample(neg, n = min(nrow(neg), n_neg)) else neg[0, ]
  ) %>%
    slice_sample(n = min(nrow(.), max_rows))
}

ale_continuous <- function(model_object, data, feature, grid_size) {
  x <- as.numeric(data[[feature]])
  ok <- is.finite(x)
  if (sum(ok) < 20L || length(unique(x[ok])) < 4L) return(tibble())
  probs <- seq(0, 1, length.out = grid_size + 1L)
  breaks <- unique(as.numeric(quantile(x[ok], probs, na.rm = TRUE, names = FALSE)))
  if (length(breaks) < 3L) return(tibble())
  rows <- purrr::map_dfr(seq_len(length(breaks) - 1L), function(k) {
    lower <- breaks[[k]]
    upper <- breaks[[k + 1L]]
    idx <- which(x >= lower & x <= upper)
    if (length(idx) == 0L) {
      return(tibble(Bin = k, N_Bin = 0L, Local_Effect = NA_real_, Feature_Value = mean(c(lower, upper))))
    }
    lower_data <- data[idx, , drop = FALSE]
    upper_data <- data[idx, , drop = FALSE]
    lower_data[[feature]] <- lower
    upper_data[[feature]] <- upper
    tibble(
      Bin = k,
      N_Bin = length(idx),
      Local_Effect = mean(predict_v5_model(model_object, upper_data) - predict_v5_model(model_object, lower_data), na.rm = TRUE),
      Feature_Value = mean(c(lower, upper))
    )
  })
  rows %>%
    mutate(
      Accumulated_Effect = cumsum(replace_na(Local_Effect, 0)),
      ALE_Effect = Accumulated_Effect - weighted.mean(Accumulated_Effect, w = pmax(N_Bin, 1), na.rm = TRUE),
      ALE_Type = "continuous"
    )
}

ale_discrete <- function(model_object, data, feature) {
  values <- sort(unique(as.numeric(data[[feature]])))
  values <- values[is.finite(values)]
  if (length(values) < 2L) return(tibble())
  base_pred <- mean(predict_v5_model(model_object, data), na.rm = TRUE)
  purrr::map_dfr(values, function(v) {
    newdata <- data
    newdata[[feature]] <- v
    tibble(
      Bin = NA_integer_,
      N_Bin = sum(as.numeric(data[[feature]]) == v, na.rm = TRUE),
      Local_Effect = NA_real_,
      Feature_Value = v,
      Accumulated_Effect = NA_real_,
      ALE_Effect = mean(predict_v5_model(model_object, newdata), na.rm = TRUE) - base_pred,
      ALE_Type = "discrete_marginal_contrast"
    )
  })
}

default_ale_features <- c(
  "rating_rank", "rating_group_B", "rating_group_C",
  "wc_ta", "re_ta", "ebit_ta", "td_ta", "s_ta", "mc_ta", "mc_td",
  "td_market_value", "fin_lev", "ROA_pc", "current_ratio", "ebitda_tie",
  "gdp_real_yoy", "industrial_production_growth_yoy", "hicp_inflation_yoy_q",
  "d_unemployment_yoy_pp", "d_long_term_gov_yield_yoy_pp", "d_public_debt_gdp_yoy_pp",
  "d_gdp_real_yoy_qoq_pp", "industrial_production_growth_qoq", "d_unemployment_qoq_pp",
  "d_hicp_inflation_qoq_pp", "d_long_term_gov_yield_qoq_pp", "d_public_debt_gdp_qoq_pp",
  "ciss_country_or_euro_q_mean", "d_ciss_country_or_euro_qoq",
  "vstoxx_q_mean", "d_vstoxx_qoq", "n_high_vstoxx_last_4q"
)

ale_for_model <- function(model_row, locked_panel) {
  model_object <- readRDS(model_row$Model_Object_Path)
  features <- intersect(default_ale_features, intersect(model_object$predictors, names(locked_panel)))
  if (length(features) == 0L) return(tibble())
  task_data <- locked_panel %>%
    filter(Horizon == model_row$Horizon) %>%
    sample_explain_rows(
      PART7_MAX_EXPLAIN_ROWS,
      stable_seed("part7_ale_rows", model_row$Predictor_Set, model_row$Horizon_Label, model_row$Model, model_row$Candidate_ID)
    )
  if (nrow(task_data) == 0L) return(tibble())
  purrr::map_dfr(features, function(feature) {
    x <- as.numeric(task_data[[feature]])
    out <- if (length(unique(x[is.finite(x)])) <= 12L) {
      ale_discrete(model_object, task_data, feature)
    } else {
      ale_continuous(model_object, task_data, feature, PART7_ALE_GRID)
    }
    if (nrow(out) == 0L) return(tibble())
    out %>%
      mutate(
        Predictor_Set = model_row$Predictor_Set,
        Horizon = model_row$Horizon,
        Horizon_Label = model_row$Horizon_Label,
        Model = model_row$Model,
        Candidate_ID = model_row$Candidate_ID,
        Feature = feature,
        N_Explained_Rows = nrow(task_data),
        N_Downgrades_Explained = sum(task_data$Outcome_Num == 1L, na.rm = TRUE),
        .before = 1L
      )
  })
}

summarise_ale_importance <- function(ale_results) {
  if (nrow(ale_results) == 0L) return(tibble())

  weighted_sd <- function(x, w) {
    ok <- is.finite(x) & is.finite(w) & w > 0
    if (sum(ok) == 0L) return(NA_real_)
    x <- x[ok]
    w <- w[ok]
    total_w <- sum(w)
    if (total_w <= 1) return(0)
    mu <- sum(w * x) / total_w
    sqrt(sum(w * (x - mu)^2) / (total_w - 1))
  }

  ale_results %>%
    group_by(Predictor_Set, Horizon, Horizon_Label, Model, Candidate_ID, Feature) %>%
    summarise(
      N_ALE_Rows = n(),
      Total_Bin_Support = sum(N_Bin, na.rm = TRUE),
      ALE_Range = max(ALE_Effect, na.rm = TRUE) - min(ALE_Effect, na.rm = TRUE),
      ALE_SD = weighted_sd(ALE_Effect, pmax(N_Bin, 1)),
      ALE_Mean_Abs = weighted.mean(abs(ALE_Effect), w = pmax(N_Bin, 1), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(Predictor_Set, Horizon, Horizon_Label, Model, Candidate_ID) %>%
    mutate(
      ALE_SD_Total = sum(ALE_SD, na.rm = TRUE),
      ALE_Global_Share = if_else(ALE_SD_Total > 0, ALE_SD / ALE_SD_Total, NA_real_),
      ALE_Rank = dense_rank(desc(ALE_SD))
    ) %>%
    ungroup() %>%
    select(-ALE_SD_Total) %>%
    arrange(Horizon, Model, Predictor_Set, ALE_Rank)
}

summarise_ale_signal_stability <- function(importance, top_n = 10L) {
  if (nrow(importance) == 0L) return(tibble())
  total_tasks <- importance %>%
    distinct(Model, Horizon, Horizon_Label, Predictor_Set, Candidate_ID) %>%
    nrow()
  importance %>%
    mutate(
      Is_Top_5 = ALE_Rank <= 5L,
      Is_Top_N = ALE_Rank <= top_n
    ) %>%
    group_by(Feature) %>%
    summarise(
      Tasks_Evaluated = n(),
      Models_Evaluated = n_distinct(Model),
      Horizons_Evaluated = n_distinct(Horizon_Label),
      Predictor_Sets_Evaluated = n_distinct(Predictor_Set),
      Coverage_Share = Tasks_Evaluated / total_tasks,
      Top_5_Count = sum(Is_Top_5, na.rm = TRUE),
      Top_5_Conditional_Share = Top_5_Count / Tasks_Evaluated,
      Top_5_Unconditional_Share = Top_5_Count / total_tasks,
      Top_10_Count = sum(Is_Top_N, na.rm = TRUE),
      Top_10_Conditional_Share = Top_10_Count / Tasks_Evaluated,
      Top_10_Unconditional_Share = Top_10_Count / total_tasks,
      Median_Rank = median(ALE_Rank, na.rm = TRUE),
      Mean_ALE_SD = mean(ALE_SD, na.rm = TRUE),
      Median_ALE_SD = median(ALE_SD, na.rm = TRUE),
      Mean_ALE_Global_Share = mean(ALE_Global_Share, na.rm = TRUE),
      Coverage_Adjusted_Mean_ALE_SD = sum(ALE_SD, na.rm = TRUE) / total_tasks,
      Coverage_Adjusted_Mean_ALE_Global_Share = sum(ALE_Global_Share, na.rm = TRUE) / total_tasks,
      .groups = "drop"
    ) %>%
    arrange(desc(Top_10_Unconditional_Share), desc(Top_10_Count), desc(Coverage_Adjusted_Mean_ALE_SD), Median_Rank)
}

summarise_rge_block_stability <- function(rge_results) {
  if (nrow(rge_results) == 0L) return(tibble())
  total_comparisons <- nrow(rge_results)
  total_rge <- sum(rge_results$RGE_Capture_Area, na.rm = TRUE)
  total_abs_probability <- sum(rge_results$Block_Abs_Probability_Mean, na.rm = TRUE)
  rge_results %>%
    group_by(Removed_Block, Removed_Block_Label) %>%
    summarise(
      Comparisons = n(),
      Models_Evaluated = n_distinct(Model),
      Horizons_Evaluated = n_distinct(Horizon_Label),
      Full_Sets_Evaluated = n_distinct(Full_Set),
      Coverage_Share = Comparisons / total_comparisons,
      Total_RGE_Capture_Area = sum(RGE_Capture_Area, na.rm = TRUE),
      Share_Total_RGE_Capture_Area = if_else(total_rge > 0, Total_RGE_Capture_Area / total_rge, NA_real_),
      Mean_RGE_Capture_Area = mean(RGE_Capture_Area, na.rm = TRUE),
      Median_RGE_Capture_Area = median(RGE_Capture_Area, na.rm = TRUE),
      Mean_Rank_MAD_Pct = mean(Rank_MAD_Pct, na.rm = TRUE),
      Total_Block_Abs_Probability_Mean = sum(Block_Abs_Probability_Mean, na.rm = TRUE),
      Share_Total_Block_Abs_Probability_Mean = if_else(
        total_abs_probability > 0,
        Total_Block_Abs_Probability_Mean / total_abs_probability,
        NA_real_
      ),
      Mean_Block_Abs_Probability_Mean = mean(Block_Abs_Probability_Mean, na.rm = TRUE),
      Median_Block_Abs_Probability_Mean = median(Block_Abs_Probability_Mean, na.rm = TRUE),
      Mean_Block_Net_Direction = mean(Block_Net_Direction, na.rm = TRUE),
      Share_Positive_Direction = mean(Block_Net_Direction > 0, na.rm = TRUE),
      Share_Negative_Direction = mean(Block_Net_Direction < 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(Share_Total_RGE_Capture_Area), desc(Share_Total_Block_Abs_Probability_Mean))
}

feature_blocks <- tribble(
  ~Block_ID, ~Block_Label, ~Variables,
  "financial", "Financial ratios", "wc_ta, re_ta, ebit_ta, td_ta, s_ta, mc_ta, mc_td, td_market_value, fin_lev, ROA_pc, current_ratio, ebitda_tie",
  "current_rating_rank", "Current rating rank", "rating_rank",
  "current_rating_group", "Current rating group", "rating_group_B, rating_group_C",
  "macro_pars_stat", "Parsimonious/static macro", "gdp_real_yoy, industrial_production_growth_yoy, hicp_inflation_yoy_q, d_unemployment_yoy_pp, d_long_term_gov_yield_yoy_pp, d_public_debt_gdp_yoy_pp",
  "macro_delta", "Macro deltas", "d_gdp_real_yoy_qoq_pp, industrial_production_growth_qoq, d_unemployment_qoq_pp, d_hicp_inflation_qoq_pp, d_long_term_gov_yield_qoq_pp, d_public_debt_gdp_qoq_pp",
  "ciss", "CISS systemic stress", "ciss_country_or_euro_q_mean, d_ciss_country_or_euro_qoq",
  "vstoxx", "VSTOXX systemic stress", "vstoxx_q_mean, d_vstoxx_qoq, n_high_vstoxx_last_4q"
)

remove_block_from_set <- function(set_name, block) {
  out <- set_name
  if (block == "current_rating_rank") out <- sub("_rating_rank$", "", out)
  if (block == "current_rating_group") out <- sub("_rating_group$", "", out)
  if (block == "ciss") out <- gsub("_ciss", "", out, fixed = TRUE)
  if (block == "vstoxx") out <- gsub("_vstoxx", "", out, fixed = TRUE)
  if (block == "macro_pars_stat") out <- gsub("_macro_pars_stat", "", out, fixed = TRUE)
  if (block == "macro_delta") out <- gsub("_macro_delta", "", out, fixed = TRUE)
  out <- gsub("financial_macro_pars_stat", "financial", out, fixed = TRUE)
  out <- gsub("financial_macro_delta", "financial", out, fixed = TRUE)
  out <- gsub("__+", "_", out)
  out <- gsub("_$", "", out)
  if (out == "financial") out <- "financial_only"
  out
}

build_rge_pairs <- function(full_sets, available_sets) {
  candidate_blocks <- c("current_rating_rank", "current_rating_group", "macro_pars_stat", "macro_delta", "ciss", "vstoxx")
  expand_grid(Full_Set = full_sets, Removed_Block = candidate_blocks) %>%
    mutate(
      Has_Block = case_when(
        Removed_Block == "current_rating_rank" ~ grepl("_rating_rank$", Full_Set),
        Removed_Block == "current_rating_group" ~ grepl("_rating_group$", Full_Set),
        Removed_Block == "macro_pars_stat" ~ grepl("macro_pars_stat", Full_Set, fixed = TRUE),
        Removed_Block == "macro_delta" ~ grepl("macro_delta", Full_Set, fixed = TRUE),
        Removed_Block == "ciss" ~ grepl("ciss", Full_Set, fixed = TRUE),
        Removed_Block == "vstoxx" ~ grepl("vstoxx", Full_Set, fixed = TRUE),
        TRUE ~ FALSE
      ),
      Reduced_Set = map2_chr(Full_Set, Removed_Block, remove_block_from_set)
    ) %>%
    filter(Has_Block, Reduced_Set %in% available_sets, Reduced_Set != Full_Set) %>%
    distinct(Full_Set, Reduced_Set, Removed_Block) %>%
    left_join(feature_blocks %>% select(Removed_Block = Block_ID, Removed_Block_Label = Block_Label), by = "Removed_Block") %>%
    arrange(Full_Set, Removed_Block)
}

build_rge_workplan <- function(prediction_index, scope) {
  available_index <- apply_common_filters(prediction_index)
  if (nrow(available_index) == 0L) {
    return(list(index = available_index, pair_tasks = tibble()))
  }

  if (scope == "all") {
    full_task_index <- available_index
  } else {
    full_task_index <- filter_by_scope(prediction_index, scope)
  }

  available_sets <- sort(unique(available_index$Predictor_Set))
  pairs <- build_rge_pairs(sort(unique(full_task_index$Predictor_Set)), available_sets)
  if (nrow(pairs) == 0L) {
    return(list(index = available_index[0, ], pair_tasks = tibble()))
  }

  available_keys <- available_index %>%
    distinct(Model, Horizon, Horizon_Label, Predictor_Set)

  pair_tasks <- full_task_index %>%
    distinct(Model, Horizon, Horizon_Label, Full_Set = Predictor_Set) %>%
    inner_join(pairs, by = "Full_Set", relationship = "many-to-many") %>%
    inner_join(
      available_keys %>% rename(Reduced_Set = Predictor_Set),
      by = c("Model", "Horizon", "Horizon_Label", "Reduced_Set")
    ) %>%
    arrange(Model, Horizon, Full_Set, Removed_Block)

  if (nrow(pair_tasks) == 0L) {
    return(list(index = available_index[0, ], pair_tasks = pair_tasks))
  }

  needed_keys <- bind_rows(
    pair_tasks %>% transmute(Model, Horizon, Horizon_Label, Predictor_Set = Full_Set),
    pair_tasks %>% transmute(Model, Horizon, Horizon_Label, Predictor_Set = Reduced_Set)
  ) %>%
    distinct()

  selected_index <- available_index %>%
    semi_join(needed_keys, by = c("Model", "Horizon", "Horizon_Label", "Predictor_Set")) %>%
    arrange(Model, Horizon, Predictor_Set)

  list(index = selected_index, pair_tasks = pair_tasks)
}

capture_curve <- function(probability, outcome) {
  ord <- order(probability, decreasing = TRUE, na.last = NA)
  y <- as.integer(outcome[ord])
  if (length(y) == 0L || sum(y == 1L) == 0L) {
    return(tibble(Share_Inspected = c(0, 1), Capture_Rate = c(0, 0)))
  }
  tibble(
    Share_Inspected = seq_along(y) / length(y),
    Capture_Rate = cumsum(y == 1L) / sum(y == 1L)
  ) %>%
    bind_rows(tibble(Share_Inspected = 0, Capture_Rate = 0), .) %>%
    arrange(Share_Inspected)
}

area_between_curves <- function(full_curve, reduced_curve) {
  grid <- sort(unique(c(full_curve$Share_Inspected, reduced_curve$Share_Inspected)))
  full_y <- approx(full_curve$Share_Inspected, full_curve$Capture_Rate, xout = grid, rule = 2)$y
  reduced_y <- approx(reduced_curve$Share_Inspected, reduced_curve$Capture_Rate, xout = grid, rule = 2)$y
  sum(diff(grid) * (head(abs(full_y - reduced_y), -1L) + tail(abs(full_y - reduced_y), -1L)) / 2)
}

run_rge <- function(predictions, pair_tasks) {
  if (nrow(pair_tasks) == 0L) return(tibble())

  out <- purrr::pmap_dfr(pair_tasks, function(Full_Set, Reduced_Set, Removed_Block, Removed_Block_Label, Model, Horizon, Horizon_Label, ...) {
    task_model <- Model
    task_horizon <- Horizon
    task_horizon_label <- Horizon_Label
    full_pred <- predictions %>%
      filter(.data$Predictor_Set == Full_Set, .data$Model == task_model, .data$Horizon == task_horizon)
    reduced_pred <- predictions %>%
      filter(.data$Predictor_Set == Reduced_Set, .data$Model == task_model, .data$Horizon == task_horizon)
    joined <- full_pred %>%
      select(Observation_ID, Outcome_Num, Full_Probability = Probability) %>%
      inner_join(
        reduced_pred %>% select(Observation_ID, Reduced_Probability = Probability),
        by = "Observation_ID"
      )
    if (nrow(joined) == 0L) return(tibble())
    rank_full <- rank(-joined$Full_Probability, ties.method = "average")
    rank_reduced <- rank(-joined$Reduced_Probability, ties.method = "average")
    full_curve <- capture_curve(joined$Full_Probability, joined$Outcome_Num)
    reduced_curve <- capture_curve(joined$Reduced_Probability, joined$Outcome_Num)
    block_delta <- joined$Full_Probability - joined$Reduced_Probability
    block_abs_delta_sum <- sum(abs(block_delta), na.rm = TRUE)
    block_signed_delta_sum <- sum(block_delta, na.rm = TRUE)
    tibble(
      Model = task_model,
      Horizon = task_horizon,
      Horizon_Label = task_horizon_label,
      Full_Set = Full_Set,
      Reduced_Set = Reduced_Set,
      Removed_Block = Removed_Block,
      Removed_Block_Label = Removed_Block_Label,
      N = nrow(joined),
      N_Downgrades = sum(joined$Outcome_Num == 1L, na.rm = TRUE),
      Probability_MAE = mean(abs(joined$Full_Probability - joined$Reduced_Probability), na.rm = TRUE),
      Block_Abs_Probability_Sum = block_abs_delta_sum,
      Block_Abs_Probability_Mean = mean(abs(block_delta), na.rm = TRUE),
      Block_Signed_Probability_Sum = block_signed_delta_sum,
      Block_Signed_Probability_Mean = mean(block_delta, na.rm = TRUE),
      Block_Net_Direction = if_else(block_abs_delta_sum > 0, block_signed_delta_sum / block_abs_delta_sum, NA_real_),
      Probability_Correlation = suppressWarnings(cor(joined$Full_Probability, joined$Reduced_Probability, use = "pairwise.complete.obs")),
      Rank_MAD_Pct = 100 * mean(abs(rank_full - rank_reduced), na.rm = TRUE) / max(nrow(joined) - 1L, 1L),
      Spearman_Rank_Correlation = suppressWarnings(cor(joined$Full_Probability, joined$Reduced_Probability, method = "spearman", use = "pairwise.complete.obs")),
      RGE_Capture_Area = area_between_curves(full_curve, reduced_curve)
    )
  })

  out %>%
    group_by(Model, Horizon, Horizon_Label, Full_Set) %>%
    mutate(
      Block_Abs_Probability_Total = sum(Block_Abs_Probability_Sum, na.rm = TRUE),
      Block_Abs_Probability_Share = if_else(Block_Abs_Probability_Total > 0, Block_Abs_Probability_Sum / Block_Abs_Probability_Total, NA_real_)
    ) %>%
    ungroup() %>%
    select(-Block_Abs_Probability_Total) %>%
    relocate(Block_Abs_Probability_Share, .after = Block_Abs_Probability_Mean) %>%
    arrange(Model, Horizon, Full_Set, Removed_Block)
}

plot_ale <- function(ale_results) {
  plot_data <- ale_results %>%
    filter(is.finite(ALE_Effect), n_distinct(Feature_Value) > 1L) %>%
    group_by(Model, Horizon_Label, Feature) %>%
    mutate(Feature_Max_Abs = max(abs(ALE_Effect), na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(Model, Horizon_Label) %>%
    filter(dense_rank(desc(Feature_Max_Abs)) <= 8L) %>%
    ungroup()
  if (nrow(plot_data) == 0L) return(invisible(NULL))
  p <- ggplot(plot_data, aes(x = Feature_Value, y = ALE_Effect, color = Horizon_Label)) +
    geom_line(linewidth = 0.45) +
    geom_point(size = 0.7) +
    facet_grid(Model ~ Feature, scales = "free_x") +
    labs(x = NULL, y = "ALE effect on predicted downgrade probability", color = NULL, title = "ALE profiles for selected locked-test models") +
    theme_minimal(base_size = 8) +
    theme(legend.position = "bottom")
  save_plot(p, "fig_01_ale_profiles_selected_features", width = 13, height = 9)
}

plot_ale_importance <- function(importance) {
  plot_data <- importance %>%
    group_by(Model, Horizon_Label) %>%
    slice_min(ALE_Rank, n = 10, with_ties = FALSE) %>%
    ungroup()
  if (nrow(plot_data) == 0L) return(invisible(NULL))
  p <- ggplot(plot_data, aes(x = Horizon_Label, y = reorder(Feature, ALE_SD), fill = ALE_SD)) +
    geom_tile() +
    facet_wrap(~ Model, scales = "free_y") +
    scale_fill_gradient(low = "#f2f2f2", high = "#2f6f73") +
    labs(x = NULL, y = NULL, fill = "ALE SD", title = "ALE-derived global importance") +
    theme_minimal(base_size = 9) +
    theme(legend.position = "bottom")
  save_plot(p, "fig_02_ale_global_importance_heatmap", width = 11, height = 8)
}

plot_ale_signal_stability <- function(stability) {
  if (nrow(stability) == 0L) return(invisible(NULL))
  plot_data <- stability %>%
    slice_max(Top_10_Count, n = 20, with_ties = FALSE) %>%
    mutate(Feature = forcats::fct_reorder(Feature, Coverage_Adjusted_Mean_ALE_SD))
  if (nrow(plot_data) == 0L) return(invisible(NULL))
  p <- ggplot(plot_data, aes(x = Coverage_Adjusted_Mean_ALE_SD, y = Feature, fill = Top_10_Unconditional_Share)) +
    geom_col(width = 0.72) +
    scale_fill_gradient(low = "#d8e2dc", high = "#2f6f73", labels = scales::percent_format(accuracy = 1)) +
    labs(x = "Coverage-adjusted mean ALE standard deviation", y = NULL, fill = "Unconditional top-10 share", title = "Stable ALE-based predictor relevance") +
    theme_minimal(base_size = 9) +
    theme(legend.position = "bottom")
  save_plot(p, "fig_04_ale_signal_stability_summary", width = 9, height = 7)
}

plot_rge <- function(rge_results) {
  if (nrow(rge_results) == 0L) return(invisible(NULL))
  p <- ggplot(rge_results, aes(x = Horizon_Label, y = Removed_Block_Label, fill = RGE_Capture_Area)) +
    geom_tile() +
    facet_wrap(~ Model) +
    scale_fill_gradient(low = "#f2f2f2", high = "#8c4f2b", na.value = "grey85") +
    labs(x = NULL, y = NULL, fill = "RGE area", title = "Block-level RGE ranking impact") +
    theme_minimal(base_size = 9) +
    theme(legend.position = "bottom")
  save_plot(p, "fig_03_rge_block_heatmap", width = 11, height = 7)
}

plot_rge_block_stability <- function(stability) {
  if (nrow(stability) == 0L) return(invisible(NULL))
  plot_data <- stability %>%
    mutate(Removed_Block_Label = forcats::fct_reorder(Removed_Block_Label, Share_Total_RGE_Capture_Area))
  if (nrow(plot_data) == 0L) return(invisible(NULL))
  p <- ggplot(plot_data, aes(x = Share_Total_RGE_Capture_Area, y = Removed_Block_Label, fill = Mean_Block_Net_Direction)) +
    geom_col(width = 0.72) +
    scale_fill_gradient2(low = "#3d5a80", mid = "#f2f2f2", high = "#9a3412", midpoint = 0, limits = c(-1, 1), na.value = "grey85") +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(x = "Share of total RGE capture area", y = NULL, fill = "Net direction", title = "Block-level RGE stability") +
    theme_minimal(base_size = 9) +
    theme(legend.position = "bottom")
  save_plot(p, "fig_05_rge_block_stability_summary", width = 9, height = 5)
}

run_part7_explainability <- function() {
  if (!file.exists(LOCKED_TEST_RDS)) stop("Missing locked-test panel: ", LOCKED_TEST_RDS, call. = FALSE)
  if (!file.exists(REGRESSOR_MANIFEST_CSV)) stop("Missing regressor manifest: ", REGRESSOR_MANIFEST_CSV, call. = FALSE)
  loaded <- load_part5_inputs()

  write_audit(feature_blocks, "table_00_rge_predictor_blocks", "Pre-specified predictor blocks for ALE/RGE interpretation")

  ale_tasks <- filter_ale_tasks(loaded$model_index) %>%
    arrange(Model, Predictor_Set, Horizon) %>%
    mutate(ALE_Task_ID = row_number(), .before = 1L)
  write_audit(
    ale_tasks %>% select(ALE_Task_ID, Model, Predictor_Set, Horizon, Horizon_Label, Candidate_ID, Model_Object_Path, Fit_Status),
    "table_01_ale_task_manifest",
    "Model/specification/horizon tasks selected for ALE"
  )

  rge_workplan <- build_rge_workplan(loaded$prediction_index, PART7_RGE_SCOPE)
  rge_index <- rge_workplan$index
  rge_pair_manifest <- rge_workplan$pair_tasks
  write_audit(
    rge_pair_manifest,
    "table_02_rge_pair_manifest",
    "Full/reduced predictor-set tasks selected for block-level RGE"
  )

  manifest <- tibble(
    Setting = c(
      "part_5_output_dir", "part_5_model_index", "part_5_prediction_index",
      "locked_test_panel", "part_7_spec_block", "part_7_selection_rule",
      "ale_scope", "rge_scope", "ale_tasks_selected", "rge_prediction_files_selected",
      "rge_full_reduced_tasks", "ale_grid", "max_explain_rows", "figure_mode", "shap_status", "tree_importance_status"
    ),
    Value = c(
      PART5_OUTPUT_DIR, loaded$model_index_file, loaded$prediction_index_file,
      LOCKED_TEST_RDS, PART7_SPEC_BLOCK, PART7_SELECTION_RULE,
      PART7_ALE_SCOPE, PART7_RGE_SCOPE, as.character(nrow(ale_tasks)), as.character(nrow(rge_index)),
      as.character(nrow(rge_pair_manifest)), as.character(PART7_ALE_GRID), as.character(PART7_MAX_EXPLAIN_ROWS), PART7_FIGURE_MODE,
      "Excluded from the main V5 explainability framework",
      "RF impurity importance, XGBoost gain, and permutation importance excluded from the main V5 explainability framework"
    )
  )
  write_audit(manifest, "table_03_part_7_manifest", "Part 7 V5 ALE/RGE manifest")

  locked_panel <- readRDS(LOCKED_TEST_RDS) %>% prepare_model_data()

  ale_results <- tibble()
  if (PART7_RUN_ALE && nrow(ale_tasks) > 0L) {
    message("Part 7A: ALE for ", nrow(ale_tasks), " model/specification/horizon task(s).")
    ale_results <- purrr::pmap_dfr(
      ale_tasks,
      function(ALE_Task_ID, Predictor_Set, Horizon, Horizon_Label, Model, Candidate_ID, Model_Object_Path, Fit_Status, ...) {
        message("[ALE ", ALE_Task_ID, "/", nrow(ale_tasks), "] ", Model, " | ", Predictor_Set, " | ", Horizon_Label)
        ale_for_model(
          tibble(
            Predictor_Set = Predictor_Set,
            Horizon = Horizon,
            Horizon_Label = Horizon_Label,
            Model = Model,
            Candidate_ID = Candidate_ID,
            Model_Object_Path = Model_Object_Path
          ),
          locked_panel
        )
      }
    )
    write_audit(ale_results, "table_04_ale_profiles", "ALE and marginal-contrast profiles")
    save_data(ale_results, "Panel_Europe_macro_part_7_ale_profiles")
    ale_importance <- summarise_ale_importance(ale_results)
    write_audit(ale_importance, "table_05_ale_global_importance", "ALE-derived global importance")
    save_data(ale_importance, "Panel_Europe_macro_part_7_ale_global_importance")
    ale_signal_stability <- summarise_ale_signal_stability(ale_importance)
    write_audit(ale_signal_stability, "table_06_ale_signal_stability", "Cross-model and cross-horizon stability of ALE-based predictor relevance")
    save_data(ale_signal_stability, "Panel_Europe_macro_part_7_ale_signal_stability")
    if (PART7_FIGURE_MODE %in% c("selected", "all")) {
      plot_ale(ale_results)
      plot_ale_importance(ale_importance)
    }
    if (PART7_FIGURE_MODE %in% c("summary", "all")) {
      plot_ale_signal_stability(ale_signal_stability)
    }
  } else {
    ale_importance <- tibble()
    ale_signal_stability <- tibble()
  }

  rge_results <- tibble()
  if (PART7_RUN_RGE && nrow(rge_index) > 0L) {
    message("Part 7B: RGE using ", nrow(rge_index), " partitioned prediction file(s) and ", nrow(rge_pair_manifest), " full/reduced task(s).")
    rge_predictions <- load_prediction_files(rge_index)
    rge_results <- run_rge(rge_predictions, rge_pair_manifest)
    write_audit(rge_results, "table_07_rge_block_importance", "Block-level RGE ranking impact and full-minus-reduced block probability contribution")
    save_data(rge_results, "Panel_Europe_macro_part_7_rge_block_importance")
    rge_block_stability <- summarise_rge_block_stability(rge_results)
    write_audit(rge_block_stability, "table_08_rge_block_stability", "Cross-model and cross-horizon stability of block-level RGE and probability contribution")
    save_data(rge_block_stability, "Panel_Europe_macro_part_7_rge_block_stability")
    if (PART7_FIGURE_MODE %in% c("selected", "all")) {
      plot_rge(rge_results)
    }
    if (PART7_FIGURE_MODE %in% c("summary", "all")) {
      plot_rge_block_stability(rge_block_stability)
    }
  } else {
    rge_block_stability <- tibble()
  }

  notes <- c(
    "Part 7 V5 ALE/RGE explainability complete.",
    "",
    paste0("Part 5 output directory: ", PART5_OUTPUT_DIR),
    paste0("Selection rule: ", PART7_SELECTION_RULE),
    paste0("ALE scope: ", PART7_ALE_SCOPE),
    paste0("RGE scope: ", PART7_RGE_SCOPE),
    paste0("Figure mode: ", PART7_FIGURE_MODE),
    paste0("RGE full/reduced tasks: ", nrow(rge_pair_manifest)),
    "SHAP is not used in the main V5 explainability framework.",
    "RF impurity importance, XGBoost gain, and permutation importance are not used as central interpretation tools.",
    "ALE is used for predictor-level functional effects on the locked-test sample.",
    "ALE-derived global importance ranks predictors by the weighted standard deviation of their ALE profiles over the empirical bin support.",
    "ALE signal stability reports both conditional shares among tasks where a predictor is available and unconditional coverage-adjusted shares across all evaluated tasks.",
    "Block-level RGE stability reports both mean block effects and total-share summaries that account for how often each block comparison is available.",
    "By default, ALE and RGE cover all validation-selected model, predictor-set, and horizon combinations; environment filters can be used for focused robustness runs.",
    "RGE compares each full specification with its available reduced predictor-set counterparts to assess the joint relevance of economically related predictor blocks under correlated predictors.",
    "RGE is computed from Part 5 locked-test predictions; no model refit is performed in Part 7.",
    "The block contribution extension is not SHAP-based: Delta_ig is defined as full-model predicted downgrade probability minus the corresponding reduced-model probability.",
    "Block_Abs_Probability_Share normalizes sum_i |Delta_ig| across the removed blocks available for the same full model, horizon, and predictor set.",
    "Block_Net_Direction equals sum_i Delta_ig divided by sum_i |Delta_ig|, so values near zero indicate offsetting positive and negative block-level probability movements."
  )
  notes_path <- file.path(OUTPUT_DIR, "part_7_ale_rge_explainability_notes.txt")
  assert_new_file(notes_path)
  writeLines(notes, notes_path)

  invisible(list(
    manifest = manifest,
    ale = ale_results,
    ale_importance = ale_importance,
    ale_signal_stability = ale_signal_stability,
    rge = rge_results,
    rge_block_stability = rge_block_stability
  ))
}

running_as_script <- function() {
  sys_frames <- sys.frames()
  any(vapply(sys_frames, function(frame) !is.null(frame$ofile), logical(1))) ||
    any(grepl("--file=", commandArgs(trailingOnly = FALSE), fixed = TRUE))
}

if (running_as_script()) {
  message("Part 7 V5 ALE/RGE explainability starting.")
  message("Part 7 V5 selection rule: ", PART7_SELECTION_RULE)
  run_part7_explainability()
  message("Part 7 V5 complete. Outputs written to: ", OUTPUT_DIR)
}

