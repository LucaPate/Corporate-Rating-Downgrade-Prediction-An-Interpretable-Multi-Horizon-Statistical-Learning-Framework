# Step 07 - Assess locked-test metric stability with issuer and time resampling.
# GitHub-ready copy: paths are repository-relative and data files are intentionally excluded.

# Uses Part 5 locked-test predictions to assess metric stability under
# issuer-cluster and quarter-year block resampling. This is intentionally not
# framed as full predictive uncertainty quantification.

set.seed(20260724)

required_packages <- c("tidyverse", "pROC", "PRROC", "knitr", "scales", "digest")
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
PART6_SPEC_BLOCK <- tolower(trimws(Sys.getenv("PART6_SPEC_BLOCK", Sys.getenv("PART4_SPEC_BLOCK", "all"))))
PART6_SELECTION_RULE <- tolower(trimws(Sys.getenv("PART6_SELECTION_RULE", Sys.getenv("PART5_SELECTION_RULE", "near_best_calibrated"))))
PART6_SCOPE <- tolower(trimws(Sys.getenv("PART6_SCOPE", "best_by_horizon_pr_auc")))
PART6_N_BOOT <- as.integer(Sys.getenv("PART6_N_BOOT", "500"))
PART6_N_BOOT <- if_else(is.na(PART6_N_BOOT) | PART6_N_BOOT < 1L, 500L, PART6_N_BOOT)
PART6_RUN_ISSUER_BOOT <- tolower(Sys.getenv("PART6_RUN_ISSUER_BOOT", "TRUE")) %in% c("true", "t", "1", "yes")
PART6_RUN_TIME_BOOT <- tolower(Sys.getenv("PART6_RUN_TIME_BOOT", "TRUE")) %in% c("true", "t", "1", "yes")
PART6_OVERWRITE_OUTPUTS <- tolower(Sys.getenv("PART6_OVERWRITE_OUTPUTS", "FALSE")) %in% c("true", "t", "1", "yes")
PART6_OUTPUT_TAG <- trimws(Sys.getenv("PART6_OUTPUT_TAG", ""))

allowed_selection_rules <- c("max_pr_auc", "near_best_parsimony", "near_best_calibrated")
if (!PART6_SELECTION_RULE %in% allowed_selection_rules) {
  stop("PART6_SELECTION_RULE must be one of: ", paste(allowed_selection_rules, collapse = ", "), call. = FALSE)
}
if (!PART6_SCOPE %in% c("best_by_horizon_pr_auc", "best_by_horizon_mcc", "all")) {
  stop("PART6_SCOPE must be one of: best_by_horizon_pr_auc, best_by_horizon_mcc, all.", call. = FALSE)
}

parse_filter <- function(value) {
  value <- trimws(value)
  if (!nzchar(value)) return(character())
  strsplit(value, ",", fixed = TRUE)[[1]] %>%
    trimws() %>%
    purrr::discard(~ !nzchar(.x))
}

selected_models_filter <- parse_filter(Sys.getenv("PART6_SELECTED_MODELS", ""))
selected_sets_filter <- parse_filter(Sys.getenv("PART6_SELECTED_REGRESSOR_SETS", ""))
selected_horizons_filter <- parse_filter(Sys.getenv("PART6_SELECTED_HORIZONS", ""))

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
PART5_OUTPUT_DIR <- file.path(V5_DIR, paste0("Part_5", part_suffix(PART6_SPEC_BLOCK), "_Outputs", selection_suffix(PART6_SELECTION_RULE)))
OUTPUT_DIR <- file.path(V5_DIR, paste0("Part_6", part_suffix(PART6_SPEC_BLOCK), "_Outputs", selection_suffix(PART6_SELECTION_RULE)))
if (nzchar(clean_file_id(PART6_OUTPUT_TAG))) {
  OUTPUT_DIR <- paste0(OUTPUT_DIR, "_", clean_file_id(PART6_OUTPUT_TAG))
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
  if (file.exists(path) && !PART6_OVERWRITE_OUTPUTS) {
    stop("Refusing to overwrite existing Part 6 V5 output: ", path,
         ". Set PART6_OVERWRITE_OUTPUTS=TRUE deliberately to replace it.", call. = FALSE)
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

save_plot <- function(plot, name, width = 9, height = 5) {
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

prob_metrics <- function(y, p) {
  ok <- is.finite(y) & is.finite(p)
  y <- as.integer(y[ok])
  p <- as.numeric(p[ok])
  if (length(y) == 0L || length(unique(y)) < 2L) {
    return(tibble(N = length(y), N_Downgrades = sum(y == 1L), Prevalence_Pct = 100 * mean(y == 1L),
                  PR_AUC = NA_real_, ROC_AUC = NA_real_, Brier = NA_real_, Log_Loss = NA_real_,
                  PR_AUC_Lift = NA_real_))
  }
  eps <- 1e-15
  p_clip <- pmin(pmax(p, eps), 1 - eps)
  roc_auc <- tryCatch(
    as.numeric(pROC::auc(pROC::roc(y, p, quiet = TRUE, levels = c(0, 1), direction = "<"))),
    error = function(e) NA_real_
  )
  pr_auc <- tryCatch(
    PRROC::pr.curve(scores.class0 = p[y == 1L], scores.class1 = p[y == 0L], curve = FALSE)$auc.integral,
    error = function(e) NA_real_
  )
  tibble(
    N = length(y),
    N_Downgrades = sum(y == 1L),
    Prevalence_Pct = 100 * mean(y == 1L),
    PR_AUC = pr_auc,
    ROC_AUC = roc_auc,
    Brier = mean((p - y)^2),
    Log_Loss = -mean(y * log(p_clip) + (1 - y) * log(1 - p_clip)),
    PR_AUC_Lift = pr_auc / mean(y == 1L)
  )
}

threshold_metrics <- function(y, p, threshold) {
  ok <- is.finite(y) & is.finite(p) & is.finite(threshold)
  y <- as.integer(y[ok])
  p <- as.numeric(p[ok])
  if (length(y) == 0L) {
    return(tibble(Threshold = threshold, Sensitivity = NA_real_, Specificity = NA_real_,
                  Precision = NA_real_, F1 = NA_real_, Balanced_Accuracy = NA_real_,
                  MCC = NA_real_, Predicted_Downgrade_Rate_Pct = NA_real_))
  }
  pred <- as.integer(p >= threshold)
  tp <- sum(pred == 1L & y == 1L)
  tn <- sum(pred == 0L & y == 0L)
  fp <- sum(pred == 1L & y == 0L)
  fn <- sum(pred == 0L & y == 1L)
  sensitivity <- if_else(tp + fn > 0, tp / (tp + fn), NA_real_)
  specificity <- if_else(tn + fp > 0, tn / (tn + fp), NA_real_)
  precision <- if_else(tp + fp > 0, tp / (tp + fp), NA_real_)
  f1 <- if_else(is.finite(precision + sensitivity) & precision + sensitivity > 0,
                2 * precision * sensitivity / (precision + sensitivity), NA_real_)
  mcc_den <- sqrt(as.numeric(tp + fp) * as.numeric(tp + fn) * as.numeric(tn + fp) * as.numeric(tn + fn))
  mcc <- if_else(mcc_den > 0, ((tp * tn) - (fp * fn)) / mcc_den, NA_real_)
  tibble(
    Threshold = threshold,
    Sensitivity = sensitivity,
    Specificity = specificity,
    Precision = precision,
    F1 = f1,
    Balanced_Accuracy = mean(c(sensitivity, specificity), na.rm = TRUE),
    MCC = mcc,
    Predicted_Downgrade_Rate_Pct = 100 * mean(pred == 1L)
  )
}

metrics_for_sample <- function(data) {
  probability <- prob_metrics(data$Outcome_Num, data$Probability)
  threshold <- threshold_metrics(data$Outcome_Num, data$Probability, unique(data$Validation_Threshold)[[1]])
  bind_cols(probability, threshold %>% select(-Threshold)) %>%
    mutate(Validation_Threshold = unique(data$Validation_Threshold)[[1]], .before = 1L)
}

load_part5_inputs <- function() {
  if (!dir.exists(PART5_OUTPUT_DIR)) {
    stop("Missing Part 5 output directory: ", PART5_OUTPUT_DIR, call. = FALSE)
  }
  index_file <- latest_matching_file(file.path(PART5_OUTPUT_DIR, "audits"), "^table_15_locked_test_prediction_file_index_.*\\.csv$")
  model_index_file <- latest_matching_file(file.path(PART5_OUTPUT_DIR, "audits"), "^table_11_locked_test_model_object_index_.*\\.csv$")
  if (is.na(index_file)) {
    stop("Missing Part 5 locked-test prediction file index in: ", file.path(PART5_OUTPUT_DIR, "audits"), call. = FALSE)
  }
  prediction_index <- readr::read_csv(index_file, show_col_types = FALSE)
  model_index <- if (!is.na(model_index_file)) readr::read_csv(model_index_file, show_col_types = FALSE) else tibble()
  list(index_file = index_file, model_index_file = model_index_file, prediction_index = prediction_index, model_index = model_index)
}

filter_index <- function(index, model_index) {
  out <- index
  if (length(selected_models_filter) > 0L) out <- out %>% filter(Model %in% selected_models_filter)
  if (length(selected_sets_filter) > 0L) out <- out %>% filter(Predictor_Set %in% selected_sets_filter)
  if (length(selected_horizons_filter) > 0L) {
    horizon_labels <- ifelse(grepl("^t\\+", selected_horizons_filter), selected_horizons_filter, paste0("t+", selected_horizons_filter))
    out <- out %>% filter(Horizon_Label %in% horizon_labels | as.character(Horizon) %in% selected_horizons_filter)
  }
  if (PART6_SCOPE == "all") return(out)

  metric_file <- latest_matching_file(
    file.path(PART5_OUTPUT_DIR, "audits"),
    if (PART6_SCOPE == "best_by_horizon_pr_auc") "^table_07_locked_test_best_by_horizon_pr_auc_.*\\.csv$" else "^table_08_locked_test_best_by_horizon_mcc_.*\\.csv$"
  )
  if (is.na(metric_file)) {
    stop("Missing Part 5 best-by-horizon table for PART6_SCOPE=", PART6_SCOPE, call. = FALSE)
  }
  selected <- readr::read_csv(metric_file, show_col_types = FALSE) %>%
    distinct(Predictor_Set, Model, Horizon, Horizon_Label)
  out %>% inner_join(selected, by = c("Predictor_Set", "Model", "Horizon", "Horizon_Label"))
}

load_predictions <- function(index) {
  missing_files <- index %>% filter(!file.exists(File_Path))
  if (nrow(missing_files) > 0L) {
    stop("Some Part 5 prediction files are missing. First missing file: ", missing_files$File_Path[[1]], call. = FALSE)
  }
  purrr::map_dfr(index$File_Path, readRDS)
}

bootstrap_indices <- function(cluster_values, n_boot, seed) {
  clusters <- unique(cluster_values)
  clusters <- clusters[!is.na(clusters)]
  set.seed(seed)
  replicate(n_boot, sample(clusters, length(clusters), replace = TRUE), simplify = FALSE)
}

stable_seed <- function(...) {
  key <- paste(..., collapse = "|")
  as.integer(strtoi(substr(digest::digest(key, algo = "xxhash32"), 1, 7), base = 16L)) %% .Machine$integer.max
}

bootstrap_one_group <- function(data, cluster_var, n_boot, method_label) {
  draws <- bootstrap_indices(
    data[[cluster_var]],
    n_boot,
    stable_seed("part6", method_label, unique(data$Predictor_Set), unique(data$Horizon_Label), unique(data$Model), unique(data$Candidate_ID))
  )
  purrr::map_dfr(seq_along(draws), function(b) {
    sampled <- purrr::map_dfr(draws[[b]], ~ data[data[[cluster_var]] == .x, , drop = FALSE])
    metrics_for_sample(sampled) %>% mutate(Bootstrap_ID = b, .before = 1L)
  })
}

run_metric_bootstrap <- function(predictions, cluster_var, n_boot, method_label) {
  if (!cluster_var %in% names(predictions)) {
    stop("Cluster variable not found in Part 5 predictions: ", cluster_var, call. = FALSE)
  }
  predictions %>%
    group_by(Predictor_Set, Horizon, Horizon_Label, Model, Candidate_ID) %>%
    group_modify(~ bootstrap_one_group(.x, cluster_var, n_boot, method_label), .keep = TRUE) %>%
    ungroup() %>%
    mutate(Bootstrap_Method = method_label, .before = 1L)
}

summarise_bootstrap <- function(draws, point_estimates) {
  metric_cols <- c("PR_AUC", "ROC_AUC", "Brier", "Log_Loss", "PR_AUC_Lift", "MCC", "F1", "Balanced_Accuracy", "Predicted_Downgrade_Rate_Pct")
  id_cols <- c("Predictor_Set", "Horizon", "Horizon_Label", "Model", "Candidate_ID")
  draws_long <- draws %>%
    select(any_of(c(id_cols, "Bootstrap_Method", "Bootstrap_ID", metric_cols))) %>%
    pivot_longer(all_of(metric_cols), names_to = "Metric", values_to = "Bootstrap_Value")
  point_long <- point_estimates %>%
    select(any_of(c(id_cols, metric_cols))) %>%
    pivot_longer(all_of(metric_cols), names_to = "Metric", values_to = "Point_Estimate")
  draws_long %>%
    group_by(across(all_of(c(id_cols, "Bootstrap_Method", "Metric")))) %>%
    summarise(
      N_Bootstrap = sum(is.finite(Bootstrap_Value)),
      Bootstrap_Mean = mean(Bootstrap_Value, na.rm = TRUE),
      Bootstrap_SD = sd(Bootstrap_Value, na.rm = TRUE),
      CI_2_5 = quantile(Bootstrap_Value, 0.025, na.rm = TRUE, names = FALSE),
      CI_5 = quantile(Bootstrap_Value, 0.050, na.rm = TRUE, names = FALSE),
      CI_50 = quantile(Bootstrap_Value, 0.500, na.rm = TRUE, names = FALSE),
      CI_95 = quantile(Bootstrap_Value, 0.950, na.rm = TRUE, names = FALSE),
      CI_97_5 = quantile(Bootstrap_Value, 0.975, na.rm = TRUE, names = FALSE),
      .groups = "drop"
    ) %>%
    left_join(point_long, by = c(id_cols, "Metric")) %>%
    relocate(Point_Estimate, .after = Metric) %>%
    arrange(Bootstrap_Method, Horizon, Model, Predictor_Set, Metric)
}

summarise_prediction_distribution <- function(predictions) {
  predictions %>%
    group_by(Predictor_Set, Horizon, Horizon_Label, Model, Candidate_ID) %>%
    summarise(
      N = n(),
      N_Downgrades = sum(Outcome_Num == 1L, na.rm = TRUE),
      Mean_Probability = mean(Probability, na.rm = TRUE),
      SD_Probability = sd(Probability, na.rm = TRUE),
      P50_Probability = quantile(Probability, 0.50, na.rm = TRUE, names = FALSE),
      P90_Probability = quantile(Probability, 0.90, na.rm = TRUE, names = FALSE),
      P95_Probability = quantile(Probability, 0.95, na.rm = TRUE, names = FALSE),
      P99_Probability = quantile(Probability, 0.99, na.rm = TRUE, names = FALSE),
      Validation_Threshold = first(Validation_Threshold),
      Share_Above_Validation_Threshold_Pct = 100 * mean(Probability >= Validation_Threshold, na.rm = TRUE),
      .groups = "drop"
    )
}

decile_calibration <- function(predictions) {
  predictions %>%
    group_by(Predictor_Set, Horizon, Horizon_Label, Model, Candidate_ID) %>%
    arrange(Probability, .by_group = TRUE) %>%
    mutate(Calibration_Bin = ntile(Probability, min(10L, n()))) %>%
    group_by(Predictor_Set, Horizon, Horizon_Label, Model, Candidate_ID, Calibration_Bin) %>%
    summarise(
      Bin_N = n(),
      Mean_Probability = mean(Probability, na.rm = TRUE),
      Observed_Downgrade_Rate = mean(Outcome_Num == 1L, na.rm = TRUE),
      Observed_Downgrade_Rate_Pct = 100 * Observed_Downgrade_Rate,
      .groups = "drop"
    )
}

plot_metric_stability <- function(summary, metric, name, title) {
  plot_data <- summary %>%
    filter(Metric == metric, is.finite(Point_Estimate), is.finite(CI_5), is.finite(CI_95)) %>%
    mutate(Model_Spec = paste(Model, Predictor_Set, sep = " | "))
  if (nrow(plot_data) == 0L) return(invisible(NULL))
  p <- ggplot(plot_data, aes(x = Horizon_Label, y = Point_Estimate, color = Bootstrap_Method)) +
    geom_pointrange(aes(ymin = CI_5, ymax = CI_95), position = position_dodge(width = 0.45)) +
    facet_wrap(~ Model_Spec, scales = "free_y") +
    labs(x = NULL, y = metric, color = NULL, title = title) +
    theme_minimal(base_size = 9) +
    theme(legend.position = "bottom")
  save_plot(p, name, width = 11, height = 6)
}

run_part6_stability <- function() {
  loaded <- load_part5_inputs()
  selected_index <- filter_index(loaded$prediction_index, loaded$model_index)
  if (nrow(selected_index) == 0L) {
    stop("No Part 5 prediction files selected for Part 6.", call. = FALSE)
  }

  predictions <- load_predictions(selected_index) %>%
    mutate(
      Predictor_Date = as.Date(Predictor_Date),
      Target_Date = as.Date(Target_Date),
      Predictor_Quarter = sprintf("%04d Q%d", lubridate::year(Predictor_Date), lubridate::quarter(Predictor_Date))
    )

  manifest <- tibble(
    Setting = c(
      "part_5_output_dir", "part_5_prediction_index", "part_6_spec_block",
      "part_6_selection_rule", "part_6_scope", "bootstrap_replications",
      "prediction_rows", "prediction_files_selected", "issuer_clusters",
      "quarter_year_blocks", "issuer_cluster_bootstrap", "quarter_year_block_bootstrap",
      "interpretation"
    ),
    Value = c(
      PART5_OUTPUT_DIR, loaded$index_file, PART6_SPEC_BLOCK, PART6_SELECTION_RULE, PART6_SCOPE,
      as.character(PART6_N_BOOT), as.character(nrow(predictions)), as.character(nrow(selected_index)),
      as.character(n_distinct(predictions$firm_id)), as.character(n_distinct(predictions$Predictor_Quarter)),
      as.character(PART6_RUN_ISSUER_BOOT), as.character(PART6_RUN_TIME_BOOT),
      "Empirical locked-test stability under resampling; not full predictive uncertainty quantification"
    )
  )
  write_audit(manifest, "table_00_part_6_manifest", "Part 6 V5 locked-test stability manifest")

  point_estimates <- predictions %>%
    group_by(Predictor_Set, Horizon, Horizon_Label, Model, Candidate_ID) %>%
    group_modify(~ metrics_for_sample(.x), .keep = TRUE) %>%
    ungroup() %>%
    arrange(Horizon, Model, Predictor_Set)
  write_audit(point_estimates, "table_01_locked_test_point_metrics", "Locked-test point metrics for Part 6 selected rows")
  save_data(point_estimates, "Panel_Europe_macro_part_6_locked_test_point_metrics")

  prediction_summary <- summarise_prediction_distribution(predictions)
  write_audit(prediction_summary, "table_02_prediction_probability_distribution", "Locked-test predicted probability distribution")
  save_data(prediction_summary, "Panel_Europe_macro_part_6_prediction_probability_distribution")

  calibration <- decile_calibration(predictions)
  write_audit(calibration, "table_03_decile_calibration", "Locked-test decile calibration")
  save_data(calibration, "Panel_Europe_macro_part_6_decile_calibration")

  all_draws <- list()
  if (PART6_RUN_ISSUER_BOOT) {
    message("Part 6A: issuer-cluster bootstrap with ", PART6_N_BOOT, " replications.")
    all_draws$issuer_cluster <- run_metric_bootstrap(predictions, "firm_id", PART6_N_BOOT, "issuer_cluster")
    save_data(all_draws$issuer_cluster, "Panel_Europe_macro_part_6_issuer_cluster_bootstrap_draws")
  }
  if (PART6_RUN_TIME_BOOT) {
    message("Part 6B: quarter-year block bootstrap with ", PART6_N_BOOT, " replications.")
    all_draws$quarter_year_block <- run_metric_bootstrap(predictions, "Predictor_Quarter", PART6_N_BOOT, "quarter_year_block")
    save_data(all_draws$quarter_year_block, "Panel_Europe_macro_part_6_quarter_year_bootstrap_draws")
  }

  if (length(all_draws) > 0L) {
    combined_draws <- bind_rows(all_draws)
    metric_summary <- summarise_bootstrap(combined_draws, point_estimates)
    write_audit(metric_summary, "table_04_metric_stability_intervals", "Locked-test empirical metric stability intervals")
    save_data(metric_summary, "Panel_Europe_macro_part_6_metric_stability_intervals")

    compact_summary <- metric_summary %>%
      filter(Metric %in% c("PR_AUC", "ROC_AUC", "Brier", "Log_Loss", "MCC")) %>%
      select(Bootstrap_Method, Predictor_Set, Horizon_Label, Model, Metric, Point_Estimate, CI_5, CI_95, Bootstrap_SD)
    write_audit(compact_summary, "table_05_compact_metric_stability", "Compact locked-test stability summary")

    plot_metric_stability(metric_summary, "PR_AUC", "fig_01_pr_auc_stability", "Locked-test empirical stability: PR-AUC")
    plot_metric_stability(metric_summary, "MCC", "fig_02_mcc_stability", "Locked-test empirical stability: MCC")
  }

  notes <- c(
    "Part 6 V5 locked-test robustness and empirical stability complete.",
    "",
    paste0("Part 5 output directory: ", PART5_OUTPUT_DIR),
    paste0("Selection rule: ", PART6_SELECTION_RULE),
    paste0("Scope: ", PART6_SCOPE),
    paste0("Bootstrap replications: ", PART6_N_BOOT),
    "Issuer-cluster bootstrap resamples firms and retains all selected locked-test rows for sampled issuers.",
    "Quarter-year block bootstrap resamples predictor quarters and retains all firms in sampled quarters.",
    "These intervals are empirical stability diagnostics for locked-test evaluation, not full predictive uncertainty quantification.",
    "Brier and log-loss are proper scoring rules and are reported as probabilistic performance metrics, not as uncertainty measures."
  )
  notes_path <- file.path(OUTPUT_DIR, "part_6_locked_test_stability_notes.txt")
  assert_new_file(notes_path)
  writeLines(notes, notes_path)

  invisible(list(manifest = manifest, point_estimates = point_estimates))
}

running_as_script <- function() {
  sys_frames <- sys.frames()
  any(vapply(sys_frames, function(frame) !is.null(frame$ofile), logical(1))) ||
    any(grepl("--file=", commandArgs(trailingOnly = FALSE), fixed = TRUE))
}

if (running_as_script()) {
  message("Part 6 V5 locked-test stability starting.")
  message("Part 6 V5 selection rule: ", PART6_SELECTION_RULE)
  message("Part 6 V5 scope: ", PART6_SCOPE)
  run_part6_stability()
  message("Part 6 V5 complete. Outputs written to: ", OUTPUT_DIR)
}

