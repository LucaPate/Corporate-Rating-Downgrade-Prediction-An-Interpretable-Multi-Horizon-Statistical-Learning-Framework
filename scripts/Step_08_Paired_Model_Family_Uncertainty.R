# Step 08 - Estimate paired model-family uncertainty relative to logit.
# GitHub-ready copy: paths are repository-relative and data files are intentionally excluded.

# Uses existing locked-test predictions only. It does not refit models.
# For each bootstrap draw, the same sampled observations are used for Logit
# and for the comparison model within each predictor specification.

set.seed(20260929)

required_packages <- c("tidyverse", "PRROC", "knitr", "digest")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop("Missing required package(s): ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

suppressPackageStartupMessages({
  library(tidyverse)
})

SEED <- 20260929L
PART6B_SPEC_BLOCK <- tolower(trimws(Sys.getenv("PART6B_SPEC_BLOCK", Sys.getenv("PART5B_SPEC_BLOCK", "all"))))
PART6B_SELECTION_RULE <- tolower(trimws(Sys.getenv("PART6B_SELECTION_RULE", Sys.getenv("PART5B_SELECTION_RULE", "near_best_calibrated"))))
PART6B_MODEL_TAG <- Sys.getenv("PART6B_MODEL_TAG", "Elastic_Net_GAM_Logit_Probit_Random_Forest_XGBoost")
PART6B_BENCHMARK_MODEL <- Sys.getenv("PART6B_BENCHMARK_MODEL", "Logit")
PART6B_N_BOOT <- as.integer(Sys.getenv("PART6B_N_BOOT", "500"))
PART6B_N_BOOT <- if_else(is.na(PART6B_N_BOOT) | PART6B_N_BOOT < 1L, 500L, PART6B_N_BOOT)
PART6B_OVERWRITE_OUTPUTS <- tolower(Sys.getenv("PART6B_OVERWRITE_OUTPUTS", "FALSE")) %in% c("true", "t", "1", "yes")
PART6B_OUTPUT_TAG <- trimws(Sys.getenv("PART6B_OUTPUT_TAG", "paired_model_family_uncertainty"))

allowed_selection_rules <- c("max_pr_auc", "near_best_parsimony", "near_best_calibrated")
if (!PART6B_SELECTION_RULE %in% allowed_selection_rules) {
  stop("PART6B_SELECTION_RULE must be one of: ", paste(allowed_selection_rules, collapse = ", "), call. = FALSE)
}

parse_filter <- function(value) {
  value <- trimws(value)
  if (!nzchar(value)) return(character())
  strsplit(value, ",", fixed = TRUE)[[1]] |>
    trimws() |>
    purrr::discard(~ !nzchar(.x))
}

selected_models_filter <- parse_filter(Sys.getenv("PART6B_SELECTED_MODELS", ""))
selected_sets_filter <- parse_filter(Sys.getenv("PART6B_SELECTED_REGRESSOR_SETS", ""))
selected_horizons_filter <- parse_filter(Sys.getenv("PART6B_SELECTED_HORIZONS", ""))

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
PART5_OUTPUT_DIR <- file.path(V5_DIR, paste0("Part_5", part_suffix(PART6B_SPEC_BLOCK), "_Outputs", selection_suffix(PART6B_SELECTION_RULE)))
OUTPUT_DIR <- file.path(V5_DIR, paste0("Part_6B", part_suffix(PART6B_SPEC_BLOCK), "_Outputs", selection_suffix(PART6B_SELECTION_RULE)))
if (nzchar(clean_file_id(PART6B_OUTPUT_TAG))) {
  OUTPUT_DIR <- paste0(OUTPUT_DIR, "_", clean_file_id(PART6B_OUTPUT_TAG))
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
  if (file.exists(path) && !PART6B_OVERWRITE_OUTPUTS) {
    stop(
      "Refusing to overwrite existing Part 6B output: ",
      path,
      ". Set PART6B_OVERWRITE_OUTPUTS=TRUE deliberately to replace it.",
      call. = FALSE
    )
  }
  invisible(path)
}

write_audit <- function(x, name, caption = NULL, digits = 4) {
  csv_path <- file.path(AUDIT_DIR, paste0(name, ".csv"))
  tex_path <- file.path(TABLE_DIR, paste0(name, ".tex"))
  assert_new_file(csv_path)
  assert_new_file(tex_path)
  readr::write_csv(x, csv_path, na = "")
  latex_table <- knitr::kable(
    x,
    format = "latex",
    booktabs = TRUE,
    longtable = nrow(x) > 30L,
    caption = caption,
    digits = digits,
    escape = TRUE
  )
  writeLines(as.character(latex_table), tex_path)
  invisible(x)
}

save_data <- function(x, name) {
  path <- file.path(DATA_DIR, paste0(name, ".rds"))
  assert_new_file(path)
  saveRDS(x, path)
  invisible(x)
}

latest_matching_file <- function(dir, pattern) {
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0L) return(NA_character_)
  files[which.max(file.info(files)$mtime)]
}

read_part5_inputs <- function() {
  if (!dir.exists(PART5_OUTPUT_DIR)) {
    stop("Missing Part 5 output directory: ", PART5_OUTPUT_DIR, call. = FALSE)
  }

  prediction_index_file <- latest_matching_file(
    file.path(PART5_OUTPUT_DIR, "data"),
    "^Panel_Europe_macro_locked_test_predictions_by_group_.*_index\\.csv$"
  )
  if (is.na(prediction_index_file)) {
    prediction_index_file <- latest_matching_file(
      file.path(PART5_OUTPUT_DIR, "audits"),
      "^table_15_locked_test_prediction_file_index_.*\\.csv$"
    )
  }

  metric_file <- file.path(
    PART5_OUTPUT_DIR,
    "audits",
    paste0("table_03_locked_test_probability_metrics_", PART6B_MODEL_TAG, ".csv")
  )

  if (is.na(prediction_index_file)) {
    stop("Missing Part 5 grouped prediction index under: ", PART5_OUTPUT_DIR, call. = FALSE)
  }
  if (!file.exists(metric_file)) {
    stop("Missing Part 5 probability metric table: ", metric_file, call. = FALSE)
  }

  prediction_index <- readr::read_csv(prediction_index_file, show_col_types = FALSE)
  if (!all(c("Predictor_Set", "Model", "Horizon", "Horizon_Label", "File_Name") %in% names(prediction_index))) {
    stop("Prediction index does not contain the expected grouped-prediction columns.", call. = FALSE)
  }

  metrics <- readr::read_csv(metric_file, show_col_types = FALSE)
  list(
    prediction_index_file = prediction_index_file,
    metric_file = metric_file,
    prediction_index = prediction_index,
    metrics = metrics
  )
}

filter_inputs <- function(index, metrics) {
  model_set <- unique(metrics$Model)
  comparison_models <- setdiff(model_set, PART6B_BENCHMARK_MODEL)
  if (length(selected_models_filter) > 0L) {
    comparison_models <- intersect(comparison_models, selected_models_filter)
  }
  models_needed <- union(PART6B_BENCHMARK_MODEL, comparison_models)

  filtered_metrics <- metrics %>%
    filter(Model %in% models_needed)

  filtered_index <- index %>%
    filter(Model %in% models_needed)

  if (length(selected_sets_filter) > 0L) {
    filtered_metrics <- filtered_metrics %>% filter(Predictor_Set %in% selected_sets_filter)
    filtered_index <- filtered_index %>% filter(Predictor_Set %in% selected_sets_filter)
  }
  if (length(selected_horizons_filter) > 0L) {
    horizon_labels <- ifelse(
      grepl("^t\\+", selected_horizons_filter),
      selected_horizons_filter,
      paste0("t+", selected_horizons_filter)
    )
    filtered_metrics <- filtered_metrics %>%
      filter(Horizon_Label %in% horizon_labels | as.character(Horizon) %in% selected_horizons_filter)
    filtered_index <- filtered_index %>%
      filter(Horizon_Label %in% horizon_labels | as.character(Horizon) %in% selected_horizons_filter)
  }

  common_specs <- filtered_metrics %>%
    distinct(Predictor_Set, Horizon, Horizon_Label, Model) %>%
    count(Predictor_Set, Horizon, Horizon_Label, name = "N_Models") %>%
    filter(N_Models == length(models_needed)) %>%
    select(Predictor_Set, Horizon, Horizon_Label)

  filtered_metrics <- filtered_metrics %>%
    inner_join(common_specs, by = c("Predictor_Set", "Horizon", "Horizon_Label"))
  filtered_index <- filtered_index %>%
    inner_join(common_specs, by = c("Predictor_Set", "Horizon", "Horizon_Label"))

  list(index = filtered_index, metrics = filtered_metrics, comparison_models = comparison_models)
}

resolve_prediction_paths <- function(index, prediction_index_file) {
  index_dir <- dirname(prediction_index_file)
  by_group_dir <- file.path(index_dir, paste0("Panel_Europe_macro_locked_test_predictions_by_group_", PART6B_MODEL_TAG))

  index %>%
    mutate(
      Resolved_File_Path = case_when(
        "File_Path" %in% names(.) & file.exists(File_Path) ~ File_Path,
        file.exists(file.path(by_group_dir, File_Name)) ~ file.path(by_group_dir, File_Name),
        TRUE ~ NA_character_
      )
    )
}

pr_auc_one <- function(y, p) {
  ok <- is.finite(y) & is.finite(p)
  y <- as.integer(y[ok])
  p <- as.numeric(p[ok])
  if (length(y) == 0L || length(unique(y)) < 2L) {
    return(NA_real_)
  }
  tryCatch(
    PRROC::pr.curve(scores.class0 = p[y == 1L], scores.class1 = p[y == 0L], curve = FALSE)$auc.integral,
    error = function(e) NA_real_
  )
}

load_predictions <- function(index) {
  missing_files <- index %>% filter(is.na(Resolved_File_Path) | !file.exists(Resolved_File_Path))
  if (nrow(missing_files) > 0L) {
    stop("Some grouped prediction files are missing. First missing file: ", missing_files$File_Name[[1]], call. = FALSE)
  }

  purrr::map_dfr(index$Resolved_File_Path, readRDS) %>%
    mutate(
      Predictor_Date = as.Date(Predictor_Date),
      Target_Date = as.Date(Target_Date),
      Issuer_Cluster = paste(Country, firm_id, sep = "__"),
      Target_Quarter_Block = sprintf("%04d Q%d", lubridate::year(Target_Date), lubridate::quarter(Target_Date))
    )
}

observed_paired_differences <- function(metrics) {
  benchmark <- metrics %>%
    filter(Model == PART6B_BENCHMARK_MODEL) %>%
    select(Predictor_Set, Horizon, Horizon_Label, Benchmark_PR_AUC = PR_AUC)

  metrics %>%
    filter(Model != PART6B_BENCHMARK_MODEL) %>%
    inner_join(benchmark, by = c("Predictor_Set", "Horizon", "Horizon_Label")) %>%
    mutate(Delta_PR_AUC_vs_Logit = PR_AUC - Benchmark_PR_AUC) %>%
    group_by(Horizon, Horizon_Label, Model) %>%
    summarise(
      Benchmark_Model = PART6B_BENCHMARK_MODEL,
      N_Paired_Specifications = n(),
      Median_Delta_PR_AUC = median(Delta_PR_AUC_vs_Logit, na.rm = TRUE),
      IQR_Delta_PR_AUC = IQR(Delta_PR_AUC_vs_Logit, na.rm = TRUE),
      Mean_Delta_PR_AUC = mean(Delta_PR_AUC_vs_Logit, na.rm = TRUE),
      Share_Positive_Delta = mean(Delta_PR_AUC_vs_Logit > 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(Horizon, desc(Median_Delta_PR_AUC), Model)
}

stable_seed <- function(...) {
  key <- paste(..., collapse = "|")
  as.integer(strtoi(substr(digest::digest(key, algo = "xxhash32"), 1, 7), base = 16L)) %% .Machine$integer.max
}

bootstrap_indices <- function(cluster_values, n_boot, seed) {
  clusters <- sort(unique(cluster_values))
  clusters <- clusters[!is.na(clusters)]
  set.seed(seed)
  replicate(n_boot, sample(clusters, length(clusters), replace = TRUE), simplify = FALSE)
}

sample_cluster_rows <- function(data, cluster_var, sampled_clusters) {
  cluster_rows <- split(seq_len(nrow(data)), data[[cluster_var]], drop = TRUE)
  sampled_rows <- unlist(cluster_rows[as.character(sampled_clusters)], use.names = FALSE)
  data[sampled_rows, , drop = FALSE]
}

bootstrap_one_spec <- function(data, cluster_var, method_label, comparison_models) {
  draws <- bootstrap_indices(
    data[[cluster_var]],
    PART6B_N_BOOT,
    stable_seed(
      "part6b",
      PART6B_SELECTION_RULE,
      method_label,
      unique(data$Predictor_Set),
      unique(data$Horizon_Label)
    )
  )

  purrr::map_dfr(seq_along(draws), function(b) {
    sampled <- sample_cluster_rows(data, cluster_var, draws[[b]])

    model_metrics <- sampled %>%
      group_by(Model) %>%
      summarise(
        PR_AUC = pr_auc_one(Outcome_Num, Probability),
        N = n(),
        N_Downgrades = sum(Outcome_Num == 1L, na.rm = TRUE),
        .groups = "drop"
      )

    benchmark_value <- model_metrics %>%
      filter(Model == PART6B_BENCHMARK_MODEL) %>%
      pull(PR_AUC)

    model_metrics %>%
      filter(Model %in% comparison_models) %>%
      transmute(
        Bootstrap_Method = method_label,
        Bootstrap_ID = b,
        Predictor_Set = unique(data$Predictor_Set),
        Horizon = unique(data$Horizon),
        Horizon_Label = unique(data$Horizon_Label),
        Model,
        Benchmark_Model = PART6B_BENCHMARK_MODEL,
        Model_PR_AUC = PR_AUC,
        Benchmark_PR_AUC = benchmark_value[[1]],
        Delta_PR_AUC_vs_Logit = PR_AUC - benchmark_value[[1]],
        N,
        N_Downgrades
      )
  })
}

run_paired_bootstrap <- function(predictions, cluster_var, method_label, comparison_models) {
  if (!cluster_var %in% names(predictions)) {
    stop("Cluster variable not found in predictions: ", cluster_var, call. = FALSE)
  }
  groups <- predictions %>%
    group_by(Predictor_Set, Horizon, Horizon_Label) %>%
    group_split()
  keys <- predictions %>%
    distinct(Predictor_Set, Horizon, Horizon_Label) %>%
    arrange(Predictor_Set, Horizon)

  purrr::imap_dfr(groups, function(group_data, group_i) {
    key_i <- keys[group_i, ]
    if (group_i == 1L || group_i %% 10L == 0L || group_i == length(groups)) {
      message(
        "  ",
        method_label,
        " specification ",
        group_i,
        "/",
        length(groups),
        ": ",
        key_i$Predictor_Set,
        " ",
        key_i$Horizon_Label
      )
    }
    bootstrap_one_spec(group_data, cluster_var, method_label, comparison_models)
  })
}

summarise_bootstrap_deltas <- function(boot_deltas, observed_summary) {
  boot_family <- boot_deltas %>%
    group_by(Bootstrap_Method, Bootstrap_ID, Horizon, Horizon_Label, Model, Benchmark_Model) %>%
    summarise(
      N_Paired_Specifications = sum(is.finite(Delta_PR_AUC_vs_Logit)),
      Median_Delta_PR_AUC = median(Delta_PR_AUC_vs_Logit, na.rm = TRUE),
      Mean_Delta_PR_AUC = mean(Delta_PR_AUC_vs_Logit, na.rm = TRUE),
      Share_Positive_Delta = mean(Delta_PR_AUC_vs_Logit > 0, na.rm = TRUE),
      .groups = "drop"
    )

  boot_family %>%
    group_by(Bootstrap_Method, Horizon, Horizon_Label, Model, Benchmark_Model) %>%
    summarise(
      N_Bootstrap = sum(is.finite(Median_Delta_PR_AUC)),
      Bootstrap_Mean_Median_Delta = mean(Median_Delta_PR_AUC, na.rm = TRUE),
      Bootstrap_SD_Median_Delta = sd(Median_Delta_PR_AUC, na.rm = TRUE),
      CI_2_5 = quantile(Median_Delta_PR_AUC, 0.025, na.rm = TRUE, names = FALSE),
      CI_5 = quantile(Median_Delta_PR_AUC, 0.050, na.rm = TRUE, names = FALSE),
      CI_50 = quantile(Median_Delta_PR_AUC, 0.500, na.rm = TRUE, names = FALSE),
      CI_95 = quantile(Median_Delta_PR_AUC, 0.950, na.rm = TRUE, names = FALSE),
      CI_97_5 = quantile(Median_Delta_PR_AUC, 0.975, na.rm = TRUE, names = FALSE),
      P_Median_Delta_GT_0 = mean(Median_Delta_PR_AUC > 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(
      observed_summary %>%
        select(
          Horizon,
          Horizon_Label,
          Model,
          Benchmark_Model,
          Observed_N_Paired_Specifications = N_Paired_Specifications,
          Observed_Median_Delta_PR_AUC = Median_Delta_PR_AUC,
          Observed_IQR_Delta_PR_AUC = IQR_Delta_PR_AUC,
          Observed_Mean_Delta_PR_AUC = Mean_Delta_PR_AUC,
          Observed_Share_Positive_Delta = Share_Positive_Delta
        ),
      by = c("Horizon", "Horizon_Label", "Model", "Benchmark_Model")
    ) %>%
    relocate(starts_with("Observed"), .after = Benchmark_Model) %>%
    arrange(Bootstrap_Method, Horizon, desc(Observed_Median_Delta_PR_AUC), Model)
}

make_compact_interval_table <- function(summary) {
  issuer <- summary %>%
    filter(Bootstrap_Method == "issuer_cluster") %>%
    select(Horizon, Horizon_Label, Model, Issuer_CI_5 = CI_5, Issuer_CI_95 = CI_95,
           Issuer_CI_2_5 = CI_2_5, Issuer_CI_97_5 = CI_97_5,
           Issuer_P_Median_Delta_GT_0 = P_Median_Delta_GT_0)
  target_quarter <- summary %>%
    filter(Bootstrap_Method == "target_quarter_block") %>%
    select(Horizon, Horizon_Label, Model, Target_Quarter_CI_5 = CI_5, Target_Quarter_CI_95 = CI_95,
           Target_Quarter_CI_2_5 = CI_2_5, Target_Quarter_CI_97_5 = CI_97_5,
           Target_Quarter_P_Median_Delta_GT_0 = P_Median_Delta_GT_0)

  summary %>%
    distinct(
      Horizon,
      Horizon_Label,
      Model,
      Benchmark_Model,
      Observed_N_Paired_Specifications,
      Observed_Median_Delta_PR_AUC,
      Observed_IQR_Delta_PR_AUC,
      Observed_Mean_Delta_PR_AUC,
      Observed_Share_Positive_Delta
    ) %>%
    left_join(issuer, by = c("Horizon", "Horizon_Label", "Model")) %>%
    left_join(target_quarter, by = c("Horizon", "Horizon_Label", "Model")) %>%
    arrange(Horizon, desc(Observed_Median_Delta_PR_AUC), Model)
}

cluster_summary <- function(predictions) {
  predictions %>%
    distinct(Predictor_Set, Horizon, Horizon_Label, Observation_ID, Issuer_Cluster, Target_Quarter_Block, Outcome_Num) %>%
    group_by(Horizon, Horizon_Label) %>%
    summarise(
      N_Specifications = n_distinct(Predictor_Set),
      N_Observations_Per_Specification = n_distinct(Observation_ID),
      N_Downgrades_Per_Specification = n_distinct(Observation_ID[Outcome_Num == 1L]),
      Issuer_Clusters = n_distinct(Issuer_Cluster),
      Target_Quarter_Blocks = n_distinct(Target_Quarter_Block),
      .groups = "drop"
    ) %>%
    arrange(Horizon)
}

run_part6b_paired_uncertainty <- function() {
  loaded <- read_part5_inputs()
  filtered <- filter_inputs(loaded$prediction_index, loaded$metrics)
  selected_index <- resolve_prediction_paths(filtered$index, loaded$prediction_index_file)
  selected_metrics <- filtered$metrics

  if (nrow(selected_index) == 0L || nrow(selected_metrics) == 0L) {
    stop("No common prediction/metric rows selected for Part 6B.", call. = FALSE)
  }
  if (!PART6B_BENCHMARK_MODEL %in% selected_metrics$Model) {
    stop("Benchmark model not found in selected metrics: ", PART6B_BENCHMARK_MODEL, call. = FALSE)
  }
  if (length(filtered$comparison_models) == 0L) {
    stop("No comparison models selected for Part 6B.", call. = FALSE)
  }

  predictions <- load_predictions(selected_index)
  observed_summary <- observed_paired_differences(selected_metrics)
  clusters <- cluster_summary(predictions)

  manifest <- tibble(
    Setting = c(
      "part_5_output_dir",
      "part_5_prediction_index",
      "part_5_metric_file",
      "part_6b_selection_rule",
      "part_6b_spec_block",
      "benchmark_model",
      "comparison_models",
      "bootstrap_replications",
      "prediction_files_selected",
      "prediction_rows",
      "paired_specifications_per_horizon_model",
      "issuer_bootstrap_unit",
      "time_bootstrap_unit",
      "interpretation"
    ),
    Value = c(
      PART5_OUTPUT_DIR,
      loaded$prediction_index_file,
      loaded$metric_file,
      PART6B_SELECTION_RULE,
      PART6B_SPEC_BLOCK,
      PART6B_BENCHMARK_MODEL,
      paste(filtered$comparison_models, collapse = ","),
      as.character(PART6B_N_BOOT),
      as.character(nrow(selected_index)),
      as.character(nrow(predictions)),
      paste(sort(unique(observed_summary$N_Paired_Specifications)), collapse = ","),
      "Country-firm issuer cluster",
      "Target quarter block",
      "Bootstrap intervals summarize stability of the median paired PR-AUC difference versus Logit across common predictor specifications."
    )
  )

  write_audit(manifest, "table_00_part_6b_manifest", "Part 6B paired model-family uncertainty manifest")
  write_audit(clusters, "table_01_part_6b_cluster_summary", "Part 6B bootstrap cluster summary")
  write_audit(observed_summary, "table_02_observed_paired_pr_auc_differences_vs_logit", "Observed paired PR-AUC differences versus Logit")
  save_data(observed_summary, "Panel_Europe_macro_part_6b_observed_paired_pr_auc_differences_vs_logit")

  message("Part 6B: issuer-cluster paired bootstrap with ", PART6B_N_BOOT, " replications.")
  issuer_deltas <- run_paired_bootstrap(predictions, "Issuer_Cluster", "issuer_cluster", filtered$comparison_models)
  save_data(issuer_deltas, "Panel_Europe_macro_part_6b_issuer_cluster_delta_replicates")

  message("Part 6B: target-quarter paired bootstrap with ", PART6B_N_BOOT, " replications.")
  quarter_deltas <- run_paired_bootstrap(predictions, "Target_Quarter_Block", "target_quarter_block", filtered$comparison_models)
  save_data(quarter_deltas, "Panel_Europe_macro_part_6b_target_quarter_delta_replicates")

  combined_deltas <- bind_rows(issuer_deltas, quarter_deltas)
  save_data(combined_deltas, "Panel_Europe_macro_part_6b_all_delta_replicates")

  interval_summary <- summarise_bootstrap_deltas(combined_deltas, observed_summary)
  compact_summary <- make_compact_interval_table(interval_summary)

  write_audit(interval_summary, "table_03_bootstrap_median_delta_pr_auc_intervals", "Bootstrap intervals for median paired PR-AUC differences versus Logit")
  write_audit(compact_summary, "table_04_compact_paired_pr_auc_uncertainty_vs_logit", "Compact paired PR-AUC uncertainty summary versus Logit")
  save_data(interval_summary, "Panel_Europe_macro_part_6b_bootstrap_median_delta_pr_auc_intervals")
  save_data(compact_summary, "Panel_Europe_macro_part_6b_compact_paired_pr_auc_uncertainty_vs_logit")

  notes <- c(
    "Part 6B paired model-family uncertainty complete.",
    "",
    paste0("Part 5 output directory: ", PART5_OUTPUT_DIR),
    paste0("Selection rule: ", PART6B_SELECTION_RULE),
    paste0("Benchmark model: ", PART6B_BENCHMARK_MODEL),
    paste0("Comparison models: ", paste(filtered$comparison_models, collapse = ", ")),
    paste0("Bootstrap replications: ", PART6B_N_BOOT),
    "Each replicate resamples issuer clusters or target-quarter blocks within a predictor specification and forecast horizon.",
    "The same resampled rows are used for the comparison model and Logit, so the bootstrap remains paired.",
    "For each replicate and model-horizon cell, the reported statistic is the median PR-AUC difference across common predictor specifications.",
    "The compact table reports both 5-95% and 2.5-97.5% intervals."
  )
  notes_path <- file.path(OUTPUT_DIR, "part_6b_paired_model_family_uncertainty_notes.txt")
  assert_new_file(notes_path)
  writeLines(notes, notes_path)

  invisible(list(
    manifest = manifest,
    cluster_summary = clusters,
    observed_summary = observed_summary,
    interval_summary = interval_summary,
    compact_summary = compact_summary
  ))
}

running_as_script <- function() {
  sys_frames <- sys.frames()
  any(vapply(sys_frames, function(frame) !is.null(frame$ofile), logical(1))) ||
    any(grepl("--file=", commandArgs(trailingOnly = FALSE), fixed = TRUE))
}

if (running_as_script()) {
  message("Part 6B V5 paired model-family uncertainty starting.")
  message("Part 6B V5 selection rule: ", PART6B_SELECTION_RULE)
  message("Part 6B V5 bootstrap replications: ", PART6B_N_BOOT)
  run_part6b_paired_uncertainty()
  message("Part 6B V5 complete. Outputs written to: ", OUTPUT_DIR)
}

