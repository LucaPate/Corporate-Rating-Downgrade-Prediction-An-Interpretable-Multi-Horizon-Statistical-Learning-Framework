# Step 06 - Summarise locked-test performance by model family.
# GitHub-ready copy: paths are repository-relative and data files are intentionally excluded.

# Builds paper-ready summaries from the existing Part 5 locked-test metrics.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(knitr)
})

get_script_dir_part5b <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) > 0L) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/")))
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

PART5B_SCRIPT_DIR <- get_script_dir_part5b()
V5_DIR <- normalizePath(file.path(PART5B_SCRIPT_DIR, ".."), winslash = "/", mustWork = TRUE)
PART5B_SELECTION_RULE <- tolower(trimws(Sys.getenv("PART5B_SELECTION_RULE", "near_best_calibrated")))
PART5B_MODEL_TAG <- Sys.getenv("PART5B_MODEL_TAG", "Elastic_Net_GAM_Logit_Probit_Random_Forest_XGBoost")

PART5B_OUTPUT_DIR <- file.path(V5_DIR, paste0("Part_5_all_Outputs_", PART5B_SELECTION_RULE))
PART5B_AUDIT_DIR <- file.path(PART5B_OUTPUT_DIR, "audits")
PART5B_TABLE_DIR <- file.path(PART5B_OUTPUT_DIR, "tables_latex")
PART5B_OVERWRITE_OUTPUTS <- tolower(Sys.getenv("PART5B_OVERWRITE_OUTPUTS", "TRUE")) %in% c("true", "t", "1", "yes")

dir.create(PART5B_AUDIT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(PART5B_TABLE_DIR, recursive = TRUE, showWarnings = FALSE)

part5b_assert_new_file <- function(path) {
  if (file.exists(path) && !PART5B_OVERWRITE_OUTPUTS) {
    stop(
      "Refusing to overwrite existing Part 5B output: ",
      path,
      ". Set PART5B_OVERWRITE_OUTPUTS=TRUE deliberately to replace it.",
      call. = FALSE
    )
  }
  invisible(path)
}

part5b_write_audit <- function(x, name, caption = NULL, digits = 4) {
  csv_path <- file.path(PART5B_AUDIT_DIR, paste0(name, ".csv"))
  tex_path <- file.path(PART5B_TABLE_DIR, paste0(name, ".tex"))
  part5b_assert_new_file(csv_path)
  part5b_assert_new_file(tex_path)
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

part5b_metrics_path <- function() {
  file.path(
    PART5B_AUDIT_DIR,
    paste0("table_03_locked_test_probability_metrics_", PART5B_MODEL_TAG, ".csv")
  )
}

model_family_summary <- function(metrics) {
  metrics %>%
    group_by(Horizon, Horizon_Label, Model) %>%
    summarise(
      N_Specifications = n_distinct(Predictor_Set),
      Median_PR_AUC = median(PR_AUC, na.rm = TRUE),
      IQR_PR_AUC = IQR(PR_AUC, na.rm = TRUE),
      Mean_PR_AUC = mean(PR_AUC, na.rm = TRUE),
      Best_PR_AUC = max(PR_AUC, na.rm = TRUE),
      Worst_PR_AUC = min(PR_AUC, na.rm = TRUE),
      Median_ROC_AUC = median(ROC_AUC, na.rm = TRUE),
      Mean_Brier = mean(Brier, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(Horizon, desc(Median_PR_AUC), desc(Best_PR_AUC), Model)
}

paired_differences_vs_benchmark <- function(metrics, benchmark_model = "Logit") {
  benchmark <- metrics %>%
    filter(Model == benchmark_model) %>%
    select(Predictor_Set, Horizon, Horizon_Label, Benchmark_PR_AUC = PR_AUC)

  metrics %>%
    filter(Model != benchmark_model) %>%
    inner_join(benchmark, by = c("Predictor_Set", "Horizon", "Horizon_Label")) %>%
    mutate(Delta_PR_AUC_vs_Logit = PR_AUC - Benchmark_PR_AUC) %>%
    group_by(Horizon, Horizon_Label, Model) %>%
    summarise(
      Benchmark_Model = benchmark_model,
      N_Paired_Specifications = n(),
      Median_Delta_PR_AUC = median(Delta_PR_AUC_vs_Logit, na.rm = TRUE),
      IQR_Delta_PR_AUC = IQR(Delta_PR_AUC_vs_Logit, na.rm = TRUE),
      Mean_Delta_PR_AUC = mean(Delta_PR_AUC_vs_Logit, na.rm = TRUE),
      Min_Delta_PR_AUC = min(Delta_PR_AUC_vs_Logit, na.rm = TRUE),
      Max_Delta_PR_AUC = max(Delta_PR_AUC_vs_Logit, na.rm = TRUE),
      Share_Positive_Delta = mean(Delta_PR_AUC_vs_Logit > 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(Horizon, desc(Median_Delta_PR_AUC), Model)
}

highest_observed_envelope <- function(metrics) {
  metrics %>%
    group_by(Horizon, Horizon_Label) %>%
    arrange(desc(PR_AUC), desc(ROC_AUC), Brier, Log_Loss, .by_group = TRUE) %>%
    slice(1L) %>%
    ungroup() %>%
    transmute(
      Horizon,
      Horizon_Label,
      Upper_Envelope_Model = Model,
      Predictor_Set,
      Candidate_ID,
      PR_AUC,
      ROC_AUC,
      Brier,
      Log_Loss,
      PR_AUC_Lift,
      Prevalence_Pct
    )
}

run_part5b_model_family_benchmark <- function() {
  metrics_file <- part5b_metrics_path()
  if (!file.exists(metrics_file)) {
    stop("Missing Part 5 locked-test metric table: ", metrics_file, call. = FALSE)
  }

  metrics <- readr::read_csv(metrics_file, show_col_types = FALSE) %>%
    mutate(
      Horizon = as.integer(Horizon),
      PR_AUC = as.numeric(PR_AUC),
      ROC_AUC = as.numeric(ROC_AUC),
      Brier = as.numeric(Brier),
      Log_Loss = as.numeric(Log_Loss),
      PR_AUC_Lift = as.numeric(PR_AUC_Lift),
      Prevalence_Pct = as.numeric(Prevalence_Pct)
    )

  family_summary <- model_family_summary(metrics)
  paired_vs_logit <- paired_differences_vs_benchmark(metrics, "Logit")
  upper_envelope <- highest_observed_envelope(metrics)

  part5b_write_audit(
    family_summary,
    paste0("table_16_locked_test_model_family_pr_auc_summary_", PART5B_MODEL_TAG),
    "Locked-test model-family PR-AUC distribution across pre-specified predictor specifications"
  )
  part5b_write_audit(
    paired_vs_logit,
    paste0("table_17_locked_test_paired_pr_auc_differences_vs_logit_", PART5B_MODEL_TAG),
    "Paired locked-test PR-AUC differences relative to Logit across common predictor specifications"
  )
  part5b_write_audit(
    upper_envelope,
    paste0("table_18_locked_test_highest_observed_pr_auc_envelope_", PART5B_MODEL_TAG),
    "Highest observed locked-test PR-AUC by horizon as a descriptive upper envelope"
  )

  notes <- c(
    "Part 5B model-family benchmark summaries complete.",
    paste0("Selection rule: ", PART5B_SELECTION_RULE),
    paste0("Model tag: ", PART5B_MODEL_TAG),
    "Tables 16 and 17 are intended as the primary locked-test comparison for the paper.",
    "Table 18 reports the highest observed locked-test PR-AUC by horizon only as a descriptive upper envelope.",
    "No model is selected using these locked-test summaries; all rows summarize validation-selected specifications already evaluated in Part 5."
  )
  writeLines(notes, file.path(PART5B_OUTPUT_DIR, paste0("part_5b_model_family_benchmark_notes_", PART5B_MODEL_TAG, ".txt")))

  invisible(list(
    family_summary = family_summary,
    paired_vs_logit = paired_vs_logit,
    upper_envelope = upper_envelope
  ))
}

if (any(startsWith(commandArgs(trailingOnly = FALSE), "--file="))) {
  message("Part 5B V5 model-family benchmark starting.")
  run_part5b_model_family_benchmark()
}

