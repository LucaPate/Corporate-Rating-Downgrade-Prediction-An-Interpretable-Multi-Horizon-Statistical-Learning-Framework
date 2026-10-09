# Step 04 - Tune downgrade-prediction models with expanding-window validation.
# GitHub-ready copy: paths are repository-relative and data files are intentionally excluded.

# Runs final V5 model tuning on the specifications produced by Part 3.
# Hyperparameters are selected primarily by validation PR-AUC; MCC is used to
# select validation thresholds and to audit downgrade/no-downgrade separation.

set.seed(20260724)

required_packages <- c(
  "tidyverse",
  "glmnet",
  "mgcv",
  "ranger",
  "xgboost",
  "pROC",
  "PRROC",
  "knitr",
  "digest",
  "scales"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop("Please install: ", paste(missing_packages, collapse = ", "), call. = FALSE)
}
invisible(lapply(required_packages, library, character.only = TRUE))

SEED <- 20260724
CACHE_VERSION <- "part4_v5_final_specs_v1"
FORCE_REFIT <- FALSE
PART4_OUTPUT_TAG <- Sys.getenv("PART4_OUTPUT_TAG", "")
PART4_OVERWRITE_OUTPUTS <- tolower(Sys.getenv("PART4_OVERWRITE_OUTPUTS", "FALSE")) %in% c("true", "t", "1", "yes")
PART4_SPEC_BLOCK <- tolower(trimws(Sys.getenv("PART4_SPEC_BLOCK", "all")))
if (!PART4_SPEC_BLOCK %in% c("all", "financial", "non_systemic", "systemic", "ciss", "vstoxx", "macro_pars_stat", "macro_delta", "rating_rank", "rating_group")) {
  stop("PART4_SPEC_BLOCK must be one of 'all', 'financial', 'non_systemic', 'systemic', 'ciss', 'vstoxx', 'macro_pars_stat', 'macro_delta', 'rating_rank', or 'rating_group'.", call. = FALSE)
}
PART4_ACTION <- tolower(trimws(Sys.getenv("PART4_ACTION", "estimate")))
if (!PART4_ACTION %in% c("estimate", "collect", "partition", "both", "none")) {
  stop("PART4_ACTION must be one of 'estimate', 'collect', 'partition', 'both', or 'none'.", call. = FALSE)
}
part4_env_flag <- function(name, default = FALSE) {
  tolower(trimws(Sys.getenv(name, if_else(default, "TRUE", "FALSE")))) %in% c("true", "t", "1", "yes")
}
PART4_SAVE_MEGA_VALIDATION_PREDICTIONS <- part4_env_flag("PART4_SAVE_MEGA_VALIDATION_PREDICTIONS", FALSE)
PART4_SAVE_SPLIT_VALIDATION_PREDICTIONS <- part4_env_flag("PART4_SAVE_SPLIT_VALIDATION_PREDICTIONS", TRUE)
PART4_SAVE_SPLIT_SELECTED_VALIDATION_PREDICTIONS <- part4_env_flag("PART4_SAVE_SPLIT_SELECTED_VALIDATION_PREDICTIONS", TRUE)
PR_AUC_NEAR_BEST_ABS_TOLERANCE <- 0.0025
PR_AUC_NEAR_BEST_REL_TOLERANCE <- 0.03
PART4_ALLOWED_SELECTION_RULES <- c("max_pr_auc", "near_best_parsimony", "near_best_calibrated")

parse_part4_selection_rules <- function(raw_rules) {
  rules <- strsplit(tolower(trimws(raw_rules)), ",")[[1]] %>%
    trimws() %>%
    purrr::discard(~ !nzchar(.x))
  if (length(rules) == 1L && identical(rules, "all")) {
    return(PART4_ALLOWED_SELECTION_RULES)
  }
  rules <- recode(
    rules,
    best_pr_auc = "max_pr_auc",
    near_best = "near_best_parsimony",
    near_best_pr_auc = "near_best_parsimony",
    calibrated = "near_best_calibrated",
    .default = rules
  )
  invalid_rules <- setdiff(rules, PART4_ALLOWED_SELECTION_RULES)
  if (length(invalid_rules) > 0L) {
    stop(
      "PART4_SELECTION_RULES contains invalid value(s): ",
      paste(invalid_rules, collapse = ", "),
      ". Allowed values are: ",
      paste(PART4_ALLOWED_SELECTION_RULES, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  unique(rules)
}

PART4_SELECTION_RULE <- tolower(trimws(Sys.getenv("PART4_SELECTION_RULE", "max_pr_auc")))
PART4_SELECTION_RULE <- recode(
  PART4_SELECTION_RULE,
  best_pr_auc = "max_pr_auc",
  near_best = "near_best_parsimony",
  near_best_pr_auc = "near_best_parsimony",
  calibrated = "near_best_calibrated",
  .default = PART4_SELECTION_RULE
)
if (!PART4_SELECTION_RULE %in% PART4_ALLOWED_SELECTION_RULES) {
  stop(
    "PART4_SELECTION_RULE must be one of: ",
    paste(PART4_ALLOWED_SELECTION_RULES, collapse = ", "),
    ".",
    call. = FALSE
  )
}
PART4_SELECTION_RULES <- parse_part4_selection_rules(
  Sys.getenv(
    "PART4_SELECTION_RULES",
    paste(PART4_ALLOWED_SELECTION_RULES, collapse = ",")
  )
)
PART4_SELECTION_RULES <- unique(c(PART4_SELECTION_RULE, PART4_SELECTION_RULES))

get_script_dir <- function() {
  file_arg <- "--file="
  args <- commandArgs(trailingOnly = FALSE)
  script_arg <- args[startsWith(args, file_arg)]
  if (length(script_arg) > 0L) {
    return(dirname(normalizePath(sub(file_arg, "", script_arg[[1]]), winslash = "/", mustWork = TRUE)))
  }

  # Resolve the script path also when Part 4 is sourced during audit checks.
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
RATING_EXPERIMENT_DIR <- V5_DIR

# Parallel settings can be changed here or through environment variables.
RUN_IN_PARALLEL <- tolower(Sys.getenv("PART4_RUN_IN_PARALLEL", "TRUE")) %in% c("true", "t", "1", "yes")
N_WORKERS <- as.integer(Sys.getenv("PART4_N_WORKERS", "3"))
MODEL_THREADS <- as.integer(Sys.getenv("PART4_MODEL_THREADS", "2"))
N_WORKERS <- if_else(is.na(N_WORKERS) | N_WORKERS < 1L, 1L, N_WORKERS)
MODEL_THREADS <- if_else(is.na(MODEL_THREADS) | MODEL_THREADS < 1L, 1L, MODEL_THREADS)

INPUT_RDS <- file.path(
  V5_DIR,
  "Part_3_Outputs",
  "data_imputed",
  "Panel_Europe_macro_expanding_window_folds_imputed.rds"
)
REGRESSOR_MANIFEST_CSV <- file.path(
  V5_DIR,
  "Part_3_Outputs",
  "audits",
  "table_20_regressor_manifest.csv"
)

part4_spec_suffix <- function(spec) {
  paste0("_", spec)
}

OUTPUT_DIR <- file.path(RATING_EXPERIMENT_DIR, paste0("Part_4", part4_spec_suffix(PART4_SPEC_BLOCK), "_Outputs"))
DATA_DIR <- file.path(OUTPUT_DIR, "data")
AUDIT_DIR <- file.path(OUTPUT_DIR, "audits")
TABLE_DIR <- file.path(OUTPUT_DIR, "tables_latex")
FIGURE_DIR <- file.path(OUTPUT_DIR, "figures")
CACHE_DIR <- file.path(OUTPUT_DIR, "cache")
TIMING_DIR <- file.path(OUTPUT_DIR, "timing")

dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(AUDIT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TABLE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIGURE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TIMING_DIR, recursive = TRUE, showWarnings = FALSE)

HORIZONS <- 1:4

# Part 4 uses the controlled V5 manifest saved by Part 3. Rating-history
# specifications are deliberately unavailable in V5.
if (!file.exists(REGRESSOR_MANIFEST_CSV)) {
  stop("Missing V5 regressor manifest from Part 3: ", REGRESSOR_MANIFEST_CSV, call. = FALSE)
}
available_regressor_manifest <- readr::read_csv(REGRESSOR_MANIFEST_CSV, show_col_types = FALSE)
if ("Has_Rating_History" %in% names(available_regressor_manifest)) {
  available_regressor_manifest <- available_regressor_manifest %>%
    filter(!(.data$Has_Rating_History %in% TRUE))
}
if (any(str_detect(available_regressor_manifest$Regressor_Set, "rating_history"))) {
  stop("Invalid V5 manifest: rating-history specifications are not allowed.", call. = FALSE)
}

default_regressor_sets <- switch(
  PART4_SPEC_BLOCK,
  all = available_regressor_manifest$Regressor_Set,
  financial = available_regressor_manifest %>%
    filter(!Has_Macro_Pars_Stat, !Has_Macro_Delta, !Has_Systemic) %>%
    pull(Regressor_Set),
  non_systemic = available_regressor_manifest %>%
    filter(!Has_Systemic) %>%
    pull(Regressor_Set),
  systemic = available_regressor_manifest %>%
    filter(Has_Systemic) %>%
    pull(Regressor_Set),
  ciss = available_regressor_manifest %>%
    filter(Has_CISS) %>%
    pull(Regressor_Set),
  vstoxx = available_regressor_manifest %>%
    filter(Has_VSTOXX) %>%
    pull(Regressor_Set),
  macro_pars_stat = available_regressor_manifest %>%
    filter(Has_Macro_Pars_Stat) %>%
    pull(Regressor_Set),
  macro_delta = available_regressor_manifest %>%
    filter(Has_Macro_Delta) %>%
    pull(Regressor_Set),
  rating_rank = available_regressor_manifest %>%
    filter(Rating_Encoding == "rating_rank") %>%
    pull(Regressor_Set),
  rating_group = available_regressor_manifest %>%
    filter(Rating_Encoding == "rating_group") %>%
    pull(Regressor_Set)
)
REGRESSOR_SETS_TO_RUN <- strsplit(
  Sys.getenv("PART4_REGRESSOR_SETS", paste(default_regressor_sets, collapse = ",")),
  ","
)[[1]] %>%
  trimws() %>%
  purrr::discard(~ !nzchar(.x))
ALL_MODELS <- c("Logit", "Probit", "Random_Forest", "XGBoost", "Elastic_Net", "GAM")

# Thresholds are concentrated near zero because downgrades are rare.
THRESHOLD_GRID <- sort(unique(c(
  seq(0.001, 0.020, by = 0.001),
  seq(0.025, 0.100, by = 0.005),
  seq(0.110, 0.500, by = 0.010),
  seq(0.550, 0.950, by = 0.050)
)))

tagged_output_name <- function(name) {
  tag <- trimws(PART4_OUTPUT_TAG)
  if (!nzchar(tag)) {
    return(name)
  }
  paste0(name, "_", clean_file_id(tag))
}

assert_new_output_file <- function(path) {
  if (file.exists(path) && !PART4_OVERWRITE_OUTPUTS) {
    stop(
      "Refusing to overwrite existing Part 4 rating output: ",
      path,
      ". Set PART4_OVERWRITE_OUTPUTS=TRUE deliberately to replace it.",
      call. = FALSE
    )
  }
  invisible(path)
}

write_audit <- function(x, name, caption = NULL, digits = 4) {
  output_name <- tagged_output_name(name)
  csv_path <- file.path(AUDIT_DIR, paste0(output_name, ".csv"))
  tex_path <- file.path(TABLE_DIR, paste0(output_name, ".tex"))
  assert_new_output_file(csv_path)
  assert_new_output_file(tex_path)
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
  path <- file.path(DATA_DIR, paste0(tagged_output_name(name), ".rds"))
  assert_new_output_file(path)
  saveRDS(x, path)
  invisible(x)
}

save_partitioned_predictions <- function(x, name) {
  if (nrow(x) == 0L) {
    return(tibble())
  }

  required_cols <- c("Predictor_Set", "Model", "Horizon")
  missing_cols <- setdiff(required_cols, names(x))
  if (length(missing_cols) > 0L) {
    stop("Cannot partition predictions; missing column(s): ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  # Store validation predictions in small reusable chunks for targeted analysis.
  partition_dir <- file.path(DATA_DIR, tagged_output_name(name))
  dir.create(partition_dir, recursive = TRUE, showWarnings = FALSE)

  index_rows <- list()
  x %>%
    arrange(Predictor_Set, Model, Horizon) %>%
    group_by(Predictor_Set, Model, Horizon, Horizon_Label) %>%
    group_walk(
      function(chunk, key) {
      Predictor_Set <- key$Predictor_Set[[1]]
      Model <- key$Model[[1]]
      Horizon <- key$Horizon[[1]]
      Horizon_Label <- key$Horizon_Label[[1]]
      file_name <- paste0(
        clean_file_id(Predictor_Set),
        "__",
        clean_file_id(Model),
        "__h",
        Horizon,
        ".rds"
      )
      file_path <- file.path(partition_dir, file_name)
      assert_new_output_file(file_path)
      saveRDS(chunk, file_path)

      index_rows[[length(index_rows) + 1L]] <<- tibble(
        Predictor_Set = Predictor_Set,
        Model = Model,
        Horizon = Horizon,
        Horizon_Label = Horizon_Label,
        N_Rows = nrow(chunk),
        File_Name = file_name,
        File_Path = normalizePath(file_path, winslash = "/", mustWork = FALSE),
        File_MB = round(file.info(file_path)$size / 1024^2, 4)
      )
    },
    .keep = TRUE
  )
  index <- bind_rows(index_rows)

  index_path <- file.path(DATA_DIR, paste0(tagged_output_name(paste0(name, "_index")), ".csv"))
  assert_new_output_file(index_path)
  readr::write_csv(index, index_path, na = "")
  invisible(index)
}

clean_file_id <- function(x) {
  x %>%
    stringr::str_replace_all("[^A-Za-z0-9_=-]+", "_") %>%
    stringr::str_replace_all("_+", "_") %>%
    stringr::str_sub(1, 180)
}

stable_seed <- function(...) {
  key <- paste(..., sep = "__")
  hash <- digest::digest(key, algo = "xxhash32", serialize = FALSE)
  SEED + (strtoi(substr(hash, 1, 6), base = 16L) %% 1000000L)
}

input_signature <- function(path) {
  info <- file.info(path)
  timestamp <- format(as.POSIXct(info$mtime), "%Y-%m-%d %H:%M:%S")
  paste(normalizePath(path, winslash = "/", mustWork = FALSE), info$size, timestamp, sep = "|")
}

signed_log <- function(x) {
  sign(x) * log1p(abs(x))
}

SIGNED_LOG_EXCLUDE <- c("rating_rank", "rating_number", "macro_rating", "rating_group_B", "rating_group_C")
GAM_FORCE_LINEAR_VARS <- c(
  SIGNED_LOG_EXCLUDE,
  "Downgrade_Previous_4Q",
  "N_Downgrade_Quarters_Previous_4Q",
  "N_Downgrades_Previous_4Q",
  "N_Downgrade_Events_Previous_4Q",
  "Notches_Lost_Previous_4Q",
  "N_Rating_Actions_Previous_4Q",
  "N_Upgrades_Previous_4Q",
  "n_high_vstoxx_last_4q",
  "high_vstoxx_90"
)

fit_preprocessor <- function(train, predictors, use_signed_log = FALSE) {
  x <- train %>%
    select(all_of(predictors)) %>%
    mutate(across(everything(), as.numeric))
  if (use_signed_log) {
    signed_log_vars <- setdiff(names(x), SIGNED_LOG_EXCLUDE)
    if (length(signed_log_vars) > 0L) {
      x <- x %>% mutate(across(all_of(signed_log_vars), signed_log))
    }
  }
  centers <- vapply(x, mean, numeric(1), na.rm = TRUE)
  scales <- vapply(x, stats::sd, numeric(1), na.rm = TRUE)
  centers[!is.finite(centers)] <- 0
  scales[!is.finite(scales) | scales == 0] <- 1
  list(predictors = predictors, use_signed_log = use_signed_log, centers = centers, scales = scales)
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

training_weights <- function(y, strategy) {
  y <- as.integer(y)
  n_pos <- sum(y == 1L)
  n_neg <- sum(y == 0L)
  if (strategy == "none" || n_pos == 0L || n_neg == 0L) {
    return(rep(1, length(y)))
  }
  full_pos <- length(y) / (2 * n_pos)
  full_neg <- length(y) / (2 * n_neg)
  w <- if_else(y == 1L, full_pos, full_neg)
  if (strategy == "sqrt_balance") {
    w <- sqrt(w)
  } else if (strategy == "quarter_balance") {
    w <- 1 + 0.25 * (w - 1)
  } else if (strategy == "half_balance") {
    w <- 1 + 0.50 * (w - 1)
  }
  as.numeric(w / mean(w))
}

prob_metrics <- function(y, p) {
  ok <- is.finite(y) & is.finite(p)
  y <- as.integer(y[ok])
  p <- as.numeric(p[ok])
  eps <- 1e-15
  p_clip <- pmin(pmax(p, eps), 1 - eps)

  roc_auc <- tryCatch(
    as.numeric(pROC::auc(pROC::roc(y, p, quiet = TRUE, levels = c(0, 1), direction = "<"))),
    error = function(e) NA_real_
  )
  pr_auc <- tryCatch(
    PRROC::pr.curve(
      scores.class0 = p[y == 1L],
      scores.class1 = p[y == 0L],
      curve = FALSE
    )$auc.integral,
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

threshold_metrics_one <- function(y, p, threshold) {
  pred <- as.integer(p >= threshold)
  tp <- sum(pred == 1L & y == 1L)
  tn <- sum(pred == 0L & y == 0L)
  fp <- sum(pred == 1L & y == 0L)
  fn <- sum(pred == 0L & y == 1L)

  sensitivity <- if_else(tp + fn > 0, tp / (tp + fn), NA_real_)
  specificity <- if_else(tn + fp > 0, tn / (tn + fp), NA_real_)
  precision <- if_else(tp + fp > 0, tp / (tp + fp), NA_real_)
  f1 <- if_else(
    is.finite(precision + sensitivity) && precision + sensitivity > 0,
    2 * precision * sensitivity / (precision + sensitivity),
    NA_real_
  )
  mcc_den <- sqrt(
    as.numeric(tp + fp) *
      as.numeric(tp + fn) *
      as.numeric(tn + fp) *
      as.numeric(tn + fn)
  )
  mcc <- if_else(mcc_den > 0, ((tp * tn) - (fp * fn)) / mcc_den, NA_real_)

  tibble(
    Threshold = threshold,
    TP = tp,
    TN = tn,
    FP = fp,
    FN = fn,
    Sensitivity = sensitivity,
    Specificity = specificity,
    Precision = precision,
    F1 = f1,
    Balanced_Accuracy = mean(c(sensitivity, specificity), na.rm = TRUE),
    MCC = mcc,
    Predicted_Downgrade_Rate_Pct = 100 * mean(pred == 1L),
    Predicts_Both_Classes = n_distinct(pred) == 2L
  )
}

clip_probability <- function(p, eps = 1e-6) {
  pmin(pmax(as.numeric(p), eps), 1 - eps)
}

logit_probit_residual_diagnostics_one <- function(y, p) {
  ok <- is.finite(y) & is.finite(p)
  y <- as.integer(y[ok])
  p <- clip_probability(p[ok])

  if (length(y) == 0L || length(unique(y)) < 2L) {
    return(tibble(
      N = length(y),
      N_Downgrades = sum(y == 1L),
      Pearson_Residual_Mean = NA_real_,
      Pearson_Residual_SD = NA_real_,
      Pearson_Residual_Max_Abs = NA_real_,
      Pearson_Residual_Abs_GT_2_Pct = NA_real_,
      Pearson_Residual_Abs_GT_3_Pct = NA_real_,
      Calibration_Intercept = NA_real_,
      Calibration_Slope = NA_real_,
      Decile_Calibration_ChiSq = NA_real_,
      Decile_Calibration_DF = NA_real_,
      Decile_Calibration_P = NA_real_,
      Diagnostic_Status = "Insufficient outcome variation"
    ))
  }

  pearson_residual <- (y - p) / sqrt(p * (1 - p))
  logit_p <- qlogis(p)

  intercept_fit <- tryCatch(
    glm(y ~ offset(logit_p), family = binomial()),
    error = function(e) e
  )
  calibration_fit <- tryCatch(
    glm(y ~ logit_p, family = binomial()),
    error = function(e) e
  )

  calibration_groups <- tibble(y = y, p = p) %>%
    mutate(group = ntile(p, min(10L, n()))) %>%
    group_by(group) %>%
    summarise(
      Observed = sum(y),
      Expected = sum(p),
      Variance = sum(p * (1 - p)),
      .groups = "drop"
    ) %>%
    filter(Variance > 0)

  calibration_chisq <- sum((calibration_groups$Observed - calibration_groups$Expected)^2 / calibration_groups$Variance)
  calibration_df <- max(nrow(calibration_groups) - 2L, 1L)

  tibble(
    N = length(y),
    N_Downgrades = sum(y == 1L),
    Pearson_Residual_Mean = mean(pearson_residual),
    Pearson_Residual_SD = sd(pearson_residual),
    Pearson_Residual_Max_Abs = max(abs(pearson_residual)),
    Pearson_Residual_Abs_GT_2_Pct = 100 * mean(abs(pearson_residual) > 2),
    Pearson_Residual_Abs_GT_3_Pct = 100 * mean(abs(pearson_residual) > 3),
    Calibration_Intercept = if (inherits(intercept_fit, "error")) NA_real_ else unname(coef(intercept_fit)[["(Intercept)"]]),
    Calibration_Slope = if (inherits(calibration_fit, "error")) NA_real_ else unname(coef(calibration_fit)[["logit_p"]]),
    Decile_Calibration_ChiSq = calibration_chisq,
    Decile_Calibration_DF = calibration_df,
    Decile_Calibration_P = 1 - pchisq(calibration_chisq, df = calibration_df),
    Diagnostic_Status = "OK"
  )
}

fit_predict_logit_probit <- function(train, valid, predictors, candidate) {
  prep <- fit_preprocessor(train, predictors, use_signed_log = TRUE)
  x_train <- apply_preprocessor(train, prep)
  x_valid <- apply_preprocessor(valid, prep)
  train_model <- bind_cols(tibble(Outcome_Num = train$Outcome_Num), x_train)
  valid_model <- bind_cols(tibble(Outcome_Num = valid$Outcome_Num), x_valid)
  weights <- training_weights(train_model$Outcome_Num, candidate$weight_strategy)

  fit <- glm(
    reformulate(predictors, response = "Outcome_Num"),
    data = train_model,
    family = binomial(link = candidate$link),
    weights = weights
  )
  as.numeric(predict(fit, newdata = valid_model, type = "response"))
}

fit_predict_rf <- function(train, valid, predictors, candidate) {
  train_model <- train %>%
    mutate(Outcome_Factor = factor(Outcome_Num, levels = c(0, 1), labels = c("No_Downgrade", "Downgrade"))) %>%
    select(Outcome_Factor, all_of(predictors))
  valid_model <- valid %>% select(all_of(predictors))
  n_train <- nrow(train_model)
  min_node_size <- max(5L, round(n_train * candidate$min_node_share))
  p <- length(predictors)
  mtry <- case_when(
    candidate$mtry_mode == "sqrtp" ~ floor(sqrt(p)),
    candidate$mtry_mode == "p3" ~ floor(p / 3),
    candidate$mtry_mode == "p2" ~ floor(p / 2),
    TRUE ~ floor(sqrt(p))
  )
  case_weights <- training_weights(train$Outcome_Num, candidate$weight_strategy)

  fit <- ranger::ranger(
    dependent.variable.name = "Outcome_Factor",
    data = train_model,
    probability = TRUE,
    num.trees = candidate$num_trees,
    mtry = max(1L, min(p, mtry)),
    min.node.size = min_node_size,
    sample.fraction = candidate$sample_fraction,
    replace = TRUE,
    case.weights = case_weights,
    seed = stable_seed(candidate$candidate_id, n_train),
    num.threads = MODEL_THREADS
  )
  as.numeric(predict(fit, data = valid_model)$predictions[, "Downgrade"])
}

fit_predict_xgb <- function(train, valid, predictors, candidate) {
  x_train <- as.matrix(train %>% select(all_of(predictors)))
  x_valid <- as.matrix(valid %>% select(all_of(predictors)))
  w_train <- training_weights(train$Outcome_Num, candidate$weight_strategy)
  dtrain <- xgboost::xgb.DMatrix(data = x_train, label = train$Outcome_Num, weight = w_train)
  dvalid <- xgboost::xgb.DMatrix(data = x_valid)

  fit <- xgboost::xgb.train(
    params = list(
      objective = "binary:logistic",
      eval_metric = "logloss",
      max_depth = candidate$max_depth,
      eta = candidate$eta,
      min_child_weight = candidate$min_child_weight,
      lambda = candidate$lambda,
      alpha = candidate$alpha,
      gamma = candidate$gamma,
      subsample = candidate$subsample,
      colsample_bytree = candidate$colsample_bytree,
      seed = stable_seed(candidate$candidate_id),
      nthread = MODEL_THREADS
    ),
    data = dtrain,
    nrounds = candidate$nrounds,
    verbose = 0
  )
  as.numeric(predict(fit, dvalid))
}

fit_predict_elastic_net <- function(train, valid, predictors, candidate) {
  prep <- fit_preprocessor(train, predictors, use_signed_log = TRUE)
  x_train <- as.matrix(apply_preprocessor(train, prep))
  x_valid <- as.matrix(apply_preprocessor(valid, prep))
  w_train <- training_weights(train$Outcome_Num, candidate$weight_strategy)

  fit <- glmnet::glmnet(
    x = x_train,
    y = train$Outcome_Num,
    family = "binomial",
    alpha = candidate$alpha,
    lambda = candidate$lambda,
    weights = w_train,
    standardize = FALSE,
    intercept = TRUE
  )
  as.numeric(predict(fit, newx = x_valid, s = candidate$lambda, type = "response"))
}

gam_smooth_variables <- function(predictors, scope, train_model, gam_k) {
  core_vars <- c("wc_ta", "re_ta", "ebit_ta", "td_ta", "mc_td", "ROA_pc")
  smooth_eligible <- setdiff(predictors, GAM_FORCE_LINEAR_VARS)
  smooth_eligible <- smooth_eligible[vapply(smooth_eligible, function(v) {
    n_distinct(train_model[[v]][is.finite(train_model[[v]])]) > gam_k
  }, logical(1))]
  if (scope == "all") {
    return(smooth_eligible)
  }
  intersect(core_vars, smooth_eligible)
}

fit_predict_gam <- function(train, valid, predictors, candidate) {
  prep <- fit_preprocessor(train, predictors, use_signed_log = TRUE)
  train_model <- bind_cols(tibble(Outcome_Num = train$Outcome_Num), apply_preprocessor(train, prep))
  valid_model <- bind_cols(tibble(Outcome_Num = valid$Outcome_Num), apply_preprocessor(valid, prep))
  smooth_vars <- gam_smooth_variables(predictors, candidate$gam_scope, train_model, candidate$gam_k)
  linear_vars <- setdiff(predictors, smooth_vars)
  smooth_terms <- paste0("s(", smooth_vars, ", k = ", candidate$gam_k, ", bs = 'cs')")
  rhs_terms <- c(smooth_terms, linear_vars)
  if (length(rhs_terms) == 0L) {
    stop("No GAM predictors available.", call. = FALSE)
  }
  w_train <- training_weights(train$Outcome_Num, candidate$weight_strategy)

  fit <- mgcv::gam(
    as.formula(paste("Outcome_Num ~", paste(rhs_terms, collapse = " + "))),
    data = train_model,
    family = binomial(link = "logit"),
    weights = w_train,
    method = "REML",
    select = TRUE,
    control = mgcv::gam.control(maxit = 100)
  )
  as.numeric(predict(fit, newdata = valid_model, type = "response"))
}

add_rating_features <- function(data) {
  required_rating_cols <- c("rating_rank", "rating_group", "macro_rating", "rating_grade")
  missing_rating_cols <- setdiff(required_rating_cols, names(data))
  if (length(missing_rating_cols) > 0L) {
    stop("Missing rating column(s): ", paste(missing_rating_cols, collapse = ", "), call. = FALSE)
  }

  data %>%
    mutate(
      rating_rank = as.numeric(rating_rank),
      macro_rating = as.numeric(macro_rating),
      rating_grade = factor(rating_grade, levels = c("Investment", "Speculative")),
      rating_group_B = as.integer(rating_group == "B"),
      rating_group_C = as.integer(rating_group == "C")
    )
}

make_regressor_manifest <- function(data) {
  # V5 specifications are controlled upstream in Part 3; no legacy fallback
  # specifications are generated here.
  tibble(Regressor_Set = character(), Variables = character())
}

build_candidate_grid <- function(models_to_run = ALL_MODELS) {
  weight_options <- c("none", "sqrt_balance", "full_balance")
  rf_weight_options <- c("none", "sqrt_balance")
  xgb_weight_options <- c("none", "sqrt_balance", "half_balance")
  elastic_net_weight_options <- c("none", "sqrt_balance", "full_balance")
  gam_weight_options <- c("none", "sqrt_balance", "full_balance")

  logit_grid <- tibble(
    Model = "Logit",
    link = "logit",
    weight_strategy = weight_options,
    candidate_id = paste0("LOGIT_weight=", weight_strategy),
    candidate_complexity = 1
  )

  probit_grid <- tibble(
    Model = "Probit",
    link = "probit",
    weight_strategy = weight_options,
    candidate_id = paste0("PROBIT_weight=", weight_strategy),
    candidate_complexity = 1
  )

  rf_grid <- tidyr::expand_grid(
    num_trees = c(500L, 1000L),
    mtry_mode = "sqrtp",
    min_node_share = c(0.010, 0.015),
    sample_fraction = 0.80,
    weight_strategy = rf_weight_options
  ) %>%
    mutate(
      Model = "Random_Forest",
      link = NA_character_,
      candidate_id = paste0(
        "RF_trees=", num_trees,
        "_mtry=", mtry_mode,
        "_minshare=", min_node_share,
        "_sample=", sample_fraction,
        "_weight=", weight_strategy
      ),
      candidate_complexity =
        log(num_trees) +
        case_when(mtry_mode == "sqrtp" ~ 1, mtry_mode == "p3" ~ 2, TRUE ~ 3) +
        (0.01 / min_node_share) +
        if_else(sample_fraction >= 1, 1, 0)
    )

  xgb_grid <- tidyr::expand_grid(
    nrounds = c(100L, 300L),
    max_depth = c(1L, 2L),
    eta = c(0.03, 0.05),
    min_child_weight = c(25, 50),
    subsample = 0.80,
    colsample_bytree = 0.80,
    weight_strategy = c("none", "sqrt_balance"),
    lambda = c(1, 10),
    alpha = 0,
    gamma = 0
  ) %>%
    mutate(
      Model = "XGBoost",
      link = NA_character_,
      candidate_id = paste0(
        "XGB_rounds=", nrounds,
        "_depth=", max_depth,
        "_eta=", eta,
        "_child=", min_child_weight,
        "_sub=", subsample,
        "_col=", colsample_bytree,
        "_weight=", weight_strategy,
        "_lambda=", lambda,
        "_alpha=", alpha,
        "_gamma=", gamma
      ),
      candidate_complexity =
        (nrounds * max_depth / eta) / min_child_weight +
        if_else(weight_strategy == "none", 0, if_else(weight_strategy == "sqrt_balance", 1, 2)) +
        (1 / lambda)
    )

  elastic_net_grid <- tidyr::expand_grid(
    alpha = c(0, 0.25, 0.50, 0.75, 1.00),
    lambda = c(0.0001, 0.0003, 0.001, 0.003, 0.010, 0.030, 0.100),
    weight_strategy = elastic_net_weight_options
  ) %>%
    mutate(
      Model = "Elastic_Net",
      link = "logit",
      candidate_id = paste0(
        "EN_alpha=", alpha,
        "_lambda=", lambda,
        "_weight=", weight_strategy
      ),
      candidate_complexity =
        (1 + alpha) * log1p(1 / lambda) +
        case_when(
          weight_strategy == "none" ~ 0,
          weight_strategy == "sqrt_balance" ~ 0.5,
          TRUE ~ 1
        )
    )

  gam_grid <- tidyr::expand_grid(
    gam_scope = c("core", "all"),
    gam_k = c(4L, 5L),
    weight_strategy = gam_weight_options
  ) %>%
    mutate(
      Model = "GAM",
      link = "logit",
      candidate_id = paste0(
        "GAM_scope=", gam_scope,
        "_k=", gam_k,
        "_weight=", weight_strategy
      ),
      candidate_complexity =
        if_else(gam_scope == "core", 6, 12) * (gam_k - 1) +
        case_when(
          weight_strategy == "none" ~ 0,
          weight_strategy == "sqrt_balance" ~ 0.5,
          TRUE ~ 1
        )
    )

  bind_rows(logit_grid, probit_grid, rf_grid, xgb_grid, elastic_net_grid, gam_grid) %>%
    mutate(across(where(is.numeric), as.numeric)) %>%
    filter(Model %in% models_to_run) %>%
    arrange(Model, candidate_id)
}

hyperparameter_grid_summary <- function(models_to_run = ALL_MODELS) {
  tribble(
    ~Model, ~Hyperparameter, ~Candidate_Values,
    "Logit", "Link function", "Logit",
    "Logit", "Class-weight strategy", "None; square-root balance; full balance",
    "Probit", "Link function", "Probit",
    "Probit", "Class-weight strategy", "None; square-root balance; full balance",
    "Random Forest", "Number of trees", "500; 1,000",
    "Random Forest", "Variables sampled at each split", "sqrt(p)",
    "Random Forest", "Minimum terminal-node size", "1.00%; 1.50% of training observations",
    "Random Forest", "Sampling fraction", "0.80",
    "Random Forest", "Class-weight strategy", "None; square-root balance",
    "XGBoost", "Boosting rounds", "100; 300",
    "XGBoost", "Maximum tree depth", "1; 2",
    "XGBoost", "Learning rate", "0.03; 0.05",
    "XGBoost", "Minimum child weight", "25; 50",
    "XGBoost", "Subsample ratio", "0.80",
    "XGBoost", "Column subsample ratio", "0.80",
    "XGBoost", "Class-weight strategy", "None; square-root balance",
    "XGBoost", "L2 regularization lambda", "1; 10",
    "XGBoost", "L1 regularization alpha", "0",
    "XGBoost", "Minimum loss reduction gamma", "0",
    "Elastic Net", "Link function", "Logit",
    "Elastic Net", "Alpha mixing parameter", "0; 0.25; 0.50; 0.75; 1.00",
    "Elastic Net", "Penalty lambda", "0.0001; 0.0003; 0.001; 0.003; 0.010; 0.030; 0.100",
    "Elastic Net", "Class-weight strategy", "None; square-root balance; full balance",
    "GAM", "Link function", "Logit",
    "GAM", "Smooth scope", "Core financial predictors; all financial predictors",
    "GAM", "Smooth basis dimension k", "4; 5",
    "GAM", "Smooth basis", "Shrinkage cubic regression spline",
    "GAM", "Class-weight strategy", "None; square-root balance; full balance"
  ) %>%
    filter(Model %in% c(models_to_run, stringr::str_replace_all(models_to_run, "_", " ")))
}

candidate_cache_path <- function(predictor_set, horizon, candidate_id) {
  file.path(
    CACHE_DIR,
    paste0(
      "h", horizon,
      "__", clean_file_id(predictor_set),
      "__", clean_file_id(candidate_id),
      ".rds"
    )
  )
}

format_timestamp <- function(x) {
  format(as.POSIXct(x), "%Y-%m-%d %H:%M:%S")
}

cache_status_one <- function(path, input_sig) {
  if (!file.exists(path)) {
    return("Missing")
  }

  cached <- tryCatch(readRDS(path), error = function(e) e)
  if (inherits(cached, "error")) {
    return("Unreadable")
  }
  if (identical(cached$cache_version, CACHE_VERSION) && identical(cached$input_signature, input_sig)) {
    "Completed_Current"
  } else {
    "Stale"
  }
}

cache_progress_table <- function(jobs, context) {
  purrr::pmap_dfr(
    jobs,
    function(Regressor_Set, Horizon, Candidate_Row, Model, candidate_id, Model_Candidate_Number, Model_Candidate_Total) {
      cache_path <- candidate_cache_path(Regressor_Set, Horizon, candidate_id)
      tibble(
        Regressor_Set = Regressor_Set,
        Horizon = Horizon,
        Model = Model,
        Candidate_ID = candidate_id,
        Cache_Path = cache_path,
        Cache_Status = cache_status_one(cache_path, context$input_sig)
      )
    }
  )
}

write_progress_snapshot <- function(jobs, context, model_tag, run_id, label) {
  progress <- cache_progress_table(jobs, context)
  summary <- progress %>%
    count(Model, Regressor_Set, Horizon, Cache_Status, name = "N_Jobs") %>%
    group_by(Model, Regressor_Set, Horizon) %>%
    mutate(
      Total_Jobs = sum(N_Jobs),
      Completed_Current = sum(if_else(Cache_Status == "Completed_Current", N_Jobs, 0L)),
      Missing_Or_Stale = Total_Jobs - Completed_Current,
      Completion_Pct = 100 * Completed_Current / Total_Jobs
    ) %>%
    ungroup() %>%
    arrange(Model, Regressor_Set, Horizon, Cache_Status)

  readr::write_csv(
    progress,
    file.path(TIMING_DIR, paste0("part4_progress_jobs_", model_tag, "_", run_id, "_", label, ".csv")),
    na = ""
  )
  readr::write_csv(
    summary,
    file.path(TIMING_DIR, paste0("part4_progress_summary_", model_tag, "_", run_id, "_", label, ".csv")),
    na = ""
  )

  summary
}

progress_by_model_specification <- function(progress_summary) {
  if (nrow(progress_summary) == 0L) {
    return(tibble())
  }

  progress_summary %>%
    distinct(Model, Regressor_Set, Horizon, Total_Jobs, Completed_Current, Missing_Or_Stale) %>%
    group_by(Model, Regressor_Set) %>%
    summarise(
      Total_Jobs = sum(Total_Jobs),
      Completed_Current = sum(Completed_Current),
      Missing_Or_Stale = sum(Missing_Or_Stale),
      Completion_Pct = 100 * Completed_Current / Total_Jobs,
      .groups = "drop"
    ) %>%
    arrange(Model, Regressor_Set)
}

print_progress_summary <- function(progress_summary, label) {
  progress_by_spec <- progress_by_model_specification(progress_summary)
  if (nrow(progress_by_spec) == 0L) {
    message("[PROGRESS ", label, "] No jobs to report.")
    return(invisible(progress_by_spec))
  }

  total_jobs <- sum(progress_by_spec$Total_Jobs)
  completed <- sum(progress_by_spec$Completed_Current)
  missing <- sum(progress_by_spec$Missing_Or_Stale)
  message(
    "[PROGRESS ",
    label,
    "] ",
    completed,
    "/",
    total_jobs,
    " jobs completed/current; ",
    missing,
    " missing or stale."
  )
  purrr::pwalk(
    progress_by_spec,
    function(Model, Regressor_Set, Total_Jobs, Completed_Current, Missing_Or_Stale, Completion_Pct) {
      message(
        "  - ",
        Model,
        " | ",
        Regressor_Set,
        ": ",
        Completed_Current,
        "/",
        Total_Jobs,
        " completed/current; ",
        Missing_Or_Stale,
        " missing or stale (",
        round(Completion_Pct, 1),
        "%)"
      )
    }
  )
  invisible(progress_by_spec)
}

job_timing_path <- function(run_id, job_i, job, candidate) {
  file.path(
    TIMING_DIR,
    paste0(
      "timing_",
      clean_file_id(run_id),
      "_job_",
      sprintf("%04d", job_i),
      "_",
      clean_file_id(candidate$Model),
      "_",
      clean_file_id(job$Regressor_Set),
      "_h",
      job$Horizon,
      "_",
      clean_file_id(candidate$candidate_id),
      ".csv"
    )
  )
}

count_run_timing_files <- function(run_id) {
  length(list.files(
    TIMING_DIR,
    pattern = paste0("^timing_", clean_file_id(run_id), "_job_.*\\.csv$")
  ))
}

write_job_timing <- function(row, run_id, job_i, job, candidate) {
  readr::write_csv(row, job_timing_path(run_id, job_i, job, candidate), na = "")
  invisible(row)
}

read_timing_rows <- function(run_id = NULL) {
  aggregate_pattern <- if (is.null(run_id)) {
    "^part4_timing_jobs_.*\\.csv$"
  } else {
    paste0("^part4_timing_jobs_.*_", clean_file_id(run_id), "\\.csv$")
  }
  aggregate_files <- list.files(TIMING_DIR, pattern = aggregate_pattern, full.names = TRUE)
  if (length(aggregate_files) > 0L) {
    return(purrr::map_dfr(aggregate_files, readr::read_csv, show_col_types = FALSE))
  }

  pattern <- if (is.null(run_id)) {
    "^timing_.*_job_.*\\.csv$"
  } else {
    paste0("^timing_", clean_file_id(run_id), "_job_.*\\.csv$")
  }
  files <- list.files(TIMING_DIR, pattern = pattern, full.names = TRUE)
  if (length(files) == 0L) {
    return(tibble())
  }
  purrr::map_dfr(files, function(path) {
    tryCatch(
      readr::read_csv(path, show_col_types = FALSE),
      error = function(e) {
        warning("Skipping unreadable timing file: ", path, " (", conditionMessage(e), ")")
        tibble()
      }
    )
  })
}

timing_summary_by_model_specification <- function(timing_rows) {
  if (nrow(timing_rows) == 0L) {
    return(tibble())
  }

  timing_rows %>%
    mutate(
      Start_Time_POSIX = as.POSIXct(Start_Time),
      End_Time_POSIX = as.POSIXct(End_Time)
    ) %>%
    group_by(Model, Regressor_Set) %>%
    summarise(
      N_Jobs = n(),
      N_Estimated = sum(Job_Status == "Estimated", na.rm = TRUE),
      N_Cache_Hit = sum(Job_Status == "Cache_Hit", na.rm = TRUE),
      N_Failed = sum(Job_Status == "Failed", na.rm = TRUE),
      Start_Time = format_timestamp(min(Start_Time_POSIX, na.rm = TRUE)),
      End_Time = format_timestamp(max(End_Time_POSIX, na.rm = TRUE)),
      Wall_Clock_Minutes = as.numeric(difftime(max(End_Time_POSIX, na.rm = TRUE), min(Start_Time_POSIX, na.rm = TRUE), units = "mins")),
      Sum_Job_Minutes = sum(Elapsed_Seconds, na.rm = TRUE) / 60,
      .groups = "drop"
    ) %>%
    arrange(Model, Regressor_Set)
}

prepare_part4_context <- function(models_to_run = ALL_MODELS) {
  if (!file.exists(INPUT_RDS)) {
    stop("Missing required Part 3 input: ", INPUT_RDS, call. = FALSE)
  }

  fold_data <- readRDS(INPUT_RDS) %>%
    add_rating_features()
  required_columns <- c(
    "Observation_ID", "Country", "firm_id", "Predictor_Date", "Target_Date",
    "Horizon", "Horizon_Label", "Fold", "Fold_Role", "Outcome_Num"
  )
  stopifnot(all(required_columns %in% names(fold_data)))
  stopifnot(all(fold_data$Fold_Role %in% c("Training", "Validation")))
  stopifnot(all(fold_data$Outcome_Num %in% c(0L, 1L)))

  saved_regressor_manifest <- if (file.exists(REGRESSOR_MANIFEST_CSV)) {
    readr::read_csv(REGRESSOR_MANIFEST_CSV, show_col_types = FALSE)
  } else {
    tibble(Regressor_Set = character(), Variables = character())
  }

  # Part 4 is restricted to the V5 specification block selected at runtime.
  # Rating-group specifications are represented by B/C dummies in the model matrix.
  regressor_manifest <- bind_rows(
    saved_regressor_manifest,
    make_regressor_manifest(fold_data)
  ) %>%
    distinct(Regressor_Set, .keep_all = TRUE)

  regressor_sets <- regressor_manifest %>%
    filter(Regressor_Set %in% REGRESSOR_SETS_TO_RUN) %>%
    mutate(
      Predictors = purrr::map(
        stringr::str_split(Variables, ",\\s*"),
        ~ unique(unlist(purrr::map(.x, function(v) {
          if (identical(v, "rating_group")) c("rating_group_B", "rating_group_C") else v
        })))
      ),
      Variables = purrr::map_chr(Predictors, ~ paste(.x, collapse = ", "))
    )

  if (nrow(regressor_sets) == 0L) {
    stop("No requested regressor set found in the regressor manifest.", call. = FALSE)
  }

  if (any(str_detect(regressor_sets$Regressor_Set, "rating_history"))) {
    stop("Rating-history specifications are excluded from V5 Part 4.", call. = FALSE)
  }

  missing_predictors <- regressor_sets %>%
    tidyr::unnest(Predictors) %>%
    filter(!Predictors %in% names(fold_data))
  if (nrow(missing_predictors) > 0L) {
    stop("Missing predictors in Part 3 data: ", paste(unique(missing_predictors$Predictors), collapse = ", "), call. = FALSE)
  }

  banned_predictors <- c("env_score", "g_score")
  esg_predictors <- regressor_sets %>%
    tidyr::unnest(Predictors) %>%
    filter(Predictors %in% banned_predictors)
  if (nrow(esg_predictors) > 0L) {
    stop(
      "ESG predictors are excluded from Part 4 main specifications because of missingness: ",
      paste(unique(esg_predictors$Predictors), collapse = ", "),
      call. = FALSE
    )
  }

  candidate_grid <- build_candidate_grid(models_to_run)
  if (nrow(candidate_grid) == 0L) {
    stop("No candidates found for models: ", paste(models_to_run, collapse = ", "), call. = FALSE)
  }

  list(
    fold_data = fold_data,
    input_sig = input_signature(INPUT_RDS),
    regressor_sets = regressor_sets,
    candidate_grid = candidate_grid
  )
}

make_prediction_jobs <- function(regressor_sets, candidate_grid) {
  tidyr::expand_grid(
    Regressor_Set = regressor_sets$Regressor_Set,
    Horizon = HORIZONS,
    Candidate_Row = seq_len(nrow(candidate_grid))
  ) %>%
    left_join(
      candidate_grid %>%
        mutate(Candidate_Row = row_number()) %>%
        select(Candidate_Row, Model, candidate_id),
      by = "Candidate_Row"
    ) %>%
    group_by(Regressor_Set, Horizon, Model) %>%
    arrange(candidate_id, .by_group = TRUE) %>%
    mutate(
      Model_Candidate_Number = row_number(),
      Model_Candidate_Total = n()
    ) %>%
    ungroup()
}

run_candidate <- function(data_h, folds, predictor_set, predictors, candidate, input_sig) {
  cache_path <- candidate_cache_path(predictor_set, candidate$Horizon, candidate$candidate_id)
  if (!FORCE_REFIT && file.exists(cache_path)) {
    cached <- readRDS(cache_path)
    if (identical(cached$cache_version, CACHE_VERSION) && identical(cached$input_signature, input_sig)) {
      return(cached$predictions)
    }
  }

  fold_predictions <- vector("list", length(folds))
  for (i in seq_along(folds)) {
    fold_i <- folds[[i]]
    train <- data_h %>% filter(Fold == fold_i, Fold_Role == "Training")
    valid <- data_h %>% filter(Fold == fold_i, Fold_Role == "Validation")

    set.seed(stable_seed(candidate$candidate_id, predictor_set, candidate$Horizon, fold_i))
    pred <- tryCatch(
      {
        if (candidate$Model %in% c("Logit", "Probit")) {
          fit_predict_logit_probit(train, valid, predictors, candidate)
        } else if (candidate$Model == "Random_Forest") {
          fit_predict_rf(train, valid, predictors, candidate)
        } else if (candidate$Model == "XGBoost") {
          fit_predict_xgb(train, valid, predictors, candidate)
        } else if (candidate$Model == "Elastic_Net") {
          fit_predict_elastic_net(train, valid, predictors, candidate)
        } else if (candidate$Model == "GAM") {
          fit_predict_gam(train, valid, predictors, candidate)
        } else {
          stop("Unknown model: ", candidate$Model, call. = FALSE)
        }
      },
      error = function(e) {
        message("Candidate failed: ", candidate$candidate_id, " / ", fold_i, " / ", e$message)
        rep(NA_real_, nrow(valid))
      }
    )

    fold_predictions[[i]] <- valid %>%
      transmute(
        Observation_ID,
        Country,
        firm_id,
        Predictor_Date,
        Target_Date,
        Horizon,
        Horizon_Label,
        Fold,
        Outcome_Num,
        Predictor_Set = predictor_set,
        Model = candidate$Model,
        Candidate_ID = candidate$candidate_id,
        Probability = pmin(pmax(as.numeric(pred), 0), 1),
        Fit_Status = if_else(all(is.na(pred)), "Failed", "OK")
      )
  }

  out <- bind_rows(fold_predictions)
  saveRDS(
    list(cache_version = CACHE_VERSION, input_signature = input_sig, predictions = out),
    cache_path
  )
  out
}

run_prediction_job <- function(job_i, jobs, context) {
  job <- jobs[job_i, ]
  predictor_set <- job$Regressor_Set
  predictors <- context$regressor_sets$Predictors[[match(predictor_set, context$regressor_sets$Regressor_Set)]]
  candidate <- context$candidate_grid[job$Candidate_Row, ] %>% mutate(Horizon = job$Horizon)
  cache_path <- candidate_cache_path(predictor_set, candidate$Horizon, candidate$candidate_id)
  cache_status_before <- cache_status_one(cache_path, context$input_sig)
  data_h <- context$fold_data %>% filter(Horizon == job$Horizon)
  folds <- data_h %>%
    filter(Fold_Role == "Validation") %>%
    distinct(Fold) %>%
    arrange(Fold) %>%
    pull(Fold)

  start_time <- Sys.time()
  message(
    "[START ",
    job_i,
    "/",
    nrow(jobs),
    "] ",
    candidate$Model,
    " configuration ", job$Model_Candidate_Number, " of ", job$Model_Candidate_Total,
    " | predictor set: ", predictor_set,
    " | horizon: t+", job$Horizon,
    " | ", candidate$candidate_id,
    " | cache before: ", cache_status_before,
    " | start: ", format_timestamp(start_time)
  )

  out <- tryCatch(
    run_candidate(data_h, folds, predictor_set, predictors, candidate, context$input_sig),
    error = function(e) {
      end_time <- Sys.time()
      timing_row <- tibble(
        Run_ID = context$run_id,
        Job_Index = job_i,
        Job_Total = nrow(jobs),
        Cache_Version = CACHE_VERSION,
        Input_Signature = context$input_sig,
        Model = candidate$Model,
        Regressor_Set = predictor_set,
        Horizon = job$Horizon,
        Horizon_Label = paste0("t+", job$Horizon),
        Candidate_ID = candidate$candidate_id,
        Cache_Status_Before = cache_status_before,
        Cache_Status_After = cache_status_one(cache_path, context$input_sig),
        Job_Status = "Failed",
        Start_Time = format_timestamp(start_time),
        End_Time = format_timestamp(end_time),
        Elapsed_Seconds = as.numeric(difftime(end_time, start_time, units = "secs")),
        N_Validation_Prediction_Rows = NA_integer_,
        N_Failed_Prediction_Rows = NA_integer_,
        Error_Message = conditionMessage(e)
      )
      write_job_timing(timing_row, context$run_id, job_i, job, candidate)
      completed_jobs <- count_run_timing_files(context$run_id)
      progress_pct <- 100 * completed_jobs / nrow(jobs)
      message(
        "[FAILED ", job_i, "/", nrow(jobs), "] ",
        candidate$Model, " | ", predictor_set, " | h", job$Horizon,
        " | elapsed: ", round(timing_row$Elapsed_Seconds / 60, 2), " min",
        " | completed/missing jobs in this run: ", completed_jobs, "/", nrow(jobs),
        " completed, ", nrow(jobs) - completed_jobs, " missing",
        " | progress: ", round(progress_pct, 2), "%"
      )
      stop(e)
    }
  )

  end_time <- Sys.time()
  job_status <- case_when(
    cache_status_before == "Completed_Current" ~ "Cache_Hit",
    nrow(out) > 0L && all(out$Fit_Status == "Failed") ~ "Failed",
    TRUE ~ "Estimated"
  )
  timing_row <- tibble(
    Run_ID = context$run_id,
    Job_Index = job_i,
    Job_Total = nrow(jobs),
    Cache_Version = CACHE_VERSION,
    Input_Signature = context$input_sig,
    Model = candidate$Model,
    Regressor_Set = predictor_set,
    Horizon = job$Horizon,
    Horizon_Label = paste0("t+", job$Horizon),
    Candidate_ID = candidate$candidate_id,
    Cache_Status_Before = cache_status_before,
    Cache_Status_After = cache_status_one(cache_path, context$input_sig),
    Job_Status = job_status,
    Start_Time = format_timestamp(start_time),
    End_Time = format_timestamp(end_time),
    Elapsed_Seconds = as.numeric(difftime(end_time, start_time, units = "secs")),
    N_Validation_Prediction_Rows = nrow(out),
    N_Failed_Prediction_Rows = sum(out$Fit_Status == "Failed", na.rm = TRUE),
    Error_Message = NA_character_
  )
  write_job_timing(timing_row, context$run_id, job_i, job, candidate)

  completed_jobs <- count_run_timing_files(context$run_id)
  progress_pct <- 100 * completed_jobs / nrow(jobs)
  message(
    "[DONE ",
    job_i,
    "/",
    nrow(jobs),
    "] ",
    candidate$Model,
    " | ", predictor_set,
    " | h", job$Horizon,
    " | status: ", job_status,
    " | elapsed: ", round(timing_row$Elapsed_Seconds / 60, 2), " min",
    " | completed/missing jobs in this run: ", completed_jobs, "/", nrow(jobs),
    " completed, ", nrow(jobs) - completed_jobs, " missing",
    " | progress: ", round(progress_pct, 2), "%"
  )

  out
}

run_part4_estimation <- function(models_to_run, run_in_parallel = RUN_IN_PARALLEL, n_workers = N_WORKERS) {
  context <- prepare_part4_context(models_to_run)
  jobs <- make_prediction_jobs(context$regressor_sets, context$candidate_grid)
  model_tag <- clean_file_id(paste(models_to_run, collapse = "_"))
  run_id <- clean_file_id(paste0(model_tag, "_", format(Sys.time(), "%Y%m%d_%H%M%S")))
  context$run_id <- run_id

  progress_before <- write_progress_snapshot(jobs, context, model_tag, run_id, "before")
  print_progress_summary(progress_before, paste0("before run ", run_id))
  message(
    "[PART4 START] run_id=", run_id,
    " | models=", paste(models_to_run, collapse = ","),
    " | regressor_sets=", nrow(context$regressor_sets),
    " | total_jobs=", nrow(jobs),
    " | parallel=", run_in_parallel,
    " | workers=", if_else(run_in_parallel, n_workers, 1L),
    " | model_threads=", MODEL_THREADS
  )

  if (run_in_parallel && n_workers > 1L && nrow(jobs) > 1L) {
    cl <- parallel::makeCluster(n_workers, outfile = "")
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterSetRNGStream(cl, SEED)
    parallel::clusterEvalQ(cl, {
      invisible(lapply(
        c("tidyverse", "glmnet", "mgcv", "ranger", "xgboost", "pROC", "PRROC", "digest", "scales"),
        library,
        character.only = TRUE
      ))
    })
    parallel::clusterExport(
      cl,
      varlist = c(
        "SEED", "CACHE_VERSION", "FORCE_REFIT", "MODEL_THREADS", "CACHE_DIR", "TIMING_DIR", "SIGNED_LOG_EXCLUDE",
        "GAM_FORCE_LINEAR_VARS",
        "clean_file_id", "stable_seed", "signed_log", "fit_preprocessor",
        "apply_preprocessor", "training_weights", "prob_metrics",
        "threshold_metrics_one", "fit_predict_logit_probit",
        "fit_predict_rf", "fit_predict_xgb", "fit_predict_elastic_net",
        "gam_smooth_variables", "fit_predict_gam", "candidate_cache_path",
        "format_timestamp", "cache_status_one", "job_timing_path",
        "count_run_timing_files", "write_job_timing",
        "run_candidate", "run_prediction_job"
      ),
      envir = .GlobalEnv
    )
    parallel::clusterExport(
      cl,
      varlist = c("jobs", "context"),
      envir = environment()
    )
    predictions <- parallel::parLapply(
      cl,
      seq_len(nrow(jobs)),
      function(job_i) run_prediction_job(job_i, jobs, context)
    )
  } else {
    predictions <- map(seq_len(nrow(jobs)), run_prediction_job, jobs = jobs, context = context)
  }

  validation_predictions <- bind_rows(predictions)
  if (PART4_SAVE_MEGA_VALIDATION_PREDICTIONS) {
    save_data(validation_predictions, paste0("Panel_Europe_macro_validation_predictions_", model_tag))
  } else {
    message("Skipping estimation-level mega validation-prediction file; cached job-level predictions remain available.")
  }

  timing_rows <- read_timing_rows(run_id)
  timing_summary <- timing_summary_by_model_specification(timing_rows)
  timing_jobs_path <- file.path(TIMING_DIR, paste0("part4_timing_jobs_", model_tag, "_", run_id, ".csv"))
  timing_summary_path <- file.path(TIMING_DIR, paste0("part4_timing_summary_", model_tag, "_", run_id, ".csv"))
  readr::write_csv(timing_rows, timing_jobs_path, na = "")
  readr::write_csv(timing_summary, timing_summary_path, na = "")

  progress_after <- write_progress_snapshot(jobs, context, model_tag, run_id, "after")
  print_progress_summary(progress_after, paste0("after run ", run_id))
  completed_jobs_final <- count_run_timing_files(run_id)
  message(
    "[PART4 COMPLETE] run_id=", run_id,
    " | completed_jobs=", completed_jobs_final, "/", nrow(jobs),
    " | progress=", round(100 * completed_jobs_final / nrow(jobs), 2), "%",
    " | timing_dir=", TIMING_DIR
  )

  notes <- c(
    paste0("Part 4 estimation completed for: ", paste(models_to_run, collapse = ", ")),
    paste0("Run ID: ", run_id),
    paste0("Predictor sets: ", paste(REGRESSOR_SETS_TO_RUN, collapse = ", ")),
    paste0("Parallel execution: ", run_in_parallel),
    paste0("Parallel workers: ", if_else(run_in_parallel, n_workers, 1L)),
    paste0("Threads per RF/XGBoost fit: ", MODEL_THREADS),
    paste0("Candidate jobs: ", nrow(jobs)),
    paste0("Timing job log: ", timing_jobs_path),
    paste0("Timing model/specification summary: ", timing_summary_path),
    "Candidate-level validation predictions are cached under the rating Part 4 output cache folder.",
    "Run the matching Part 4 collect wrapper after the model-specific scripts finish."
  )
  notes_path <- file.path(OUTPUT_DIR, paste0("model_tuning_notes_", model_tag, ".txt"))
  assert_new_output_file(notes_path)
  writeLines(notes, notes_path)
  invisible(validation_predictions)
}

read_cached_predictions <- function(jobs, context, allow_missing = FALSE) {
  cached_predictions <- vector("list", nrow(jobs))
  missing_cache <- character()

  for (job_i in seq_len(nrow(jobs))) {
    job <- jobs[job_i, ]
    candidate <- context$candidate_grid[job$Candidate_Row, ] %>% mutate(Horizon = job$Horizon)
    path <- candidate_cache_path(job$Regressor_Set, job$Horizon, candidate$candidate_id)

    if (!file.exists(path)) {
      missing_cache <- c(missing_cache, path)
      next
    }

    cached <- readRDS(path)
    if (!identical(cached$cache_version, CACHE_VERSION) || !identical(cached$input_signature, context$input_sig)) {
      missing_cache <- c(missing_cache, path)
      next
    }

    cached_predictions[[job_i]] <- cached$predictions
  }

  if (length(missing_cache) > 0L && !allow_missing) {
    stop(
      "Missing or stale cache files. Re-run Part 4 estimation for the requested V5 block. First missing file: ",
      missing_cache[[1]],
      call. = FALSE
    )
  }

  bind_rows(cached_predictions)
}

partition_cached_predictions <- function(models_to_partition = ALL_MODELS, allow_missing = FALSE) {
  context <- prepare_part4_context(models_to_partition)
  jobs <- make_prediction_jobs(context$regressor_sets, context$candidate_grid)
  groups <- jobs %>%
    distinct(Regressor_Set, Horizon, Model) %>%
    arrange(Regressor_Set, Model, Horizon)

  all_dir <- file.path(DATA_DIR, tagged_output_name("Panel_Europe_macro_all_validation_predictions_by_group"))
  selected_dir <- file.path(DATA_DIR, tagged_output_name("Panel_Europe_macro_selected_validation_predictions_by_group"))
  dir.create(all_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(selected_dir, recursive = TRUE, showWarnings = FALSE)

  selected_specs_path <- file.path(DATA_DIR, paste0(tagged_output_name("Panel_Europe_macro_selected_model_specifications"), ".rds"))
  selected_specs <- if (file.exists(selected_specs_path)) {
    readRDS(selected_specs_path)
  } else {
    message("Selected model specifications not found; selected-prediction partitions will be skipped.")
    tibble()
  }

  all_index_rows <- list()
  selected_index_rows <- list()

  for (group_i in seq_len(nrow(groups))) {
    group <- groups[group_i, ]
    group_jobs <- jobs %>%
      filter(
        .data$Regressor_Set == group$Regressor_Set,
        .data$Horizon == group$Horizon,
        .data$Model == group$Model
      )
    chunk <- read_cached_predictions(group_jobs, context, allow_missing = allow_missing)
    file_name <- paste0(
      clean_file_id(group$Regressor_Set),
      "__",
      clean_file_id(group$Model),
      "__h",
      group$Horizon,
      ".rds"
    )
    all_path <- file.path(all_dir, file_name)
    assert_new_output_file(all_path)
    saveRDS(chunk, all_path)

    all_index_rows[[length(all_index_rows) + 1L]] <- tibble(
      Predictor_Set = group$Regressor_Set,
      Model = group$Model,
      Horizon = group$Horizon,
      Horizon_Label = unique(chunk$Horizon_Label)[[1]],
      N_Rows = nrow(chunk),
      File_Name = file_name,
      File_Path = normalizePath(all_path, winslash = "/", mustWork = FALSE),
      File_MB = round(file.info(all_path)$size / 1024^2, 4)
    )

    if (nrow(selected_specs) > 0L) {
      selected_candidate_ids <- selected_specs %>%
        filter(
          .data$Predictor_Set == group$Regressor_Set,
          .data$Horizon == group$Horizon,
          .data$Model == group$Model
        ) %>%
        pull(Candidate_ID)

      if (length(selected_candidate_ids) > 0L) {
        selected_chunk <- chunk %>%
          filter(.data$Candidate_ID %in% selected_candidate_ids)
        selected_path <- file.path(selected_dir, file_name)
        assert_new_output_file(selected_path)
        saveRDS(selected_chunk, selected_path)

        selected_index_rows[[length(selected_index_rows) + 1L]] <- tibble(
          Predictor_Set = group$Regressor_Set,
          Model = group$Model,
          Horizon = group$Horizon,
          Horizon_Label = unique(selected_chunk$Horizon_Label)[[1]],
          N_Rows = nrow(selected_chunk),
          File_Name = file_name,
          File_Path = normalizePath(selected_path, winslash = "/", mustWork = FALSE),
          File_MB = round(file.info(selected_path)$size / 1024^2, 4)
        )
      }
    }

    if (group_i %% 25L == 0L || group_i == nrow(groups)) {
      message("[PART4 PARTITION] ", group_i, "/", nrow(groups), " groups written")
    }
  }

  all_index <- bind_rows(all_index_rows)
  all_index_path <- file.path(DATA_DIR, paste0(tagged_output_name("Panel_Europe_macro_all_validation_predictions_by_group_index"), ".csv"))
  assert_new_output_file(all_index_path)
  readr::write_csv(all_index, all_index_path, na = "")
  write_audit(
    all_index,
    "table_11_all_validation_prediction_file_index",
    "Partitioned validation-prediction files by predictor set, model, and horizon"
  )

  selected_index <- bind_rows(selected_index_rows)
  if (nrow(selected_index) > 0L) {
    selected_index_path <- file.path(DATA_DIR, paste0(tagged_output_name("Panel_Europe_macro_selected_validation_predictions_by_group_index"), ".csv"))
    assert_new_output_file(selected_index_path)
    readr::write_csv(selected_index, selected_index_path, na = "")
    write_audit(
      selected_index,
      "table_12_selected_validation_prediction_file_index",
      "Partitioned selected-candidate validation-prediction files by predictor set, model, and horizon"
    )
  }

  invisible(list(all_index = all_index, selected_index = selected_index))
}

selection_rule_label <- function(rule) {
  switch(
    rule,
    max_pr_auc = "Max validation PR-AUC; ties broken by ROC-AUC, Brier, Log Loss, then candidate complexity",
    near_best_parsimony = "Near-best validation PR-AUC within tolerance; ties broken by candidate complexity, PR-AUC, ROC-AUC, Brier, then Log Loss",
    near_best_calibrated = "Near-best validation PR-AUC within tolerance; ties broken by Brier, Log Loss, PR-AUC, ROC-AUC, then candidate complexity",
    stop("Unknown Part 4 selection rule: ", rule, call. = FALSE)
  )
}

select_hyperparameters_by_rule <- function(probability_metrics_scored, rule) {
  selection_pool <- probability_metrics_scored %>%
    group_by(Predictor_Set, Horizon, Horizon_Label, Model)

  selected <- switch(
    rule,
    max_pr_auc = selection_pool %>%
      arrange(
        desc(PR_AUC),
        desc(ROC_AUC),
        Brier,
        Log_Loss,
        candidate_complexity,
        .by_group = TRUE
      ),
    near_best_parsimony = selection_pool %>%
      filter(.data$Near_Best_PR_AUC %in% TRUE) %>%
      arrange(
        candidate_complexity,
        desc(PR_AUC),
        desc(ROC_AUC),
        Brier,
        Log_Loss,
        .by_group = TRUE
      ),
    near_best_calibrated = selection_pool %>%
      filter(.data$Near_Best_PR_AUC %in% TRUE) %>%
      arrange(
        Brier,
        Log_Loss,
        desc(PR_AUC),
        desc(ROC_AUC),
        candidate_complexity,
        .by_group = TRUE
      ),
    stop("Unknown Part 4 selection rule: ", rule, call. = FALSE)
  )

  selected %>%
    slice(1L) %>%
    ungroup() %>%
    mutate(
      Selection_Rule_ID = rule,
      Selection_Rule = selection_rule_label(rule)
    )
}

build_selection_outputs <- function(rule, selected_hyperparameters, validation_predictions) {
  # Reuse cached validation predictions; only the candidate-selection layer changes.
  selected_predictions <- validation_predictions %>%
    semi_join(
      selected_hyperparameters %>% select(Predictor_Set, Horizon, Model, Candidate_ID),
      by = c("Predictor_Set", "Horizon", "Model", "Candidate_ID")
    )

  threshold_results <- selected_predictions %>%
    group_by(Predictor_Set, Horizon, Horizon_Label, Model, Candidate_ID) %>%
    group_modify(~ map_dfr(
      THRESHOLD_GRID,
      function(threshold_i) threshold_metrics_one(.x$Outcome_Num, .x$Probability, threshold_i)
    )) %>%
    ungroup() %>%
    mutate(Selection_Rule_ID = rule)

  selected_thresholds <- threshold_results %>%
    group_by(Predictor_Set, Horizon, Horizon_Label, Model, Candidate_ID) %>%
    arrange(desc(Predicts_Both_Classes), desc(MCC), desc(F1), desc(Balanced_Accuracy), Threshold, .by_group = TRUE) %>%
    slice(1L) %>%
    ungroup()

  selected_fold_performance <- selected_predictions %>%
    group_by(Predictor_Set, Horizon, Horizon_Label, Model, Candidate_ID, Fold) %>%
    group_modify(~ prob_metrics(.x$Outcome_Num, .x$Probability)) %>%
    ungroup() %>%
    mutate(Selection_Rule_ID = rule)

  logit_probit_residual_diagnostics <- selected_predictions %>%
    filter(Model %in% c("Logit", "Probit")) %>%
    group_by(Predictor_Set, Horizon, Horizon_Label, Model, Candidate_ID) %>%
    group_modify(~ logit_probit_residual_diagnostics_one(.x$Outcome_Num, .x$Probability)) %>%
    ungroup() %>%
    mutate(Selection_Rule_ID = rule) %>%
    arrange(Predictor_Set, Horizon, Model)

  selected_model_specifications <- selected_hyperparameters %>%
    left_join(
      selected_thresholds %>%
        select(Predictor_Set, Horizon, Model, Candidate_ID, Threshold, MCC, F1, Balanced_Accuracy),
      by = c("Predictor_Set", "Horizon", "Model", "Candidate_ID")
    ) %>%
    arrange(Predictor_Set, Horizon, Model)

  list(
    selected_hyperparameters = selected_hyperparameters,
    selected_predictions = selected_predictions,
    threshold_results = threshold_results,
    selected_thresholds = selected_thresholds,
    selected_fold_performance = selected_fold_performance,
    logit_probit_residual_diagnostics = logit_probit_residual_diagnostics,
    selected_model_specifications = selected_model_specifications
  )
}

collect_part4_results <- function(models_to_collect = ALL_MODELS, allow_missing = FALSE) {
  context <- prepare_part4_context(models_to_collect)
  jobs <- make_prediction_jobs(context$regressor_sets, context$candidate_grid)
  validation_predictions <- read_cached_predictions(jobs, context, allow_missing = allow_missing)

  if (nrow(validation_predictions) == 0L) {
    stop("No cached validation predictions found.", call. = FALSE)
  }

  candidate_lookup <- jobs %>%
    transmute(Regressor_Set, Horizon, Candidate_Row) %>%
    left_join(
      context$candidate_grid %>% mutate(Candidate_Row = row_number()),
      by = "Candidate_Row"
    ) %>%
    select(-Candidate_Row)

  probability_metrics <- validation_predictions %>%
    filter(Fit_Status == "OK") %>%
    group_by(Predictor_Set, Horizon, Horizon_Label, Model, Candidate_ID) %>%
    group_modify(~ prob_metrics(.x$Outcome_Num, .x$Probability)) %>%
    ungroup() %>%
    left_join(
      candidate_lookup,
      by = c(
        "Predictor_Set" = "Regressor_Set",
        "Horizon",
        "Model",
        "Candidate_ID" = "candidate_id"
      )
    )

  probability_metrics_scored <- probability_metrics %>%
    group_by(Predictor_Set, Horizon, Horizon_Label, Model) %>%
    mutate(
      Best_PR_AUC = max(PR_AUC, na.rm = TRUE),
      PR_AUC_Gap_From_Best = Best_PR_AUC - PR_AUC,
      PR_AUC_Near_Best_Tolerance = pmax(
        PR_AUC_NEAR_BEST_ABS_TOLERANCE,
        PR_AUC_NEAR_BEST_REL_TOLERANCE * Best_PR_AUC
      ),
      Near_Best_PR_AUC = PR_AUC_Gap_From_Best <= PR_AUC_Near_Best_Tolerance,
      Complexity_Rank = dense_rank(candidate_complexity)
    ) %>%
    ungroup()

  selected_hyperparameters_by_rule <- setNames(
    lapply(
      PART4_SELECTION_RULES,
      function(rule) select_hyperparameters_by_rule(probability_metrics_scored, rule)
    ),
    PART4_SELECTION_RULES
  )
  selection_outputs_by_rule <- purrr::imap(
    selected_hyperparameters_by_rule,
    ~ build_selection_outputs(.y, .x, validation_predictions)
  )
  primary_selection_outputs <- selection_outputs_by_rule[[PART4_SELECTION_RULE]]
  selected_hyperparameters <- primary_selection_outputs$selected_hyperparameters
  selected_predictions <- primary_selection_outputs$selected_predictions
  threshold_results <- primary_selection_outputs$threshold_results
  selected_thresholds <- primary_selection_outputs$selected_thresholds
  selected_fold_performance <- primary_selection_outputs$selected_fold_performance
  logit_probit_residual_diagnostics <- primary_selection_outputs$logit_probit_residual_diagnostics
  selected_model_specifications <- primary_selection_outputs$selected_model_specifications

  all_selected_hyperparameters <- bind_rows(purrr::map(selection_outputs_by_rule, "selected_hyperparameters"))
  all_selected_thresholds <- bind_rows(purrr::map(selection_outputs_by_rule, "selected_thresholds"))
  all_selected_fold_performance <- bind_rows(purrr::map(selection_outputs_by_rule, "selected_fold_performance"))
  all_logit_probit_residual_diagnostics <- bind_rows(purrr::map(selection_outputs_by_rule, "logit_probit_residual_diagnostics"))
  all_selected_model_specifications <- bind_rows(purrr::map(selection_outputs_by_rule, "selected_model_specifications"))

  candidate_grid_by_model <- context$candidate_grid %>%
    count(Model, name = "Candidates_Per_Horizon")
  fold_audit <- context$fold_data %>%
    group_by(Horizon, Horizon_Label, Fold, Fold_Role) %>%
    summarise(
      N_Observations = n(),
      N_Downgrades = sum(Outcome_Num),
      Downgrade_Rate_Pct = 100 * mean(Outcome_Num),
      First_Target_Date = min(Target_Date),
      Last_Target_Date = max(Target_Date),
      .groups = "drop"
    )
  validation_fold_counts <- fold_audit %>%
    filter(Fold_Role == "Validation") %>%
    distinct(Horizon, Fold) %>%
    count(Horizon, name = "Validation_Folds")
  estimation_counts <- candidate_grid_by_model %>%
    mutate(
      Predictor_Sets = nrow(context$regressor_sets),
      Horizons = length(HORIZONS),
      Validation_Folds_Per_Horizon = paste(validation_fold_counts$Validation_Folds, collapse = ", "),
      Validation_Estimations =
        Candidates_Per_Horizon * nrow(context$regressor_sets) * sum(validation_fold_counts$Validation_Folds),
      Final_Test_Estimations = length(HORIZONS) * nrow(context$regressor_sets)
    )
  timing_rows <- read_timing_rows()
  timing_summary <- timing_summary_by_model_specification(timing_rows)

  write_audit(hyperparameter_grid_summary(models_to_collect), "table_00_hyperparameter_grid_summary", "Hyperparameter grids used for model tuning")
  write_audit(
    context$regressor_sets %>%
      transmute(
        Regressor_Set,
        N_Predictors = lengths(Predictors),
        Variables
      ),
    "table_00b_regressor_sets",
    "Predictor sets used in model tuning"
  )
  write_audit(candidate_grid_by_model, "table_01_candidate_counts", "Candidate specifications per horizon")
  write_audit(fold_audit, "table_02_validation_fold_audit", "Expanding-window tuning fold audit")
  write_audit(estimation_counts, "table_03_estimation_counts", "Number of model estimations implied by the tuning design")
  write_audit(
    probability_metrics_scored %>%
      select(
        Predictor_Set, Horizon_Label, Model, Candidate_ID, N, N_Downgrades,
        PR_AUC, Best_PR_AUC, PR_AUC_Gap_From_Best, PR_AUC_Near_Best_Tolerance,
        Near_Best_PR_AUC, Complexity_Rank, candidate_complexity,
        ROC_AUC, Brier, Log_Loss
      ),
    "table_04_all_probability_metrics",
    "Pooled validation probability metrics for all candidates with near-best PR-AUC diagnostics"
  )
  write_audit(
    selected_hyperparameters %>%
      select(
        Predictor_Set, Horizon_Label, Model, Candidate_ID, N, N_Downgrades,
        PR_AUC, Best_PR_AUC, PR_AUC_Gap_From_Best, PR_AUC_Near_Best_Tolerance,
        candidate_complexity, ROC_AUC, Brier, Log_Loss, Selection_Rule
      ),
    "table_05_selected_hyperparameters",
    "Selected hyperparameters by horizon and model using validation PR-AUC"
  )
  write_audit(
    selected_thresholds %>% select(Predictor_Set, Horizon_Label, Model, Candidate_ID, Threshold, Sensitivity, Specificity, Precision, F1, Balanced_Accuracy, MCC, Predicted_Downgrade_Rate_Pct),
    "table_06_selected_thresholds",
    "Selected validation thresholds by horizon and model"
  )
  write_audit(
    selected_fold_performance %>% select(Predictor_Set, Horizon_Label, Model, Fold, N, N_Downgrades, PR_AUC, ROC_AUC, Brier, Log_Loss),
    "table_07_selected_fold_probability_metrics",
    "Fold-level probability metrics for selected candidates"
  )
  write_audit(
    logit_probit_residual_diagnostics,
    "table_07b_logit_probit_residual_diagnostics",
    "Out-of-validation residual and calibration diagnostics for selected Logit and Probit candidates"
  )
  write_audit(
    all_selected_model_specifications %>%
      select(
        Selection_Rule_ID, Predictor_Set, Horizon_Label, Model, Candidate_ID,
        PR_AUC, Best_PR_AUC, PR_AUC_Gap_From_Best, PR_AUC_Near_Best_Tolerance,
        Near_Best_PR_AUC, Complexity_Rank, candidate_complexity,
        ROC_AUC, Brier, Log_Loss, Threshold, MCC, F1, Balanced_Accuracy,
        Selection_Rule
      ),
    "table_05b_selected_model_specifications_all_selection_rules",
    "Selected model specifications under each validation-selection rule"
  )
  for (selection_rule_i in PART4_SELECTION_RULES) {
    selection_outputs_i <- selection_outputs_by_rule[[selection_rule_i]]
    write_audit(
      selection_outputs_i$selected_hyperparameters %>%
        select(
          Selection_Rule_ID, Predictor_Set, Horizon_Label, Model, Candidate_ID,
          N, N_Downgrades, PR_AUC, Best_PR_AUC, PR_AUC_Gap_From_Best,
          PR_AUC_Near_Best_Tolerance, Near_Best_PR_AUC, Complexity_Rank,
          candidate_complexity, ROC_AUC, Brier, Log_Loss, Selection_Rule
        ),
      paste0("table_05_selected_hyperparameters_", selection_rule_i),
      paste0("Selected hyperparameters using validation rule: ", selection_rule_i)
    )
    write_audit(
      selection_outputs_i$selected_thresholds %>%
        select(
          Selection_Rule_ID, Predictor_Set, Horizon_Label, Model, Candidate_ID,
          Threshold, Sensitivity, Specificity, Precision, F1,
          Balanced_Accuracy, MCC, Predicted_Downgrade_Rate_Pct
        ),
      paste0("table_06_selected_thresholds_", selection_rule_i),
      paste0("Selected validation thresholds using validation rule: ", selection_rule_i)
    )
    write_audit(
      selection_outputs_i$selected_fold_performance %>%
        select(
          Selection_Rule_ID, Predictor_Set, Horizon_Label, Model, Candidate_ID,
          Fold, N, N_Downgrades, PR_AUC, ROC_AUC, Brier, Log_Loss
        ),
      paste0("table_07_selected_fold_probability_metrics_", selection_rule_i),
      paste0("Fold-level probability metrics using validation rule: ", selection_rule_i)
    )
    write_audit(
      selection_outputs_i$logit_probit_residual_diagnostics,
      paste0("table_07b_logit_probit_residual_diagnostics_", selection_rule_i),
      paste0("Logit/Probit residual diagnostics using validation rule: ", selection_rule_i)
    )
  }
  if (nrow(timing_rows) > 0L) {
    write_audit(
      timing_rows %>% arrange(Model, Regressor_Set, Horizon, Candidate_ID, Job_Index),
      "table_09_part_4_timing_jobs",
      "Part 4 job-level timing log with start and end time for each model, specification, horizon, and candidate"
    )
    write_audit(
      timing_summary,
      "table_10_part_4_timing_by_model_specification",
      "Part 4 timing summary by model and specification"
    )
  }

  if (PART4_SAVE_SPLIT_VALIDATION_PREDICTIONS) {
    split_index <- save_partitioned_predictions(
      validation_predictions,
      "Panel_Europe_macro_all_validation_predictions_by_group"
    )
    write_audit(
      split_index,
      "table_11_all_validation_prediction_file_index",
      "Partitioned validation-prediction files by predictor set, model, and horizon"
    )
  } else {
    split_index <- tibble()
  }
  if (PART4_SAVE_MEGA_VALIDATION_PREDICTIONS) {
    save_data(validation_predictions, "Panel_Europe_macro_all_validation_predictions")
  } else {
    message("Skipping collected mega validation-prediction file; set PART4_SAVE_MEGA_VALIDATION_PREDICTIONS=TRUE to save it.")
  }
  save_data(context$candidate_grid, "Panel_Europe_macro_candidate_grid")
  save_data(probability_metrics_scored, "Panel_Europe_macro_all_probability_metrics")
  save_data(selected_hyperparameters, "Panel_Europe_macro_selected_hyperparameters")
  save_data(threshold_results, "Panel_Europe_macro_threshold_results")
  save_data(selected_thresholds, "Panel_Europe_macro_selected_thresholds")
  save_data(all_selected_hyperparameters, "Panel_Europe_macro_selected_hyperparameters_all_selection_rules")
  save_data(all_selected_thresholds, "Panel_Europe_macro_selected_thresholds_all_selection_rules")
  save_data(all_selected_fold_performance, "Panel_Europe_macro_selected_fold_performance_all_selection_rules")
  save_data(all_logit_probit_residual_diagnostics, "Panel_Europe_macro_logit_probit_residual_diagnostics_all_selection_rules")
  save_data(all_selected_model_specifications, "Panel_Europe_macro_selected_model_specifications_all_selection_rules")
  for (selection_rule_i in PART4_SELECTION_RULES) {
    selection_outputs_i <- selection_outputs_by_rule[[selection_rule_i]]
    save_data(selection_outputs_i$selected_hyperparameters, paste0("Panel_Europe_macro_selected_hyperparameters_", selection_rule_i))
    save_data(selection_outputs_i$threshold_results, paste0("Panel_Europe_macro_threshold_results_", selection_rule_i))
    save_data(selection_outputs_i$selected_thresholds, paste0("Panel_Europe_macro_selected_thresholds_", selection_rule_i))
    save_data(selection_outputs_i$selected_fold_performance, paste0("Panel_Europe_macro_selected_fold_performance_", selection_rule_i))
    save_data(selection_outputs_i$selected_model_specifications, paste0("Panel_Europe_macro_selected_model_specifications_", selection_rule_i))
    save_data(selection_outputs_i$logit_probit_residual_diagnostics, paste0("Panel_Europe_macro_logit_probit_residual_diagnostics_", selection_rule_i))
  }
  if (PART4_SAVE_SPLIT_SELECTED_VALIDATION_PREDICTIONS) {
    selected_split_index <- save_partitioned_predictions(
      selected_predictions,
      "Panel_Europe_macro_selected_validation_predictions_by_group"
    )
    write_audit(
      selected_split_index,
      "table_12_selected_validation_prediction_file_index",
      "Partitioned selected-candidate validation-prediction files by predictor set, model, and horizon"
    )
    selected_split_indices_by_rule <- purrr::imap(
      selection_outputs_by_rule,
      function(selection_outputs_i, selection_rule_i) {
        split_index_i <- save_partitioned_predictions(
          selection_outputs_i$selected_predictions,
          paste0("Panel_Europe_macro_selected_validation_predictions_by_group_", selection_rule_i)
        )
        write_audit(
          split_index_i,
          paste0("table_12_selected_validation_prediction_file_index_", selection_rule_i),
          paste0("Partitioned selected-candidate validation-prediction files using validation rule: ", selection_rule_i)
        )
        split_index_i
      }
    )
  } else {
    selected_split_index <- tibble()
    selected_split_indices_by_rule <- setNames(
      rep(list(tibble()), length(PART4_SELECTION_RULES)),
      PART4_SELECTION_RULES
    )
  }
  save_data(selected_predictions, "Panel_Europe_macro_selected_validation_predictions")
  save_data(selected_model_specifications, "Panel_Europe_macro_selected_model_specifications")
  save_data(logit_probit_residual_diagnostics, "Panel_Europe_macro_logit_probit_residual_diagnostics")
  for (selection_rule_i in PART4_SELECTION_RULES) {
    save_data(
      selection_outputs_by_rule[[selection_rule_i]]$selected_predictions,
      paste0("Panel_Europe_macro_selected_validation_predictions_", selection_rule_i)
    )
  }
  save_data(timing_rows, "Panel_Europe_macro_part_4_timing_jobs")
  save_data(timing_summary, "Panel_Europe_macro_part_4_timing_by_model_specification")

  ggplot(selected_hyperparameters, aes(x = Horizon_Label, y = PR_AUC, fill = Model)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.65) +
    facet_wrap(~ Predictor_Set) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.001)) +
    labs(x = NULL, y = "Validation PR-AUC", fill = NULL) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
  fig_01_path <- file.path(FIGURE_DIR, paste0(tagged_output_name("fig_01_selected_validation_pr_auc"), ".png"))
  assert_new_output_file(fig_01_path)
  ggsave(fig_01_path, width = 8, height = 5, dpi = 300)

  ggplot(selected_hyperparameters, aes(x = Horizon_Label, y = ROC_AUC, fill = Model)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.65) +
    facet_wrap(~ Predictor_Set) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.001)) +
    labs(x = NULL, y = "Validation ROC-AUC", fill = NULL) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
  fig_02_path <- file.path(FIGURE_DIR, paste0(tagged_output_name("fig_02_selected_validation_roc_auc"), ".png"))
  assert_new_output_file(fig_02_path)
  ggsave(fig_02_path, width = 8, height = 5, dpi = 300)

  ggplot(selected_thresholds, aes(x = Horizon_Label, y = MCC, fill = Model)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.65) +
    facet_wrap(~ Predictor_Set) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.001)) +
    labs(x = NULL, y = "Validation MCC at selected threshold", fill = NULL) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
  fig_03_path <- file.path(FIGURE_DIR, paste0(tagged_output_name("fig_03_selected_validation_mcc"), ".png"))
  assert_new_output_file(fig_03_path)
  ggsave(fig_03_path, width = 8, height = 5, dpi = 300)

  part_4_manifest <- tibble(
    Setting = c(
      "Input dataset",
      "Cache version",
      "Seed",
      "Regressor sets",
      "Models collected",
      "Horizons",
      "Main-specification rule",
      "Primary hyperparameter criterion",
      "Primary selection rule id",
      "Saved selection rule ids",
      "Threshold criterion",
      "Logit/Probit diagnostics",
      "Timing/progress tracking",
      "Mega validation-prediction files",
      "Partitioned all validation-prediction files",
      "Partitioned selected validation-prediction files",
      "Locked test used in Part 4"
    ),
    Value = c(
      basename(INPUT_RDS),
      CACHE_VERSION,
      as.character(SEED),
      paste(REGRESSOR_SETS_TO_RUN, collapse = ", "),
      paste(models_to_collect, collapse = ", "),
      paste(paste0("t+", HORIZONS), collapse = ", "),
      paste0("V5 specification block: ", PART4_SPEC_BLOCK),
      selection_rule_label(PART4_SELECTION_RULE),
      PART4_SELECTION_RULE,
      paste(PART4_SELECTION_RULES, collapse = ", "),
      "Pooled expanding-window validation MCC",
      "Logit/Probit residual diagnostics are reported only when those models are included in the collect run",
      "Job-level start/end times and before/after cache completion snapshots are saved under the V5 Part 4 timing folder",
      if_else(PART4_SAVE_MEGA_VALIDATION_PREDICTIONS, "Saved", "Skipped by default; enable with PART4_SAVE_MEGA_VALIDATION_PREDICTIONS=TRUE"),
      if_else(PART4_SAVE_SPLIT_VALIDATION_PREDICTIONS, "Saved by predictor set, model, and horizon", "Skipped"),
      if_else(PART4_SAVE_SPLIT_SELECTED_VALIDATION_PREDICTIONS, "Saved by predictor set, model, and horizon", "Skipped"),
      "No"
    )
  )
  write_audit(part_4_manifest, "table_08_part_4_manifest", "Part 4 tuning manifest")

  tuning_artifact <- list(
    manifest = part_4_manifest,
    regressor_sets = context$regressor_sets,
    candidate_grid = context$candidate_grid,
    probability_metrics = probability_metrics_scored,
    selection_rules_saved = PART4_SELECTION_RULES,
    primary_selection_rule = PART4_SELECTION_RULE,
    selection_outputs_by_rule = selection_outputs_by_rule,
    selected_hyperparameters_all_selection_rules = all_selected_hyperparameters,
    selected_thresholds_all_selection_rules = all_selected_thresholds,
    selected_model_specifications_all_selection_rules = all_selected_model_specifications,
    selected_hyperparameters = selected_hyperparameters,
    selected_thresholds = selected_thresholds,
    selected_model_specifications = selected_model_specifications,
    all_validation_prediction_file_index = split_index,
    selected_validation_prediction_file_index = selected_split_index,
    selected_validation_prediction_file_index_by_rule = selected_split_indices_by_rule,
    logit_probit_residual_diagnostics = logit_probit_residual_diagnostics,
    logit_probit_residual_diagnostics_all_selection_rules = all_logit_probit_residual_diagnostics,
    timing_jobs = timing_rows,
    timing_by_model_specification = timing_summary,
    threshold_grid = THRESHOLD_GRID,
    input_signature = context$input_sig,
    cache_version = CACHE_VERSION
  )
  save_data(tuning_artifact, "Panel_Europe_macro_part_4_tuning_artifact")

  notes <- c(
    "Part 4 tuning results collected.",
    "",
    paste0("Input dataset: ", INPUT_RDS),
    paste0("Regressor sets: ", paste(REGRESSOR_SETS_TO_RUN, collapse = ", ")),
    paste0("Models collected: ", paste(models_to_collect, collapse = ", ")),
    paste0("Primary selection rule id: ", PART4_SELECTION_RULE),
    paste0("Primary selection rule: ", selection_rule_label(PART4_SELECTION_RULE)),
    paste0("Saved selection rule ids: ", paste(PART4_SELECTION_RULES, collapse = ", ")),
    paste0("Mega validation-prediction files: ", if_else(PART4_SAVE_MEGA_VALIDATION_PREDICTIONS, "saved", "skipped")),
    paste0("Partitioned validation-prediction files: ", if_else(PART4_SAVE_SPLIT_VALIDATION_PREDICTIONS, "saved", "skipped")),
    "Timing and progress logs are saved under the V5 Part 4 timing folder and collected when available.",
    "The locked test sample is not used in Part 4."
  )
  notes_path <- file.path(OUTPUT_DIR, paste0(tagged_output_name("model_tuning_notes_collected"), ".txt"))
  assert_new_output_file(notes_path)
  writeLines(notes, notes_path)

  invisible(tuning_artifact)
}

running_as_script <- function() {
  file_arg <- "--file="
  args <- commandArgs(trailingOnly = FALSE)
  any(startsWith(args, file_arg))
}

if (running_as_script() && PART4_ACTION != "none") {
  models_to_run <- ALL_MODELS
  message("Part 4 V5 action: ", PART4_ACTION)
  message("Part 4 V5 specification block: ", PART4_SPEC_BLOCK)
  message("Part 4 V5 regressor sets: ", paste(REGRESSOR_SETS_TO_RUN, collapse = ", "))

  if (PART4_ACTION %in% c("estimate", "both")) {
    run_part4_estimation(models_to_run)
  }
  if (PART4_ACTION %in% c("collect", "both")) {
    collect_part4_results(models_to_run)
  }
  if (PART4_ACTION == "partition") {
    partition_cached_predictions(models_to_run)
  }
}


