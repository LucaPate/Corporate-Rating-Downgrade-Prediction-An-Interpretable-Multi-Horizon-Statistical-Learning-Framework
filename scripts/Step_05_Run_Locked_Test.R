# Step 05 - Refit selected specifications and evaluate them on the locked test sample.
# GitHub-ready copy: paths are repository-relative and data files are intentionally excluded.

# Refits the selected V5 specifications on the
# pre-test training sample, evaluates them on the locked test sample, and saves
# fitted model objects plus model-interpretation summaries.
# The specification block is inherited from Part 4 through PART4_SPEC_BLOCK
# so Part 5 can use the same block-specific tuning outputs.

get_script_dir_part5 <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) > 0L) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/")))
  }

  # Resolve the script path also when Part 5 is sourced during audit checks.
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
  normalizePath(getwd(), winslash = "/")
}

PART5_SCRIPT_DIR <- get_script_dir_part5()
V5_DIR <- normalizePath(file.path(PART5_SCRIPT_DIR, ".."), winslash = "/", mustWork = TRUE)
RATING_EXPERIMENT_DIR <- V5_DIR

previous_part4_action <- Sys.getenv("PART4_ACTION", unset = NA_character_)
Sys.setenv(PART4_ACTION = "none")
source(file.path(PART5_SCRIPT_DIR, "Step_04_Tune_Models_Expanding_Window.R"))
if (is.na(previous_part4_action)) {
  Sys.unsetenv("PART4_ACTION")
} else {
  Sys.setenv(PART4_ACTION = previous_part4_action)
}

PART5_RATING_SPEC <- PART4_SPEC_BLOCK
part5_spec_suffix <- function(spec) {
  if (spec %in% c("none")) "" else paste0("_", spec)
}

part5_normalize_selection_rule <- function(rule) {
  rule <- tolower(trimws(rule))
  recode(
    rule,
    best_pr_auc = "max_pr_auc",
    near_best = "near_best_parsimony",
    near_best_pr_auc = "near_best_parsimony",
    calibrated = "near_best_calibrated",
    .default = rule
  )
}

PART5_SELECTION_RULE <- part5_normalize_selection_rule(
  Sys.getenv("PART5_SELECTION_RULE", PART4_SELECTION_RULE)
)
if (!PART5_SELECTION_RULE %in% PART4_ALLOWED_SELECTION_RULES) {
  stop(
    "PART5_SELECTION_RULE must be one of: ",
    paste(PART4_ALLOWED_SELECTION_RULES, collapse = ", "),
    ".",
    call. = FALSE
  )
}

part5_selection_suffix <- function(selection_rule) {
  paste0("_", clean_file_id(selection_rule))
}

PART5_OUTPUT_DIR <- file.path(
  RATING_EXPERIMENT_DIR,
  paste0(
    "Part_5",
    part5_spec_suffix(PART5_RATING_SPEC),
    "_Outputs",
    part5_selection_suffix(PART5_SELECTION_RULE)
  )
)
PART5_DATA_DIR <- file.path(PART5_OUTPUT_DIR, "data")
PART5_AUDIT_DIR <- file.path(PART5_OUTPUT_DIR, "audits")
PART5_TABLE_DIR <- file.path(PART5_OUTPUT_DIR, "tables_latex")
PART5_FIGURE_DIR <- file.path(PART5_OUTPUT_DIR, "figures")
PART5_MODEL_DIR <- file.path(PART5_OUTPUT_DIR, "models")
PART5_INTERPRETATION_DIR <- file.path(PART5_OUTPUT_DIR, "model_interpretation")
PART5_OVERWRITE_OUTPUTS <- tolower(Sys.getenv("PART5_OVERWRITE_OUTPUTS", "FALSE")) %in% c("true", "t", "1", "yes")
part5_env_flag <- function(name, default = FALSE) {
  tolower(trimws(Sys.getenv(name, if_else(default, "TRUE", "FALSE")))) %in% c("true", "t", "1", "yes")
}
PART5_SAVE_MEGA_LOCKED_TEST_PREDICTIONS <- part5_env_flag("PART5_SAVE_MEGA_LOCKED_TEST_PREDICTIONS", FALSE)
PART5_SAVE_SPLIT_LOCKED_TEST_PREDICTIONS <- part5_env_flag("PART5_SAVE_SPLIT_LOCKED_TEST_PREDICTIONS", TRUE)
PART5_SAVE_PREDICTIONS_IN_ARTIFACT <- part5_env_flag("PART5_SAVE_PREDICTIONS_IN_ARTIFACT", FALSE)

dir.create(PART5_DATA_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(PART5_AUDIT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(PART5_TABLE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(PART5_FIGURE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(PART5_MODEL_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(PART5_INTERPRETATION_DIR, recursive = TRUE, showWarnings = FALSE)

PART5_LOCKED_INPUT_RDS <- file.path(
  V5_DIR,
  "Part_3_Outputs",
  "data_imputed",
  "Panel_Europe_macro_locked_test_imputed.rds"
)
PART5_PART4_EXPANDING_INPUT_RDS <- file.path(
  V5_DIR,
  "Part_3_Outputs",
  "data_imputed",
  "Panel_Europe_macro_expanding_window_folds_imputed.rds"
)
part5_rating_output_name <- function(stem, extension) {
  tag <- clean_file_id(PART4_OUTPUT_TAG)
  suffix <- if (nzchar(tag)) paste0("_", tag) else ""
  paste0(stem, suffix, ".", extension)
}

part5_selection_output_name <- function(stem, extension, selection_rule = PART5_SELECTION_RULE) {
  part5_rating_output_name(paste0(stem, "_", clean_file_id(selection_rule)), extension)
}

part5_selection_output_path <- function(subdir, stem, extension, selection_rule = PART5_SELECTION_RULE, allow_max_pr_auc_legacy = TRUE) {
  base_dir <- file.path(
    RATING_EXPERIMENT_DIR,
    paste0("Part_4", part5_spec_suffix(PART5_RATING_SPEC), "_Outputs"),
    subdir
  )
  selection_path <- file.path(base_dir, part5_selection_output_name(stem, extension, selection_rule))
  legacy_path <- file.path(base_dir, part5_rating_output_name(stem, extension))

  if (file.exists(selection_path)) {
    return(selection_path)
  }
  if (allow_max_pr_auc_legacy && identical(selection_rule, "max_pr_auc") && file.exists(legacy_path)) {
    return(legacy_path)
  }
  selection_path
}

part5_rating_selection_label <- function() {
  tag <- clean_file_id(PART4_OUTPUT_TAG)
  selection_label <- paste0("selection=", PART5_SELECTION_RULE)
  if (nzchar(tag)) {
    paste0("V5/Part_4", part5_spec_suffix(PART5_RATING_SPEC), "_Outputs/", tag, " (", selection_label, ")")
  } else {
    paste0("V5/Part_4", part5_spec_suffix(PART5_RATING_SPEC), "_Outputs (", selection_label, ")")
  }
}

PART5_SELECTED_SPECS_RDS <- part5_selection_output_path(
  "data",
  "Panel_Europe_macro_selected_model_specifications",
  "rds"
)
PART5_CANDIDATE_GRID_RDS <- file.path(
  RATING_EXPERIMENT_DIR,
  paste0("Part_4", part5_spec_suffix(PART5_RATING_SPEC), "_Outputs"),
  "data",
  part5_rating_output_name("Panel_Europe_macro_candidate_grid", "rds")
)
PART5_TUNING_ARTIFACT_RDS <- file.path(
  RATING_EXPERIMENT_DIR,
  paste0("Part_4", part5_spec_suffix(PART5_RATING_SPEC), "_Outputs"),
  "data",
  part5_rating_output_name("Panel_Europe_macro_part_4_tuning_artifact", "rds")
)
PART5_SELECTED_HYPERPARAMETERS_CSV <- part5_selection_output_path(
  "audits",
  "table_05_selected_hyperparameters",
  "csv"
)
PART5_SELECTED_THRESHOLDS_CSV <- part5_selection_output_path(
  "audits",
  "table_06_selected_thresholds",
  "csv"
)
PART5_SELECTED_SPECS_LABEL <- part5_rating_selection_label()

PART5_MODEL_THREADS <- as.integer(Sys.getenv("PART5_MODEL_THREADS", Sys.getenv("PART4_MODEL_THREADS", "2")))
MODEL_THREADS <- if_else(is.na(PART5_MODEL_THREADS) | PART5_MODEL_THREADS < 1L, 1L, PART5_MODEL_THREADS)
PART5_ALLOW_CDS <- tolower(Sys.getenv("PART5_ALLOW_CDS", "FALSE")) %in% c("true", "t", "1", "yes")

part5_assert_new_file <- function(path) {
  if (file.exists(path) && !PART5_OVERWRITE_OUTPUTS) {
    stop(
      "Refusing to overwrite existing Part 5 V5 output: ",
      path,
      ". Set PART5_OVERWRITE_OUTPUTS=TRUE deliberately to replace it.",
      call. = FALSE
    )
  }
  invisible(path)
}

part5_write_audit <- function(x, name, caption = NULL, digits = 4) {
  csv_path <- file.path(PART5_AUDIT_DIR, paste0(name, ".csv"))
  tex_path <- file.path(PART5_TABLE_DIR, paste0(name, ".tex"))
  part5_assert_new_file(csv_path)
  part5_assert_new_file(tex_path)
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

part5_save_data <- function(x, name) {
  path <- file.path(PART5_DATA_DIR, paste0(name, ".rds"))
  part5_assert_new_file(path)
  saveRDS(x, path)
  invisible(x)
}

part5_save_partitioned_predictions <- function(x, name) {
  if (nrow(x) == 0L) {
    return(tibble())
  }

  required_cols <- c("Predictor_Set", "Model", "Horizon")
  missing_cols <- setdiff(required_cols, names(x))
  if (length(missing_cols) > 0L) {
    stop("Cannot partition locked-test predictions; missing column(s): ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  # Store locked-test predictions in small chunks so later analysis can load only the needed model/specification/horizon.
  partition_dir <- file.path(PART5_DATA_DIR, name)
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
        part5_assert_new_file(file_path)
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
  index_path <- file.path(PART5_DATA_DIR, paste0(name, "_index.csv"))
  part5_assert_new_file(index_path)
  readr::write_csv(index, index_path, na = "")
  invisible(index)
}

normalize_part5_models <- function(models_to_run, available_models) {
  if (is.null(models_to_run)) {
    models_to_run <- Sys.getenv("PART5_MODELS", "ALL")
  }
  if (length(models_to_run) == 1L) {
    models_to_run <- strsplit(models_to_run, ",", fixed = TRUE)[[1]]
  }

  models_to_run <- trimws(models_to_run)
  models_to_run <- models_to_run[nzchar(models_to_run)]
  if (length(models_to_run) == 0L || any(toupper(models_to_run) == "ALL")) {
    return(sort(unique(available_models)))
  }

  model_aliases <- c(
    "logit" = "Logit",
    "probit" = "Probit",
    "rf" = "Random_Forest",
    "random forest" = "Random_Forest",
    "random_forest" = "Random_Forest",
    "xgb" = "XGBoost",
    "xgboost" = "XGBoost",
    "elastic net" = "Elastic_Net",
    "elastic-net" = "Elastic_Net",
    "elastic_net" = "Elastic_Net",
    "enet" = "Elastic_Net",
    "glmnet" = "Elastic_Net",
    "gam" = "GAM"
  )

  normalized <- vapply(
    models_to_run,
    function(model_i) {
      key <- tolower(model_i)
      if (key %in% names(model_aliases)) {
        model_aliases[[key]]
      } else {
        model_i
      }
    },
    character(1)
  )

  unique(normalized)
}

read_part5_selected_specs <- function() {
  if (!file.exists(PART5_SELECTED_SPECS_RDS)) {
    stop("Missing Part 4 selected specifications: ", PART5_SELECTED_SPECS_RDS, call. = FALSE)
  }

  selected_specs <- readRDS(PART5_SELECTED_SPECS_RDS) %>%
    mutate(Selection_Source = PART5_SELECTED_SPECS_LABEL)

  if (!"candidate_id" %in% names(selected_specs)) {
    selected_specs <- selected_specs %>% mutate(candidate_id = Candidate_ID)
  }
  if (!all(c("Threshold", "MCC", "F1", "Balanced_Accuracy") %in% names(selected_specs))) {
    if (!file.exists(PART5_SELECTED_THRESHOLDS_CSV)) {
      stop("Missing selected thresholds from the Part 4 collector.", call. = FALSE)
    }
    selected_thresholds <- readr::read_csv(PART5_SELECTED_THRESHOLDS_CSV, show_col_types = FALSE)
    selected_specs <- selected_specs %>%
      left_join(
        selected_thresholds %>%
          select(Predictor_Set, Horizon_Label, Model, Candidate_ID, Threshold, MCC, F1, Balanced_Accuracy),
        by = c("Predictor_Set", "Horizon_Label", "Model", "Candidate_ID")
      )
  }

  duplicate_specs <- selected_specs %>%
    count(Predictor_Set, Horizon, Model, name = "N") %>%
    filter(N > 1L)
  if (nrow(duplicate_specs) > 0L) {
    stop("Duplicate selected Part 5 specifications detected.", call. = FALSE)
  }

  selected_specs %>%
    arrange(Model, Predictor_Set, Horizon)
}

check_part4_artifact_current <- function(path, label) {
  if (!file.exists(path)) {
    return(invisible(FALSE))
  }

  artifact <- readRDS(path)
  expected_signature <- input_signature(PART5_PART4_EXPANDING_INPUT_RDS)
  if (!identical(artifact$input_signature, expected_signature)) {
    stop(
      label,
      " tuning artifact is stale relative to the current Part 3 expanding-window input. Re-run the relevant Part 4 collector.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

check_part4_selection_current <- function() {
  check_part4_artifact_current(PART5_TUNING_ARTIFACT_RDS, paste0("Macro-rating Part 4 (", part5_rating_selection_label(), ")"))
  invisible(TRUE)
}

prepare_part5_context <- function(models_to_run = NULL) {
  if (!file.exists(PART5_LOCKED_INPUT_RDS)) {
    stop("Missing locked-test input from Part 3: ", PART5_LOCKED_INPUT_RDS, call. = FALSE)
  }

  check_part4_selection_current()

  locked_data <- readRDS(PART5_LOCKED_INPUT_RDS) %>%
    add_rating_features()
  selected_specs <- read_part5_selected_specs()
  models_to_run <- normalize_part5_models(models_to_run, selected_specs$Model)

  missing_models <- setdiff(models_to_run, unique(selected_specs$Model))
  if (length(missing_models) > 0L) {
    stop(
      "Requested model(s) not available in the selected Part 4 specifications: ",
      paste(missing_models, collapse = ", "),
      ". Re-run Part 4 estimation and collection for the relevant V5 block first.",
      call. = FALSE
    )
  }

  selected_specs <- selected_specs %>%
    filter(Model %in% models_to_run) %>%
    arrange(Model, Predictor_Set, Horizon)

  required_columns <- c(
    "Observation_ID", "Country", "firm_id", "Predictor_Date", "Target_Date",
    "Horizon", "Horizon_Label", "Fold", "Fold_Role", "Outcome_Num"
  )
  stopifnot(all(required_columns %in% names(locked_data)))
  stopifnot(all(locked_data$Fold_Role %in% c("Training", "Test")))
  stopifnot(all(locked_data$Outcome_Num %in% c(0L, 1L)))

  saved_regressor_manifest <- if (file.exists(REGRESSOR_MANIFEST_CSV)) {
    readr::read_csv(REGRESSOR_MANIFEST_CSV, show_col_types = FALSE)
  } else {
    tibble(Regressor_Set = character(), Variables = character())
  }
  regressor_manifest <- bind_rows(
    make_regressor_manifest(locked_data),
    saved_regressor_manifest
  ) %>%
    distinct(Regressor_Set, .keep_all = TRUE)

  regressor_sets <- regressor_manifest %>%
    filter(Regressor_Set %in% unique(selected_specs$Predictor_Set)) %>%
    mutate(
      Predictors = purrr::map(
        stringr::str_split(Variables, ",\\s*"),
        ~ unique(unlist(purrr::map(.x, function(v) {
          if (identical(v, "rating_group")) c("rating_group_B", "rating_group_C") else v
        })))
      ),
      Variables = purrr::map_chr(Predictors, ~ paste(.x, collapse = ", "))
    )

  missing_sets <- setdiff(unique(selected_specs$Predictor_Set), regressor_sets$Regressor_Set)
  if (length(missing_sets) > 0L) {
    stop("Missing regressor set(s): ", paste(missing_sets, collapse = ", "), call. = FALSE)
  }

  missing_predictors <- regressor_sets %>%
    tidyr::unnest(Predictors) %>%
    filter(!Predictors %in% names(locked_data))
  if (nrow(missing_predictors) > 0L) {
    stop("Missing predictors in locked-test data: ", paste(unique(missing_predictors$Predictors), collapse = ", "), call. = FALSE)
  }

  banned_esg_predictors <- c("env_score", "g_score")
  selected_predictors <- regressor_sets %>%
    tidyr::unnest(Predictors)
  esg_predictors <- selected_predictors %>%
    filter(Predictors %in% banned_esg_predictors)
  if (nrow(esg_predictors) > 0L) {
    stop(
      "ESG predictors are excluded from Part 5 locked-test main specifications: ",
      paste(unique(esg_predictors$Predictors), collapse = ", "),
      call. = FALSE
    )
  }
  if (!PART5_ALLOW_CDS) {
    cds_predictors <- selected_predictors %>%
      filter(Predictors == "cds_log_change")
    if (nrow(cds_predictors) > 0L) {
      stop(
        "CDS is excluded from Part 5 main locked-test specifications. Use a separate robustness script or set PART5_ALLOW_CDS=TRUE deliberately.",
        call. = FALSE
      )
    }
  }

  list(
    locked_data = locked_data,
    selected_specs = selected_specs,
    regressor_sets = regressor_sets,
    models_to_run = models_to_run
  )
}

preflight_part5_locked_test <- function(models_to_run = NULL) {
  context <- prepare_part5_context(models_to_run)
  split_audit <- context$locked_data %>%
    filter(Horizon %in% unique(context$selected_specs$Horizon)) %>%
    group_by(Horizon, Horizon_Label, Fold_Role) %>%
    summarise(
      N_Observations = n(),
      N_Downgrades = sum(Outcome_Num),
      Downgrade_Rate_Pct = 100 * mean(Outcome_Num),
      First_Target_Date = min(Target_Date),
      Last_Target_Date = max(Target_Date),
      .groups = "drop"
    )

  model_spec_audit <- context$selected_specs %>%
    count(Model, Predictor_Set, name = "N_Horizons") %>%
    arrange(Model, Predictor_Set)

  list(
    models_to_run = context$models_to_run,
    selected_specifications = context$selected_specs,
    regressor_sets = context$regressor_sets,
    split_audit = split_audit,
    model_spec_audit = model_spec_audit
  )
}

part5_candidate_value <- function(candidate, name, default = NA) {
  if (!name %in% names(candidate)) {
    return(default)
  }
  value <- candidate[[name]]
  if (length(value) == 0L || is.na(value[[1]])) {
    return(default)
  }
  value[[1]]
}

part5_model_file_name <- function(candidate) {
  candidate_id <- substr(clean_file_id(candidate$Candidate_ID), 1L, 140L)
  paste0(
    "model_",
    clean_file_id(candidate$Model),
    "__",
    clean_file_id(candidate$Predictor_Set),
    "__h",
    candidate$Horizon,
    "__",
    candidate_id,
    ".rds"
  )
}

part5_save_model_object <- function(model_object, candidate) {
  if (is.null(model_object)) {
    return(NA_character_)
  }
  path <- file.path(PART5_MODEL_DIR, part5_model_file_name(candidate))
  part5_assert_new_file(path)
  saveRDS(model_object, path)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

part5_model_meta <- function(model_object) {
  candidate <- model_object$candidate
  tibble(
    Predictor_Set = candidate$Predictor_Set,
    Horizon = as.integer(candidate$Horizon),
    Horizon_Label = candidate$Horizon_Label,
    Model = candidate$Model,
    Candidate_ID = candidate$Candidate_ID
  )
}

part5_model_object <- function(fit, prep, predictors, candidate, train, test, model_input_columns = predictors) {
  list(
    model = fit,
    preprocessor = prep,
    predictors = predictors,
    model_input_columns = model_input_columns,
    candidate = as.list(candidate),
    training_rows = nrow(train),
    test_rows = nrow(test),
    training_downgrades = sum(train$Outcome_Num),
    test_downgrades = sum(test$Outcome_Num),
    fitted_at = Sys.time()
  )
}

part5_max_finite <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) NA_real_ else max(x)
}

compute_logit_probit_collinearity <- function(train, predictors, candidate) {
  prep <- fit_preprocessor(train, predictors, use_signed_log = TRUE)
  x_train <- apply_preprocessor(train, prep)
  x_matrix <- as.matrix(x_train)
  storage.mode(x_matrix) <- "double"

  finite_rows <- stats::complete.cases(x_matrix)
  x_matrix <- x_matrix[finite_rows, , drop = FALSE]
  n_obs <- nrow(x_matrix)
  n_predictors <- ncol(x_matrix)

  if (n_predictors == 0L) {
    return(tibble())
  }

  predictor_names <- colnames(x_matrix)
  predictor_sd <- apply(x_matrix, 2, stats::sd, na.rm = TRUE)
  zero_variance <- !is.finite(predictor_sd) | predictor_sd == 0
  design_rank <- tryCatch(qr(x_matrix)$rank, error = function(e) NA_integer_)
  condition_number <- tryCatch(as.numeric(kappa(x_matrix, exact = FALSE)), error = function(e) NA_real_)

  corr_matrix <- tryCatch(stats::cor(x_matrix), error = function(e) NULL)
  max_abs_corr <- rep(NA_real_, n_predictors)
  if (!is.null(corr_matrix) && n_predictors > 1L) {
    diag(corr_matrix) <- NA_real_
    max_abs_corr <- apply(abs(corr_matrix), 1, part5_max_finite)
  }

  vif <- rep(NA_real_, n_predictors)
  if (n_predictors > 1L && n_obs > 2L) {
    for (j in seq_len(n_predictors)) {
      y <- x_matrix[, j]
      others <- x_matrix[, -j, drop = FALSE]
      if (zero_variance[[j]] || ncol(others) == 0L) {
        next
      }
      fit <- tryCatch(stats::lm.fit(x = cbind(`(Intercept)` = 1, others), y = y), error = function(e) NULL)
      if (is.null(fit)) {
        next
      }
      tss <- sum((y - mean(y))^2)
      rss <- sum(fit$residuals^2)
      if (!is.finite(tss) || tss <= .Machine$double.eps) {
        next
      }
      r_squared <- max(0, min(1, 1 - rss / tss))
      vif[[j]] <- if (r_squared >= 1) Inf else 1 / (1 - r_squared)
    }
  }

  tibble(
    Predictor_Set = candidate$Predictor_Set,
    Horizon = as.integer(candidate$Horizon),
    Horizon_Label = candidate$Horizon_Label,
    Model = candidate$Model,
    Candidate_ID = candidate$Candidate_ID,
    Feature = predictor_names,
    N_Training_Rows = nrow(train),
    N_Complete_Rows = n_obs,
    N_Predictors = n_predictors,
    Design_Rank = as.integer(design_rank),
    Full_Rank = !is.na(design_rank) && design_rank == n_predictors,
    Condition_Number = condition_number,
    Zero_Variance = zero_variance,
    VIF = vif,
    Max_Abs_Correlation = max_abs_corr
  )
}

fit_locked_logit_probit <- function(train, test, predictors, candidate) {
  prep <- fit_preprocessor(train, predictors, use_signed_log = TRUE)
  x_train <- apply_preprocessor(train, prep)
  x_test <- apply_preprocessor(test, prep)
  train_model <- bind_cols(tibble(Outcome_Num = train$Outcome_Num), x_train)
  test_model <- bind_cols(tibble(Outcome_Num = test$Outcome_Num), x_test)
  weights <- training_weights(train_model$Outcome_Num, candidate$weight_strategy)

  fit <- glm(
    reformulate(predictors, response = "Outcome_Num"),
    data = train_model,
    family = binomial(link = candidate$link),
    weights = weights
  )
  list(
    probability = as.numeric(predict(fit, newdata = test_model, type = "response")),
    model_object = part5_model_object(fit, prep, predictors, candidate, train, test, names(train_model))
  )
}

fit_locked_rf <- function(train, test, predictors, candidate) {
  train_model <- train %>%
    mutate(Outcome_Factor = factor(Outcome_Num, levels = c(0, 1), labels = c("No_Downgrade", "Downgrade"))) %>%
    select(Outcome_Factor, all_of(predictors))
  test_model <- test %>% select(all_of(predictors))
  n_train <- nrow(train_model)
  min_node_size <- max(5L, round(n_train * candidate$min_node_share))
  p <- length(predictors)
  mtry_mode <- part5_candidate_value(candidate, "mtry_mode", "sqrtp")
  mtry <- case_when(
    mtry_mode == "sqrtp" ~ floor(sqrt(p)),
    mtry_mode == "p3" ~ floor(p / 3),
    mtry_mode == "p2" ~ floor(p / 2),
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
    importance = "impurity",
    seed = stable_seed(candidate$candidate_id, n_train),
    num.threads = MODEL_THREADS
  )
  list(
    probability = as.numeric(predict(fit, data = test_model)$predictions[, "Downgrade"]),
    model_object = part5_model_object(fit, NULL, predictors, candidate, train, test, names(train_model))
  )
}

fit_locked_xgb <- function(train, test, predictors, candidate) {
  x_train <- as.matrix(train %>% select(all_of(predictors)))
  x_test <- as.matrix(test %>% select(all_of(predictors)))
  w_train <- training_weights(train$Outcome_Num, candidate$weight_strategy)
  dtrain <- xgboost::xgb.DMatrix(data = x_train, label = train$Outcome_Num, weight = w_train)
  dtest <- xgboost::xgb.DMatrix(data = x_test)

  fit <- xgboost::xgb.train(
    params = list(
      objective = "binary:logistic",
      eval_metric = "logloss",
      max_depth = candidate$max_depth,
      eta = candidate$eta,
      min_child_weight = candidate$min_child_weight,
      lambda = part5_candidate_value(candidate, "lambda", 1),
      alpha = part5_candidate_value(candidate, "alpha", 0),
      gamma = part5_candidate_value(candidate, "gamma", 0),
      subsample = candidate$subsample,
      colsample_bytree = candidate$colsample_bytree,
      seed = stable_seed(candidate$candidate_id),
      nthread = MODEL_THREADS
    ),
    data = dtrain,
    nrounds = candidate$nrounds,
    verbose = 0
  )
  list(
    probability = as.numeric(predict(fit, dtest)),
    model_object = part5_model_object(fit, NULL, predictors, candidate, train, test, predictors)
  )
}

fit_locked_elastic_net <- function(train, test, predictors, candidate) {
  prep <- fit_preprocessor(train, predictors, use_signed_log = TRUE)
  x_train <- as.matrix(apply_preprocessor(train, prep))
  x_test <- as.matrix(apply_preprocessor(test, prep))
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
  list(
    probability = as.numeric(predict(fit, newx = x_test, s = candidate$lambda, type = "response")),
    model_object = part5_model_object(fit, prep, predictors, candidate, train, test, colnames(x_train))
  )
}

part5_gam_smooth_variables <- function(predictors, scope, train_model, gam_k) {
  core_vars <- c("wc_ta", "re_ta", "ebit_ta", "td_ta", "mc_td", "ROA_pc")
  smooth_eligible <- setdiff(predictors, GAM_FORCE_LINEAR_VARS)
  smooth_eligible <- smooth_eligible[vapply(smooth_eligible, function(v) {
    n_distinct(train_model[[v]][is.finite(train_model[[v]])]) > gam_k
  }, logical(1))]
  if (identical(scope, "all")) {
    return(smooth_eligible)
  }
  intersect(core_vars, smooth_eligible)
}

fit_locked_gam <- function(train, test, predictors, candidate) {
  prep <- fit_preprocessor(train, predictors, use_signed_log = TRUE)
  train_model <- bind_cols(tibble(Outcome_Num = train$Outcome_Num), apply_preprocessor(train, prep))
  test_model <- bind_cols(tibble(Outcome_Num = test$Outcome_Num), apply_preprocessor(test, prep))
  weights <- training_weights(train_model$Outcome_Num, candidate$weight_strategy)
  smooth_vars <- part5_gam_smooth_variables(predictors, candidate$gam_scope, train_model, candidate$gam_k)
  linear_vars <- setdiff(predictors, smooth_vars)
  if (length(smooth_vars) == 0L && length(linear_vars) == 0L) {
    stop("No GAM predictors available.", call. = FALSE)
  }

  smooth_terms <- paste0("s(", smooth_vars, ", k = ", candidate$gam_k, ", bs = 'cs')")
  model_terms <- c(smooth_terms, linear_vars)
  fit <- mgcv::gam(
    as.formula(paste("Outcome_Num ~", paste(model_terms, collapse = " + "))),
    data = train_model,
    family = binomial(link = "logit"),
    weights = weights,
    method = "REML",
    control = mgcv::gam.control(maxit = 100)
  )
  list(
    probability = as.numeric(predict(fit, newdata = test_model, type = "response")),
    model_object = part5_model_object(fit, prep, predictors, candidate, train, test, names(train_model))
  )
}

fit_locked_candidate <- function(train, test, predictors, candidate) {
  if (candidate$Model %in% c("Logit", "Probit")) {
    fit_locked_logit_probit(train, test, predictors, candidate)
  } else if (candidate$Model == "Random_Forest") {
    fit_locked_rf(train, test, predictors, candidate)
  } else if (candidate$Model == "XGBoost") {
    fit_locked_xgb(train, test, predictors, candidate)
  } else if (candidate$Model == "Elastic_Net") {
    fit_locked_elastic_net(train, test, predictors, candidate)
  } else if (candidate$Model == "GAM") {
    fit_locked_gam(train, test, predictors, candidate)
  } else {
    stop("Unknown model: ", candidate$Model, call. = FALSE)
  }
}

extract_model_interpretation <- function(model_object) {
  if (is.null(model_object)) {
    return(tibble())
  }
  meta <- part5_model_meta(model_object)
  model_name <- meta$Model[[1]]
  fit <- model_object$model

  if (model_name %in% c("Logit", "Probit")) {
    coef_table <- as.data.frame(summary(fit)$coefficients)
    coef_table$Feature <- rownames(coef_table)
    names(coef_table)[seq_len(min(4L, ncol(coef_table) - 1L))] <- c("Estimate", "Std_Error", "Statistic", "P_Value")[seq_len(min(4L, ncol(coef_table) - 1L))]
    return(bind_cols(meta, as_tibble(coef_table)) %>%
      mutate(Interpretation_Type = "Coefficient", Abs_Estimate = abs(Estimate)) %>%
      select(all_of(names(meta)), Interpretation_Type, Feature, Estimate, Std_Error, Statistic, P_Value, Abs_Estimate))
  }

  if (model_name == "Elastic_Net") {
    lambda <- model_object$candidate$lambda
    coef_matrix <- as.matrix(glmnet::coef.glmnet(fit, s = lambda))
    return(tibble(Feature = rownames(coef_matrix), Estimate = as.numeric(coef_matrix[, 1])) %>%
      bind_cols(meta, .) %>%
      mutate(
        Interpretation_Type = "Coefficient",
        Lambda = lambda,
        Alpha = model_object$candidate$alpha,
        Abs_Estimate = abs(Estimate),
        Nonzero = Estimate != 0
      ) %>%
      select(all_of(names(meta)), Interpretation_Type, Feature, Estimate, Abs_Estimate, Nonzero, Alpha, Lambda))
  }

  if (model_name == "Random_Forest") {
    importance <- fit$variable.importance
    if (is.null(importance)) {
      return(tibble())
    }
    return(tibble(Feature = names(importance), Importance = as.numeric(importance)) %>%
      arrange(desc(Importance)) %>%
      bind_cols(meta, .) %>%
      mutate(Interpretation_Type = "Impurity variable importance") %>%
      select(all_of(names(meta)), Interpretation_Type, Feature, Importance))
  }

  if (model_name == "XGBoost") {
    importance <- xgboost::xgb.importance(feature_names = model_object$predictors, model = fit)
    if (nrow(importance) == 0L) {
      return(tibble())
    }
    return(as_tibble(importance) %>%
      bind_cols(meta, .) %>%
      mutate(Interpretation_Type = "Tree gain variable importance") %>%
      select(all_of(names(meta)), Interpretation_Type, everything()))
  }

  if (model_name == "GAM") {
    gam_summary <- summary(fit)
    parametric <- as.data.frame(gam_summary$p.table)
    parametric$Feature <- rownames(parametric)
    names(parametric)[seq_len(min(4L, ncol(parametric) - 1L))] <- c("Estimate", "Std_Error", "Statistic", "P_Value")[seq_len(min(4L, ncol(parametric) - 1L))]
    smooth <- as.data.frame(gam_summary$s.table)
    smooth$Feature <- rownames(smooth)
    if (nrow(smooth) > 0L) {
      names(smooth)[seq_len(min(4L, ncol(smooth) - 1L))] <- c("EDF", "Ref_DF", "Statistic", "P_Value")[seq_len(min(4L, ncol(smooth) - 1L))]
    }
    bind_rows(
      bind_cols(meta, as_tibble(parametric)) %>%
        mutate(Interpretation_Type = "GAM parametric coefficient", Abs_Estimate = abs(Estimate)) %>%
        select(all_of(names(meta)), Interpretation_Type, Feature, Estimate, Std_Error, Statistic, P_Value, Abs_Estimate),
      bind_cols(meta, as_tibble(smooth)) %>%
        mutate(Interpretation_Type = "GAM smooth term") %>%
        select(any_of(c(names(meta), "Interpretation_Type", "Feature", "EDF", "Ref_DF", "Statistic", "P_Value")))
    )
  } else {
    tibble()
  }
}

run_locked_test_one <- function(spec_i, context) {
  spec <- context$selected_specs[spec_i, ]
  predictors <- context$regressor_sets$Predictors[[match(spec$Predictor_Set, context$regressor_sets$Regressor_Set)]]
  candidate <- spec
  candidate$candidate_id <- candidate$Candidate_ID
  candidate$Horizon <- spec$Horizon

  data_h <- context$locked_data %>%
    filter(Horizon == spec$Horizon)
  train <- data_h %>% filter(Fold_Role == "Training")
  test <- data_h %>% filter(Fold_Role == "Test")

  start_time <- Sys.time()
  message(
    "[START ",
    spec_i,
    "/",
    nrow(context$selected_specs),
    "] Locked test | ",
    spec$Model,
    " | ",
    spec$Predictor_Set,
    " | h",
    spec$Horizon,
    " | ",
    spec$Candidate_ID,
    " | start: ",
    format_timestamp(start_time)
  )

  fit_result <- tryCatch(
    fit_locked_candidate(train, test, predictors, candidate),
    error = function(e) {
      message(
        "[FAILED ",
        spec_i,
        "/",
        nrow(context$selected_specs),
        "] Locked test | ",
        spec$Model,
        " | ",
        spec$Predictor_Set,
        " | h",
        spec$Horizon,
        " | ",
        conditionMessage(e)
      )
      list(
        probability = rep(NA_real_, nrow(test)),
        model_object = NULL,
        error_message = conditionMessage(e)
      )
    }
  )

  end_time <- Sys.time()
  pred <- fit_result$probability
  fit_status <- if_else(all(is.na(pred)), "Failed", "OK")
  model_path <- if (identical(fit_status, "OK")) {
    part5_save_model_object(fit_result$model_object, candidate)
  } else {
    NA_character_
  }
  model_index <- tibble(
    Predictor_Set = spec$Predictor_Set,
    Horizon = spec$Horizon,
    Horizon_Label = spec$Horizon_Label,
    Model = spec$Model,
    Candidate_ID = spec$Candidate_ID,
    Model_Object_Path = model_path,
    Fit_Status = fit_status
  )
  interpretation <- if (identical(fit_status, "OK")) {
    extract_model_interpretation(fit_result$model_object)
  } else {
    tibble()
  }
  collinearity <- if (identical(fit_status, "OK") && spec$Model %in% c("Logit", "Probit")) {
    compute_logit_probit_collinearity(train, predictors, candidate)
  } else {
    tibble()
  }

  predictions <- test %>%
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
      Predictor_Set = spec$Predictor_Set,
      Model = spec$Model,
      Candidate_ID = spec$Candidate_ID,
      Validation_PR_AUC = spec$PR_AUC,
      Validation_ROC_AUC = spec$ROC_AUC,
      Validation_Brier = spec$Brier,
      Validation_Log_Loss = spec$Log_Loss,
      Validation_Threshold = spec$Threshold,
      Probability = pmin(pmax(as.numeric(pred), 0), 1),
      Fit_Status = fit_status
    )

  timing <- tibble(
    Job_Index = spec_i,
    Job_Total = nrow(context$selected_specs),
    Model = spec$Model,
    Predictor_Set = spec$Predictor_Set,
    Horizon = spec$Horizon,
    Horizon_Label = spec$Horizon_Label,
    Candidate_ID = spec$Candidate_ID,
    Start_Time = format_timestamp(start_time),
    End_Time = format_timestamp(end_time),
    Elapsed_Seconds = as.numeric(difftime(end_time, start_time, units = "secs")),
    N_Training_Rows = nrow(train),
    N_Test_Rows = nrow(test),
    N_Test_Downgrades = sum(test$Outcome_Num),
    Model_Object_Path = model_path,
    Fit_Status = fit_status
  )

  message(
    "[DONE ",
    spec_i,
    "/",
    nrow(context$selected_specs),
    "] Locked test | ",
    spec$Model,
    " | ",
    spec$Predictor_Set,
    " | h",
    spec$Horizon,
    " | status: ",
    timing$Fit_Status,
    " | elapsed: ",
    round(timing$Elapsed_Seconds / 60, 2),
    " min"
  )

  list(predictions = predictions, timing = timing, model_index = model_index, interpretation = interpretation, collinearity = collinearity)
}

decile_calibration_one <- function(data) {
  ok <- is.finite(data$Outcome_Num) & is.finite(data$Probability)
  data <- data[ok, ]
  if (nrow(data) == 0L) {
    return(tibble())
  }

  data %>%
    mutate(Calibration_Decile = ntile(Probability, min(10L, n()))) %>%
    group_by(Calibration_Decile) %>%
    summarise(
      N = n(),
      N_Downgrades = sum(Outcome_Num),
      Observed_Downgrade_Rate = mean(Outcome_Num),
      Mean_Predicted_Probability = mean(Probability),
      Expected_Downgrades = sum(Probability),
      Calibration_Error = Observed_Downgrade_Rate - Mean_Predicted_Probability,
      Min_Predicted_Probability = min(Probability),
      Max_Predicted_Probability = max(Probability),
      .groups = "drop"
    )
}

run_part5_locked_test <- function(models_to_run = NULL) {
  context <- prepare_part5_context(models_to_run)
  model_tag <- clean_file_id(paste(context$models_to_run, collapse = "_"))
  selection_tag <- clean_file_id(PART4_OUTPUT_TAG)
  if (nzchar(selection_tag)) {
    model_tag <- paste(model_tag, selection_tag, sep = "_")
  }

  split_audit <- context$locked_data %>%
    filter(Horizon %in% unique(context$selected_specs$Horizon)) %>%
    group_by(Horizon, Horizon_Label, Fold_Role) %>%
    summarise(
      N_Observations = n(),
      N_Downgrades = sum(Outcome_Num),
      Downgrade_Rate_Pct = 100 * mean(Outcome_Num),
      First_Target_Date = min(Target_Date),
      Last_Target_Date = max(Target_Date),
      .groups = "drop"
    )

  results <- purrr::map(seq_len(nrow(context$selected_specs)), run_locked_test_one, context = context)
  locked_predictions <- bind_rows(purrr::map(results, "predictions"))
  timing <- bind_rows(purrr::map(results, "timing"))
  model_object_index <- bind_rows(purrr::map(results, "model_index"))
  model_interpretation <- bind_rows(purrr::map(results, "interpretation"))
  logit_probit_collinearity <- bind_rows(purrr::map(results, "collinearity"))
  logit_probit_collinearity_summary <- if (nrow(logit_probit_collinearity) > 0L) {
    logit_probit_collinearity %>%
      group_by(Predictor_Set, Horizon, Horizon_Label, Model, Candidate_ID) %>%
      summarise(
        N_Training_Rows = max(N_Training_Rows, na.rm = TRUE),
        N_Complete_Rows = max(N_Complete_Rows, na.rm = TRUE),
        N_Predictors = max(N_Predictors, na.rm = TRUE),
        Design_Rank = max(Design_Rank, na.rm = TRUE),
        Full_Rank = all(Full_Rank %in% TRUE),
        Condition_Number = part5_max_finite(Condition_Number),
        Max_VIF = part5_max_finite(VIF),
        N_VIF_Above_5 = sum(VIF > 5, na.rm = TRUE),
        N_VIF_Above_10 = sum(VIF > 10, na.rm = TRUE),
        Max_Abs_Correlation = part5_max_finite(Max_Abs_Correlation),
        N_Zero_Variance = sum(Zero_Variance %in% TRUE, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(Model, Predictor_Set, Horizon)
  } else {
    tibble()
  }

  if (nrow(model_interpretation) > 0L) {
    sort_value <- rep(0, nrow(model_interpretation))
    if ("Abs_Estimate" %in% names(model_interpretation)) {
      sort_value <- pmax(sort_value, dplyr::coalesce(model_interpretation$Abs_Estimate, 0))
    }
    if ("Importance" %in% names(model_interpretation)) {
      sort_value <- pmax(sort_value, dplyr::coalesce(model_interpretation$Importance, 0))
    }
    if ("Gain" %in% names(model_interpretation)) {
      sort_value <- pmax(sort_value, dplyr::coalesce(model_interpretation$Gain, 0))
    }
    if ("EDF" %in% names(model_interpretation)) {
      sort_value <- pmax(sort_value, dplyr::coalesce(model_interpretation$EDF, 0))
    }
    model_interpretation <- model_interpretation %>%
      mutate(Sort_Value = sort_value) %>%
      arrange(Model, Predictor_Set, Horizon, desc(Sort_Value)) %>%
      select(-Sort_Value)
  }

  if (nrow(locked_predictions) == 0L || all(locked_predictions$Fit_Status == "Failed")) {
    stop("No valid locked-test predictions were produced.", call. = FALSE)
  }

  key_cols <- c("Predictor_Set", "Horizon", "Horizon_Label", "Model", "Candidate_ID")
  validation_reference <- context$selected_specs %>%
    transmute(
      Predictor_Set,
      Horizon,
      Horizon_Label,
      Model,
      Candidate_ID,
      Validation_PR_AUC = PR_AUC,
      Validation_ROC_AUC = ROC_AUC,
      Validation_Brier = Brier,
      Validation_Log_Loss = Log_Loss,
      Validation_Threshold = Threshold,
      Validation_MCC = MCC,
      Validation_F1 = F1,
      Validation_Balanced_Accuracy = Balanced_Accuracy
    )

  probability_metrics <- locked_predictions %>%
    filter(Fit_Status == "OK") %>%
    group_by(across(all_of(key_cols))) %>%
    group_modify(~ prob_metrics(.x$Outcome_Num, .x$Probability)) %>%
    ungroup() %>%
    left_join(validation_reference, by = key_cols) %>%
    mutate(
      Delta_PR_AUC = PR_AUC - Validation_PR_AUC,
      Delta_ROC_AUC = ROC_AUC - Validation_ROC_AUC,
      Delta_Brier = Brier - Validation_Brier,
      Delta_Log_Loss = Log_Loss - Validation_Log_Loss
    ) %>%
    arrange(Model, Predictor_Set, Horizon)

  threshold_metrics <- locked_predictions %>%
    filter(Fit_Status == "OK") %>%
    group_by(across(all_of(key_cols)), Validation_Threshold) %>%
    group_modify(~ threshold_metrics_one(.x$Outcome_Num, .x$Probability, unique(.y$Validation_Threshold))) %>%
    ungroup() %>%
    left_join(
      validation_reference %>%
        select(all_of(key_cols), Validation_MCC, Validation_F1, Validation_Balanced_Accuracy),
      by = key_cols
    ) %>%
    mutate(
      Delta_MCC = MCC - Validation_MCC,
      Delta_F1 = F1 - Validation_F1,
      Delta_Balanced_Accuracy = Balanced_Accuracy - Validation_Balanced_Accuracy
    ) %>%
    arrange(Model, Predictor_Set, Horizon)

  residual_calibration_diagnostics <- locked_predictions %>%
    filter(Fit_Status == "OK") %>%
    group_by(across(all_of(key_cols))) %>%
    group_modify(~ logit_probit_residual_diagnostics_one(.x$Outcome_Num, .x$Probability)) %>%
    ungroup() %>%
    mutate(
      Residual_Diagnostic_Type = if_else(
        Model %in% c("Logit", "Probit"),
        "Logit/Probit Pearson residuals from predicted probabilities",
        "Probability-scale Pearson residuals from predicted probabilities"
      )
    ) %>%
    arrange(Model, Predictor_Set, Horizon)

  decile_calibration <- locked_predictions %>%
    filter(Fit_Status == "OK") %>%
    group_by(across(all_of(key_cols))) %>%
    group_modify(~ decile_calibration_one(.x)) %>%
    ungroup() %>%
    arrange(Model, Predictor_Set, Horizon, Calibration_Decile)

  locked_summary <- probability_metrics %>%
    left_join(
      threshold_metrics %>%
        select(all_of(key_cols), Threshold, Sensitivity, Specificity, Precision, F1, Balanced_Accuracy, MCC, Predicted_Downgrade_Rate_Pct),
      by = key_cols
    ) %>%
    left_join(
      residual_calibration_diagnostics %>%
        select(all_of(key_cols), Calibration_Intercept, Calibration_Slope, Decile_Calibration_P),
      by = key_cols
    )

  best_by_horizon_pr_auc <- locked_summary %>%
    group_by(Horizon, Horizon_Label) %>%
    arrange(desc(PR_AUC), desc(ROC_AUC), Brier, Log_Loss, .by_group = TRUE) %>%
    slice(1L) %>%
    ungroup()

  best_by_horizon_mcc <- locked_summary %>%
    group_by(Horizon, Horizon_Label) %>%
    arrange(desc(MCC), desc(F1), desc(Balanced_Accuracy), .by_group = TRUE) %>%
    slice(1L) %>%
    ungroup()

  if (PART5_SAVE_SPLIT_LOCKED_TEST_PREDICTIONS) {
    locked_prediction_file_index <- part5_save_partitioned_predictions(
      locked_predictions,
      paste0("Panel_Europe_macro_locked_test_predictions_by_group_", model_tag)
    )
  } else {
    locked_prediction_file_index <- tibble()
  }

  part5_manifest <- tibble(
    Setting = c(
      "Locked-test input",
      "Part 4 selected specifications",
      "Macro-rating Part 4 output tag",
      "Part 4 selection rule id",
      "Models evaluated",
      "Regressor sets",
      "Horizons",
      "Training target period",
      "Locked test target period",
      "Selection rule",
      "Threshold rule",
      "ESG variables used",
      "CDS variables used",
      "Model threads",
      "Mega locked-test prediction file",
      "Partitioned locked-test prediction files",
      "Predictions embedded in artifact",
      "Fitted model objects",
      "Model interpretation outputs",
      "Logit/Probit collinearity diagnostics"
    ),
    Value = c(
      basename(PART5_LOCKED_INPUT_RDS),
      PART5_SELECTED_SPECS_LABEL,
      if_else(nzchar(clean_file_id(PART4_OUTPUT_TAG)), clean_file_id(PART4_OUTPUT_TAG), "None"),
      PART5_SELECTION_RULE,
      paste(context$models_to_run, collapse = ", "),
      paste(unique(context$selected_specs$Predictor_Set), collapse = ", "),
      paste(paste0("t+", sort(unique(context$selected_specs$Horizon))), collapse = ", "),
      paste(range(context$locked_data$Target_Date[context$locked_data$Fold_Role == "Training"]), collapse = " to "),
      paste(range(context$locked_data$Target_Date[context$locked_data$Fold_Role == "Test"]), collapse = " to "),
      paste0("Part 4 validation-selected specifications using ", PART5_SELECTION_RULE, "; the locked test is evaluation-only"),
      "Validation-selected threshold from Part 4",
      "No",
      if_else(PART5_ALLOW_CDS, "Allowed if present in selected specifications", "No"),
      as.character(MODEL_THREADS),
      if_else(PART5_SAVE_MEGA_LOCKED_TEST_PREDICTIONS, "Saved", "Skipped by default; enable with PART5_SAVE_MEGA_LOCKED_TEST_PREDICTIONS=TRUE"),
      if_else(PART5_SAVE_SPLIT_LOCKED_TEST_PREDICTIONS, "Saved by predictor set, model, and horizon", "Skipped"),
      if_else(PART5_SAVE_PREDICTIONS_IN_ARTIFACT, "Saved", "Skipped by default; enable with PART5_SAVE_PREDICTIONS_IN_ARTIFACT=TRUE"),
      PART5_MODEL_DIR,
      PART5_INTERPRETATION_DIR,
      PART5_AUDIT_DIR
    )
  )

  part5_write_audit(context$selected_specs, paste0("table_01_locked_test_selected_specifications_", model_tag), "Part 4 selected specifications evaluated on the locked test")
  part5_write_audit(split_audit, paste0("table_02_locked_test_split_audit_", model_tag), "Locked-test split composition")
  part5_write_audit(probability_metrics, paste0("table_03_locked_test_probability_metrics_", model_tag), "Locked-test probability metrics")
  part5_write_audit(threshold_metrics, paste0("table_04_locked_test_threshold_metrics_", model_tag), "Locked-test threshold metrics at validation-selected thresholds")
  part5_write_audit(residual_calibration_diagnostics, paste0("table_05_locked_test_residual_calibration_diagnostics_", model_tag), "Locked-test residual and calibration diagnostics")
  part5_write_audit(decile_calibration, paste0("table_06_locked_test_decile_calibration_", model_tag), "Locked-test decile calibration")
  part5_write_audit(best_by_horizon_pr_auc, paste0("table_07_locked_test_best_by_horizon_pr_auc_", model_tag), "Best locked-test model by horizon using PR-AUC")
  part5_write_audit(best_by_horizon_mcc, paste0("table_08_locked_test_best_by_horizon_mcc_", model_tag), "Best locked-test model by horizon using MCC at validation-selected threshold")
  part5_write_audit(timing, paste0("table_09_locked_test_timing_", model_tag), "Locked-test refit and prediction timing")
  part5_write_audit(part5_manifest, paste0("table_10_part_5_manifest_", model_tag), "Part 5 locked-test manifest")
  part5_write_audit(model_object_index, paste0("table_11_locked_test_model_object_index_", model_tag), "Saved fitted model objects from locked-test refits")
  part5_write_audit(model_interpretation, paste0("table_12_locked_test_model_interpretation_", model_tag), "Locked-test fitted-model coefficients and variable-importance summaries")
  part5_write_audit(logit_probit_collinearity, paste0("table_13_locked_test_logit_probit_collinearity_", model_tag), "Logit and Probit predictor-level collinearity diagnostics on the locked-test refit training sample")
  part5_write_audit(logit_probit_collinearity_summary, paste0("table_14_locked_test_logit_probit_collinearity_summary_", model_tag), "Logit and Probit collinearity summary by specification and horizon")
  if (nrow(locked_prediction_file_index) > 0L) {
    part5_write_audit(
      locked_prediction_file_index,
      paste0("table_15_locked_test_prediction_file_index_", model_tag),
      "Partitioned locked-test prediction files by predictor set, model, and horizon"
    )
  }
  interpretation_csv <- file.path(PART5_INTERPRETATION_DIR, paste0("locked_test_model_interpretation_", model_tag, ".csv"))
  part5_assert_new_file(interpretation_csv)
  readr::write_csv(model_interpretation, interpretation_csv, na = "")

  if (PART5_SAVE_MEGA_LOCKED_TEST_PREDICTIONS) {
    part5_save_data(locked_predictions, paste0("Panel_Europe_macro_locked_test_predictions_", model_tag))
  } else {
    message("Skipping mega locked-test prediction file; set PART5_SAVE_MEGA_LOCKED_TEST_PREDICTIONS=TRUE to save it.")
  }
  part5_save_data(probability_metrics, paste0("Panel_Europe_macro_locked_test_probability_metrics_", model_tag))
  part5_save_data(threshold_metrics, paste0("Panel_Europe_macro_locked_test_threshold_metrics_", model_tag))
  part5_save_data(residual_calibration_diagnostics, paste0("Panel_Europe_macro_locked_test_residual_calibration_diagnostics_", model_tag))
  part5_save_data(decile_calibration, paste0("Panel_Europe_macro_locked_test_decile_calibration_", model_tag))
  part5_save_data(logit_probit_collinearity, paste0("Panel_Europe_macro_locked_test_logit_probit_collinearity_", model_tag))
  part5_save_data(logit_probit_collinearity_summary, paste0("Panel_Europe_macro_locked_test_logit_probit_collinearity_summary_", model_tag))
  part5_save_data(
    c(
      list(
      manifest = part5_manifest,
      selected_specifications = context$selected_specs,
      probability_metrics = probability_metrics,
      threshold_metrics = threshold_metrics,
      residual_calibration_diagnostics = residual_calibration_diagnostics,
      decile_calibration = decile_calibration,
      best_by_horizon_pr_auc = best_by_horizon_pr_auc,
      best_by_horizon_mcc = best_by_horizon_mcc,
      locked_prediction_file_index = locked_prediction_file_index,
      model_object_index = model_object_index,
      model_interpretation = model_interpretation,
      logit_probit_collinearity = logit_probit_collinearity,
      logit_probit_collinearity_summary = logit_probit_collinearity_summary,
      timing = timing
      ),
      if (PART5_SAVE_PREDICTIONS_IN_ARTIFACT) {
        list(predictions = locked_predictions)
      } else {
        list()
      }
    ),
    paste0("Panel_Europe_macro_part_5_locked_test_artifact_", model_tag)
  )

  part5_model_plot_labels <- c(
    Elastic_Net = "EN",
    GAM = "GAM",
    Logit = "Logit",
    Probit = "Probit",
    Random_Forest = "RF",
    XGBoost = "XGBoost"
  )

  part5_predictor_plot_labels <- c(
    financial_only = "F",
    financial_rating_group = "F+Rg",
    financial_rating_rank = "F+Rr",
    financial_ciss = "F+C",
    financial_ciss_rating_group = "F+C+Rg",
    financial_ciss_rating_rank = "F+C+Rr",
    financial_vstoxx = "F+V",
    financial_vstoxx_rating_group = "F+V+Rg",
    financial_vstoxx_rating_rank = "F+V+Rr",
    financial_macro_delta = "F+dM",
    financial_macro_delta_rating_group = "F+dM+Rg",
    financial_macro_delta_rating_rank = "F+dM+Rr",
    financial_macro_delta_ciss = "F+dM+C",
    financial_macro_delta_ciss_rating_group = "F+dM+C+Rg",
    financial_macro_delta_ciss_rating_rank = "F+dM+C+Rr",
    financial_macro_delta_vstoxx = "F+dM+V",
    financial_macro_delta_vstoxx_rating_group = "F+dM+V+Rg",
    financial_macro_delta_vstoxx_rating_rank = "F+dM+V+Rr",
    financial_macro_pars_stat = "F + M",
    financial_macro_pars_stat_rating_group = "F+M+Rg",
    financial_macro_pars_stat_rating_rank = "F+M+Rr",
    financial_macro_pars_stat_ciss = "F+M+C",
    financial_macro_pars_stat_ciss_rating_group = "F+M+C+Rg",
    financial_macro_pars_stat_ciss_rating_rank = "F+M+C+Rr",
    financial_macro_pars_stat_vstoxx = "F+M+V",
    financial_macro_pars_stat_vstoxx_rating_group = "F+M+V+Rg",
    financial_macro_pars_stat_vstoxx_rating_rank = "F+M+V+Rr"
  )

  part5_predictor_plot_palette <- c(
    financial_only = "#2F5597",
    financial_rating_group = "#5B7DBA",
    financial_rating_rank = "#9BB3DA",
    financial_ciss = "#007C89",
    financial_ciss_rating_group = "#43A6B2",
    financial_ciss_rating_rank = "#9BD5DB",
    financial_vstoxx = "#6F4E9B",
    financial_vstoxx_rating_group = "#9678BF",
    financial_vstoxx_rating_rank = "#C6B6DD",
    financial_macro_delta = "#B45F06",
    financial_macro_delta_rating_group = "#D58A3A",
    financial_macro_delta_rating_rank = "#EDC18F",
    financial_macro_delta_ciss = "#A65F2A",
    financial_macro_delta_ciss_rating_group = "#C7895F",
    financial_macro_delta_ciss_rating_rank = "#E5C0A6",
    financial_macro_delta_vstoxx = "#8E5A83",
    financial_macro_delta_vstoxx_rating_group = "#B082A8",
    financial_macro_delta_vstoxx_rating_rank = "#D8BDD1",
    financial_macro_pars_stat = "#3F7F3F",
    financial_macro_pars_stat_rating_group = "#6FA86F",
    financial_macro_pars_stat_rating_rank = "#B5D6B5",
    financial_macro_pars_stat_ciss = "#4F8C73",
    financial_macro_pars_stat_ciss_rating_group = "#7CB59D",
    financial_macro_pars_stat_ciss_rating_rank = "#C1DED2",
    financial_macro_pars_stat_vstoxx = "#66733A",
    financial_macro_pars_stat_vstoxx_rating_group = "#96A566",
    financial_macro_pars_stat_vstoxx_rating_rank = "#D1D9AE"
  )

  part5_wrap_predictor_labels <- function(labels, width = 40L) {
    labels <- dplyr::recode(labels, !!!part5_predictor_plot_labels, .default = gsub("_", " ", labels))
    vapply(
      labels,
      function(label) paste(strwrap(label, width = width), collapse = "\n"),
      character(1)
    )
  }

  part5_prepare_plot_metrics <- function(data) {
    data %>%
      mutate(
        Model = factor(
          Model,
          levels = names(part5_model_plot_labels),
          labels = unname(part5_model_plot_labels)
        ),
        Predictor_Set = factor(Predictor_Set, levels = names(part5_predictor_plot_palette))
      )
  }

  part5_legend_theme <- theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.text = element_text(size = 7.2),
    legend.key.width = grid::unit(0.38, "cm"),
    legend.key.height = grid::unit(0.38, "cm"),
    plot.margin = margin(t = 6, r = 10, b = 8, l = 8)
  )

  probability_metrics_plot <- part5_prepare_plot_metrics(probability_metrics)
  threshold_metrics_plot <- part5_prepare_plot_metrics(threshold_metrics)
  decile_calibration_plot <- part5_prepare_plot_metrics(decile_calibration)

  ggplot(probability_metrics_plot, aes(x = Horizon_Label, y = PR_AUC, fill = Predictor_Set)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.65) +
    facet_wrap(~ Model) +
    scale_fill_manual(
      values = part5_predictor_plot_palette,
      labels = part5_wrap_predictor_labels,
      drop = FALSE
    ) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.001)) +
    labs(x = NULL, y = "Locked-test PR-AUC", fill = NULL) +
    theme_minimal(base_size = 11) +
    part5_legend_theme +
    guides(fill = guide_legend(nrow = 3, byrow = TRUE, keywidth = grid::unit(0.38, "cm"), keyheight = grid::unit(0.38, "cm")))
  fig_01_path <- file.path(PART5_FIGURE_DIR, paste0("fig_01_locked_test_pr_auc_", model_tag, ".png"))
  part5_assert_new_file(fig_01_path)
  ggsave(fig_01_path, width = 12.5, height = 7.8, dpi = 300)

  ggplot(probability_metrics_plot, aes(x = Horizon_Label, y = ROC_AUC, fill = Predictor_Set)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.65) +
    facet_wrap(~ Model) +
    scale_fill_manual(
      values = part5_predictor_plot_palette,
      labels = part5_wrap_predictor_labels,
      drop = FALSE
    ) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.001)) +
    labs(x = NULL, y = "Locked-test ROC-AUC", fill = NULL) +
    theme_minimal(base_size = 11) +
    part5_legend_theme +
    guides(fill = guide_legend(nrow = 3, byrow = TRUE, keywidth = grid::unit(0.38, "cm"), keyheight = grid::unit(0.38, "cm")))
  fig_02_path <- file.path(PART5_FIGURE_DIR, paste0("fig_02_locked_test_roc_auc_", model_tag, ".png"))
  part5_assert_new_file(fig_02_path)
  ggsave(fig_02_path, width = 12.5, height = 7.8, dpi = 300)

  ggplot(threshold_metrics_plot, aes(x = Horizon_Label, y = MCC, fill = Predictor_Set)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.65, na.rm = TRUE) +
    geom_text(
      data = threshold_metrics_plot[is.na(threshold_metrics_plot$MCC), , drop = FALSE],
      aes(y = 0, label = "n/a", group = Predictor_Set),
      position = position_dodge(width = 0.75),
      vjust = -0.35,
      size = 2.4,
      show.legend = FALSE
    ) +
    facet_wrap(~ Model) +
    scale_fill_manual(
      values = part5_predictor_plot_palette,
      labels = part5_wrap_predictor_labels,
      drop = FALSE
    ) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.001)) +
    labs(
      x = NULL,
      y = "Locked-test MCC",
      fill = NULL,
      caption = "n/a = MCC undefined because the locked-test threshold predicts a single class."
    ) +
    theme_minimal(base_size = 11) +
    part5_legend_theme +
    guides(fill = guide_legend(nrow = 3, byrow = TRUE, keywidth = grid::unit(0.38, "cm"), keyheight = grid::unit(0.38, "cm")))
  fig_03_path <- file.path(PART5_FIGURE_DIR, paste0("fig_03_locked_test_mcc_", model_tag, ".png"))
  part5_assert_new_file(fig_03_path)
  ggsave(fig_03_path, width = 12.5, height = 7.8, dpi = 300)

  ggplot(decile_calibration_plot, aes(x = Mean_Predicted_Probability, y = Observed_Downgrade_Rate, color = Predictor_Set)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
    geom_point(size = 1.8) +
    geom_line(linewidth = 0.5) +
    facet_grid(Model ~ Horizon_Label) +
    scale_color_manual(
      values = part5_predictor_plot_palette,
      labels = part5_wrap_predictor_labels,
      drop = FALSE
    ) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(x = "Mean predicted probability", y = "Observed downgrade rate", color = NULL) +
    theme_minimal(base_size = 10) +
    part5_legend_theme +
    guides(color = guide_legend(nrow = 3, byrow = TRUE, keywidth = grid::unit(0.38, "cm"), keyheight = grid::unit(0.38, "cm")))
  fig_04_path <- file.path(PART5_FIGURE_DIR, paste0("fig_04_locked_test_decile_calibration_", model_tag, ".png"))
  part5_assert_new_file(fig_04_path)
  ggsave(fig_04_path, width = 12.5, height = 8.6, dpi = 300)

  notes <- c(
    "Part 5 locked-test evaluation complete.",
    "",
    paste0("Models evaluated: ", paste(context$models_to_run, collapse = ", ")),
    paste0("Output tag: ", model_tag),
    paste0("Part 4 selection rule: ", PART5_SELECTION_RULE),
    paste0("Output folder: ", PART5_OUTPUT_DIR),
    "The locked test is evaluation-only; no hyperparameter or threshold is selected on the locked test.",
    paste0("Mega locked-test prediction file: ", if_else(PART5_SAVE_MEGA_LOCKED_TEST_PREDICTIONS, "saved", "skipped")),
    paste0("Partitioned locked-test prediction files: ", if_else(PART5_SAVE_SPLIT_LOCKED_TEST_PREDICTIONS, "saved", "skipped")),
    "Use table_07 for the best model by locked-test PR-AUC and table_08 for the best model by locked-test MCC.",
    "Residual and calibration diagnostics are in table_05; decile calibration is in table_06.",
    "Fitted model objects are saved in the models subfolder.",
    "Coefficients and variable-importance summaries are saved in table_12 and in model_interpretation.",
    "Logit/Probit collinearity diagnostics are saved in tables 13 and 14."
  )
  notes_path <- file.path(PART5_OUTPUT_DIR, paste0("locked_test_notes_", model_tag, ".txt"))
  part5_assert_new_file(notes_path)
  writeLines(notes, notes_path)

  message("Part 5 locked-test evaluation complete. Outputs written to: ", PART5_OUTPUT_DIR)
  invisible(
    list(
      manifest = part5_manifest,
      predictions = locked_predictions,
      probability_metrics = probability_metrics,
      threshold_metrics = threshold_metrics,
      residual_calibration_diagnostics = residual_calibration_diagnostics,
      decile_calibration = decile_calibration,
      best_by_horizon_pr_auc = best_by_horizon_pr_auc,
      best_by_horizon_mcc = best_by_horizon_mcc,
      model_object_index = model_object_index,
      model_interpretation = model_interpretation,
      logit_probit_collinearity = logit_probit_collinearity,
      logit_probit_collinearity_summary = logit_probit_collinearity_summary,
      timing = timing
    )
  )
}

running_as_script_part5 <- function() {
  file_arg <- "--file="
  args <- commandArgs(trailingOnly = FALSE)
  any(startsWith(args, file_arg))
}

if (running_as_script_part5()) {
  message("Part 5 V5 locked-test evaluation starting.")
  message("Part 5 V5 specification block: ", PART5_RATING_SPEC)
  message("Part 5 V5 selection rule: ", PART5_SELECTION_RULE)
  message("Part 5 V5 output folder: ", PART5_OUTPUT_DIR)
  run_part5_locked_test()
}





