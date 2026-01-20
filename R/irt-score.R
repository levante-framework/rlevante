#' given trial data and model record, score data from corresponding model
#' @keywords internal
#'
#' @param trial_data_task trial data from one task and one dataset
#' @param mod_spec list with entries item_task, dataset, model_set, subset, itemtype, nfact, invariance
#' @param mod_rec ModelRecord object
#' @return tibble with scores
score_irt <- \(trial_data_task, mod_spec, mod_rec) {
  message(glue::glue('--Using IRT scoring'))

  # prep new data for model
  data_filtered <- trial_data_task |> rename(group = "site") |> dedupe_items()
  data_wide <- data_filtered |> to_mirt_shape_grouped()
  data_prepped <- data_wide |> select(-"group")
  groups <- data_wide |> pull("group")
  data_group <- unique(groups)

  # handle data having groups that aren't in model
  if (any(!(data_group %in% mod_rec@group_names))) {
    # for metric or configural models, scoring not possible
    if (!is.na(mod_spec$invariance) & mod_spec$invariance %in% c("metric", "configural")) {
      message(glue::glue("For scoring with {mod_spec$invariance} models, all groups in data must be in specified model."))
      return()
      # for scalar models, scoring using any group should be equivalent
    } else if (!is.na(mod_spec$invariance) & mod_spec$invariance == "scalar") {
      data_group <- mod_rec@group_names[[1]]
    }
  }

  # subset data to items present in model
  overlap_items <- intersect(colnames(data_prepped), items(mod_rec))
  data_aligned <- data_prepped |> select(!!overlap_items)
  # add columns with NA values for items present in model but not in data
  missing_items <- setdiff(items(mod_rec), colnames(data_prepped))
  data_aligned[,missing_items] <- NA

  # set up mirt model object for data using parameter values from model record
  mod_vals <- model_vals(mod_rec)
  if (model_class(mod_rec) == "SingleGroupClass") {
    # reconstruct single group model
    mod <- mirt::mirt(data = mod_rec@data, pars = mod_vals, TOL = NaN)
  } else if (model_class(mod_rec) == "MultipleGroupClass") {
    # reconstruct multiple group model
    mod_recon <- mirt::multipleGroup(data = mod_rec@data, group = mod_rec@groups, pars = mod_vals, TOL = NaN)
    # extract single group model for given group
    mod <- mirt::extract.group(mod_recon, group = data_group)
  }

  # get scores from model
  scores <- mirt::fscores(mod, method = "EAP", response.pattern = data_aligned)

  # return scores tibble with better names and run_ids added back in
  scores |>
    as_tibble() |>
    rename(score = "F1", score_se = "SE_F1") |>
    mutate(run_id = rownames(data_prepped), .before = everything()) |>
    mutate(score_type = "ability", scoring_model = mod_spec_str(mod_spec))
}

#' scores from CAT
#' @keywords internal
#'
#' @param runs run data from one task and one dataset
score_cat <- \(runs) {
  message(glue::glue('--Using CAT scoring'))
  runs |>
    filter(!is.na(.data$test_comp_theta_estimate)) |>
    select("run_id", score = "test_comp_theta_estimate", score_se = "test_comp_theta_se") |>
    mutate(score_type = "ability_cat")
}

#' scores for PA
#' @keywords internal
#'
#' @param trial_data_task trial data from one task and one dataset
#' @param dataset dataset
score_pa <- \(trial_data_task, dataset)  {
  message(glue::glue('--Using PA scoring'))

  pa_max_trials <- list(
    pilot_western_ca_main = 57,
    pilot_uniandes_co_bogota = 20,
    pilot_uniandes_co_rural = 20
  )
  if (!(dataset %in% names(pa_max_trials))) {
    message(glue::glue("Can't rescore task pa for dataset {dataset}, skipping"))
    return()
  }
  trial_data_task |>
    group_by(.data$run_id) |>
    filter(n() > 3) |>
    summarise(score = sum(.data$correct) / pa_max_trials[[dataset]]) |>
    mutate(score_type = "prop_correct")
}

#' scores for SRE
#' @keywords internal
#'
#' @param trial_data_task trial data from one task and one dataset
#' @param dataset dataset
score_sre <- \(trial_data_task, dataset) {
  message(glue::glue('--Using SRE scoring'))

  trial_data_task |>
    group_by(.data$run_id) |>
    filter(.data$trial_number <= 180) |>
    summarise(score = (sum(.data$correct) - sum(!.data$correct)) / 180) |>
    mutate(score_type = "guessing_adjusted_number_correct")
}

task_scoring_fun <- \(task) {
  task_metrics <- tibble::tribble(
    ~item_task, ~scoring_fun,
    "hf"     ,  score_irt,
    "sds"    ,  score_irt,
    "mg"     ,  score_irt,
    "math"   ,  score_irt,
    "matrix" ,  score_irt,
    "mrot"   ,  score_irt,
    "trog"   ,  score_irt,
    "vocab"  ,  score_irt,
    "tom"    ,  score_irt,
    "pa"     ,  score_pa,
    "sre"    ,  score_sre,
    "swr"    ,  score_cat
  ) |> tibble::deframe() |> purrr::pluck(task)
}

mod_spec_str <- \(spec) {
  spec[c("model_set", "subset", "itemtype", "nfact", "invariance")] |> purrr::discard(is.na) |> paste(collapse = "_")
}

#' score
#' @keywords internal
#' @export
#'
#' @param task task
#' @param dataset dataset
#' @param trials trial data from one task and one dataset
#' @param runs run data from one task and one dataset
#' @param scoring_table table returned by get_scoring_table
#' @param registry_table  table returned by get_registry_table
score <- \(task, dataset, trials, runs, scoring_table, registry_table) {

  message(glue::glue('Scoring data for task "{task}" and dataset "{dataset}"'))

  irt_tasks <- c("matrix", "mrot", "math", "hf", "mg", "sds", "trog", "vocab", "tom")
  cat_tasks <- c("swr")
  custom_tasks <- list(pa = score_pa, sre = score_sre)

  if (task %in% irt_tasks) {
    mod_spec <- get_model_spec(task, dataset, scoring_table)
    mod_rec <- get_model_record(mod_spec, registry_table)
    scores <- score_irt(trials, mod_spec, mod_rec)
  } else if (task %in% cat_tasks) {
    scores <- score_cat(runs)
  } else if (task %in% names(custom_tasks)) {
    scoring_fun <- custom_tasks[[task]]
    scores <- purrr::exec(scoring_fun, trials, dataset)
  } else {
    message(glue::glue('--No scoring method found'))
    scores <- NULL
  }

  scores
}
