#' given trial data and model record, score data from corresponding model
#'
#' @param trial_data_task trial data from one task and one dataset
#' @param mod_spec list with entries item_task, dataset, model_set, subset, itemtype, nfact, invariance
#' @param mod_rec ModelRecord object
#' @return tibble with scores
#'
#' @export
score_irt <- \(trial_data_task, mod_spec, mod_rec) {
  message(glue('--Using IRT scoring'))

  if (!is.na(mod_spec$invariance) & mod_spec$invariance %in% c("metric", "configural")) {
    message(glue("Can't rescore with {mod_spec$invariance} models, skipping"))
    return()
  }

  # prep new data for model
  data_filtered <- trial_data_task |> rename(group = "site") |> dedupe_items() |> remove_no_var_items()

  data_wide <- data_filtered |> to_mirt_shape_grouped()
  data_prepped <- data_wide |> select(-"group")
  groups <- data_wide |> pull("group")

  # subset data to items present in model
  overlap_items <- intersect(colnames(data_prepped), items(mod_rec))
  data_aligned <- data_prepped |> select(!!overlap_items)

  # get model parameter values
  mod_vals <- model_vals(mod_rec) |> select(-"parnum")
  if ((!is.na(mod_spec$invariance) & mod_spec$invariance == "scalar") | n_distinct(groups) == 1) {
    mod_vals <- mod_vals |> filter(.data$class != "GroupPars") |> select(-"group") |> distinct()
  }

  # get data parameter structure
  if (model_class(mod_rec) == "SingleGroupClass") {
    data_pars <- mirt::mirt(data = data_aligned, pars = "values")
  } else if (model_class(mod_rec) == "MultipleGroupClass") {
    data_pars <- mirt::multipleGroup(data = data_aligned, group = groups, pars = "values")
  }

  # replace data parameter values with model values
  # TODO: is dropping item from the model that are not in the data problematic?
  data_vals <- data_pars |>
    filter(.data$class != "GroupPars") |>
    select("group", "item", "class", "name", "parnum") |>
    left_join(mod_vals, by = c("item", "class", "name")) |>
    bind_rows(data_pars |> filter(.data$class == "GroupPars"))
  assertthat::assert_that(nrow(data_vals) == nrow(data_pars))

  # set up mirt model for data using constructed parameter values
  if (model_class(mod_rec) == "SingleGroupClass") {
    mod <- mirt::mirt(data = data_aligned, pars = data_vals, TOL = NaN)
  } else if (model_class(mod_rec) == "MultipleGroupClass") {
    mod <- mirt::multipleGroup(data = data_aligned, group = groups, pars = data_vals, TOL = NaN)
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
#'
#' @param runs run data from one task and one dataset
#' @export
score_cat <- \(runs) {
  message(glue('--Using CAT scoring'))
  runs |>
    filter(!is.na(.data$test_comp_theta_estimate)) |>
    select("run_id", score = "test_comp_theta_estimate", score_se = "test_comp_theta_se") |>
    mutate(score_type = "ability_cat")
}

#' scores for PA
#'
#' @param trial_data_task trial data from one task and one dataset
#' @param dataset dataset
#' @export
score_pa <- \(trial_data_task, dataset)  {
  message(glue('--Using PA scoring'))

  pa_max_trials <- list(
    pilot_western_ca_main = 57,
    pilot_uniandes_co_bogota = 20,
    pilot_uniandes_co_rural = 20
  )
  if (!(dataset %in% names(pa_max_trials))) {
    message(glue("Can't rescore task pa for dataset {dataset}, skipping"))
    return()
  }
  trial_data_task |>
    group_by(.data$run_id) |>
    filter(n() > 3) |>
    summarise(score = sum(.data$correct) / pa_max_trials[[dataset]]) |>
    mutate(score_type = "prop_correct")
}

#' scores for SRE
#'
#' @param trial_data_task trial data from one task and one dataset
#' @param dataset dataset
#' @export
score_sre <- \(trial_data_task, dataset) {
  message(glue('--Using SRE scoring'))

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
  ) |> tibble::deframe() |> pluck(task)
}

mod_spec_str <- \(spec) {
  spec[c("model_set", "subset", "itemtype", "nfact", "invariance")] |> discard(is.na) |> paste(collapse = "_")
}

#' score
#'
#' @param task task
#' @param dataset dataset
#' @param trials trial data from one task and one dataset
#' @param runs run data from one task and one dataset
#' @param scoring_table table returned by get_scoring_table
#' @param registry_table  table returned by get_registry_table
#'
#' @export
score <- \(task, dataset, trials, runs, scoring_table, registry_table) {

  message(glue('Scoring data for task "{task}" and dataset "{dataset}"'))

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
    scores <- exec(scoring_fun, trials, dataset)
  } else {
    message(glue('--No scoring method found'))
  }

  scores
}
