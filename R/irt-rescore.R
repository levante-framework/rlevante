#' given trial data and model record, score data from corresponding model
#'
#' @param trial_data_task trial data from one task and one dataset
#' @param mod_spec list with entries item_task, dataset, model_set, subset, itemtype, nfact, invariance
#' @param mod_rec ModelRecord object
#' @return tibble with scores
#'
#' @export
rescore <- \(trial_data_task, mod_spec, mod_rec) {

  message(glue('Scoring data for task "{mod_spec$item_task}" and dataset "{mod_spec$dataset}"'))
  if (mod_spec$invariance == "metric") {
    message("Can't rescore with metric models, skipping")
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
  if (mod_spec$invariance == "scalar") {
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
    left_join(mod_vals) |>
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
    rename(ability = "F1", se = "SE_F1") |>
    mutate(run_id = rownames(data_prepped), .before = everything()) #|>
    # mutate(model = mod_file)
}
