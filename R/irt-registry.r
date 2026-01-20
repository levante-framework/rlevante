#' Fetch model registry file index table
#' @keywords internal
#' @export
fetch_registry_table <- \(version = "current") {
  ds <- redivis::redivis$organization("levante")$dataset(name = "levante_metadata_scoring:e97h", version = version)
  ds$table("model_registry:rqwv")$to_tibble() |>
    mutate(redivis_source = ds$get()$scoped_reference, .before = everything())
}

#' Fetch scoring specification table
#' @keywords internal
#' @export
fetch_scoring_table <- \(version = "current") {
  ds <- redivis::redivis$organization("levante")$dataset("levante_metadata_scoring:e97h", version = version)
  ds$table("scoring_models:t416")$to_tibble() |>
    mutate(redivis_source = ds$get()$scoped_reference, .before = everything())
}

#' get the scoring specification in scoring_table for a given task + dataset
#' @keywords internal
#'
#' @param score_task string indicating task
#' @param score_dataset string indicating dataset
#' @param scoring_table tibble returned by get_scoring_table()
#' @return list with entries item_task, dataset, model_set, subset, itemtype, nfact, invariance
get_model_spec <- \(score_task, score_dataset, scoring_table) {
  mod_spec <- scoring_table |> filter(.data$item_task == score_task, .data$dataset == score_dataset)
  if (nrow(mod_spec) == 0) stop(glue::glue('No scoring model specified for task "{score_task}" and dataset "{score_dataset}"'))
  if (nrow(mod_spec) > 1) stop(glue::glue('Multiple scoring models specified for task "{score_task}" and dataset "{score_dataset}"'))
  mod_spec |> as.list()
}

#' convert model spec to filename
#' @keywords internal
#' @export
model_spec_filename <- \(spec) {
  mod_basename <- spec[c("item_task", "itemtype", "nfact", "invariance")] |> purrr::discard(is.na) |> paste(collapse = "_")
  mod_filename <- glue::glue("{spec$item_task}/{spec$model_set}/{spec$subset}/{mod_basename}.rds")
}

#' fetch file from model registry
#' @keywords internal
#' @export
get_registry_file <- \(mod_filename, registry_table) {
  # get file_id corresponding to filename
  mod_file <- registry_table |> filter(.data$file_name == mod_filename) |> pull("file_id")

  # download file
  mod_path <- redivis::redivis$file(mod_file)$download(overwrite = TRUE)

  # read in file
  mod_rec <- readr::read_rds(mod_path)

  # delete file
  file.remove(mod_path)

  mod_rec
}

#' get the model record indexed in registry_table for a given scoring specification
#' @keywords internal
#'
#' @param spec list with entries item_task, model_set, subset, itemtype, nfact, invariance
#' @param registry_table tibble returned by get_registry_table()
#' @return ModelRecord object
get_model_record <- \(spec, registry_table) {

  # construct filename from specification
  mod_filename <- model_spec_filename(spec)

  # fetch corresponding file from model registry
  get_registry_file(mod_filename, registry_table)
}

# invariance as recorded in mirt
# (subset of c("free_means", "free_var", "intercepts", "slopes"))
# to shorthand name
# (one of c("configural", "metric", "scalar_intercepts", "scalar_slopes_and_intercepts", "full")
translate_invariance <- \(invariance_terms) {
  invariances <- list(
    configural = "",
    metric = c("slopes"),
    scalar = c("free_means", "free_var", "intercepts", "slopes")
  )

  invariances |>
    purrr::map(\(inv) setequal(inv, invariance_terms)) |>
    purrr::keep(identity) |>
    names()
}
