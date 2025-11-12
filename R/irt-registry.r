#' Get model registry file index table
#' @export
get_registry_table <- \() {
  redivis::redivis$organization("levante")$dataset("scoring:e97h")$table("model_registry:rqwv")$to_tibble()
}

#' Get scoring specification table
#' @export
get_scoring_table <- \() {
  redivis::redivis$organization("levante")$dataset("scoring:e97h")$table("scoring_models:t416")$to_tibble()
}

#' get the scoring specification in scoring_table for a given task + dataset
#'
#' @param scoring_table tibble returned by get_scoring_table()
#' @param score_task string indicating task
#' @param score_dataset string indicating dataset
#' @return list with entries item_task, dataset, model_set, subset, itemtype, nfact, invariance
#'
#' @export
get_model_spec <- \(scoring_table, score_task, score_dataset) {
  mod_spec <- scoring_table |> filter("item_task" == score_task, "dataset" == score_dataset)
  if (nrow(mod_spec) == 0) stop(glue('No scoring model specified for task "{score_task}" and dataset "{score_dataset}"'))
  if (nrow(mod_spec) > 1) stop(glue('Multiple scoring models specified for task "{score_task}" and dataset "{score_dataset}"'))
  mod_spec |> as.list()
}

#' get the model record indexed in registry_table for a given scoring specification
#'
#' @param spec list with entries item_task, dataset, model_set, subset, itemtype, nfact, invariance
#' @param registry_table tibble returned by get_registry_table()
#' @return ModelRecord object
#'
#' @export
get_model_record <- \(spec, registry_table) {

  # construct filename from specification
  mod_filename <- glue("{spec$item_task}/{spec$model_set}/{spec$subset}/{spec$item_task}_{spec$itemtype}_{spec$nfact}_{spec$invariance}.rds")

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

# map invariance as recorded in mirt
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
    map(\(inv) setequal(inv, invariance_terms)) |>
    keep(identity) |>
    names()
}
