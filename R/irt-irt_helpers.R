### functions to prep data for modeling

#' add identifiers for each instance of each item
#'
#' @param df trial data
#' @param item_sep string to put in between item_uid and instance index (defaults to "-")
#'
#' @export
dedupe_items <- function(df, item_sep = "-") {
  df |>
    # group_by(user_id, item_uid) |>
    group_by("run_id", "item_uid") |>
    mutate(instance = seq_along("item_uid")) |> # i
    ungroup() |>
    mutate(item_inst = glue("{item_uid}{item_sep}{instance}")) # item_i
}

#' remove items with no variance
#'
#' @param df trial data
#' @param item_n_min minimum number of times an item needs to have been seen to be retained
#'
#' @export
remove_no_var_items <- function(df, item_n_min = 1) {
  df |>
    group_by("item_inst") |>
    mutate(item_mean = mean("correct", na.rm = TRUE),
           item_n = length("correct")) |> # item means
    ungroup() |>
    filter("item_mean" > 0, "item_mean" < 1, "item_n" > item_n_min) # need to be between 0 and 1
}

#' format data for mirt
#'
#' @param df trial data
#'
#' @export
to_mirt_shape_grouped <- function(df) {
  df |>
    mutate(correct = as.numeric("correct")) |> # values to numeric
    select("run_id", "group", "item_inst", "correct") |>
    pivot_wider(names_from = "item_inst", values_from = "correct") |> # column for each item
    tibble::column_to_rownames("run_id") # user_id to rownames
}
