### functions to prep data for modeling

#' add identifiers for each instance of each item
#' @keywords internal
#'
#' @param df trial data
#' @param item_sep string to put in between item_uid and instance index (defaults to "-")
dedupe_items <- function(df, item_sep = "-") {
  df |>
    # group_by(user_id, item_uid) |>
    group_by(.data$run_id, .data$item_uid) |>
    mutate(instance = seq_along(.data$item_uid)) |> # i
    ungroup() |>
    mutate(item_inst = glue::glue("{item_uid}{item_sep}{instance}")) # item_i
}

#' remove items with no variance
#' @keywords internal
#'
#' @param df trial data
remove_no_var_items <- function(df) {
  df |>
    group_by(.data$item_inst) |>
    filter(n_distinct(.data$correct) > 1) |>
    ungroup()
}

#' remove items that aren't shared across all groups
#' @keywords internal
#'
#' @param df trial data
remove_nonshared_items <- function(df) {
  n_groups_total <- length(unique(df$group))

  df |>
    group_by(.data$item_uid) |>
    mutate(n_groups = n_distinct(.data$group)) |>
    ungroup() |>
    filter(.data$n_groups == n_groups_total) # need to be in N or more groups
}

#' remove items with no invariance in any single group
#' @keywords internal
#'
#' @inheritParams remove_no_var_items
remove_no_var_items_bygroup <- function(df) {
  df |>
    group_by(.data$item_inst, .data$group) |>
    mutate(n_cat_group = n_distinct(.data$correct)) |>
    group_by(.data$item_inst) |>
    filter(n_distinct(n_cat_group) == 1) |>
    ungroup()
}

#' format data for mirt
#' @keywords internal
#'
#' @param df trial data
to_mirt_shape <- function(df) {
  df |>
    mutate(correct = as.numeric(.data$correct)) |> # values to numeric
    select("run_id", "item_inst", "correct") |>
    tidyr::pivot_wider(names_from = "item_inst", values_from = "correct") |> # column for each item
    tibble::column_to_rownames("run_id") # user_id to rownames
}

#' format data for mirt
#' @keywords internal
#'
#' @param df trial data
to_mirt_shape_grouped <- function(df) {
  df |>
    mutate(correct = as.numeric(.data$correct)) |> # values to numeric
    select("run_id", "group", "item_inst", "correct") |>
    tidyr::pivot_wider(names_from = "item_inst", values_from = "correct") |> # column for each item
    tibble::column_to_rownames("run_id") # user_id to rownames
}
