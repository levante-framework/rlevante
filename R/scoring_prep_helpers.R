#' recode correctness and/or items for several tasks
#'
#' @param df trial data
#' @param slider_threshold max normalized distance from slider target
#'
#' @export
recode_trials <- \(df, slider_threshold = 0.15) {
  item_fixes <- tribble(
    ~item_uid,             ~answer_fixed,
    "math_subtract_37_24", "13"
  )

  # recode correctness for HF, SDS, math slider items, and items with wrong answers
  df |>
    mutate(original_correct = correct, .after = correct) |>
    recode_hf() |>
    # recode_sds() |>
    recode_wrong_items(item_fixes) |>
    recode_slider(slider_threshold) |>
    recode_tom() |>
    # set chance values for slider items accordingly
    mutate(chance = if_else(item_group == "slider", 1 / slider_threshold / 100, chance),
           chance = chance |> replace_na(0))
}

#' recode correctness + reclassify items for HF
#'
#' @inheritParams recode_trials
#'
#' @export
recode_hf <- \(df) {
  hf_trials <- df |>
    filter(.data$item_task == "hf") |>
    # code too fast/slow RTs as incorrect
    mutate(response_fast = .data$rt_numeric < 200, response_slow = .data$rt_numeric > 2000,
           correct = .data$correct & !.data$response_fast & !.data$response_slow) |>
    select(-"response_fast", -"response_slow") |>
    # recode items based on whether they're same as previous item
    group_by(.data$run_id, .data$item_group) |>
    arrange(.data$trial_number) |>
    mutate(hf_type = case_when(
      is.na(lag(.data$item)) ~ "start",
      .data$item == lag(.data$item) ~ "stay",
      .data$item != lag(.data$item) ~ "switch")) |>
    ungroup() |>
    mutate(item = paste(.data$item, .data$hf_type, sep = "_"),
           item_uid = paste(.data$item_group, .data$item, sep = "_")) |>
    # filter(hf_type != "start") |>
    select(-"hf_type")

  df |>
    filter("item_task" != "hf") |>
    bind_rows(hf_trials)
}

#' recode correctness for slider
#'
#' @inheritParams recode_trials
#'
#' @export
recode_slider <- \(df, slider_threshold) {
  slider_trials <- df |>
    filter(.data$item_group == "slider") |>
    # get target and max values out of item
    tidyr::separate_wider_delim(cols = "item", "_",
                                names = c("target", "max_value"),
                                cols_remove = FALSE) |>
    # convert target and max values to numeric and compute if within threshold
    mutate(target = .data$target |> stringr::str_replace("^0", "0."),
           across(c(.data$target, .data$max_value), as.numeric),
           correct = (abs(as.numeric(.data$response) - .data$target) / .data$max_value < slider_threshold)) |>
    # remove trials where response greater than max value (must be from a bug)
    filter(as.numeric(.data$response) <= .data$max_value) |>
    select(-c("target", "max_value"))
  df |>
    filter(.data$item_group != "slider") |>
    bind_rows(slider_trials)
}

#' recode correctness for items with wrong answers
#'
#' @inheritParams recode_trials
#' @param wrong_items tibble with columns item_uid and answer_fixed
#'
#' @export
recode_wrong_items <- \(df, wrong_items) {
  wrong_trials <- df |>
    right_join(wrong_items) |>
    mutate(correct = !is.na(.data$response) & .data$response == .data$answer_fixed)
  df |>
    anti_join(wrong_items) |>
    bind_rows(wrong_trials)
}

#' recode items for ToM
#'
#' @inheritParams recode_trials
#' @export
recode_tom <- \(df) {
  tom <- df |> filter(.data$item_task == "tom")
  tom_disagg <- tom |>
    mutate(story = stringr::str_extract(.data$item_original, "^[0-9]+"),
           item_uid = glue("{item_task}_story{story}_{item_group}_{item}"))
  df |>
    filter(.data$item_task != "tom") |>
    bind_rows(tom_disagg)
}

# source("rescore_sds.R")
# recode_sds <- \(df) {
#   sds <- df |>
#     filter(item_task == "sds") |> #, site != "us_pilot", item_group!="3unique") |>
#     filter(!str_detect(response, "mittel|rote|gelb|blau|grün")) |>
#     filter(!(site == "pilot_western_ca" & timestamp < "2025-02-21"))
#
#   # ToDo: apply per user and run to 3-match and 4-match (maybe test on 2-match? shouldn't influence outcome)
#   dimension_indices <- c(size = 1, color = 2, shape = 3, number = 4, bgcolor = 5)
#
#   sds_parsed <- sds |>
#     filter(!item_group %in% c("dimensions", "same")) |>
#     mutate(selection = map(response, parse_response)) |>
#     select(site, run_id, trial_id, trial_type = item_group, item, response,
#            selection, correct, timestamp)
#
#   # Order by user, run, and timestamp (or trial_number)
#   sds_trials <- sds_parsed |>
#     group_by(run_id, item_group) |>
#     arrange(timestamp, .by_group = TRUE) |>
#     mutate(trial_index = NA_integer_) |>
#     mutate(trial_index = if_else(item == "choice1", 1L, NA_integer_)) |>
#     mutate(trial_index = cumsum(!is.na(trial_index))) |>
#     ungroup() |>
#     filter(trial_index != 0)
#
#   sds_disambig <- sds_trials %>%
#     group_by(run_id, trial_type, trial_index) %>%
#     mutate(
#       item = case_when(
#         trial_type %in% c("3match", "4match") ~ paste0("choice", row_number()),
#         TRUE ~ item
#       )
#     ) %>%
#     ungroup()
#
#   # Nest by trial
#   sds_nested <- sds_disambig |>
#     select(run_id, trial_type, trial_index, item, response, selection, correct, trial_id) |>
#     group_by(run_id, trial_type, trial_index) |>
#     arrange(item) |>
#     nest(data = c(item, response, selection, correct, trial_id)) |>
#     ungroup()
#
#   # Score each row based on accumulating previous selections
#   sds_rescored <- sds_nested |>
#     ungroup() |>
#     mutate(trial_correct = map2(data, trial_type, \(df, type) {
#       ignore_dims <- case_when(
#         type == "same" ~ list(c("number", "bgcolor")),
#         type == "2match" ~ list(c("number", "bgcolor")),
#         type %in% c("3match", "4match") ~ list(c("size")),
#         TRUE ~ list(character(0))
#       )[[1]]
#
#       previous <- list()
#       map_lgl(df$selection, \(sel) {
#         result <- compare_selections(sel, previous, ignore_dims)
#         previous <<- append(previous, list(sel))
#         result
#       })
#     })) |>
#     unnest(c(data, trial_correct))
#
#   sds_trials <- sds |>
#     select(-item_uid) |>
#     left_join(sds_rescored |> select(trial_id, trial_correct, trial_item = item, trial_index)) |>
#     mutate(#original_correct = correct,
#       correct = if_else(!is.na(trial_correct), trial_correct, correct),
#       item = if_else(!is.na(trial_item), trial_item, item),
#       item_uid = paste(item_task, item_group, item, sep = "_")) |>
#     # original_item = item,
#     # ) |>
#     select(-trial_correct, -trial_item)
#
#   df |>
#     filter(item_task != "sds") |>
#     bind_rows(sds_trials)
# }
