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
    mutate(original_correct = .data$correct, .after = "correct") |>
    recode_hf() |>
    recode_sds() |>
    recode_wrong_items(item_fixes) |>
    recode_slider(slider_threshold) |>
    recode_tom() |>
    # set chance values for slider items accordingly
    mutate(chance = if_else(.data$item_group == "slider", 1 / slider_threshold / 100, .data$chance),
           chance = .data$chance |> replace_na(0))
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
    filter(.data$item_task != "hf") |>
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
    bind_rows(wrong_trials) |>
    select(-"answer_fixed")
}

#' recode items for ToM
#'
#' @inheritParams recode_trials
#' @export
recode_tom <- \(df) {
  tom <- df |> filter(.data$item_task == "tom")
  tom_disagg <- tom |>
    mutate(story = stringr::str_extract(.data$item_original, "^[0-9]+"),
           item_uid = glue("{item_task}_story{story}_{item_group}_{item}")) |>
    select(-"story")
  df |>
    filter(.data$item_task != "tom") |>
    bind_rows(tom_disagg)
}

#' recode correctness for SDS
#'
#' @inheritParams recode_trials
#' @export
recode_sds <- function(df) {

  sds_data <- df |>
    filter(.data$item_task == "sds" & str_detect(.data$item_group, "match")) |>
    filter(!str_detect(.data$response, "mittel|rote|gelb|blau|gr\u00FCn")) |>
    filter(!(.data$dataset == "pilot_western_ca_main" & .data$timestamp < "2025-02-21"))

  sds_indexed <- sds_data |>
    mutate(different = str_detect(.data$item_original, "different")) |>
    group_by(.data$run_id, .data$item_group) |>
    arrange(.data$timestamp, .by_group = TRUE) |>
    # use positions of "choice1" to infer a trial index grouping choices together
    mutate(trial_index = if_else(.data$item == "choice1", 1, 0)) |>
    mutate(trial_index = cumsum(.data$trial_index)) |>
    # use positions of "different" prompt to infer trial index
    mutate(trial_index_s = as.numeric(!.data$different)) |>
    mutate(trial_index_s = cumsum(.data$trial_index_s)) |>
    ungroup()

  sds_match <- sds_indexed |>
    # remove blocks that have any mis-indexed trials
    group_by(.data$run_id, .data$item_group) |>
    filter(all(.data$trial_index == .data$trial_index_s)) |>
    # remove trials if they have fewer (or too many) rows than they should
    # e.g. only 2 rows for 3match
    mutate(match_k = str_extract(.data$item_group, "^.") |> as.numeric()) |>
    group_by(.data$run_id, .data$item_group, .data$trial_index) |>
    filter(n() == unique(.data$match_k)) |>
    # remove trials that don't have consistent response options for every response
    filter(n_distinct(.data$distractors) == 1) |>
    ungroup() |>
    # remove rows with any response that isn't two cards
    # filter(str_count(response, ":") == 2) |>
    select("run_id", "trial_index", "item_group", "match_k", "trial_id",
           "item", resp = "response", opts = "distractors", "correct", "original_correct")

  # parse response and options strings into vectors of stimuli
  sds_opts <- sds_match |>
    mutate(resp_parsed = .data$resp |> map(parse_response) |> map(sort),
           opts_parsed = .data$opts |> map(parse_response) |> map(sort))

  sds_coded <- sds_opts |>
    # code dimension values for each stimulus in response and options
    mutate(resp_coded = map2(.data$resp_parsed, .data$item_group, code_dims),
           opts_coded = map2(.data$opts_parsed, .data$item_group, code_dims))

  sds_dims <- sds_coded |>
    mutate(opts_dims = map(.data$opts_coded, match_opts_dims),
           resp_dims = map2(.data$resp_coded, .data$opts_dims, match_resp_dims)) |>
    mutate(n_matches = map_int(.data$opts_dims, sum))

  sds_correct <- sds_dims |>
    mutate(subtrial_match = map_int(.data$resp_dims, length) > 0) |>
    select("run_id", "item_group", "trial_index", "trial_id", "resp", "subtrial_match") |>
    nest(trials = c("trial_id", "resp", "subtrial_match")) |>
    mutate(new = map(.data$trials, \(tr) map_lgl(1:nrow(tr), \(i) i == 1 | !(tr$resp[i] %in% tr$resp[1:(i-1)]))),
           correct = map2(.data$trials, .data$new, \(tr, ne) tr |> mutate(new = ne, correct = .data$subtrial_match & new))) |>
    select(-"new", -"trials") |>
    unnest("correct") |>
    select("run_id", "trial_id", "correct")

  sds_trials <- sds_data |> select(-"correct") |> inner_join(sds_correct)

  df |>
    filter(!(.data$item_task == "sds" & str_detect(.data$item_group, "match"))) |>
    bind_rows(sds_trials)
}
