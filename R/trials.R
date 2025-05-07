# helper function for processing trial data

remove_practice_trials <- function(trials) {
  trials |>
    mutate(practice = .data$is_practice_trial |
             stringr::str_detect(.data$assessment_stage, "practice") |
             stringr::str_detect(.data$assessment_stage, "instructions") |
             stringr::str_detect(.data$corpus_trial_type, "training") |
             stringr::str_detect(.data$corpus_trial_type, "practice")) |>
    filter(is.na(.data$practice) | !.data$practice) |>
    select(-c("practice", "is_practice_trial"))
}

# adds trial indices
add_trial_numbers <- function(trials) {
  trials |>
    group_by(.data$user_id, .data$run_id) |>
    arrange(.data$server_timestamp) |>
    mutate(trial_number = 1:n()) |>
    ungroup()
}

# get item metadata and join with trials
add_trial_items <- function(trials) {
  trial_items <- get_trial_items()
  trial_id_map <- trial_items |>
    mutate(trials = map(trials, jsonlite::fromJSON)) |>
    unnest(trials) |>
    rename(trial_id = "trials", item_group = "group", item = "entry")

  trials |>
    # sre | pa -> item_id, swr -> answer
    mutate(roar_item_id = case_when(
      stringr::str_detect(task_id, "^pa(-|$)") ~ glue("pa_{item_id}"),
      stringr::str_detect(task_id, "^sre(-|$)") ~ glue("sre_{item_id}"),
      stringr::str_detect(task_id, "^swr(-|$)") ~ glue("swr_{answer}"),
    ) |> as.character()) |>
    left_join(trial_id_map, by = "trial_id") |>
    mutate(item_uid = if_else(!is.na(roar_item_id), roar_item_id, item_uid),
           item_task = if_else(is.na(item_task), task_id, item_task),
           item_group = replace_na(item_group, ""),
           item = replace_na(item, "")) |>
    filter(!is.na(item_uid)) |>
    select(-roar_item_id, -item_id, -answer)
}

# add numeric RTs
convert_rts <- function(trials) {
  trials |> mutate(rt_numeric = suppressWarnings(as.numeric(.data$rt)),
                   .after = .data$rt)
}

code_numberline <- function(trials, threshold = 0.15) {
  slider_trials <- trials |>
    filter(.data$item_group == "slider") |>
    tidyr::separate_wider_delim(.data$item, "_",
                                names = c("answer", "max_value"),
                                cols_remove = FALSE) |>
    mutate(answer = .data$answer |> stringr::str_replace("^0", "0."),
           across(c(.data$answer, .data$max_value), as.numeric),
           correct = (abs(as.numeric(.data$response) - .data$answer) / .data$max_value < threshold)) |>
    select(-c("answer", "max_value"))
  trials |>
    filter(.data$item_group != "slider") |>
    bind_rows(slider_trials)
}
