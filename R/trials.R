# helper function for processing trial data

remove_practice_trials <- function(trials) {
  trials |>
    mutate(practice = .data$is_practice_trial |
             str_detect(.data$assessment_stage, "practice") |
             str_detect(.data$assessment_stage, "instructions") |
             str_detect(.data$corpus_trial_type, "training") |
             str_detect(.data$corpus_trial_type, "practice")) |>
    filter(is.na(.data$practice) | !.data$practice) |>
    select(-.data$practice, -.data$is_practice_trial)
}

# adds trial indices
add_trial_numbers <- function(trials) {
  trials |>
    group_by(user_id, run_id) |>
    arrange(server_timestamp) |>
    mutate(trial_number = 1:n()) |>
    ungroup()
}

# get item metadata and join with trials
add_trial_items <- function(trials) {
  trial_items <- get_trial_items()
  trial_id_map <- trial_items |>
    mutate(trials = trials |> str_split(",")) |>
    unnest(trials) |>
    rename(trial_id = trials, item_group = group, item = entry) |>
    mutate(trial_id = str_trim(trial_id))
  trials |> inner_join(trial_id_map, by = "trial_id")
}

# add numeric RTs
convert_rts <- function(trials) {
  trials |> mutate(rt_numeric = suppressWarnings(as.numeric(rt)), .after = rt)
}
