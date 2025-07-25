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
add_item_ids <- function(trials) {

  # get item IDs coded by trial
  trial_items <- get_trial_items()
  trial_map <- trial_items |>
    mutate(trials = trials |> map(jsonlite::fromJSON) |> map(unlist)) |>
    unnest(trials) |>
    select(item_uid_trial = item_uid, trial_id = trials)

  # get item IDs coded by item (corpus_trial_type, item, answer, distractors)
  mapping_items <- get_mapping_items()
  item_map <- mapping_items |>
    mutate(across(c(corpus_trial_type, item, answer, distractors),
                  \(s) replace_na(s, ""))) |>
    mutate(item_key = paste(corpus_trial_type, item, answer, distractors) |>
             str_trim()) |>
    select(item_uid_mapping = item_uid, item_key) |>
    distinct()

  # fixes for wrong item UIDs in item banks
  itembank_recodes <- c(
    "sds_same_same"          = "sds_same_"            ,
    "math_compare_65_67"     = "math_compare_67_65"   ,
    "math_compare_27_36"     = "math_compare_36_27"   ,
    "math_compare_390_435"   = "math_compare_435_390" ,
    "math_compare_69_82"     = "math_compare_82_69"   ,
    "math_compare_823_861"   = "math_compare_861_823" ,
    "math_fraction_12_14_34" = "math_fraction_12_14"  ,
    "math_fraction_18_48_58" = "math_fraction_18_48"  ,
    "math_fraction_32_14_74" = "math_fraction_32_14"  ,
    "math_fraction_16_13_12" = "math_fraction_16_13"  ,
    "math_subtract_17_12"    = "math_subtract_17_!2"  ,
    "ha_knock_action"        = "tom_knock_papers_action",
    "ha_knock_attribution"   = "tom_knock_papers_attribution"
  )

  trials_prepped <- trials |>
    # create item IDs for ROAR tasks (sre | pa -> item_id, swr -> answer)
    mutate(item_uid_roar = case_when(
      stringr::str_detect(task_id, "^pa(-|$)") ~ glue("pa_{item_id}"),
      stringr::str_detect(task_id, "^sre(-|$)") ~ glue("sre_{item_id}"),
      stringr::str_detect(task_id, "^swr(-|$)") ~ glue("swr_{answer}"),
    ) |> as.character()) |>
    # fix wrong item IDs in item banks
    mutate(item_uid = item_uid |>
             stringr::str_replace("^mrot_3d_.*?_", "mrot_3d_shape_") |>
             stringr::str_replace("^tom_ha_", "ha_") |>
             stringr::str_replace("^vocab__", "vocab_word_") |>
             forcats::fct_recode(!!!itembank_recodes)) |>
    # remove stray SDS instruction items
    filter(is.na(item_uid) | !stringr::str_detect(item_uid, "-instr")) |>
    # recode memory-game answers
    mutate(answer = if_else(task_id == "memory-game", as.character(str_count(answer, ":")), answer)) |>
    # create item key for joining with item map
    mutate(across(c(corpus_trial_type, item, answer, distractors),
                  \(s) replace_na(s, ""))) |>
    mutate(item_key = paste(corpus_trial_type, item, answer, distractors) |>
             str_trim()) |>
    # remove trials with no item information
    filter(!is.na(item_uid) | !is.na(item_key))

  trials_joined <- trials_prepped |>
    select(trial_id, task_id, item_key, item_uid, item_uid_roar) |>
    # join in trial item ID map
    left_join(trial_map, by = "trial_id") |>
    # join in item mapping item ID map
    left_join(item_map, by = "item_key")

  trials_mapped <- trials_joined |>
    # move all item ID columns to rows
    tidyr::pivot_longer(contains("item_uid"),
                        names_to = "item_uid_source", values_to = "item_uid") |>
    # filter to each trial's present item IDs
    group_by(trial_id, task_id) |>
    filter(!is.na(item_uid)) |>
    # collapse item ID sources
    group_by(trial_id, task_id, item_uid) |>
    summarise(item_uid_source = list(item_uid_source)) |>
    ungroup()

  # check that no trials have multiple conflicted item IDs
  conflicts <- trials_mapped |> group_by(trial_id) |> filter(n() > 1)
  assertthat::assert_that(nrow(conflicts) == 0)

  # join mapped trials back into overall trials
  trials_prepped |>
    select(-item_uid) |>
    left_join(trials_mapped, by = c("task_id", "trial_id")) |>
    filter(item_key != "")
}

add_item_metadata <- function(trials) {
  corpus_items <- get_corpus_items() |> distinct()
  trials |>
    filter(!is.na(item_uid)) |>
    left_join(corpus_items, by = "item_uid") |>
    # code item_task for roar tasks
    mutate(item_task = if_else(
      item_uid_source == "item_uid_roar",
      str_extract(task_id, "^[A-z]*"),
      item_task
    )) |>
    rename(item_original = item, item_group = group, item = entry)
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
