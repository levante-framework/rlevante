# helper function for processing trial data

remove_practice_trials <- function(trials) {
  trials |>
    mutate(practice = .data$is_practice_trial |
             str_detect(.data$assessment_stage, "practice") |
             str_detect(.data$corpus_trial_type, "practice")) |>
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

add_item_ids <- function(trials) {

  # get item IDs coded by trial
  trial_items <- get_trial_items()
  trial_map <- trial_items |>
    mutate(trials = trials |> map(jsonlite::fromJSON) |> map(unlist)) |>
    unnest(trials) |>
    select(item_uid_trial = "item_uid", trial_id = "trials")

  # get item IDs coded by item (corpus_trial_type, item, answer, distractors)
  mapping_items <- get_mapping_items()
  item_map <- mapping_items |>
    mutate(across(c("corpus_trial_type", "item", "answer", "distractors"),
                  \(s) replace_na(s, ""))) |>
    mutate(item_key = paste(.data$corpus_trial_type, .data$item,
                            .data$answer, .data$distractors) |>
             str_trim()) |>
    select(item_uid_mapping = "item_uid", "item_key") |>
    distinct()

  # fixes for wrong item UIDs in item banks
  itembank_recodes <- c(
    "sds_same_same"              = "sds_same_",
    "math_compare_65_67"         = "math_compare_67_65",
    "math_compare_27_36"         = "math_compare_36_27",
    "math_compare_390_435"       = "math_compare_435_390",
    "math_compare_69_82"         = "math_compare_82_69",
    "math_compare_823_861"       = "math_compare_861_823",
    "math_fraction_12_13_16"   = "math_fraction_12_13",
    "math_fraction_12_14_34"   = "math_fraction_12_14",
    "math_fraction_13_17_421"  = "math_fraction_13_17",
    "math_fraction_13_23_1"    = "math_fraction_13_23",
    "math_fraction_13_25_1115" = "math_fraction_13_25",
    "math_fraction_14_24_34"   = "math_fraction_14_24",
    "math_fraction_16_13_12"   = "math_fraction_16_13",
    "math_fraction_17_314_514" = "math_fraction_17_314",
    "math_fraction_18_48_58"   = "math_fraction_18_48",
    "math_fraction_19_110_190" = "math_fraction_19_110",
    "math_fraction_23_13_13"   = "math_fraction_23_13",
    "math_fraction_28_39_712"  = "math_fraction_28_39",
    "math_fraction_32_14_74"   = "math_fraction_32_14",
    "math_fraction_34_12_14"   = "math_fraction_34_12",
    "math_fraction_34_716_516" = "math_fraction_34_716",
    "math_fraction_37_114_514" = "math_fraction_37_114",
    "math_fraction_39_13_23"   = "math_fraction_39_13",
    "math_fraction_48_18_38"   = "math_fraction_48_18",
    "math_fraction_178_54_78"  = "math_fraction_178_54",
    "math_fraction_34_716_516" = "math_fraction_34_7_16",
    # "math_fraction_512_16_14"  = "math_fraction_512_16",
    # "math_fraction_512_16_712" = "math_fraction_512_16",
    "math_fraction_710_25_310" = "math_fraction_710_25",
    "math_subtract_17_12"        = "math_subtract_17_!2",
    "math_line_639_1000"         = "math_line_649_1000",
    "math_missing_x_300_400_500" = "math_missing_300_400_500",
    "ha_knock_action"            = "tom_knock_papers_action",
    "ha_knock_attribution"       = "tom_knock_papers_attribution"
  )

  trials_prepped <- trials |>
    # create item IDs for ROAR tasks (sre | pa -> item_id, swr -> answer)
    mutate(item_uid_roar = case_when(
      str_detect(task_id, "^pa(-|$)") ~ glue("pa_{item_id}"),
      str_detect(task_id, "^sre(-|$)") ~ glue("sre_{item_id}"),
      str_detect(task_id, "^swr(-|$)") ~ glue("swr_{answer}"),
    ) |> as.character()) |>
    # fix wrong item IDs in item banks
    mutate(item_uid = .data$item_uid |>
             str_replace("^mrot_3d_.*?_", "mrot_3d_shape_") |>
             str_replace("^mrot_(.*)_200", "mrot_\\1_160") |>
             str_replace("^mrot_(.*)_240", "mrot_\\1_120") |>
             str_replace("^mrot_(.*)_280", "mrot_\\1_080") |>
             str_replace("^mrot_(.*)_320", "mrot_\\1_040") |>
             str_replace("^tom_ha_", "ha_") |>
             str_replace("^vocab__", "vocab_word_") |>
             forcats::fct_recode(!!!itembank_recodes) |>
             as.character() |>
             na_if("math_fraction_512_16") |>
             na_if("mg_forward_3grid_len2")) |>
    # remove stray SDS instruction items
    filter(is.na(.data$item_uid) | !str_detect(.data$item_uid, "-instr")) |>
    # recode memory-game answers
    mutate(answer = if_else(.data$task_id == "memory-game",
                            as.character(str_count(.data$answer, ":")),
                            .data$answer)) |>
    # create item key for joining with item map
    mutate(across(c(.data$corpus_trial_type, .data$item, .data$answer, .data$distractors),
                  \(s) replace_na(s, ""))) |>
    mutate(item_key = paste(.data$corpus_trial_type, .data$item, .data$answer, .data$distractors) |>
             str_trim()) |>
    # remove trials with no item information
    filter(!is.na(.data$item_uid) | !is.na(.data$item_key))

  trials_joined <- trials_prepped |>
    select("trial_id", "task_id", "item_key", "item_uid", "item_uid_roar") |>
    # join in trial item ID map
    left_join(trial_map, by = "trial_id") |>
    # join in item mapping item ID map
    left_join(item_map, by = "item_key")

  trials_mapped <- trials_joined |>
    # move all item ID columns to rows
    tidyr::pivot_longer(contains("item_uid"),
                        names_to = "item_uid_source", values_to = "item_uid") |>
    # filter to each trial's present item IDs
    group_by(.data$trial_id, .data$task_id) |>
    filter(!is.na(.data$item_uid)) |>
    # collapse item ID sources
    group_by(.data$trial_id, .data$task_id, .data$item_uid) |>
    summarise(item_uid_source = list(.data$item_uid_source)) |>
    ungroup()

  # check that no trials have multiple conflicted item IDs
  conflicts <- trials_mapped |> group_by(.data$trial_id) |> filter(n() > 1) |> ungroup()
  # message(nrow(conflicts))
  # assertthat::assert_that(nrow(conflicts) == 0)

  # join mapped trials back into overall trials
  trials_prepped |>
    select(-"item_uid") |>
    left_join(trials_mapped, by = c("task_id", "trial_id")) |>
    filter(!is.na(.data$item_uid) | .data$item_key != "")
}


add_item_metadata <- function(trials) {
  corpus_items <- get_corpus_items() |> distinct()
  trials |>
    filter(!is.na(.data$item_uid)) |>
    left_join(corpus_items, by = "item_uid") |>
    # code item_task for roar tasks
    mutate(item_task = if_else(
      .data$item_uid_source == "item_uid_roar",
      str_extract(.data$task_id, "^[A-z]*"),
      .data$item_task
    )) |>
    mutate(group = replace_na(.data$group, ""),
           entry = replace_na(.data$entry, "")) |>
    rename(item_original = "item", item_group = "group", item = "entry")
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
    mutate(answer = .data$answer |> str_replace("^0", "0."),
           across(c(.data$answer, .data$max_value), as.numeric),
           correct = (abs(as.numeric(.data$response) - .data$answer) / .data$max_value < threshold)) |>
    select(-c("answer", "max_value"))
  trials |>
    filter(.data$item_group != "slider") |>
    bind_rows(slider_trials)
}
