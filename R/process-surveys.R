#' Process survey data
#' @keywords internal
#' @export
#'
#' @inheritParams process_runs
#' @param survey_types Character vector of survey types to include (caregiver,
#'   student, teacher).
#' @param remove_incomplete_surveys Boolean indicating whether to drop surveys
#'   that were marked as incomplete (defaults to FALSE).
#'
#' @examples
#' \dontrun{
#' dataset_spec <- list(list(name = "levante-example-dataset:bm7r", version = "current"))
#' surveys <- process_surveys(dataset_spec)
#' }
process_surveys <- function(dataset_spec,
                            survey_types = c("caregiver", "student", "teacher"),
                            remove_incomplete_surveys = FALSE,
                            max_results = NULL) {

  where_str <- build_filter("survey_id", survey_types)
  query_str <- glue::glue("SELECT * FROM survey_responses {where_str}") |> stringr::str_trim()

  surveys <- get_datasets_data(dataset_spec,
                               query_getter("survey_responses", query_str, max_results))
  if (nrow(surveys) == 0) return()

  if (remove_incomplete_surveys) surveys <- surveys |> filter(.data$is_complete)

  surveys |>
    add_survey_items() |>
    code_survey_data()
}


# get survey item metadata and join with survey data
add_survey_items <- function(surveys) {

  survey_items <- fetch_survey_items()
  suppressWarnings(
    survey_items_coded <- survey_items |>
      mutate(values = coalesce(.data$values, "[]"),
             values = .data$values |> purrr::map(jsonlite::fromJSON) |> purrr::map(as.numeric))
  )

  surveys |>
    rename(variable = "question_id", survey_type = "survey_id") |>
    inner_join(survey_items_coded, by = c("survey_type", "variable"))

}

reverse_value <- \(v, vals) if (any(is.na(vals)) || !(v %in% vals)) NA else rev(vals)[which(vals == v)]

code_survey_data <- function(surveys) {
  surveys |>
    # code 0/1 for true/false
    mutate(value = if_else(is.na(.data$numeric_response) & !is.na(.data$boolean_response),
                           as.numeric(.data$boolean_response), .data$numeric_response),
           .after = .data$variable) |>
    arrange(.data$variable_order) |>
    mutate(variable = forcats::fct_inorder(.data$variable)) |>
    # reverse code values if needed
    mutate(value = if_else(.data$reverse_coded,
                           purrr::map2_dbl(.data$value, .data$values, reverse_value),
                           .data$value)) |>
    select("redivis_source", "survey_response_id", "survey_type", "survey_part",
           "user_id", "child_id", contains("construct"), "question_type",
           "variable", "variable_order", "value", "boolean_response",
           "string_response", "numeric_response", "is_complete",
           timestamp = "created_at")
}


#' Link survey data to participant data
#' @keywords internal
#' @export
#'
#' @param surveys Survey data as returned by `process_surveys()`.
#' @param participants Participant data as returned by `process_participants()`.
#'
#' @examples
#' \dontrun{
#' dataset_spec <- list(list(name = "levante-example-dataset:bm7r", version = "current"))
#' surveys <- process_surveys(dataset_spec)
#' participants <- process_participants(dataset_spec)
#' survey_data <- surveys |> link_surveys(participants)
#' }
link_surveys <- function(surveys, participants) {

  user_survey_data <- surveys |>
    mutate(survey_group = .data$survey_part) |>
    tidyr::nest(survey_data = -c("survey_type", "survey_response_id",
                                 "timestamp", "survey_group", "user_id", "child_id")) |>
    mutate(n_responses = purrr::map_int(.data$survey_data, nrow)) |>
    filter(.data$n_responses > 1)

  children <- participants |> rename(child_id = "user_id")

  # student survey -- user_id is child
  survey_student <- user_survey_data |>
    filter(.data$survey_type == "student") |>
    mutate(child_id = .data$user_id) |>
    select(-"survey_group")

  # teacher survey -- user_id is teacher
  teachers <- children |>
    filter(.data$teacher_id != "") |>
    select("child_id", "teacher_id") |>
    tidyr::nest(children = -c("teacher_id"))

  survey_teacher <- user_survey_data |>
    filter(.data$survey_type == "teacher") |>
    select(-"child_id") |>
    left_join(teachers, by = c("user_id" = "teacher_id")) |>
    tidyr::unnest("children") |>
    select(-"survey_group")

  parents <- children |>
    select("child_id", "parent1_id", "parent2_id") |>
    tidyr::pivot_longer(cols = c("parent1_id", "parent2_id"), names_to = NULL, values_to = "parent_id") |>
    filter(!is.na(.data$parent_id))

  # caregiver survey, household (across children) section
  survey_household <- user_survey_data |>
    filter(.data$survey_type == "caregiver", stringr::str_detect(.data$survey_group, "caregiver")) |>
    # rename(survey_household = "survey_data") |>
    select(-"survey_group", -"child_id") |>
    inner_join(parents, by = c("user_id" = "parent_id"), relationship = "many-to-many") |>
    relocate("child_id", .after = "user_id")

  # caregiver survey, child-specific section
  survey_child <- user_survey_data |>
    filter(.data$survey_type == "caregiver", .data$survey_group == "child_specific") |>
    # rename(survey_child = "survey_data") |>
    select(-"survey_group")

  # caregiver survey combined
  survey_caregiver <- bind_rows(survey_household, survey_child) |>
    group_by(.data$survey_response_id, .data$survey_type, .data$user_id,
             .data$child_id, .data$timestamp) |>
    summarise(survey_data = list(purrr::list_rbind(.data$survey_data)),
              n_responses = sum(.data$n_responses))

  # recombine separated out survey types
  survey_combined <- bind_rows(survey_student, survey_teacher, survey_caregiver) |> #, survey_linked) |>
    left_join(children |> select("child_id", "birth_month", "birth_year", "site", "dataset"),
              by = c("child_id")) |>
    mutate(age = compute_age(.data$birth_month, .data$birth_year, .data$timestamp)) |>
    select(-contains("birth_")) |>
    rename(respondent_id = "user_id", survey_timestamp = "timestamp") |>
    mutate(survey_type = .data$survey_type |> stringr::str_replace("student", "child")) |>
    relocate("survey_data", .after = everything())

  survey_combined |> tidyr::unnest("survey_data") |>
    mutate(survey_part = .data$survey_part |> stringr::str_replace("student", "child")) |>
    select(-"n_responses") |>
    relocate("site", "dataset", "redivis_source", .before = everything())
}
