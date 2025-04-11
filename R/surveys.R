# get survey item metadata and join with survey data
add_survey_items <- function(surveys) {

  survey_items <- get_survey_items()
  suppressWarnings(
    survey_items_coded <- survey_items |>
      mutate(values = coalesce(.data$values, "[]"),
             values = .data$values |> map(jsonlite::fromJSON) |> map(as.numeric))
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
                           map2_dbl(.data$value, .data$values, reverse_value),
                           .data$value)) |>
    select("dataset", "survey_type", "user_id", "child_id",
           contains("construct"), "question_type", "variable", "variable_order",
           "value", timestamp = "created_at")
}


#' Link survey data to participant data
#'
#' @param surveys Survey data as returned by `get_surveys()`.
#' @param participants Participant data as returned by `get_participants()`.
#'
#' @export
link_surveys <- function(surveys, participants) {

  user_survey_data <- surveys |>
    nest(survey_data = -c(.data$dataset, .data$survey_type, .data$timestamp,
                          .data$user_id, .data$child_id))

  survey_linked <- user_survey_data |> filter(!is.na(.data$child_id))
  survey_unlinked <- user_survey_data |> filter(is.na(.data$child_id)) |> select(-"child_id")

  children <- participants |> rename(child_id = "user_id")

  # student survey -- user_id is child
  survey_student <- survey_unlinked |>
    filter(.data$survey_type == "student") |>
    mutate(child_id = .data$user_id)

  # teacher survey -- user_id is teacher
  teachers <- children |>
    filter(.data$teacher_id != "") |>
    select("dataset", "child_id", "teacher_id") |>
    nest(children = -c(.data$dataset, .data$teacher_id))

  survey_teacher <- survey_unlinked |>
    filter(.data$survey_type == "teacher") |>
    left_join(teachers, by = c("dataset", "user_id" = "teacher_id")) |>
    unnest(children)

  # caregiver survey -- user_id is caregiver
  caregivers <- children |>
    filter(!is.na(.data$parent1_id)) |>
    select("dataset", "child_id", "parent1_id") |>
    nest(children = -c(.data$dataset, .data$parent1_id))

  survey_caregiver <- survey_unlinked |>
    filter(.data$survey_type == "caregiver") |>
    left_join(caregivers,
              by = c("dataset", "user_id" = "parent1_id")) |>
    unnest(children)

  # recombine separated out survey types
  survey_combined <- bind_rows(survey_student, survey_teacher, survey_caregiver, survey_linked) |>
    select(-"children") |>
    left_join(children |> select("dataset", "child_id", "sex", "birth_month", "birth_year"),
              by = c("dataset", "child_id")) |>
    mutate(age = compute_age(.data$birth_month, .data$birth_year, .data$timestamp)) |>
    select(-contains("birth_"), -"timestamp") |>
    relocate(.data$survey_data, .after = everything())

  survey_combined |> unnest(.data$survey_data)
}
