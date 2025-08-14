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
    select("dataset", "survey_response_id", "survey_type", "survey_part",
           "user_id", "child_id", contains("construct"), "question_type",
           "variable", "variable_order", "value", timestamp = "created_at")
}


#' Link survey data to participant data
#'
#' @param surveys Survey data as returned by `get_surveys()`.
#' @param participants Participant data as returned by `get_participants()`.
#'
#' @export
link_surveys <- function(surveys, participants) {

  user_survey_data <- surveys |>
    mutate(survey_group = survey_part) |>
    nest(survey_data = -c("dataset", "survey_type", "survey_response_id", "timestamp",
                          "survey_group", "user_id", "child_id"))

  children <- participants |> rename(child_id = "user_id")

  # student survey -- user_id is child
  survey_student <- user_survey_data |>
    filter(.data$survey_type == "student") |>
    mutate(child_id = user_id)

  # teacher survey -- user_id is teacher
  teachers <- children |>
    filter(.data$teacher_id != "") |>
    select("dataset", "child_id", "teacher_id") |>
    nest(children = -c("dataset", "teacher_id"))

  survey_teacher <- user_survey_data |>
    filter(.data$survey_type == "teacher") |>
    select(-child_id) |>
    left_join(teachers, by = c("dataset", "user_id" = "teacher_id")) |>
    unnest(children) |>
    select(-children)

  survey_household <- user_survey_data |>
    filter(.data$survey_type == "caregiver", str_detect(survey_group, "caregiver")) |>
    rename(survey_household = survey_data) |>
    select(-survey_group)

  survey_child <- user_survey_data |>
    filter(.data$survey_type == "caregiver", survey_group == "child_specific") |>
    rename(survey_child = survey_data) |>
    select(-survey_group)

  survey_caregiver <- survey_child |>
    left_join(survey_household) |>
    mutate(survey_data = map2(survey_child, survey_household, bind_rows)) |>
    select(-survey_child, -survey_household)

  # recombine separated out survey types
  survey_combined <- bind_rows(survey_student, survey_teacher, survey_caregiver) |> #, survey_linked) |>
    left_join(children |> select("dataset", "child_id", "sex", "birth_month", "birth_year"),
              by = c("dataset", "child_id")) |>
    mutate(age = compute_age(.data$birth_month, .data$birth_year, .data$timestamp)) |>
    select(-contains("birth_"), -"survey_group") |>
    rename(survey_timestamp = timestamp) |>
    relocate(.data$survey_data, .after = everything())

  survey_combined |> unnest(.data$survey_data)
}
