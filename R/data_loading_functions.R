#' Get redivis datasets
#'
#' Retrieve LEVANTE project datasets from Redivis and prepare them for further analysis.
#'
#' @return A list of one or more datasets from a specific organization in the Redivis repository. In our case that is normally "levante"
#' @export
#' @examples

# remember to specify at least one table in addition to the dataset name(s)
get_datasets <- function(dataset_names, org_name = "levante", tables = NULL) {
  org <- redivis::organization(org_name)

  datasets <- dataset_names |> set_names() |> map(\(dn) org$dataset(dn))

  get_table_names <- \(ds) {
    if (!is.null(tables)) tables else ds$list_tables() |> map(\(t) t$name)
  }

  get_dataset_tables <- \(ds, dn) {
    ds |> get_table_names() |> set_names() |> map(\(tn) {
      message(glue::glue("Fetching data for dataset {dn} table {tn}"))
      ds$table(tn)$to_tibble()
    })
  }

  imap(datasets, get_dataset_tables)
}

#' Fix some stuff in tables
#'
#' There can be some anomalies in LEVANTE data stored in Redivis. This function
#' will clean up some of the most common
#'
#' @return A Levante specific function to clean up some known potential data issues.
#' @export
#' @examples

fix_table_types <- function(table_data) {
  table_data |>
    mutate(across(where(is_character),
                  \(x) x |> na_if("null") |> na_if("None"))) #|>
    # mutate(across(matches("birth_"), as.integer),
    #        across(matches("difficulty"), as.double),
    #        across(matches("rt"), as.character),
    #        across(matches("grade"), as.character),
    #        across(matches("example"), as.character),
    #        across(matches("growth_mind"), as.character),
    #        across(matches("lonely_"), as.character),
    #        across(matches("math_"), as.character),
    #        across(matches("learning_"), as.character),
    #        across(matches("reading_"), as.character),
    #        across(matches("teacher_"), as.character),
    #        across(matches("school_"), as.character),
    #        across(matches("class_"), as.character), # class_friends, class_help ..
    #        across(matches("time_finished|last_updated|created_at|date_created|date_closed|date_opened"), lubridate::as_datetime),
    #        across(matches("email_verified|is_reliable|is_bestrun|is_practice_trial"), as.logical))
}

#' Combine tables into a single megatable
#'
#' longer description
#'
#' @return For the case where the data you're using is in multiple tables, this
#'  function will combine them into one single large table if needed.
#'
#' @export
#' @examples

combine_datasets <- function(dataset_tables) {
  all_table_names <- map(dataset_tables, names) |> unlist() |> unique()
  dataset_tables |>
    # map(\(ds) ds |> map(\(t) fix_table_types(t))) |>
    list_transpose(template = all_table_names, simplify = FALSE) |>
    # map(list_rbind) |>
    map(\(dt) list_rbind(dt, names_to = "dataset")) |>
    map(\(ds) ds |> select(-matches("validation_err_msg")))
}

#' Collect user data
#'
#' longer description
#'
#' @return Assemble & join user data with groups
#'
#' @export
#' @examples

collect_users <- function(dataset_data) {

  births <- dataset_data$users |>
    select(user_id, birth_month, birth_year) |>
    mutate(across(contains("birth"), \(v) na_if(v, 0))) |>
    filter(!is.na(birth_month), !is.na(birth_year)) |>
    mutate(dob = ym(paste(birth_year, birth_month, sep = "-")))

  ages <- dataset_data$runs |>
    select(user_id, run_id, time_started) |>
    inner_join(births) |>
    mutate(age = as.numeric(difftime(time_started, dob, units = "days")) / 365.25)

  user_ages <- ages |>
    select(user_id, run_id, age) |>
    nest(ages = -user_id)

  user_groups <- distinct(dataset_data$user_groups) |>
    left_join(distinct(dataset_data$groups), by = "group_id") |>
    tidyr::nest(groups = -user_id)

  dataset_data$users |>
    left_join(user_groups, by = "user_id") |>
    left_join(user_ages, by = "user_id")
}

#' @export
remove_practice_trials <- function(trials) {
  trials |>
    mutate(practice = is_practice_trial |
             str_detect(assessment_stage, "practice") |
             str_detect(assessment_stage, "instructions") |
             str_detect(corpus_trial_type, "training") |
             str_detect(corpus_trial_type, "practice")) |>
    filter(is.na(practice) | !practice) |>
    select(-practice, -is_practice_trial)
}

