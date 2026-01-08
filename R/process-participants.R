#' Process participants
#' @keywords internal
#'
#' @inheritParams process_runs
#'
#' @export
#' @examples
#' \dontrun{
#' dataset_spec <- list(list(name = "levante-example-dataset:bm7r", version = "current"))
#' participants <- process_participants(dataset_spec)
#' }
process_participants <- function(dataset_spec, max_results = NULL) {

  user_vars <- c(
    "sites.site_name AS dataset",
    "users.user_id",
    "users.birth_month",
    "users.birth_year",
    "users.parent1_id",
    "users.parent2_id",
    "users.teacher_id",
    "user_schools.school_id",
    "user_classes.class_id"
  )
  query_str <- glue::glue(
    "SELECT {paste(user_vars, collapse = ', ')} FROM users
     LEFT JOIN user_sites ON users.user_id = user_sites.user_id
     LEFT JOIN sites ON user_sites.site_id = sites.site_id
     LEFT JOIN user_schools ON users.user_id = user_schools.user_id
     LEFT JOIN user_classes ON users.user_id = user_classes.user_id
     WHERE users.user_type IN ('student', 'guest')"
  )

  participants <- get_datasets_data(dataset_spec,
                                    query_getter("users", query_str, max_results))

  dataset_names <- list(
    "pilot_uniandes_co_bogota"     = "CO-bogota-pilot",
    "pilot_uniandes_co_rural"      = "CO-rural-pilot",
    "pilot_western_ca_main"        = "CA-western-pilot",
    "pilot_mpieva_de_main"         = "DE-mpieva-pilot",
    "pilot_langcog_us_downex"      = "US-downward_extension-pilot",
    "partner_mpib_de_main"         = "partner-mpib-de",
    "partner_childexplore_intl_ef" = "partner-childexplore-intl",
    "partner_sparklab_us_downex"   = "partner-sparklab-us"
  )

  suppressWarnings(
    participants |>
      mutate(dataset = .data$dataset |> forcats::fct_recode(!!!dataset_names),
             site = .data$dataset |> stringr::str_extract("^[A-z]+_[A-z]+_[A-z]+(?=_)"),
             .before = "dataset") |>
      relocate("redivis_source", .after = "dataset") |>
      arrange(.data$dataset, .data$user_id)
  )
}
