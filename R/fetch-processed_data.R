#' Get participants
#'
#' `get_participants()` returns information about participants as a data frame. See the [rlevante documentation](https://levante-framework.github.io/rlevante/index.html) for more information about how to access LEVANTE datasets and codebooks.
#' @param data_source Name of Redivis dataset
#' @param version Version of Redivis dataset
#' @returns A data frame where each row contains information about a child participant.
#' @export
#' @examples
#' \dontrun{
#' participants <- get_participants(data_source = "levante_data_example:d0rt", version = "current")
#' }
get_participants <- function(data_source, version = "current") {
  dataset_spec <- list(list(name = data_source, version = version))
  get_datasets_data(dataset_spec, table_getter("participants"))
}

#' Get scores
#'
#' `get_scores()` returns information about scores as a data frame. See the [rlevante documentation](https://levante-framework.github.io/rlevante/index.html) for more information about how to access LEVANTE datasets and codebooks.
#' @inheritParams get_participants
#' @returns A data frame where each row is a task ability score. See our [Scoring and Psychometrics page](https://researcher.levante-network.org/measures/scoring-and-psychometrics) to learn how to interpret scores.
#' @export
#' @examples
#' \dontrun{
#' scores <- get_scores(data_source = "levante_data_example:d0rt", version = "current")
#' }
get_scores <- function(data_source, version = "current") {
  dataset_spec <- list(list(name = data_source, version = version))
  get_datasets_data(dataset_spec, table_getter("scores"))
}

#' Get surveys
#'
#' `get_surveys()` returns information about survey responses as a data frame. See the [rlevante documentation](https://levante-framework.github.io/rlevante/index.html) for more information about how to access LEVANTE datasets and codebooks.
#' @inheritParams get_participants
#' @returns A data frame where each row is a survey item response.
#' @export
#' @examples
#' \dontrun{
#' surveys <- get_surveys(data_source = "levante_data_example:d0rt", version = "current")
#' }
get_surveys <- function(data_source, version = "current") {
  dataset_spec <- list(list(name = data_source, version = version))
  get_datasets_data(dataset_spec, table_getter("surveys"))
}

#' Get trials
#'
#' `get_trials()` returns information about each trial as a data frame. See the [rlevante documentation](https://levante-framework.github.io/rlevante/index.html) for more information about how to access LEVANTE datasets and codebooks.
#' @inheritParams get_participants
#' @returns A data frame where each row is a trial.
#' @export
#' @examples
#' \dontrun{
#' trials <- get_trials(data_source = "levante_data_example:d0rt", version = "current")
#' }
get_trials <- function(data_source, version = "current") {
  dataset_spec <- list(list(name = data_source, version = version))
  get_datasets_data(dataset_spec, table_getter("trials"))
}

