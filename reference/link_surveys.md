# Link survey data to participant data

Link survey data to participant data

## Usage

``` r
link_surveys(surveys, participants)
```

## Arguments

- surveys:

  Survey data as returned by \`process_surveys()\`.

- participants:

  Participant data as returned by \`process_participants()\`.

## Examples

``` r
if (FALSE) { # \dontrun{
dataset_spec <- list(list(name = "levante_data_example:d0rt", version = "current"))
surveys <- process_surveys(dataset_spec)
participants <- process_participants(dataset_spec)
survey_data <- surveys |> link_surveys(participants)
} # }
```
