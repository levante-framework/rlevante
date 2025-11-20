# Link survey data to participant data

Link survey data to participant data

## Usage

``` r
link_surveys(surveys, participants)
```

## Arguments

- surveys:

  Survey data as returned by \`get_surveys()\`.

- participants:

  Participant data as returned by \`get_participants()\`.

## Examples

``` r
if (FALSE) { # \dontrun{
dataset_spec <- list(list(name = "levante-example-dataset:bm7r", version = "current"))
surveys <- get_surveys(dataset_spec)
participants <- get_participants(dataset_spec)
survey_data <- surveys |> link_surveys(participants)
} # }
```
