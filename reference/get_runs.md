# Get run data

Get run data

## Usage

``` r
get_runs(
  dataset_spec,
  remove_incomplete_runs = TRUE,
  remove_invalid_runs = TRUE,
  max_results = NULL
)
```

## Arguments

- dataset_spec:

  List of dataset names and versions to retrieve.

- remove_incomplete_runs:

  Boolean indicating whether to drop runs that were marked as incomplete
  (defaults to TRUE).

- remove_invalid_runs:

  Boolean indicating whether to drop runs that were marked as invalid
  (defaults to TRUE).

- max_results:

  Max number of records to load for each table (defaults to entire
  table).

## Examples

``` r
if (FALSE) { # \dontrun{
dataset_spec <- list(list(name = "levante-example-dataset:bm7r", version = "current"))
runs <- get_runs(dataset_spec)
} # }
```
