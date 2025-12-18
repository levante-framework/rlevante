# Process participants

Process participants

## Usage

``` r
process_participants(dataset_spec, max_results = NULL)
```

## Arguments

- dataset_spec:

  List of dataset names and versions to retrieve.

- max_results:

  Max number of records to load for each table (defaults to entire
  table).

## Examples

``` r
if (FALSE) { # \dontrun{
dataset_spec <- list(list(name = "levante-example-dataset:bm7r", version = "current"))
participants <- process_participants(dataset_spec)
} # }
```
