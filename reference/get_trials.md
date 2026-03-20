# Get trials

`get_trials()` returns information about each trial as a data frame. See
the [rlevante
documentation](https://levante-framework.github.io/rlevante/index.html)
for more information about how to access LEVANTE datasets and codebooks.

## Usage

``` r
get_trials(data_source, version = "current")
```

## Arguments

- data_source:

  Name of Redivis dataset

- version:

  Version of Redivis dataset

## Value

A data frame where each row is a trial.

## Examples

``` r
if (FALSE) { # \dontrun{
trials <- get_trials(dataset = "levante_data_example:d0rt", version = "current")
} # }
```
