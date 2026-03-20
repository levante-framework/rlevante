# Get scores

`get_scores()` returns information about scores as a data frame. See the
[rlevante
documentation](https://levante-framework.github.io/rlevante/index.html)
for more information about how to access LEVANTE datasets and codebooks.

## Usage

``` r
get_scores(data_source, version = "current")
```

## Arguments

- data_source:

  Name of Redivis dataset

- version:

  Version of Redivis dataset

## Value

A data frame where each row is a task ability score. See our [Scoring
and Psychometrics
page](https://researcher.levante-network.org/measures/scoring-and-psychometrics)
to learn how to interpret scores.

## Examples

``` r
if (FALSE) { # \dontrun{
scores <- get_scores(dataset = "levante_data_example:d0rt", version = "current")
} # }
```
