# Get item parameters

`get_item_parameters()` returns the IRT item parameters used in LEVANTE
scoring as a data frame. See the [rlevante
documentation](https://levante-framework.github.io/rlevante/index.html)
for more information about how to access LEVANTE datasets and codebooks.

## Usage

``` r
get_item_parameters(version = "current")
```

## Arguments

- version:

  Version of the Redivis scoring metadata dataset.

## Value

A data frame where each row is an item parameter estimate. See our
[Scoring and Psychometrics
page](https://researcher.levante-network.org/measures/scoring-and-psychometrics)
to learn how to interpret these values.

## Examples

``` r
if (FALSE) { # \dontrun{
item_parameters <- get_item_parameters()
} # }
```
