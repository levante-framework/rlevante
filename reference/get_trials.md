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

## Details

The returned `correct` column is **not yet scoring-ready**: LEVANTE's
IRT models are calibrated on trials passed through
[`recode_trials()`](https://levante-framework.github.io/rlevante/reference/recode_trials.md)
(slider thresholding, Hearts & Flowers / SDS / ToM recodes, item-key
fixes, and chance backfill). Apply
[`recode_trials()`](https://levante-framework.github.io/rlevante/reference/recode_trials.md)
before scoring. See
[`vignette("scoring-and-model-registry")`](https://levante-framework.github.io/rlevante/articles/scoring-and-model-registry.md)
for the full pipeline.

## Examples

``` r
if (FALSE) { # \dontrun{
trials <- get_trials(data_source = "levante_data_example:d0rt", version = "current")
} # }
```
