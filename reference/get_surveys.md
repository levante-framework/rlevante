# Get surveys

`get_surveys()` returns information about survey responses as a data
frame. See the [rlevante
documentation](https://levante-framework.github.io/rlevante/index.html)
for more information about how to access LEVANTE datasets and codebooks.

## Usage

``` r
get_surveys(data_source, version = "current")
```

## Arguments

- data_source:

  Name of Redivis dataset

- version:

  Version of Redivis dataset

## Value

A data frame where each row is a survey item response.

## Examples

``` r
if (FALSE) { # \dontrun{
surveys <- get_surveys(dataset = "levante_data_example:d0rt", version = "current")
} # }
```
