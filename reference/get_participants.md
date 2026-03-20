# Get participants

`get_participants()` returns information about participants as a data
frame. See the [rlevante
documentation](https://levante-framework.github.io/rlevante/index.html)
for more information about how to access LEVANTE datasets and codebooks.

## Usage

``` r
get_participants(data_source, version = "current")
```

## Arguments

- data_source:

  Name of Redivis dataset

- version:

  Version of Redivis dataset

## Value

A data frame where each row contains information about a child
participant.

## Examples

``` r
if (FALSE) { # \dontrun{
participants <- get_participants(dataset = "levante_data_example:d0rt", version = "current")
} # }
```
