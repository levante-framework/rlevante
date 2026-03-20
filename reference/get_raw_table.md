# Get raw table

`get_raw_table()` returns any table from Redivis as a data frame. See
the [rlevante
documentation](https://levante-framework.github.io/rlevante/index.html)
for more information about how to access LEVANTE datasets and codebooks.

## Usage

``` r
get_raw_table(table_name, data_source, data_source_version = "current")
```

## Arguments

- table_name:

  String indicating name of table to get

- data_source:

  String indicating which Redivis dataset to get

- data_source_version:

  String indicating which version of `data_source` to get

## Value

A data frame corresponding to the specified Redivis table.

## Examples

``` r
if (FALSE) { # \dontrun{
runs <- get_raw_table(table_name = "runs", data_source = "levante_data_example_raw:bm7r")
} # }
```
