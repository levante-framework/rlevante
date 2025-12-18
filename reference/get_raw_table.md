# Get raw table

Get raw table

## Usage

``` r
get_raw_table(table_name, data_source, version = "current")
```

## Arguments

- table_name:

  String indicating name of table to get

- data_source:

  Name of Redivis dataset

- version:

  Version of Redivis dataset

## Examples

``` r
if (FALSE) { # \dontrun{
runs <- get_raw_table(table_name = "runs", dataset = "levante-data-example:bm7r")
} # }
```
