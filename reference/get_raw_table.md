# Get raw table

Get raw table

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

  String indicating which version of \`data_source\` to get

## Examples

``` r
if (FALSE) { # \dontrun{
runs <- get_raw_table(table_name = "runs", data_source = "levante_data_example_raw:bm7r")
} # }
```
