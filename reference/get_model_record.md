# get the model record indexed in registry_table for a given scoring specification

get the model record indexed in registry_table for a given scoring
specification

## Usage

``` r
get_model_record(spec, registry_table)
```

## Arguments

- spec:

  list with entries item_task, dataset, model_set, subset, itemtype,
  nfact, invariance

- registry_table:

  tibble returned by get_registry_table()

## Value

ModelRecord object
