# get the model record indexed in registry_dir for a given scoring specification

get the model record indexed in registry_dir for a given scoring
specification

## Usage

``` r
get_model_record(spec, registry_dir)
```

## Arguments

- spec:

  Model specification (list with names item_task, model_set, subset,
  itemtype, nfact, invariance).

- registry_dir:

  Model registry directory as returned by
  [`fetch_registry_dir()`](https://levante-framework.github.io/levantemodels/reference/fetch_registry_dir.md).

## Value

ModelRecord object
