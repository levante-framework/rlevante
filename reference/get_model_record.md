# get the model record indexed in registry_dir for a given scoring specification

get the model record indexed in registry_dir for a given scoring
specification

## Usage

``` r
get_model_record(spec, registry_dir)
```

## Arguments

- spec:

  list with entries item_task, model_set, subset, itemtype, nfact,
  invariance

- registry_dir:

  tibble returned by registry_dir()

## Value

ModelRecord object
