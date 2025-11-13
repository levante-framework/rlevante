# given trial data and model record, score data from corresponding model

given trial data and model record, score data from corresponding model

## Usage

``` r
score_irt(trial_data_task, mod_spec, mod_rec)
```

## Arguments

- trial_data_task:

  trial data from one task and one dataset

- mod_spec:

  list with entries item_task, dataset, model_set, subset, itemtype,
  nfact, invariance

- mod_rec:

  ModelRecord object

## Value

tibble with scores
