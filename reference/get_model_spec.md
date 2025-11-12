# get the scoring specification in scoring_table for a given task + dataset

get the scoring specification in scoring_table for a given task +
dataset

## Usage

``` r
get_model_spec(scoring_table, score_task, score_dataset)
```

## Arguments

- scoring_table:

  tibble returned by get_scoring_table()

- score_task:

  string indicating task

- score_dataset:

  string indicating dataset

## Value

list with entries item_task, dataset, model_set, subset, itemtype,
nfact, invariance
