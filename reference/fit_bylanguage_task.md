# wrapper: given task, fit by_language model for each language

wrapper: given task, fit by_language model for each language

## Usage

``` r
fit_bylanguage_task(task_data, models, priors, task, registry_dir)
```

## Arguments

- task_data:

  dataframe with columns item_task, the value of subset_var, and data,
  where data is trial-level data

- models:

  dataframe of models to fit, specified with columns nfact (number of
  factors), itemtype (Rasch/2PL), invariance (configural/metric/scalar)

- priors:

  list of priors, where names are parameter names (e.g. d, a1) and
  values are vectors of length 3 (priorType, val1, val2)

- task:

  one value in item_task

- registry_dir:

  string indicating directory in which to save models
