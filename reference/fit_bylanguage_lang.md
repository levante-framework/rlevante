# wrapper: given language, fit by_language model for each task

wrapper: given language, fit by_language model for each task

## Usage

``` r
fit_bylanguage_lang(task_data, models, priors, lang, registry_dir)
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

- lang:

  string indicating language

- registry_dir:

  string indicating directory in which to save models
