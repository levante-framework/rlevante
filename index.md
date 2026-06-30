# levantemodels

Internal R tooling for LEVANTE data **processing and psychometric
modeling**. This package holds the model intermediates — trial recoding,
the IRT scoring pipeline, the model registry, and the `ModelRecord`
class.

For **loading** LEVANTE data into R (the user-facing `get_*` functions),
see the companion
[`levante`](https://github.com/levante-framework/levante-r) package.
`levantemodels` is internally-facing tooling and depends on `levante`
for data access.

Some useful links: - [An overview of the LEVANTE
project](https://researcher.levante-network.org/overview). - [Details
about LEVANTE’s scoring and
psychometrics](https://researcher.levante-network.org/measures/scoring-and-psychometrics). -
[Details about the LEVANTE child
tasks](https://researcher.levante-network.org/measures/direct-child-measures).

## Installation

``` r

# install.packages("devtools") # if you need the devtools package
devtools::install_github("levante-framework/levantemodels")
```

## What’s here

- **Trial recoding** —
  [`recode_trials()`](https://levante-framework.github.io/levantemodels/reference/recode_trials.md)
  makes loaded trial data scoring-ready (slider thresholding, Hearts &
  Flowers / SDS / ToM recodes, item-key fixes).
- **The IRT scoring pipeline** —
  [`score()`](https://levante-framework.github.io/levantemodels/reference/score.md)
  /
  [`score_irt()`](https://levante-framework.github.io/levantemodels/reference/score_irt.md),
  and the model registry accessors
  [`fetch_scoring_table()`](https://levante-framework.github.io/levantemodels/reference/fetch_scoring_table.md),
  `fetch_registry_table()`,
  [`fetch_registry_dir()`](https://levante-framework.github.io/levantemodels/reference/fetch_registry_dir.md).
- **The `ModelRecord` class** and its accessors
  ([`items()`](https://levante-framework.github.io/levantemodels/reference/ModelRecord.md),
  [`model_vals()`](https://levante-framework.github.io/levantemodels/reference/ModelRecord.md),
  [`model_class()`](https://levante-framework.github.io/levantemodels/reference/ModelRecord.md),
  [`scores()`](https://levante-framework.github.io/levantemodels/reference/ModelRecord.md)).
- **Processing** —
  [`process_trials()`](https://levante-framework.github.io/levantemodels/reference/process_trials.md),
  [`process_runs()`](https://levante-framework.github.io/levantemodels/reference/process_runs.md),
  [`process_surveys()`](https://levante-framework.github.io/levantemodels/reference/process_surveys.md),
  [`process_participants()`](https://levante-framework.github.io/levantemodels/reference/process_participants.md).

See the [Scoring and the model
registry](https://levante-framework.github.io/levantemodels/articles/scoring-and-model-registry.html)
vignette for the full trials → score pipeline.
