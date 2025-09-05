# RMSTSS data-recipes bundle

This bundle provides R scripts to add declarative data generation (“recipes”) to your package and app.

## Files

- `R/recipe_sim.R`: Core engines, validation, IO.
- `R/recipe_convenience.R`: Quick builders (`recipe_quick_aft`), grids, and `rmst_from_recipe`.
- `tests/testthat/test-recipe.R`: Minimal tests.
- `inst/recipes/stratified_pwexp_dep.yml`: Example YAML recipe.

## Install into your package

1. Copy `R/*.R` into your package `R/` directory.
2. Optionally copy `inst/recipes/*.yml` into your package `inst/recipes/`.
3. Ensure `DESCRIPTION` has:
   - `Imports: stats`
   - `Suggests: yaml, jsonlite, testthat (>= 3.0.0)`
4. Run:
   ```r
   devtools::document()
   devtools::test()
   ```

## Quick start in R

```r
source("R/recipe_sim.R")
source("R/recipe_convenience.R")

covs <- list(
  list(name="age", type="continuous", dist="normal", params=list(mean=62, sd=10),
       transform=c("center(60)","scale(10)")),
  list(name="gender", type="categorical", dist="bernoulli", params=list(p=0.45))
)

rec <- recipe_quick_aft(300, 24, "aft_weibull",
                        baseline=list(shape=1.3, scale=12),
                        treat_effect=-0.25, covariates=covs,
                        target_censoring=0.25, allocation="1:1", seed=2025)

dat <- simulate_from_recipe(rec)
str(dat)
attributes(dat)[c("tau","achieved_censoring")]
```
