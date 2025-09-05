# tests/testthat/test-recipe.R
test_that("target censoring is approximately achieved", {
  rec <- recipe_quick_aft(
    n = 2000, tau = 24, model = "aft_lognormal",
    baseline = list(mu = 3, sigma = 0.7),
    treat_effect = -0.2,
    covariates = list(list(name="x", type="continuous", dist="normal", params=list(mean=0, sd=1))),
    target_censoring = 0.30, seed = 1
  )
  dat <- simulate_from_recipe(rec)
  expect_true(abs(attr(dat, "achieved_censoring") - 0.30) < 0.03)
})

test_that("cox piecewise exponential runs", {
  rec <- list(
    n = 500,
    covariates = list(defs = list(list(name="age", type="continuous", dist="normal", params=list(mean=60, sd=8)))),
    event_time = list(
      model = "cox_pwexp",
      baseline = list(rates = c(0.1, 0.06, 0.03), cuts = c(6, 18)),
      effects = list(treatment = -0.4, covariates = list(age = 0.01)),
      tau = 24
    ),
    censoring = list(mode = "target_overall", target = 0.25, admin_time = 30),
    treatment = list(assignment = "randomization", allocation = "1:1")
  )
  rec <- validate_recipe(rec)
  dat <- simulate_from_recipe(rec, seed = 123)
  expect_true(nrow(dat) == 500)
  expect_true(all(dat$time > 0))
})
