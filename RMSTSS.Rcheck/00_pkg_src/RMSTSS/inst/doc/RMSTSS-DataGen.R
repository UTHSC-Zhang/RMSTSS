## ----setup, include=FALSE-----------------------------------------------------
is_pkgdown <- identical(Sys.getenv("IN_PKGDOWN"), "true")
knitr::opts_chunk$set(
  collapse = TRUE,
   cache = TRUE,
  cache.lazy = FALSE,   # materialize objects now (safer)
  autodep = TRUE,  
  comment = NA,
  fig.width = 6, fig.height = 4,
  warning = FALSE, message = FALSE,
  eval = is_pkgdown || identical(Sys.getenv("NOT_CRAN"), "true")
)
library(RMSTSS)
set.seed(1)

## ----ex1, eval=TRUE-----------------------------------------------------------
covs1 <- list(
  list(name="age",   type="continuous",  dist="normal",     params=list(mean=62, sd=10),
       transform=c("center(60)","scale(10)")),
  list(name="sex",   type="categorical", dist="bernoulli",  params=list(p=0.45)),
  list(name="stage", type="categorical", dist="ordinal",
       params=list(prob=c(0.3,0.5,0.2), labels=c("I","II","III"))),
  list(name="x",     type="continuous",  dist="lognormal",  params=list(meanlog=0, sdlog=0.6))
)

rec1 <- list(
  n = 300,
  covariates = list(defs = covs1),
  treatment  = list(assignment="randomization", allocation="1:1"),
  event_time = list(model="aft_lognormal",
                    baseline=list(mu=3.0, sigma=0.6),
                    effects=list(intercept=0, treatment=-0.25,
                                 covariates=list(age=0.01, sex=-0.2, x=0.05))),
  censoring  = list(mode="target_overall", target=0.25, admin_time=36),
  seed = 11
)

dat1 <- simulate_from_recipe(validate_recipe(rec1))
head(dat1)
attr(dat1, "achieved_censoring")

## ----ex2, eval=TRUE-----------------------------------------------------------
rec2 <- rec1
rec2$event_time <- list(model="aft_weibull",
                        baseline=list(shape=1.3, scale=12),
                        effects=list(intercept=0, treatment=-0.20,
                                     covariates=list(age=0.008, x=0.04)))
dat2 <- simulate_from_recipe(validate_recipe(rec2), seed=12)
summary(dat2$time)
attr(dat2, "achieved_censoring")

## ----ex3, eval=TRUE-----------------------------------------------------------
rec3 <- list(
  n = 400,
  covariates = list(defs = covs1),
  treatment  = list(assignment="randomization", allocation="1:1"),
  event_time = list(model="ph_pwexp",
                    baseline=list(rates=c(0.05), cuts=numeric(0)),
                    effects=list(intercept=0, treatment=-0.3,
                                 covariates=list(age=0.01, x=0.03))),
  censoring  = list(mode="target_overall", target=0.20, admin_time=30),
  seed = 13
)
dat3 <- simulate_from_recipe(validate_recipe(rec3))

## ----summ3, eval=TRUE, echo=FALSE---------------------------------------------
summ3 <- data.frame(
  Metric = c("n", "Events", "Censoring rate", "Mean time", "Median time"),
  Value  = c(nrow(dat3),
             sum(dat3$status == 1, na.rm = TRUE),
             round(mean(dat3$status == 0, na.rm = TRUE), 3),
             round(mean(dat3$time,   na.rm = TRUE), 2),
             round(stats::median(dat3$time, na.rm = TRUE), 2))
)
knitr::kable(summ3, caption = "Example 3 — Summary")


## ----ex4, eval=TRUE-----------------------------------------------------------
rec4 <- list(
  n = 500,
  covariates = list(defs = list(
    list(name="age", type="continuous",  dist="normal",    params=list(mean=60, sd=8)),
    list(name="sex", type="categorical", dist="bernoulli", params=list(p=0.5)),
    list(name="x",   type="continuous",  dist="lognormal", params=list(meanlog=0, sdlog=0.5))
  )),
  treatment  = list(assignment="randomization", allocation="1:1"),
  event_time = list(model="ph_pwexp",
                    baseline=list(rates=c(0.10, 0.06, 0.03), cuts=c(6, 18)),
                    effects=list(intercept=0, treatment=-0.4,
                                 covariates=list(age=0.01, x=0.03))),
  censoring  = list(mode="target_overall", target=0.25, admin_time=30)
)
dat4 <- simulate_from_recipe(validate_recipe(rec4), seed=123)


## ----summ4, eval=TRUE, echo=FALSE---------------------------------------------

summ4 <- data.frame(
  Metric = c("n", "Events", "Censoring rate", "Mean time", "Median time"),
  Metric = c("n", "Events", "Censoring rate", "Mean time", "Median time"),
  Value  = c(nrow(dat4),
             sum(dat4$status == 1, na.rm = TRUE),
             round(mean(dat4$status == 0, na.rm = TRUE), 3),
             round(mean(dat4$time,   na.rm = TRUE), 2),
             round(stats::median(dat4$time, na.rm = TRUE), 2))
)
knitr::kable(summ4, caption = "Example 4 — Summary")

## ----batch, eval=TRUE---------------------------------------------------------
base <- validate_recipe(rec2)

out_dir <- file.path(tempdir(), "rmstss-manifest-demo")
unlink(out_dir, recursive = TRUE, force = TRUE)

man <- generate_recipe_sets(
  base_recipe = base,
  vary = list(n = c(200, 400),
              "event_time.effects.treatment" = c(-0.15, -0.25)),
  out_dir  = out_dir,
  formats  = c("rds","csv"),
  n_reps   = 1,
  seed_base = 2025
)

# Inspect the first row's compact metadata (fields only; no file paths)
m <- readRDS(file.path(out_dir, "manifest.rds"))
names(m)
if ("meta" %in% names(m) && length(m$meta[[1]]) > 0) {
  list(model = m$meta[[1]]$model,
       baseline = m$meta[[1]]$baseline,
       effects = m$meta[[1]]$effects,
       achieved_censoring = m$meta[[1]]$achieved_censoring,
       n = m$meta[[1]]$n)
} else {
  "Manifest is minimal (older run); use rebuild_manifest() to enrich."
}

# Load datasets back
sets <- load_recipe_sets(file.path(out_dir, "manifest.rds"))
attr(sets[[1]]$data, "achieved_censoring")
str(sets[[1]]$meta)

