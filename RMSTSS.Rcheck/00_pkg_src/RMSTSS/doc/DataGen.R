## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = NA,
  fig.width = 6, fig.height = 4,
  warning = FALSE, message = FALSE,
  # Only evaluate heavy chunks when NOT_CRAN=true
  eval = identical(Sys.getenv("NOT_CRAN"), "true")
)
library(RMSTSS)
set.seed(1)

## ----covariates, eval=TRUE----------------------------------------------------
covs <- list(
  list(name="age",   type="continuous",  dist="normal",     params=list(mean=62, sd=10),
       transform=c("center(60)","scale(10)")),
  list(name="sex",   type="categorical", dist="bernoulli",  params=list(p=0.45)),
  list(name="stage", type="categorical", dist="ordinal",
       params=list(prob=c(0.3,0.5,0.2), labels=c("I","II","III")))
)

# gen_covariates() is internal; for the vignette we simulate a tiny dataset
# with a supported model and just show the covariate columns.
tmp <- list(
  n = 5,
  covariates = list(defs = covs),
  treatment  = NULL,
  event_time = list(
    model    = "aft_lognormal",
    baseline = list(mu = 3, sigma = 0.4),
    effects  = list(intercept = 0),
    tau      = 1
  ),
  censoring  = list(mode = "explicit", administrative = list(time = 1e6))
)

d <- simulate_from_recipe(validate_recipe(tmp), seed = 101)
cov_cols <- setdiff(names(d), c("time", "status", "arm"))
if (length(cov_cols)) {
  d[, cov_cols, drop = FALSE]
} else {
  head(d)
}


## -----------------------------------------------------------------------------
# tr <- list(assignment="randomization", allocation="1:1")

## ----ex1, eval=TRUE-----------------------------------------------------------
covs1 <- list(
  list(name="age", type="continuous", dist="normal", params=list(mean=62, sd=10),
       transform=c("center(60)","scale(10)")),
  list(name="sex", type="categorical", dist="bernoulli", params=list(p=0.45))
)

rec1 <- list(
  n = 300,
  covariates = list(defs = covs1),
  treatment  = list(assignment="randomization", allocation="1:1"),
  event_time = list(model="aft_lognormal",
                    baseline=list(mu=3.0, sigma=0.6),
                    effects=list(intercept=0, treatment=-0.25,
                                 covariates=list(age=0.01, sex=-0.2)),
                    tau=24),
  censoring  = list(mode="target_overall", target=0.25, admin_time=36),
  seed = 11
)

rec1 <- validate_recipe(rec1)
dat1 <- simulate_from_recipe(rec1)
c(tau = attr(dat1, "tau"), achieved_censoring = attr(dat1, "achieved_censoring"))
head(dat1)

## ----ex2, eval=TRUE-----------------------------------------------------------
rec2 <- rec1
rec2$event_time <- list(model="aft_weibull",
                        baseline=list(shape=1.3, scale=12),
                        effects=list(intercept=0, treatment=-0.20,
                                     covariates=list(age=0.008)),
                        tau=24)
dat2 <- simulate_from_recipe(validate_recipe(rec2), seed=12)
summary(dat2$time)

## ----ex3, eval=TRUE-----------------------------------------------------------
rec3 <- list(
  n = 400,
  covariates = list(defs = covs1),
  treatment  = list(assignment="randomization", allocation="1:1"),
  event_time = list(model="cox_pwexp",
                    baseline=list(rates=c(0.05), cuts=numeric(0)),  # single segment = exponential
                    effects=list(intercept=0, treatment=-0.3,
                                 covariates=list(age=0.01)),
                    tau=24),
  censoring  = list(mode="target_overall", target=0.20, admin_time=30),
  seed = 13
)
dat3 <- simulate_from_recipe(validate_recipe(rec3))
mean(dat3$status==0)

## ----ex4, eval=TRUE-----------------------------------------------------------
rec4 <- list(
  n = 500,
  covariates = list(defs = list(list(name="age", type="continuous", dist="normal", params=list(mean=60, sd=8)))),
  treatment  = list(assignment="randomization", allocation="1:1"),
  event_time = list(model="cox_pwexp",
                    baseline=list(rates=c(0.10, 0.06, 0.03), cuts=c(6, 18)),
                    effects=list(intercept=0, treatment=-0.4,
                                 covariates=list(age=0.01)),
                    tau=24),
  censoring  = list(mode="target_overall", target=0.25, admin_time=30)
)
dat4 <- simulate_from_recipe(validate_recipe(rec4), seed=123)
c(n=nrow(dat4), events=sum(dat4$status==1), cens_rate=mean(dat4$status==0))

## ----ex5, eval=TRUE-----------------------------------------------------------
rec5 <- rec4
rec5$covariates$defs <- c(rec5$covariates$defs,
                          list(list(name="stage", type="categorical", dist="categorical",
                                    params=list(prob=c(0.3,0.5,0.2), labels=c("I","II","III")))))
rec5$treatment <- list(assignment="stratified", allocation="1:1", stratify_by=c("stage"))
d5 <- simulate_from_recipe(validate_recipe(rec5), seed=9)
prop.table(table(d5$stage, d5$arm), 1)

## ----ex6, eval=TRUE-----------------------------------------------------------
rec6 <- list(
  n = 800,
  covariates = list(defs = list(
    list(name="x", type="continuous", dist="normal", params=list(mean=0, sd=1)),
    list(name="sex", type="categorical", dist="bernoulli", params=list(p=0.5))
  )),
  treatment  = list(assignment="logistic_ps",
                    ps_model=list(formula="~ 1 + x + sex",
                                  beta=c(-0.3, 1.2, -0.6))),
  event_time = list(model="cox_pwexp", baseline=list(rates=c(0.05), cuts=numeric(0)),
                    effects=list(intercept=0, treatment=0.0,
                                 covariates=list(x=0.0, sex=0.0)), tau=12),
  censoring  = list(mode="target_overall", target=0.15, admin_time=50)
)
d6 <- simulate_from_recipe(validate_recipe(rec6), seed=7)
cor(d6$x, d6$arm)

## ----floor, eval=TRUE---------------------------------------------------------
rec_floor <- rec3
rec_floor$censoring <- list(mode="explicit", administrative=list(time=30))
dat_floor <- simulate_from_recipe(validate_recipe(rec_floor), seed=99)
mean(dat_floor$status==0)  # floor

