rec <- list(
   seed = 20250904,
   n = 500,
   covariates = list(
      defs = list(
         list(name="age", type="continuous", dist="normal", params=list(mean=62, sd=10), transform=c("center(60)","scale(10)")),
         list(name="stab_glu", type="continuous", dist="lognormal", params=list(meanlog=4.55, sdlog=0.15)),
         list(name="gender", type="categorical", dist="bernoulli", params=list(p=0.45))
      )
   ),
   event_time = list(
      model = "aft_lognormal",
      baseline = list(mu = 3.0, sigma = 0.7),
      effects = list(
         treatment = -0.25,
         covariates = list(age = 0.01, gender = -0.10, stab_glu = 0.004)
      ),
      tau = 24
   ),
   censoring = list(
      mode = "target_overall",
      target = 0.25,
      admin_time = 30,
      family = "exponential"
   ),
   treatment = list(
      assignment = "randomization",
      allocation = "1:1"
   )
)

dat <- simulate_from_recipe(rec)
attr(dat, "achieved_censoring")
head(dat)
