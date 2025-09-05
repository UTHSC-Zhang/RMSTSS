pkgname <- "RMSTSS"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "RMSTSS-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('RMSTSS')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("DC.power.analytical")
### * DC.power.analytical

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: DC.power.analytical
### Title: Analyze Power for RMST Model with Dependent Censoring (Analytic)
### Aliases: DC.power.analytical

### ** Examples

# Generate sample pilot data
set.seed(123)
n_pilot <- 150
pilot_df <- data.frame(
  time = rexp(n_pilot, rate = 0.1),
  arm = rep(0:1, each = n_pilot / 2),
  age = rnorm(n_pilot, mean = 60, sd = 10)
)
# Introduce a treatment effect
pilot_df$time[pilot_df$arm == 1] <- pilot_df$time[pilot_df$arm == 1] * 1.2

# Create competing event indicators
# Assume 70% primary event, 15% dependent censoring, 15% independent censoring
event_type <- sample(0:2, n_pilot, replace = TRUE, prob = c(0.7, 0.15, 0.15))
pilot_df$status <- ifelse(event_type == 0, 1, 0)
pilot_df$dep_cens_status <- ifelse(event_type == 1, 1, 0)
pilot_df$time[event_type != 0] <- pilot_df$time[event_type != 0] * 0.8

# Run the power analysis
dc_power_results <- DC.power.analytical(
  pilot_data = pilot_df,
  time_var = "time",
  status_var = "status",
  arm_var = "arm",
  dep_cens_status_var = "dep_cens_status",
  sample_sizes = c(200, 300, 400),
  linear_terms = "age",
  L = 20,
  alpha = 0.05
)
print(dc_power_results$results_data)
print(dc_power_results$results_plot)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("DC.power.analytical", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("DC.ss.analytical")
### * DC.ss.analytical

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: DC.ss.analytical
### Title: Find Sample Size for RMST Model with Dependent Censoring
###   (Analytic)
### Aliases: DC.ss.analytical

### ** Examples

# Generate sample pilot data with a clear treatment effect
set.seed(456)
n_pilot <- 200
pilot_df_ss <- data.frame(
  time = rexp(n_pilot, rate = 0.2),
  arm = rep(0:1, each = n_pilot / 2),
  age = rnorm(n_pilot, mean = 55, sd = 8)
)
# Introduce a treatment effect
pilot_df_ss$time[pilot_df_ss$arm == 1] <- pilot_df_ss$time[pilot_df_ss$arm == 1] * 1.5

# Create competing event indicators
event_type <- sample(0:2, n_pilot, replace = TRUE, prob = c(0.6, 0.2, 0.2))
pilot_df_ss$status <- ifelse(event_type == 0, 1, 0)
pilot_df_ss$dep_cens_status <- ifelse(event_type == 1, 1, 0)
pilot_df_ss$time[event_type != 0] <- pilot_df_ss$time[event_type != 0] * 0.7

# Run the sample size search
dc_ss_results <- DC.ss.analytical(
  pilot_data = pilot_df_ss,
  time_var = "time",
  status_var = "status",
  arm_var = "arm",
  dep_cens_status_var = "dep_cens_status",
  target_power = 0.80,
  linear_terms = "age",
  L = 15,
  alpha = 0.05,
  n_start = 100,
  n_step = 50
)
print(dc_ss_results$results_data)
print(dc_ss_results$results_plot)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("DC.ss.analytical", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("GAM.power.boot")
### * GAM.power.boot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: GAM.power.boot
### Title: Calculate Power for a Semiparametric Additive RMST Model via
###   Simulation
### Aliases: GAM.power.boot

### ** Examples

## Not run: 
##D pilot_df <- data.frame(
##D   time = rexp(100, 0.08),
##D   status = rbinom(100, 1, 0.7),
##D   arm = rep(0:1, each = 50),
##D   age = rnorm(100, 60, 10)
##D )
##D # Add a treatment effect
##D pilot_df$time[pilot_df$arm == 1] <- pilot_df$time[pilot_df$arm == 1] * 1.3
##D 
##D power_results <- GAM.power.boot(
##D   pilot_data = pilot_df,
##D   time_var = "time",
##D   status_var = "status",
##D   arm_var = "arm",
##D   sample_sizes = c(100, 150),
##D   linear_terms = "age",
##D   L = 15,
##D   n_sim = 100, # Use more sims in practice, e.g., 1000
##D   parallel.cores = 2
##D )
##D print(power_results$results_data)
##D print(power_results$results_plot)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("GAM.power.boot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("GAM.ss.boot")
### * GAM.ss.boot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: GAM.ss.boot
### Title: Find Sample Size for a Semiparametric Additive RMST Model via
###   Simulation
### Aliases: GAM.ss.boot

### ** Examples

## Not run: 
##D pilot_df_effect <- data.frame(
##D   time = c(stats::rexp(50, 0.1), stats::rexp(50, 0.04)), # Effect
##D   status = stats::rbinom(100, 1, 0.9),
##D   arm = rep(0:1, each = 50)
##D )
##D 
##D ss_results <- GAM.ss.boot(
##D   pilot_data = pilot_df_effect,
##D   time_var = "time",
##D   status_var = "status",
##D   arm_var = "arm",
##D   target_power = 0.80,
##D   L = 15,
##D   n_sim = 100,      # Low n_sim for example
##D   n_start = 100,
##D   n_step = 50,
##D   patience = 2,
##D   parallel.cores = 2
##D )
##D print(ss_results$results_data)
##D print(ss_results$results_plot)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("GAM.ss.boot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("MS.power.analytical")
### * MS.power.analytical

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: MS.power.analytical
### Title: Analyze Power for a Multiplicative Stratified RMST Model
###   (Analytic)
### Aliases: MS.power.analytical

### ** Examples

set.seed(123)
pilot_df_strat <- data.frame(
 time = rexp(120, 0.15),
 status = rbinom(120, 1, 0.6),
 arm = rep(0:1, each = 60),
 region = factor(rep(c("A", "B", "C"), each = 40))
)
pilot_df_strat$time[pilot_df_strat$arm == 1] <- pilot_df_strat$time[pilot_df_strat$arm == 1] * 1.5

power_results <- MS.power.analytical(
 pilot_data = pilot_df_strat,
 time_var = "time", status_var = "status", arm_var = "arm", strata_var = "region",
 sample_sizes = c(50, 75, 100),
 L = 10, alpha = 0.05
)
print(power_results$results_data)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("MS.power.analytical", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("MS.power.boot")
### * MS.power.boot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: MS.power.boot
### Title: Analyze Power for a Multiplicative Stratified RMST Model via
###   Simulation
### Aliases: MS.power.boot

### ** Examples

## Not run: 
##D pilot_df_strat <- data.frame(
##D  time = rexp(120, 0.15),
##D  status = rbinom(120, 1, 0.6),
##D  arm = rep(0:1, each = 60),
##D  region = factor(rep(c("A", "B", "C"), each = 40))
##D )
##D pilot_df_strat$time[pilot_df_strat$arm == 1] <- pilot_df_strat$time[pilot_df_strat$arm == 1] * 1.4
##D 
##D power_results <- MS.power.boot(
##D  pilot_data = pilot_df_strat,
##D  time_var = "time", status_var = "status", arm_var = "arm", strata_var = "region",
##D  sample_sizes = c(50, 75),
##D  L = 10,
##D  n_sim = 100 # Low n_sim for example
##D )
##D print(power_results$results_data)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("MS.power.boot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("MS.ss.analytical")
### * MS.ss.analytical

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: MS.ss.analytical
### Title: Find Sample Size for a Multiplicative Stratified RMST Model
###   (Analytic)
### Aliases: MS.ss.analytical

### ** Examples

set.seed(456)
pilot_df_strat_effect <- data.frame(
 time = c(rexp(60, 0.15), rexp(60, 0.08)), # Effect
 status = rbinom(120, 1, 0.7),
 arm = rep(0:1, each = 60),
 region = factor(rep(c("A", "B"), each = 60))
)

ss_results <- MS.ss.analytical(
 pilot_data = pilot_df_strat_effect,
 time_var = "time", status_var = "status", arm_var = "arm", strata_var = "region",
 target_power = 0.80, L = 10,
 n_start = 100, n_step = 50
)
print(ss_results$results_data)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("MS.ss.analytical", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("MS.ss.boot")
### * MS.ss.boot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: MS.ss.boot
### Title: Estimate Sample Size for a Multiplicative Stratified RMST Model
###   via Simulation
### Aliases: MS.ss.boot

### ** Examples

## Not run: 
##D pilot_df_strat_effect <- data.frame(
##D  time = c(rexp(60, 0.15), rexp(60, 0.08)), # Effect
##D  status = rbinom(120, 1, 0.7),
##D  arm = rep(0:1, each = 60),
##D  region = factor(rep(c("A", "B", "C"), each = 40))
##D )
##D ss_results <- MS.ss.boot(
##D  pilot_data = pilot_df_strat_effect,
##D  time_var = "time", status_var = "status", arm_var = "arm", strata_var = "region",
##D  target_power = 0.80, L = 10,
##D  n_sim = 100, # Low n_sim for example
##D  n_start = 100,
##D  n_step = 50, patience = 2
##D )
##D print(ss_results$results_data)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("MS.ss.boot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("additive.power.analytical")
### * additive.power.analytical

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: additive.power.analytical
### Title: Analyze Power for a Stratified Additive RMST Model (Analytic)
### Aliases: additive.power.analytical

### ** Examples

set.seed(123)
pilot_df_strat <- data.frame(
 time = rexp(150, 0.1),
 status = rbinom(150, 1, 0.8),
 arm = rep(0:1, each = 75),
 region = factor(rep(c("A", "B", "C"), each = 50)),
 age = rnorm(150, 60, 10)
)
# Introduce an additive treatment effect
pilot_df_strat$time[pilot_df_strat$arm == 1] <-
  pilot_df_strat$time[pilot_df_strat$arm == 1] + 1.5

power_results <- additive.power.analytical(
  pilot_data = pilot_df_strat,
  time_var = "time", status_var = "status", arm_var = "arm", strata_var = "region",
  sample_sizes = c(100, 150, 200),
  linear_terms = "age",
  L = 12
)
print(power_results$results_data)
print(power_results$results_plot)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("additive.power.analytical", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("additive.ss.analytical")
### * additive.ss.analytical

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: additive.ss.analytical
### Title: Find Sample Size for a Stratified Additive RMST Model (Analytic)
### Aliases: additive.ss.analytical

### ** Examples

set.seed(123)
pilot_df_strat <- data.frame(
 time = rexp(150, 0.1),
 status = rbinom(150, 1, 0.8),
 arm = rep(0:1, each = 75),
 region = factor(rep(c("A", "B", "C"), each = 50)),
 age = rnorm(150, 60, 10)
)
# Introduce an additive treatment effect
pilot_df_strat$time[pilot_df_strat$arm == 1] <-
  pilot_df_strat$time[pilot_df_strat$arm == 1] + 1.5

  # Find the required sample size per stratum for 80% power
  ss_results <- additive.ss.analytical(
    pilot_data = pilot_df_strat,
    time_var = "time", status_var = "status",
    arm_var = "arm", strata_var = "region",
    target_power = 0.50,
    L = 18, #
    n_start = 200,
    n_step = 50,
    max_n_per_arm = 1000
  )
  print(ss_results$results_data)
  print(ss_results$results_plot)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("additive.ss.analytical", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("gen_covariates")
### * gen_covariates

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: gen_covariates
### Title: Generate covariate matrix/data frame from a recipe
### Aliases: gen_covariates
### Keywords: internal

### ** Examples

defs <- list(
  list(name="x", type="continuous", dist="normal", params=list(mean=0, sd=1)),
  list(name="z", type="categorical", dist="bernoulli", params=list(p=0.3))
)
X <- gen_covariates(10, list(defs = defs))
str(X)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("gen_covariates", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("gen_event_times")
### * gen_event_times

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: gen_event_times
### Title: Generate event times from the event-time block
### Aliases: gen_event_times
### Keywords: internal

### ** Examples

et <- list(model="cox_exp", baseline=list(rate=0.05),
           effects=list(treatment=-0.3, covariates=list()))
X  <- data.frame(x=rnorm(10))
A  <- rbinom(10,1,0.5)
T  <- gen_event_times(et, A, X)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("gen_event_times", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("linear.power.analytical")
### * linear.power.analytical

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: linear.power.analytical
### Title: Analyze Power for a Linear RMST Model (Analytic)
### Aliases: linear.power.analytical

### ** Examples

pilot_df <- data.frame(
  time = rexp(100, 0.1),
  status = rbinom(100, 1, 0.7),
  arm = rep(0:1, each = 50),
  age = rnorm(100, 55, 10)
)
power_results <- linear.power.analytical(
  pilot_data = pilot_df,
  time_var = "time",
  status_var = "status",
  arm_var = "arm",
  linear_terms = "age",
  sample_sizes = c(100, 200, 300),
  L = 10
)
print(power_results$results_data)
print(power_results$results_plot)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("linear.power.analytical", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("linear.power.boot")
### * linear.power.boot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: linear.power.boot
### Title: Analyze Power for a Linear RMST Model via Simulation
### Aliases: linear.power.boot

### ** Examples

## Not run: 
##D pilot_df <- data.frame(
##D   time = rexp(100, 0.1),
##D   status = rbinom(100, 1, 0.7),
##D   arm = rep(0:1, each = 50),
##D   age = rnorm(100, 60, 8)
##D )
##D # Introduce a treatment effect for a more interesting example
##D pilot_df$time[pilot_df$arm == 1] <- pilot_df$time[pilot_df$arm == 1] * 1.5
##D 
##D power_results <- linear.power.boot(
##D   pilot_data = pilot_df,
##D   time_var = "time",
##D   status_var = "status",
##D   arm_var = "arm",
##D   linear_terms = "age",
##D   sample_sizes = c(100, 150, 200),
##D   L = 10,
##D   n_sim = 200 # Use more simulations in practice (e.g., 1000)
##D )
##D print(power_results$results_data)
##D print(power_results$results_plot)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("linear.power.boot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("linear.ss.analytical")
### * linear.ss.analytical

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: linear.ss.analytical
### Title: Find Sample Size for a Linear RMST Model (Analytic)
### Aliases: linear.ss.analytical

### ** Examples

pilot_df <- data.frame(
  time = c(rexp(50, 0.1), rexp(50, 0.07)), # Introduce an effect
  status = rbinom(100, 1, 0.8),
  arm = rep(0:1, each = 50),
  age = rnorm(100, 55, 10)
)
ss_results <- linear.ss.analytical(
  pilot_data = pilot_df,
  time_var = "time",
  status_var = "status",
  arm_var = "arm",
  target_power = 0.80,
  L = 10
)
print(ss_results$results_data)
print(ss_results$results_plot)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("linear.ss.analytical", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("linear.ss.boot")
### * linear.ss.boot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: linear.ss.boot
### Title: Find Sample Size for a Linear RMST Model via Simulation
### Aliases: linear.ss.boot

### ** Examples

## Not run: 
##D pilot_df_effect <- data.frame(
##D   time = c(rexp(50, 0.1), rexp(50, 0.05)), # Effect present
##D   status = rbinom(100, 1, 0.8),
##D   arm = rep(0:1, each = 50)
##D )
##D ss_results <- linear.ss.boot(
##D   pilot_data = pilot_df_effect,
##D   time_var = "time",
##D   status_var = "status",
##D   arm_var = "arm",
##D   target_power = 0.80,
##D   L = 10,
##D   n_sim = 200, # Low n_sim for example
##D   patience = 2,
##D   n_start = 100,
##D   n_step = 50,
##D   max_n_per_arm = 500
##D )
##D print(ss_results$results_data)
##D print(ss_results$results_plot)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("linear.ss.boot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("recipe_grid")
### * recipe_grid

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: recipe_grid
### Title: Build a grid of recipes by varying parameters
### Aliases: recipe_grid

### ** Examples

base <- recipe_quick_aft(200, 24, "aft_lognormal",
  baseline = list(mu=3, sigma=0.7),
  treat_effect = -0.2,
  covariates = list(list(name="x", type="continuous", dist="normal", params=list(mean=0, sd=1))),
  target_censoring = 0.25
)
gs <- recipe_grid(base, list(n = c(125, 250), "censoring.target" = c(0.2, 0.3)))
length(gs)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("recipe_grid", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("recipe_quick_aft")
### * recipe_quick_aft

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: recipe_quick_aft
### Title: Quick AFT recipe builder
### Aliases: recipe_quick_aft

### ** Examples

covs <- list(
  list(name="age", type="continuous", dist="normal", params=list(mean=62, sd=10),
       transform=c("center(60)","scale(10)")),
  list(name="gender", type="categorical", dist="bernoulli", params=list(p=0.45))
)
rec <- recipe_quick_aft(n=300, tau=24, model="aft_weibull",
                        baseline=list(shape=1.3, scale=12),
                        treat_effect=-0.25, covariates=covs,
                        target_censoring=0.25, allocation="1:1", seed=2025)
dat <- simulate_from_recipe(rec)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("recipe_quick_aft", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rmst_from_recipe")
### * rmst_from_recipe

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rmst_from_recipe
### Title: Simulate from a recipe and run an analysis function
### Aliases: rmst_from_recipe

### ** Examples

## Not run: 
##D # Suppose your package provides `rmst_fit(data, tau, ...)`
##D res <- rmst_from_recipe(
##D   recipe_quick_aft(200, 24, "aft_lognormal",
##D                    baseline = list(mu=3, sigma=0.7),
##D                    treat_effect = -0.2,
##D                    covariates = list(list(name="x", type="continuous",
##D                                           dist="normal", params=list(mean=0, sd=1)))),
##D   analyze_fn = rmst_fit
##D )
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rmst_from_recipe", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("run_app")
### * run_app

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: run_app
### Title: Launch the RMSTdesign Shiny Application
### Aliases: run_app

### ** Examples

## Not run: 
##D   RMSTSS::run_app()
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("run_app", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("simulate_from_recipe")
### * simulate_from_recipe

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: simulate_from_recipe
### Title: Simulate time-to-event data from a declarative recipe
### Aliases: simulate_from_recipe

### ** Examples

set.seed(1)
rec <- list(
  n = 200,
  covariates = list(defs = list(
    list(name="age", type="continuous", dist="normal", params=list(mean=62, sd=10),
         transform=c("center(60)","scale(10)")),
    list(name="gender", type="categorical", dist="bernoulli", params=list(p=0.45))
  )),
  event_time = list(
    model="aft_lognormal",
    baseline=list(mu=3.0, sigma=0.7),
    effects=list(treatment=-0.25, covariates=list(age=0.01, gender=-0.10)),
    tau=24
  ),
  censoring = list(mode="target_overall", target=0.25, admin_time=30),
  treatment = list(assignment="randomization", allocation="1:1")
)
dat <- simulate_from_recipe(rec)
attr(dat, "tau")
attr(dat, "achieved_censoring")
head(dat)

## Not run: 
##D # YAML round-trip (requires Suggests: yaml)
##D tmp <- tempfile(fileext = ".yml")
##D write_recipe_yaml(rec, tmp)
##D dat2 <- simulate_from_recipe(tmp)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("simulate_from_recipe", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("validate_recipe")
### * validate_recipe

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: validate_recipe
### Title: Validate a simulation recipe (schema + parameter checks)
### Aliases: validate_recipe

### ** Examples

rec <- list(
  n = 50,
  covariates = list(defs = list(
    list(name="age", type="continuous", dist="normal", params=list(mean=60, sd=10)),
    list(name="gender", type="categorical", dist="bernoulli", params=list(p=0.4))
  )),
  event_time = list(
    model="aft_lognormal", baseline=list(mu=3, sigma=0.7),
    effects=list(treatment=-0.3, covariates=list(age=0.01, gender=-0.1)),
    tau=24
  ),
  censoring = list(mode="target_overall", target=0.25, admin_time=30),
  treatment = list(assignment="randomization", allocation="1:1")
)
validate_recipe(rec)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("validate_recipe", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("write_recipe_yaml")
### * write_recipe_yaml

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: write_recipe_yaml
### Title: Write a recipe to YAML
### Aliases: write_recipe_yaml

### ** Examples

## Not run: 
##D rec <- list(n=10, covariates=list(defs=list(
##D   list(name="x", type="continuous", dist="normal", params=list(mean=0, sd=1))
##D )), event_time=list(model="cox_exp", baseline=list(rate=0.05),
##D     effects=list(treatment=-0.3, covariates=list()), tau=12),
##D censoring=list(mode="target_overall", target=0.2, admin_time=18),
##D treatment=list(assignment="randomization", allocation="1:1"))
##D write_recipe_yaml(rec, tempfile(fileext=".yml"))
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("write_recipe_yaml", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
