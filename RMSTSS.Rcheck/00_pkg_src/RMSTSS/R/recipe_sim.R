# R/recipe_sim.R
# Core simulation engines, validation, and I/O for "data recipes".
# Paste into your package and run devtools::document().

#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x

#' @keywords internal
stop_path <- function(path, msg) stop(sprintf("%s: %s", path, msg), call. = FALSE)

#' @keywords internal
.require <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Package '%s' is required for this operation.", pkg), call. = FALSE)
  }
}

#' Coerce a recipe into a list (optionally read from YAML/JSON)
#'
#' Converts a recipe supplied as a list or a path to `.yml/.yaml`/`.json`
#' into a standard R list. This is used internally by validators and simulators.
#'
#' @param recipe A list, or a length-1 character path to a YAML/JSON file.
#' @return A named list representing the recipe.
#' @keywords internal
as_list_recipe <- function(recipe) {
  if (is.character(recipe) && length(recipe) == 1L) {
    ext <- tolower(tools::file_ext(recipe))
    if (ext %in% c("yml", "yaml")) {
      .require("yaml")
      return(yaml::read_yaml(recipe))
    }
    if (ext == "json") {
      .require("jsonlite")
      return(jsonlite::read_json(recipe, simplifyVector = TRUE))
    }
    stop("Unsupported recipe extension. Use .yml/.yaml or .json.")
  }
  if (!is.list(recipe)) stop("`recipe` must be a list or a path to YAML/JSON.")
  recipe
}

# -------- Validation helpers (internal) ------------------------------------

#' @keywords internal
.validate_prob <- function(p, path) {
  if (!is.numeric(p) || any(is.na(p))) stop_path(path, "must be numeric & non-NA")
  if (any(p < -1e-12 | p > 1 + 1e-12)) stop_path(path, "must lie in [0,1]")
}

#' @keywords internal
.validate_positive <- function(x, path) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x <= 0) stop_path(path, "must be > 0")
}

#' @keywords internal
.validate_cov_defs <- function(defs) {
  if (!is.list(defs) || !length(defs)) stop("covariates.defs must be a non-empty list")
  nm <- vapply(defs, function(d) d$name %||% NA_character_, character(1))
  if (anyNA(nm) || any(nm == "")) stop("Every covariate must have a non-empty `name`")
  if (anyDuplicated(nm)) stop("Covariate names must be unique")
  for (i in seq_along(defs)) {
    d <- defs[[i]]
    base <- sprintf("covariates.defs[[%d]]", i)
    ty <- d$type %||% stop_path(base, "missing `type`")
    if (!ty %in% c("continuous", "categorical")) stop_path(base, "type must be continuous|categorical")
    dist <- d$dist %||% stop_path(base, "missing `dist`")
    prms <- d$params %||% list()
    if (ty == "continuous") {
      if (dist %in% c("normal")) {
        .validate_positive(prms$sd %||% NA, paste0(base, ".params$sd"))
      } else if (dist %in% c("lognormal")) {
        .validate_positive(prms$sdlog %||% NA, paste0(base, ".params$sdlog"))
      } else if (dist %in% c("uniform")) {
        if (is.null(prms$min) || is.null(prms$max) || prms$min >= prms$max)
          stop_path(base, "uniform requires params{min < max}")
      } else if (dist %in% c("weibull")) {
        .validate_positive(prms$shape %||% NA, paste0(base, ".params$shape"))
        .validate_positive(prms$scale %||% NA, paste0(base, ".params$scale"))
      } else if (dist %in% c("gamma")) {
        .validate_positive(prms$shape %||% NA, paste0(base, ".params$shape"))
        if (is.null(prms$rate) && is.null(prms$scale))
          stop_path(base, "gamma requires `rate` or `scale`")
      } else {
        stop_path(base, sprintf("unknown continuous dist '%s'", dist))
      }
    } else {
      if (dist == "bernoulli") {
        .validate_prob(prms$p %||% NA, paste0(base, ".params$p"))
      } else if (dist == "categorical") {
        prob <- prms$prob %||% stop_path(base, "categorical requires params$prob")
        .validate_prob(prob, paste0(base, ".params$prob"))
        if (abs(sum(prob) - 1) > 1e-6) stop_path(base, "categorical probs must sum to 1")
        if (!is.null(prms$labels) && length(prms$labels) != length(prob))
          stop_path(base, "labels length must match length(prob)")
      } else {
        stop_path(base, sprintf("unknown categorical dist '%s'", dist))
      }
    }
  }
}

#' Validate a simulation recipe (schema + parameter checks)
#'
#' Ensures a user-supplied recipe contains all required sections
#' and that distribution parameters are valid and compatible.
#'
#' @param recipe A list, or a YAML/JSON file path describing the simulation.
#'
#' @details
#' **Required blocks**
#' - `n` (positive number of subjects)
#' - `covariates.defs` (list of covariate definitions)
#' - `event_time` (AFT lognormal/Weibull or Cox exponential/piecewise-exp)
#' - `censoring` (target overall or explicit)
#' - `treatment` (randomization, stratified, or logistic_ps)
#'
#' **Supported models**
#' - `event_time$model` in {"aft_lognormal","aft_weibull","cox_exp","cox_pwexp"}
#'
#' @return The validated (possibly augmented) recipe list; errors on failure.
#' @examples
#' rec <- list(
#'   n = 50,
#'   covariates = list(defs = list(
#'     list(name="age", type="continuous", dist="normal", params=list(mean=60, sd=10)),
#'     list(name="gender", type="categorical", dist="bernoulli", params=list(p=0.4))
#'   )),
#'   event_time = list(
#'     model="aft_lognormal", baseline=list(mu=3, sigma=0.7),
#'     effects=list(treatment=-0.3, covariates=list(age=0.01, gender=-0.1)),
#'     tau=24
#'   ),
#'   censoring = list(mode="target_overall", target=0.25, admin_time=30),
#'   treatment = list(assignment="randomization", allocation="1:1")
#' )
#' validate_recipe(rec)
#' @export
validate_recipe <- function(recipe) {
  r <- as_list_recipe(recipe)
  # top-level
  if (is.null(r$n) || !is.numeric(r$n) || r$n <= 0) stop("`n` must be a positive number")
  if (is.null(r$covariates) || is.null(r$covariates$defs)) stop("`covariates.defs` required")
  .validate_cov_defs(r$covariates$defs)

  # event_time
  et <- r$event_time %||% stop("`event_time` block required")
  model <- et$model %||% stop("event_time$model is required")
  allowed_models <- c("aft_lognormal", "aft_weibull", "cox_exp", "cox_pwexp")
  if (!model %in% allowed_models) {
    stop(sprintf("event_time$model must be one of: %s", paste(allowed_models, collapse=", ")))
  }
  if (model == "aft_lognormal") {
    bl <- et$baseline %||% stop("event_time.baseline{mu, sigma} required")
    .validate_positive(bl$sigma %||% NA, "event_time.baseline$sigma")
  } else if (model == "aft_weibull") {
    bl <- et$baseline %||% stop("event_time.baseline{shape, scale} required")
    .validate_positive(bl$shape %||% NA, "event_time.baseline$shape")
    .validate_positive(bl$scale %||% NA, "event_time.baseline$scale")
  } else if (model == "cox_exp") {
    bl <- et$baseline %||% list()
    .validate_positive(bl$rate %||% NA, "event_time.baseline$rate")
  } else if (model == "cox_pwexp") {
    bl <- et$baseline %||% stop("event_time.baseline{rates, cuts} required")
    rates <- bl$rates %||% stop("cox_pwexp requires baseline$rates")
    if (!is.numeric(rates) || any(rates <= 0)) stop("baseline$rates must be positive numeric")
    cuts  <- bl$cuts %||% numeric(0)
    if (length(cuts) && (any(diff(cuts) <= 0) || any(cuts <= 0))) {
      stop("baseline$cuts must be strictly increasing positive times")
    }
    if (length(rates) != length(cuts) + 1L) {
      stop("cox_pwexp requires length(baseline$rates) = length(baseline$cuts) + 1")
    }
  }
  if (!is.null(et$effects$treatment) && !is.numeric(et$effects$treatment))
    stop("event_time.effects.treatment must be numeric if provided")
  if (!is.null(et$effects$covariates) && !is.list(et$effects$covariates))
    stop("event_time.effects.covariates must be a named list of numeric coefficients")
  .validate_positive(et$tau %||% NA, "event_time$tau")

  # censoring
  cz <- r$censoring %||% stop("`censoring` block required")
  mode <- cz$mode %||% stop("censoring$mode required")
  if (mode == "target_overall") {
    .validate_prob(cz$target %||% NA, "censoring$target")
    if (!is.null(cz$admin_time)) .validate_positive(cz$admin_time, "censoring$admin_time")
    if (!is.null(cz$family) && !cz$family %in% c("exponential"))
      stop("censoring$family currently 'exponential' only")
  } else if (mode == "explicit") {
    if (!is.null(cz$administrative$time)) .validate_positive(cz$administrative$time, "censoring.administrative$time")
    if (!is.null(cz$random)) {
      d <- cz$random$dist %||% stop("censoring.random$dist required")
      p <- cz$random$params %||% list()
      if (d == "exponential") .validate_positive(p$rate %||% NA, "censoring.random.params$rate")
      else if (d == "lognormal") .validate_positive(p$sdlog %||% NA, "censoring.random.params$sdlog")
      else if (d == "uniform") {
        if (is.null(p$min) || is.null(p$max) || p$min >= p$max)
          stop("censoring.random.uniform requires min<max")
      } else stop("censoring.random$dist must be exponential|lognormal|uniform")
    }
    if (!is.null(cz$dependent)) {
      if (is.null(cz$dependent$formula)) stop("censoring.dependent$formula required when dependent is set")
      if (!is.null(cz$dependent$scale)) .validate_positive(cz$dependent$scale, "censoring.dependent$scale")
    }
  } else stop("censoring$mode must be 'target_overall' or 'explicit'")

  # treatment
  tr <- r$treatment %||% stop("`treatment` block required")
  assign <- tr$assignment %||% stop("treatment$assignment required")
  if (!assign %in% c("randomization", "logistic_ps", "stratified")) {
    stop("treatment$assignment must be randomization|logistic_ps|stratified")
  }
  if (assign %in% c("randomization","stratified")) {
    if (is.null(tr$allocation)) stop("randomization/stratified requires `allocation` like '1:1'")
    if (assign == "stratified" && is.null(tr$stratify_by))
      stop("stratified randomization requires `stratify_by` (vector of covariate names)")
  } else {
    if (is.null(tr$ps_model$formula)) stop("treatment.logistic_ps requires ps_model$formula")
  }

  r
}

# -------- Covariate generation (internal) ----------------------------------

#' @keywords internal
.apply_transforms <- function(x, transforms) {
  if (is.null(transforms)) return(x)
  for (t in transforms) {
    if (grepl("^center\\(", t)) {
      c0 <- as.numeric(sub("^center\\((.*)\\)$", "\\1", t))
      x <- x - c0
    } else if (grepl("^scale\\(", t)) {
      s0 <- as.numeric(sub("^scale\\((.*)\\)$", "\\1", t))
      x <- x / s0
    } else if (t == "log1p") {
      x <- log1p(x)
    } else {
      warning(sprintf("Unknown transform '%s' ignored", t))
    }
  }
  x
}

#' @keywords internal
.gen_one_cov <- function(n, def) {
  dist <- def$dist
  p <- def$params %||% list()
  if (def$type == "continuous") {
    x <- switch(dist,
      normal     = rnorm(n, mean = p$mean %||% 0, sd = p$sd %||% 1),
      lognormal  = rlnorm(n, meanlog = p$meanlog %||% 0, sdlog = p$sdlog %||% 1),
      uniform    = runif(n, min = p$min %||% 0, max = p$max %||% 1),
      weibull    = rweibull(n, shape = p$shape, scale = p$scale),
      gamma      = {
        rate <- p$rate
        if (is.null(rate) && !is.null(p$scale)) rate <- 1 / p$scale
        rgamma(n, shape = p$shape, rate = rate)
      },
      stop(sprintf("unknown continuous dist '%s'", dist))
    )
    x <- .apply_transforms(x, def$transform)
    return(x)
  } else { # categorical
    if (dist == "bernoulli") {
      x <- stats::rbinom(n, 1L, prob = p$p)
      return(x)
    } else if (dist == "categorical") {
      prob <- p$prob; labels <- p$labels %||% seq_along(prob)
      return(sample(labels, size = n, replace = TRUE, prob = prob))
    }
  }
}

#' Generate covariate matrix/data frame from a recipe
#'
#' Produces the covariate columns defined under `covariates.defs`, including
#' optional transforms (e.g., `center(60)`, `scale(10)`, `log1p`).
#'
#' @param n Integer sample size.
#' @param cov_block The `covariates` list from a validated recipe.
#' @return A `data.frame` with one column per covariate.
#' @examples
#' defs <- list(
#'   list(name="x", type="continuous", dist="normal", params=list(mean=0, sd=1)),
#'   list(name="z", type="categorical", dist="bernoulli", params=list(p=0.3))
#' )
#' X <- gen_covariates(10, list(defs = defs))
#' str(X)
#' @keywords internal
gen_covariates <- function(n, cov_block) {
  defs <- cov_block$defs
  X <- setNames(vector("list", length(defs)), vapply(defs, `[[`, "", "name"))
  for (i in seq_along(defs)) {
    nm <- defs[[i]]$name
    X[[nm]] <- .gen_one_cov(n, defs[[i]])
  }
  as.data.frame(X, check.names = TRUE)
}

# -------- Treatment assignment (internal) ----------------------------------

#' @keywords internal
.parse_allocation <- function(x) {
  if (is.numeric(x)) {
    p <- x / sum(x)
  } else if (is.character(x)) {
    parts <- strsplit(x, ":", fixed = TRUE)[[1]]
    nums <- as.numeric(parts)
    if (any(is.na(nums))) stop("allocation string must be like '1:1' or '2:1'")
    p <- nums / sum(nums)
  } else stop("allocation must be numeric vector or string '1:1'")
  p
}

#' @keywords internal
.assign_stratified <- function(n, tr_block, X) {
  p <- .parse_allocation(tr_block$allocation)
  if (length(p) != 2L) stop("stratified supports 2 arms currently")
  strata_vars <- tr_block$stratify_by
  if (!all(strata_vars %in% names(X))) stop("stratify_by variables not found in covariates")
  S <- X[strata_vars]
  key <- do.call(paste, c(S, sep = "\r"))
  A <- integer(n)
  for (lev in unique(key)) {
    idx <- which(key == lev)
    A[idx] <- stats::rbinom(length(idx), 1L, prob = p[2])
  }
  A
}

#' @keywords internal
assign_treatment <- function(n, tr_block, X) {
  if (tr_block$assignment == "randomization") {
    p <- .parse_allocation(tr_block$allocation)
    if (length(p) != 2L) stop("current implementation supports 2 arms")
    return(stats::rbinom(n, 1L, prob = p[2])) # 0=control, 1=treatment
  } else if (tr_block$assignment == "stratified") {
    return(.assign_stratified(n, tr_block, X))
  } else {
    # logistic PS: logit Pr(T=1|X) = <R expression over X>
    ftxt <- tr_block$ps_model$formula
    rhs  <- sub("^~", "", ftxt)
    env  <- list2env(as.list(X), parent = baseenv())
    eta  <- eval(parse(text = rhs), envir = env)
    p1   <- stats::plogis(eta)
    p1   <- pmin(pmax(p1, 1e-4), 1 - 1e-4)
    return(stats::rbinom(n, 1L, prob = p1))
  }
}

# -------- Event-time models (internal) -------------------------------------

#' @keywords internal
.gen_event_aft_lognormal <- function(A, X, et) {
  bl <- et$baseline
  mu <- bl$mu %||% 0
  sigma <- bl$sigma %||% 1
  beta_t <- et$effects$treatment %||% 0
  betas  <- et$effects$covariates %||% list()
  lin <- mu + beta_t * A
  if (length(betas)) {
    for (nm in names(betas)) {
      if (!nm %in% names(X)) warning(sprintf("coef for '%s' provided but covariate not found", nm))
      xi <- X[[nm]] %||% 0
      lin <- lin + betas[[nm]] * xi
    }
  }
  eps <- stats::rnorm(length(A), mean = 0, sd = sigma)
  T  <- exp(lin + eps)
  as.numeric(T)
}

#' @keywords internal
.gen_event_aft_weibull <- function(A, X, et) {
  bl <- et$baseline
  shape <- bl$shape
  scale0 <- bl$scale
  beta_t <- et$effects$treatment %||% 0
  betas  <- et$effects$covariates %||% list()
  lin <- beta_t * A
  if (length(betas)) {
    for (nm in names(betas)) lin <- lin + (betas[[nm]] * (X[[nm]] %||% 0))
  }
  scale_i <- scale0 * exp(lin)
  stats::rweibull(length(A), shape = shape, scale = scale_i)
}

#' @keywords internal
.gen_event_cox_exp <- function(A, X, et) {
  rate0 <- et$baseline$rate
  beta_t <- et$effects$treatment %||% 0
  betas  <- et$effects$covariates %||% list()
  eta <- beta_t * A
  if (length(betas)) {
    for (nm in names(betas)) {
      xi <- X[[nm]] %||% 0
      eta <- eta + betas[[nm]] * xi
    }
  }
  U <- stats::runif(length(A))
  T <- -log(U) / (rate0 * exp(eta))
  as.numeric(T)
}

#' @keywords internal
.gen_event_cox_pwexp <- function(A, X, et) {
  rates <- et$baseline$rates
  cuts  <- et$baseline$cuts %||% numeric(0)
  beta_t <- et$effects$treatment %||% 0
  betas  <- et$effects$covariates %||% list()
  eta <- beta_t * A
  if (length(betas)) {
    for (nm in names(betas)) eta <- eta + (betas[[nm]] * (X[[nm]] %||% 0))
  }
  E <- stats::rexp(length(A), rate = 1) / exp(eta)
  Hbound <- c(0, cumsum(rates[seq_along(cuts)] * diff(c(0, cuts))))
  k <- findInterval(E, Hbound, rightmost.closed = TRUE) + 1L
  k[k > length(rates)] <- length(rates)
  Hprev <- Hbound[pmax(1, k - 1L)]
  tprev <- c(0, cuts)[k]
  lambda_k <- rates[k]
  dt <- (E - Hprev) / lambda_k
  t <- tprev + dt
  as.numeric(t)
}

#' Generate event times from the event-time block
#'
#' Dispatches to the chosen model under `event_time$model`.
#'
#' @param et_block The `event_time` list from a validated recipe.
#' @param A Integer vector (0/1) treatment indicator.
#' @param X Data frame of covariates.
#' @return Numeric vector of event times.
#' @examples
#' et <- list(model="cox_exp", baseline=list(rate=0.05),
#'            effects=list(treatment=-0.3, covariates=list()))
#' X  <- data.frame(x=rnorm(10))
#' A  <- rbinom(10,1,0.5)
#' T  <- gen_event_times(et, A, X)
#' @keywords internal
gen_event_times <- function(et_block, A, X) {
  model <- et_block$model
  if (model == "aft_lognormal") return(.gen_event_aft_lognormal(A, X, et_block))
  if (model == "aft_weibull")   return(.gen_event_aft_weibull(A, X, et_block))
  if (model == "cox_exp")       return(.gen_event_cox_exp(A, X, et_block))
  if (model == "cox_pwexp")     return(.gen_event_cox_pwexp(A, X, et_block))
  stop("Unsupported event time model (extend here).")
}

# -------- Censoring (internal) ---------------------------------------------

#' @keywords internal
.achieved_cens_exp <- function(T_event, rate, admin_time = Inf) {
  C_rand <- stats::rexp(length(T_event), rate = rate)
  C <- pmin(C_rand, admin_time)
  status <- as.integer(T_event <= C)
  mean(status == 0)
}

#' @keywords internal
.solve_rate_for_target <- function(T_preview, target, admin_time = Inf, tol = 0.005) {
  f <- function(rate) .achieved_cens_exp(T_preview, rate, admin_time) - target
  lo <- 1e-6; hi <- 0.1
  for (k in 1:20) {
    if (sign(f(lo)) != sign(f(hi))) break
    hi <- hi * 2
  }
  if (sign(f(lo)) == sign(f(hi))) {
    warning("Could not bracket censoring target; using hi bound as rate")
    return(hi)
  }
  uniroot(f, c(lo, hi), tol = tol)$root
}

#' @keywords internal
gen_censoring <- function(cz_block, T_event, X) {
  mode <- cz_block$mode
  if (mode == "target_overall") {
    admin <- cz_block$admin_time %||% Inf
    n_prev <- min(length(T_event), 5000L)
    idx <- if (length(T_event) > n_prev) sample.int(length(T_event), n_prev) else seq_along(T_event)
    rate <- .solve_rate_for_target(T_event[idx], cz_block$target, admin_time = admin, tol = 0.002)
    C_rand <- stats::rexp(length(T_event), rate = rate)
    return(pmin(C_rand, admin))
  } else {
    admin <- cz_block$administrative$time %||% Inf
    if (!is.null(cz_block$random)) {
      d <- cz_block$random$dist
      p <- cz_block$random$params
      base_rate <- if (d == "exponential") p$rate else NA_real_
      if (!is.null(cz_block$dependent) && d == "exponential") {
        rhs  <- sub("^~", "", cz_block$dependent$formula)
        env  <- list2env(as.list(X), parent = baseenv())
        gX   <- eval(parse(text = rhs), envir = env)
        sc   <- cz_block$dependent$scale %||% 1
        rate_i <- base_rate * exp(sc * gX)
        Cr <- stats::rexp(length(T_event), rate = rate_i)
      } else {
        Cr <- switch(d,
          exponential = stats::rexp(length(T_event), rate = p$rate),
          lognormal   = stats::rlnorm(length(T_event), meanlog = p$meanlog %||% 0, sdlog = p$sdlog),
          uniform     = stats::runif(length(T_event), min = p$min, max = p$max),
          stop("unknown censoring random dist")
        )
      }
    } else Cr <- rep(Inf, length(T_event))
    pmin(Cr, admin)
  }
}

# -------- Public API -------------------------------------------------------

#' Simulate time-to-event data from a declarative recipe
#'
#' Generates a dataset with columns `time`, `status`, `arm`, and the
#' user-defined covariates. The resulting object carries attributes:
#' `attr(dat, "tau")` (analysis truncation) and
#' `attr(dat, "achieved_censoring")` (observed censoring fraction).
#'
#' @param recipe A list or YAML/JSON path describing covariates, event_time,
#'   censoring, and treatment blocks (see examples).
#' @param n_override Optional integer to override `n` in the recipe.
#' @param seed Optional RNG seed. If `NULL`, uses `recipe$seed` when present.
#'
#' @return A `data.frame` with survival outcome and covariates.
#'
#' @section Schema (minimal):
#' - `n` (int), `seed` (optional)
#' - `covariates.defs`: list of items with `name`, `type`, `dist`, `params`, `transform` (optional)
#' - `event_time`: `model`, `baseline`, `effects{treatment, covariates}`, `tau`
#' - `censoring`: `mode = target_overall|explicit` (+ parameters)
#' - `treatment`: `assignment = randomization|stratified|logistic_ps` (+ parameters)
#'
#' @seealso \\link[=validate_recipe]{validate_recipe}, \\link[=write_recipe_yaml]{write_recipe_yaml}
#' @examples
#' set.seed(1)
#' rec <- list(
#'   n = 200,
#'   covariates = list(defs = list(
#'     list(name="age", type="continuous", dist="normal", params=list(mean=62, sd=10),
#'          transform=c("center(60)","scale(10)")),
#'     list(name="gender", type="categorical", dist="bernoulli", params=list(p=0.45))
#'   )),
#'   event_time = list(
#'     model="aft_lognormal",
#'     baseline=list(mu=3.0, sigma=0.7),
#'     effects=list(treatment=-0.25, covariates=list(age=0.01, gender=-0.10)),
#'     tau=24
#'   ),
#'   censoring = list(mode="target_overall", target=0.25, admin_time=30),
#'   treatment = list(assignment="randomization", allocation="1:1")
#' )
#' dat <- simulate_from_recipe(rec)
#' attr(dat, "tau")
#' attr(dat, "achieved_censoring")
#' head(dat)
#'
#' \dontrun{
#' # YAML round-trip (requires Suggests: yaml)
#' tmp <- tempfile(fileext = ".yml")
#' write_recipe_yaml(rec, tmp)
#' dat2 <- simulate_from_recipe(tmp)
#' }
#'
#' @importFrom stats rnorm rlnorm runif rweibull rgamma rbinom plogis rexp model.frame
#' @importFrom stats model.matrix uniroot
#' @export
simulate_from_recipe <- function(recipe, n_override = NULL, seed = NULL) {
  r <- validate_recipe(recipe)
  if (!is.null(seed)) {
    set.seed(seed)
  } else if (!is.null(r$seed)) {
    set.seed(r$seed)
  }
  n <- n_override %||% r$n
  X <- gen_covariates(n, r$covariates)
  A <- assign_treatment(n, r$treatment, X)
  T_event <- gen_event_times(r$event_time, A, X)
  C_time <- gen_censoring(r$censoring, T_event, X)
  time   <- pmin(T_event, C_time)
  status <- as.integer(T_event <= C_time)
  out <- data.frame(time = as.numeric(time),
                    status = status,
                    arm = as.integer(A),
                    X,
                    check.names = TRUE)
  attr(out, "tau") <- r$event_time$tau
  attr(out, "achieved_censoring") <- mean(status == 0)
  out
}

#' Write a recipe to YAML
#'
#' Convenience wrapper to serialize a recipe list to a YAML file.
#'
#' @param recipe A recipe list (see \\link[=simulate_from_recipe]{simulate_from_recipe} examples).
#' @param path File path ending in `.yml` or `.yaml`.
#' @return Invisibly returns `path`.
#' @examples
#' \dontrun{
#' rec <- list(n=10, covariates=list(defs=list(
#'   list(name="x", type="continuous", dist="normal", params=list(mean=0, sd=1))
#' )), event_time=list(model="cox_exp", baseline=list(rate=0.05),
#'     effects=list(treatment=-0.3, covariates=list()), tau=12),
#' censoring=list(mode="target_overall", target=0.2, admin_time=18),
#' treatment=list(assignment="randomization", allocation="1:1"))
#' write_recipe_yaml(rec, tempfile(fileext=".yml"))
#' }
#' @export
write_recipe_yaml <- function(recipe, path) {
  .require("yaml")
  yaml::write_yaml(recipe, path)
  invisible(path)
}
