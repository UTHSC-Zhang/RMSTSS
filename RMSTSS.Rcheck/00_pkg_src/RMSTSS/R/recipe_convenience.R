# R/recipe_convenience.R
# User-friendly builders & wrappers

#' Quick AFT recipe builder
#'
#' Construct a minimal recipe for an AFT model with log-normal or Weibull
#' baseline, randomization, and target censoring.
#'
#' @param n Sample size.
#' @param tau RMST truncation time.
#' @param model Either `"aft_lognormal"` or `"aft_weibull"`.
#' @param baseline Named list of baseline params. For log-normal: `list(mu, sigma)`;
#'   for Weibull: `list(shape, scale)`.
#' @param treat_effect Numeric treatment coefficient on the linear predictor.
#' @param covariates List of covariate definitions (see examples).
#' @param target_censoring Overall censoring fraction (0–1).
#' @param allocation Randomization allocation string, e.g., `"1:1"`.
#' @param seed Optional RNG seed.
#' @return A recipe list suitable for \\link[=simulate_from_recipe]{simulate_from_recipe}.
#' @examples
#' covs <- list(
#'   list(name="age", type="continuous", dist="normal", params=list(mean=62, sd=10),
#'        transform=c("center(60)","scale(10)")),
#'   list(name="gender", type="categorical", dist="bernoulli", params=list(p=0.45))
#' )
#' rec <- recipe_quick_aft(n=300, tau=24, model="aft_weibull",
#'                         baseline=list(shape=1.3, scale=12),
#'                         treat_effect=-0.25, covariates=covs,
#'                         target_censoring=0.25, allocation="1:1", seed=2025)
#' dat <- simulate_from_recipe(rec)
#' @export
recipe_quick_aft <- function(n, tau, model = c("aft_lognormal", "aft_weibull"),
                             baseline, treat_effect = 0, covariates,
                             target_censoring = 0.25, allocation = "1:1", seed = NULL) {
  model <- match.arg(model)
  rec <- list(
    seed = seed, n = n,
    covariates = list(defs = covariates),
    event_time = list(model = model, baseline = baseline,
                      effects = list(treatment = treat_effect, covariates = list()),
                      tau = tau),
    censoring = list(mode = "target_overall", target = target_censoring,
                     admin_time = max(tau, tau * 1.25)),
    treatment = list(assignment = "randomization", allocation = allocation)
  )
  validate_recipe(rec)
}

#' Build a grid of recipes by varying parameters
#'
#' Takes a base recipe and returns a list of recipes where named
#' fields are varied over supplied vectors (Cartesian product).
#'
#' @param base_recipe A valid recipe list.
#' @param vary Named list; names are dotted paths (e.g., `"n"`, `"censoring.target"`,
#'   `"event_time.effects.treatment"`), values are vectors to grid over.
#' @return A list of recipe lists.
#' @examples
#' base <- recipe_quick_aft(200, 24, "aft_lognormal",
#'   baseline = list(mu=3, sigma=0.7),
#'   treat_effect = -0.2,
#'   covariates = list(list(name="x", type="continuous", dist="normal", params=list(mean=0, sd=1))),
#'   target_censoring = 0.25
#' )
#' gs <- recipe_grid(base, list(n = c(125, 250), "censoring.target" = c(0.2, 0.3)))
#' length(gs)
#' @export
recipe_grid <- function(base_recipe, vary) {
  base_recipe <- validate_recipe(base_recipe)
  keys <- names(vary)
  grid <- expand.grid(vary, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)

  set_in_list <- function(x, parts, value) {
    if (length(parts) == 1L) {
      x[[parts[1]]] <- value
      return(x)
    } else {
      head <- parts[1]
      tail <- parts[-1]
      x[[head]] <- set_in_list(x[[head]] %||% list(), tail, value)
      return(x)
    }
  }

  out <- vector("list", nrow(grid))
  for (i in seq_len(nrow(grid))) {
    r <- base_recipe
    for (k in seq_along(keys)) {
      parts <- strsplit(keys[[k]], ".", fixed = TRUE)[[1]]
      r <- set_in_list(r, parts, grid[[k]][[i]])
    }
    out[[i]] <- validate_recipe(r)
  }
  out
}

#' Simulate from a recipe and run an analysis function
#'
#' A thin convenience wrapper that generates a dataset and immediately
#' passes it to an analysis function (e.g., your RMST estimator).
#'
#' @param recipe A recipe list or path.
#' @param analyze_fn A function with signature like `function(data, tau, ...)`.
#' @param ... Additional arguments forwarded to `analyze_fn`.
#' @param n_override Optional integer, overrides `n` in `recipe`.
#' @param seed Optional RNG seed.
#' @return A list with elements `data` and `result`.
#' @examples
#' \dontrun{
#' # Suppose your package provides `rmst_fit(data, tau, ...)`
#' res <- rmst_from_recipe(
#'   recipe_quick_aft(200, 24, "aft_lognormal",
#'                    baseline = list(mu=3, sigma=0.7),
#'                    treat_effect = -0.2,
#'                    covariates = list(list(name="x", type="continuous",
#'                                           dist="normal", params=list(mean=0, sd=1)))),
#'   analyze_fn = rmst_fit
#' )
#' }
#' @export
rmst_from_recipe <- function(recipe, analyze_fn, ..., n_override = NULL, seed = NULL) {
  dat <- simulate_from_recipe(recipe, n_override = n_override, seed = seed)
  tau <- attr(dat, "tau")
  result <- analyze_fn(dat, tau = tau, ...)
  list(data = dat, result = result)
}
