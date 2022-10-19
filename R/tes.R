#' Total Error Sum
#'
#' @description Calculate the total error sum. `tes()` is a metric that is in the same units as (the square of) the original data.
#' Total error sum adds up the 'truth' values and the 'predicted' values and squares the difference between the two.
#'
#' @param data a `data.frame` containting the `truth` and `estimate` columns.
#' @param truth The column identifier for the true results (that is `numeric`). This should be an unquoted column name although
#' this argument is passed by expression and supports quasiquotation (you can unquote column names).
#' @param estimate 	The column identifier for the predicted results (that is also `numeric`). As with `truth` this can be s
#' pecified different ways but the primary method is to use an unquoted variable name.
#' @param na_rm A `logical` value indicating whether NA values should be stripped before the computation proceeds.
#' @param event_level 	A single string. Either "`first`" or "`second`" to specify which level of `truth` to consider as
#' the "event". The default uses an internal helper that generally defaults to "`first`", however, if the deprecated
#' global option `yardstick.event_first` is set, that will be used instead with a warning.
#' @param ... not in use
#'
#' @return A tibble with columns .metric, .estimator, and .estimate and 1 row of values.
#' @export
#'
#' @examples
#' \dontrun{
#' tes(data, ...)
#'
#' ## S3 method for class 'data.frame'
#' tes(data, truth, estimate, na_rm = TRUE, ...)
#'
#' tes_vec(truth, estimate, na_rm = TRUE, ...)
#' }
tes <- function(data, ...) {
  UseMethod("tes")
}

tes <- yardstick::new_prob_metric(
  tes,
  direction = "zero"
)

#' @export
#' @rdname tes
tes.data.frame <- function(data,
                           truth,
                           ...,
                           na_rm = TRUE,
                           event_level = yardstick:::yardstick_event_level()) {
  estimate <- yardstick::dots_to_estimate(data, !!! rlang::enquos(...))

  yardstick::metric_summarizer(
    metric_nm = "tes",
    metric_fn = tes_vec,
    data = data,
    truth = !!rlang::enquo(truth),
    estimate = !!estimate,
    na_rm = na_rm,
    event_level = event_level
  )
}

#' @export
#' @rdname tes
tes_vec <- function(truth,
                    estimate,
                    na_rm = TRUE,
                    event_level = yardstick:::yardstick_event_level(),
                    ...) {
  estimator <- yardstick::finalize_estimator(truth, metric_class = "tes")

  # estimate here is a matrix of class prob columns
  tes_impl <- function(truth, estimate) {
    tes_estimator_impl(truth, estimate, estimator, event_level)
  }

  yardstick::metric_vec_template(
    metric_impl = tes_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    estimator = estimator,
    cls = c("factor", "numeric")
  )
}

tes_estimator_impl <- function(truth, estimate, estimator, event_level) {
  if (identical(estimator, "binary")) {
    tes_binary(truth, estimate, event_level)
  }
  else {
    tes_multiclass(truth, estimate)
  }
}

tes_binary <- function(truth, estimate, event_level) {
  if (!identical(event_level, "first")) {
    lvls <- levels(truth)
    truth <- stats::relevel(truth, lvls[[2]])
  }

  tes_multiclass(truth, estimate)
}

tes_multiclass <- function(truth, estimate) {
  y <- sum((as.integer(truth) == 1))
  x <- sum(estimate)

  out <- (y - x)^2

  out
}
