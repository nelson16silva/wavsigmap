#' Expand Data Length
#'
#' \code{growlen} exapands the length of a time series or a numeric vector
#' by running an auto arima regression for forecasting and backcasting.
#' Extending the sample by inserting forecast can be a simple
#' remedy for the end point problem.
#'
#' @param x A time series or a numeric vector.
#' @param h An integer. Number of periods for expanding the data.
#' @param direction Where to grow the data. Allowed values are
#' \code{one_side} (default) and \code{both}. The \code{one_side}
#' grows the data on the end of the series (forecasting). \code{both}
#' grows on both sides of the series (auto arima backcasting and forecasting).
#'
#' @return A time series or a numeric vector. The length of the returned
#' value is the original length plus \code{h}.
#' @export
#' @importFrom stats ts is.ts time
#'
#' @examples
#' GDPC1_grow <- growlen(GDPC1, h = 4)
#'
#' growlen(GDPC1, h = 8, direction = both)
#'
#' # EbayesThresh
#'
#' GDPC1_grow_wavelet <- signal(GDPC1_grow, boundary = "reflection", n.levels = 5, vscale = "level")
#'
#' gap_wavelet <- 100 * (GDPC1_grow - GDPC1_grow_wavelet) /  GDPC1_grow_wavelet
#'
#' # Comparing gap_wavelet (end point adjust) with the original GDPC1_GDPPOT
#'
#' ts.plot(ts.intersect(GDPC1_GDPPOT, gap_wavelet), col = c(1, 2)); abline(h = 0)
#' legend(1980, 9, legend = c("GDPC1_GDPPOT", "gap_wavelet"), col = c(1, 2), lty = 1)

growlen <- function(x, h, direction = "one_side") {
  exp_direc <- rlang::enexpr(direction)
  if (!any(as.character(exp_direc) %in% c("one_side", "both"))) {
    stop('Argument direction must be "one_side" or "both" (with or without quotes).')
  }
  if (exp_direc == "both") {
    if (h < 2) stop("h must be greather than 1.")
    hback <- h %/% 2
    hforew <- h - hback
    xbackward <- rev(forecast::forecast(forecast::auto.arima(rev(x)), h = hback)$mean)
    xforeward <- forecast::forecast(forecast::auto.arima(x), h = hforew)$mean
    x_exp <- c(xbackward, x, xforeward)
  } else {
    if (h < 1) stop("h must be greather than 0.")
    xforeward <- forecast::forecast(forecast::auto.arima(x), h = h)$mean
    x_exp <- c(x, xforeward)
  }
  if (is.ts(x)) {
    end_time <- time(xforeward)[length(xforeward)]
    freq <- attributes(xforeward)$ts[3]
    x_exp <- ts(x_exp, end = end_time, frequency = freq)
  }
  x_exp
}

