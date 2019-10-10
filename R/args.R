# #' Wavelet Class
# #'
# #' This function defines a class based on the arguments such that
# #' the function for estimating the signal is the appropriated one.
# #'
# #' @param x A numeric vector or a time series.
# #' @param wt NULL or a character ("dwt" or "modwt") for defining the the wavelet method decompostion
# #' for empirical Bayes thresholding. Default is NULL, implying that
# #' \code{dwt} is used if the signal is estimated from empirical Bayes thresholding.
# #' @param ... named additional parameters.
# #'
# #' @return A vector or a time series. If user intend to use a method
# #' from package \code{wmtsa} nothing is done and the function return the
# #' same data passed to the function. However, if the signal estimation
# #' is from package \code{EbayesThresh} or \code{wavethresh}, the function
# #' return an object of class "ebthr_dwt", "ebthr_modwt" or "wthr".
# #' @export
# #' @seealso \code{?wmtsa::wavShrink, ?Ebayesthresh::EbayesThresh::ebayesthresh.wavelet,
# #' ?wavethresh::threshold}
# #' @examples
# #' # Default (from wmtsa package)
# #' wav_args(GDPC1)
# #' # EbayesThresh and waveslim
# #' wav_args(GDPC1, wt = "modwt", a = NA)
# #' # wavethresh
# #' wav_args(GDPC1, filter.number = 4)
wav_args <- function(x, wt = NULL, ...) {

  # check dots

  check_dot_args(...)

  # check x

  if (!(is(x, "numeric") || is(x, "ts"))) {
    rlang::abort(
      paste0("x must be a vector of class ts or numeric; not ", class(x), ".")
    )
  }

  # check wt

  if (!is.null(wt)) {
    if (!(wt %in% c("dwt", "modwt"))) {
      stop("wt arg must be NULL, dwt or modwt.")
    }
  }

  add_args <- list(...)

  # check boundary and boundary treatment

  if (!length(add_args) == 0) {
    if (!is.null(add_args$boundary)) {
      if (length(add_args) == 1) {
        if (!add_args$boundary %in% c("periodic", "reflection", FALSE, TRUE)) {
          stop("Unknown boundary condition")
        }
        if (!is.null(wt)) {
          if (wt == "dwt") {
            attr(x, "class") <- "ebthr_dwt"
          } else {
            attr(x, "class") <- "ebthr_modwt"
          }
        } else if (add_args$boundary %in% c("periodic", "reflection")) {
          attr(x, "class") <- "ebthr_dwt"
        }
        else {
          attr(x, "class") <- "wthr"
        }
      }
      add_args$boundary <- NULL
    }
  }

  wshrformals <- names(formals(wmtsa::wavShrink))

  wdformals <- names(formals(wavethresh::wd))
  thrformals <- names(formals(wavethresh::threshold.wd))

  dwtformals <- names(formals(waveslim::dwt))
  ebthrformals <- names(formals(EbayesThresh::ebayesthresh.wavelet))

  any_wshr <- any(names(add_args) %in% wshrformals)
  any_wthr <- any(names(add_args) %in% c(wdformals, thrformals))
  any_ebthr <- any(names(add_args) %in% c(dwtformals, ebthrformals))

  if (!is.null(wt) & (any_wshr || any_wthr)) stop("incompatible arguments to pass")

  if (!is.null(wt)) {
    if (wt == "dwt") {
      attr(x, "class") <- "ebthr_dwt"
    } else {
      attr(x, "class") <- "ebthr_modwt"
    }
  } else {
    if (any(names(add_args) %in% c(wdformals, thrformals))) {
      attr(x, "class") <- "wthr"
    }
    if (any(names(add_args) %in% c(dwtformals, ebthrformals))) {
      if (is.null(wt)) {
        attr(x, "class") <- "ebthr_dwt"
      } else {
        if (wt == "dwt") {
          attr(x, "class") <- "ebthr_dwt"
        } else {
          attr(x, "class") <- "ebthr_modwt"
        }
      }
    }
  }
  x
}
