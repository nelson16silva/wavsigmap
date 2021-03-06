#' Wavelet-Based Signal Estimation
#'
#' This function is a generic function that estimates a signal by using
#'  several methods of wavelet shirinkage and thresholding available in \code{R}.
#'
#'
#' @param x A time series or a numeric vector. If the data is not of length
#' \eqn{2 ^ J} for some integer \eqn{J}, the function
#' \code{\link[WiSEBoot]{padVector}} of the \code{WiSEBoot} package increases the length of data to
#' achieve the particular length requirement based on reflection type.
#' See the help of the mentioned function for details. After the
#' signal computation, the length is again adjusted for the orginal length.
#' @param wt NULL, "dwt" or "modwt". If EbayesThresh is computed and
#' \code{wt = NULL}, \code{dwt} wavelet transform is the default.
#' @param ... Addtional parameters (see details).
#'
#' @details This function is a wrapper to estimate wavelet-based
#' signal through 3 available packages in R: \code{wmtsa, EbayesThres}
#' and \code{wavethresh}. The advantage of using \code{wavsigmap::signal}
#' is that the way of obtaining the signal is uniform. User just select the
#' parameters and get the signal directly. So, the focus of this function is
#' the signal only, not the wavelet coefficients.
#'
#' Addtional parameters to pass in "..." are described in the
#' following help pages:
#' \itemize{
#' \item \code{\link[wmtsa]{wavShrink}}
#' \item \code{\link[waveslim]{dwt}} (or \code{\link[waveslim]{modwt}})  and \code{\link[EbayesThresh]{ebayesthresh.wavelet}}
#' \item \code{\link[wavethresh]{wd}} and \code{\link[wavethresh]{threshold.wd}}
#' }
#'
#' Parameters should be passed consistently for one of the three options above or an error
#' messsage will be presented. Do not mix paramenters from different methods. If one
#' want to estimate a signal from \code{\link[wmtsa]{wavShrink}} the parameter choice is
#' restricted to that function. For example, \code{n.level = 5} and
#' \code{a = NA} gives an error because \code{n.level} is related to \code{\link[wmtsa]{wavShrink}} and
#' \code{a} to \code{\link[EbayesThresh]{ebayesthresh.wavelet}}. To use this two paramenters, the correct specifation would be
#' \code{n.levels = 5} and \code{a = NA} (note that \code{n.levels} is a parameter of
#' the \code{\link[waveslim]{dwt}}/\code{\link[waveslim]{modwt}}).
#'
#' If any parameter of the \code{\link[waveslim]{dwt}} (or \code{\link[waveslim]{modwt}}) is passed in "...",
#' the \code{signal} function automatically implies \code{\link[EbayesThresh]{ebayesthresh.wavelet}} method
#' for estimating the wavelet signal. For example, if only the
#' wavelet function is passed on "...", the choice could be
#' \code{wf = "haar"} or \code{wavelet = "haar"}. Note that \code{wf} is a paramenter
#' of \code{\link[waveslim]{dwt}} and \code{wavelet} is one of \code{\link[wmtsa]{wavShrink}}. However, the signal estimated
#' for that single choice is differente becase \code{wf} implies \code{\link[EbayesThresh]{ebayesthresh.wavelet}}
#' and \code{wavelet = "haar"} calls \code{\link[wmtsa]{wavShrink}}.
#'
#' The parameter \code{xtr} of the function \code{\link[EbayesThresh]{ebayesthresh.wavelet}} is automatically
#' obtained from \cr
#' \code{wavsigmap::signal}, then it must not be passed. The
#' same observation is true for \code{\link[wavethresh]{wd}} of the function \code{\link[wavethresh]{threshold.wd}}.
#'
#' It is importante to mention one more thing: the parameter \code{type}
#' appears two times in package \code{wavethresh}, in the function
#' \code{\link[wavethresh]{wd}} and \code{\link[wavethresh]{threshold.wd}}. So, as showed in the example bellow,
#' if this parameter should be used in these two functions it needs to be
#' passed as a string vector. For example, \code{type = c("hard", "station")}
#' use "hard" in \code{\link[wavethresh]{threshold.wd}} and "station" in \code{\link[wavethresh]{wd}}. The
#' order is not imporatant and it is not necessary to pass the two
#' \code{type} simultaneously.
#'
#' In summary, based on paramenters passed in "..." this function
#' select the appropriated method to estimate the signal. If nothing
#' exist in "..." and \code{wt = NULL}, the default is the same as in \code{\link[wmtsa]{wavShrink}}.
#'
#' @references
#' \itemize{
#' \item Bernard W. Silverman, Ludger Evers, Kan Xu, Peter Carbonetto and
#' Matthew Stephens (2017). EbayesThresh: Empirical Bayes Thresholding
#' and Related Methods. R package version 1.4-12.
#' https://CRAN.R-project.org/package=EbayesThresh
#'
#' \item William Constantine and Donald Percival (2017). wmtsa: Wavelet
#' Methods for Time Series Analysis. R package version 2.0-3.
#' https://CRAN.R-project.org/package=wmtsa
#'
#' \item   Brandon Whitcher (2019). waveslim: Basic Wavelet Routines for One-,
#' Two- And Three-Dimensional Signal Processing. R package version
#' 1.7.5.1. \cr
#' https://CRAN.R-project.org/package=waveslim
#'
#' \item   Guy Nason (2016). wavethresh: Wavelets Statistics and Transforms. R
#' package version 4.6.8. https://CRAN.R-project.org/package=wavethresh
#' }
#'
#' @return A time series or a vector representing
#' the wavelet-based signal estimation. The length of the signal
#' is the same as the orignal data.
#' @export
#' @seealso \code{\link{map_wav_args}}
#'
#' @examples
#' # wmtsa::wavShrink
#' wavsigmap::signal(GDPC1)
#'
#' # EbayesThresh::ebayesthresh.wavelet
#' wavsigmap::signal(GDPC1 , a = NA)
#'
#' # wavethresh::threshold
#' wavsigmap::signal(GDPC1, boundary = 2,
#'                   filter.number = 10, policy = "cv",
#'                   type = c("hard", "station"))
signal <- function(x, wt = NULL, ...) {
  y <- wav_args(x, wt, ...)
  xdnoise <- wav_signal(y, ...)
  attributes(xdnoise) <- attributes(x)
  xdnoise
}
