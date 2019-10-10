#' wavsigmap: Wavelet-Based Signal Estimation Map.
#'
#' This package integrates in a single function several methods
#' available in R packages for estimating wavelet-based signal.
#' Wavelet shirinkage and threshold methods use a lot
#' parameters. A functional available in this \code{wavsigmap}
#' package combines them and allows one to estimate many models in an
#' easy way. Such approach can help users in the task of choosing the
#' appropriate wavelet model for a given specific target. An example
#' is provided in the context of the output gap estimation.
#'
#' @section Methods available:
#'
#' This package uses methods available in three other packages in R:
#' \itemize{
#' \item wmtsa
#'
#' \item EbayesThresh
#'
#' \item wavethresh
#' }
#'
#' See help page for function \code{\link{signal}} for more details.
#'
#' @references
#' \itemize{
#' \item Bernard W. Silverman, Ludger Evers, Kan Xu, Peter Carbonetto and
#' Matthew Stephens (2017). EbayesThresh: Empirical Bayes Thresholding
#' and Related Methods. R package version 1.4-12.
#' https://CRAN.R-project.org/package=EbayesThresh
#'
#' \item   Brandon Whitcher (2019). waveslim: Basic Wavelet Routines for One-,
#' Two- And Three-Dimensional Signal Processing. R package version
#' 1.7.5.1. \cr https://CRAN.R-project.org/package=waveslim
#'
#' \item   Guy Nason (2016). wavethresh: Wavelets Statistics and Transforms. R
#' package version 4.6.8. https://CRAN.R-project.org/package=wavethresh
#'
#' \item William Constantine and Donald Percival (2017). wmtsa: Wavelet
#' Methods for Time Series Analysis. R package version 2.0-3.
#' https://CRAN.R-project.org/package=wmtsa
#' }
#'
#' @seealso \code{\link{signal}, \link{map_wav_signal}}
#'
## @docType package
## @name wavsigmap
"_PACKAGE"
