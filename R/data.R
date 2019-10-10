#' Real Gross Domestic Product
#'
#' Real gross domestic product is the inflation adjusted value of
#' the goods and services produced by labor and property located in the United States.
#'
#' @format An object of class \code{ts}. From 1949Qtr1 to 2019Qtr2.
#' @references U.S. Bureau of Economic Analysis,
#' Real Gross Domestic Product [GDPC1], retrieved from FRED,
#' Federal Reserve Bank of St. Louis;
#' https://fred.stlouisfed.org/series/GDPC1, September 11, 2019.
#' @source \url{https://fred.stlouisfed.org/series/GDPC1}
"GDPC1"

#' Real Potential Gross Domestic Product
#'
#' Real potential GDP is the CBO’s estimate of the output the
#' economy would produce with a high rate of use of its capital
#' and labor resources. The data is adjusted to remove
#' the effects of inflation.
#'
#' @format An object of class \code{ts}. From 1949Qtr1 to 2019Qtr2.
#' @references U.S. Congressional Budget Office, Real Potential
#' Gross Domestic Product [GDPPOT], retrieved from FRED, Federal
#' Reserve Bank of St. Louis;
#' https://fred.stlouisfed.org/series/GDPPOT, September 11, 2019.
#' @source \url{https://fred.stlouisfed.org/series/GDPC1}
"GDPPOT"

#' 100*(Real Gross Domestic Product-Real Potential
#' Gross Domestic Product)/Real Potential Gross Domestic Product
#' @source \url{https://fred.stlouisfed.org/graph/?g=f1cZ}
#' @format An object of class \code{ts}. From 1949Qtr1 to 2019Qtr2.
"GDPC1_GDPPOT"
