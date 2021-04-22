#' Map Wavelet Arguments
#'
#' Get a tibble combining all arguments to be used in wavelet-based
#' signal estimation. This tibble is the input of the function
#' \code{\link{map_wav_signal}}. Then, this function is a preparation
#' for using one of the two main functions of the package: \code{\link{map_wav_signal}}.
#'
#' @param list A named list with arguments for wavelet-based signal
#' estimantion. Each desired parameter to be combined with others
#' must be allocated in a vector with two exception: \strong{\code{type}}
#'  and \strong{\code{levels}} used in \strong{\code{wavethresh}} package should
#'  entry in a \strong{list}, as showed in the example bellow. Default spectifications
#' of the the original functions (see details section on help for
#' \code{\link{signal}}) are used for parameters note
#' included in the list.
#'
#' @return A tibble
#' @export
#' @seealso \code{\link{map_wav_signal}, \link{signal}}
#' @examples
#' # Some arguments used in wmtsa::wavShrink
#'
#' wshr_wmtsa <- list(
#' wavelet = c("haar", "d4", "d6", "s8"),
#' n.level = 1:3,
#' shrink.fun = c("hard", "soft", "mid"),
#' thresh.fun = c("universal", "minimax", "adaptive"),
#' xform = c("dwt", "modwt"),
#' reflect = TRUE
#' )
#'
#' map_wav_args(wshr_wmtsa)
#'
#' # Some arguments used in waveslim::dwt (or modwt) and
#' # EbayesThresh::ebayesthresh.wavelet
#'
#' ebthr_wt <- list(
#' wf = c("haar", "d4", "d6", "fk6", "d8", "fk8", "la8"),
#' n.levels = 4:6,
#' boundary = c("periodic", "reflection"),
#' wt = c("dwt", "modwt")
#' )
#'
#' ebthr_ebwav <- list(
#' vscale = c("level", "independet"),
#' prior = c("laplace", "cauchy"),
#' a = seq(0.1 , 4, by = 0.4),
#' threshrule = c("median", "mean", "soft", "hard")
#' )
#'
#' map_wav_args(c(ebthr_wt, ebthr_ebwav))
#'
#' # Some arguments of wavethresh::wd and wavethresh::threshold. Note
#' # the special way of writing arguments for type and levels.
#'
#' wthr_list <- list(
# wd list
#' filter.number = 1:4,
#' family = c("DaubLeAsymm", "DaubExPhase"),
#' bc = c("periodic", "symmetric"),
#' #   type for wd and threshold
#' type = list(c("wavelet", "soft"), c("wavelet", "hard"),
#'             c("station", "soft"), c("station", "hard")),
#' # threshold list
#' policy = c(
#'   "universal",
#'   "sure",
#'   "cv",
#'   "BayesThresh"
#' ),
#' levels = list(3:8, 4:8, 5:8),
#' alpha = seq(0, 2, by = 0.5),
#' beta = seq(0, 3, by = 0.2),
#' by.level = TRUE,
#' boundary = c(TRUE, FALSE)
#' )
#'
#' map_wav_args(wthr_list)
#'
map_wav_args <- function(list) {

  if (!is.list(list)) stop("argument must be a named list.")
  if (!length(list) > 0) stop("length of the list must be greather than zero.")

  # dimensions

  list_len <- length(list)
  list_elem_len <- sapply(list, length)
  tbl_rows <- prod(list_elem_len)

  # repetitions

  if (list_len > 2) {
    # first element
    list[[1]] <- rep(list[[1]], each = tbl_rows / list_elem_len[1])
    # between first and last elements
    for(i in ((list_len - 1):2)) {
      each_rep = prod(list_elem_len[(i + 1):list_len])
      list[[i]] <- rep(rep(list[[i]],
                           each = each_rep),
                       times = tbl_rows / (each_rep * list_elem_len[i]) )
    }
    # last element
    list[[list_len]] <- rep(list[[list_len]],
                            times = tbl_rows / length(list[[list_len]]))
  } else if (list_len == 2) {
    list[[1]] <- rep(list[[1]], each = tbl_rows / list_elem_len[1])
    list[[2]] <- rep(list[[2]], times = tbl_rows / length(list[[list_len]]))
  }

  tibble::as_tibble(list)
}


#' Map Wavelet Signal
#'
#' This functional computes wavelet-based signal by mapping diferent arguments.
#'
#'
#' @param x A time series or a numeric vector.
#' @param args A tibble get from the function \code{\link{map_wav_args}}.
#'
#' @return A tibble. The respective estimated signal is presented in a
#' specific column of the tibble. Each row of this column has a possibly
#' wavelet-based signal estimated in a list form. Examples bellow explain
#' how to extract the time series of a given wavelet model.
#' @export
#' @seealso \code{\link{map_wav_args}, \link{signal}}
#'
#' @examples
#' library(magrittr)
#' library(rlang)
#' library(purrr)
#' library(ggplot2)
#' library(tidyr)
#' library(tibble)
#'
#' # EbayesThresh::ebayesthresh.wavelet
#'
#' ebthr_wt <- list(
#' wf = c("haar", "la8"),
#' n.levels = 4:5,
#' boundary = "reflection",
#' wt = "modwt"
#' )

#' ebthr_ebwav <- list(
#' vscale = "level",
#' a = c(0.5, 1),
#' threshrule = c("median", "soft")
#' )
#'
#' args_ebthr <- map_wav_args(c(ebthr_wt, ebthr_ebwav))
#'
#' GDP_wavelet <- map_wav_signal(GDPC1, args_ebthr)
#'
#' # Output gap estimation from wavelets and rmse in relation
#' # to GDPC1_GDPPOT.
#'
#' GDP_gap_wav <- GDP_wavelet %>%
#' dplyr::mutate(gap = purrr::map(GDPC1_signal, ~ 100 * (GDPC1 - .x) / .x),
#'              rmse = purrr::map_dbl(gap, ~sqrt(mean((GDPC1_GDPPOT - .x) ^ 2))))
#'
#' # RMSE graph
#'
#' GDP_gap_wav %>%
#'   ggplot(aes(x = wf, y = rmse, color = threshrule)) +
#'   geom_jitter() +
#'   facet_wrap(a ~ n.levels)
#'
#' # The best wavelet model to replicate GDPC1_GDPPOT
#'
#' gap_wav_best <- GDP_gap_wav[which.min(GDP_gap_wav$rmse), ]
#'
#' # Graph of GDP_gap_wav and GDPC1_GDPPOT
#' gap_wav_best %>%
#' `[`(1 ,) %>%
#'   add_column(date = list(time(GDPC1)), GDPC1_GDPPOT = list(GDPC1_GDPPOT)) %>%
#'   unnest(date, gap, GDPC1_GDPPOT) %>%
#'   ggplot(aes(x = date, y = gap, color = "Wavelet")) +
#'   geom_line() +
#'   geom_line(aes(y = GDPC1_GDPPOT, color = "GDPC1_GDPPOT")) +
#'   xlab("") +
#'   ylab("%") +
#'   labs(colour = "Output GAP")
#'
#' # Find a best "a" prior for Empirical EbayesThresh to
#' # replicate GDPC1_GDPPOT:
#'
#' best_a_prior <- function(x, ...) {
#' gdpw <- wavsigmap::signal(GDPC1, a = x, ...)
#' gapw <- 100 * (GDPC1 - gdpw) / gdpw
#' sqrt(mean((GDPC1_GDPPOT - gapw) ^ 2))
#' }
#'
#' args_ebthr2 <- args_ebthr %>%
#' dplyr::select(-a) %>%
#' dplyr::mutate(
#' a = pmap_dbl(., ~optimise(best_a_prior, c(0.01, 3), ...)$minimum))
#'
#' # wavethresh example
#'
#' # Find the best alpha and beta priors to replicate GDPC1_GDPPOT
#'
#' best_alpha_beta <- function(x, ...) {
#' gdpw <- wavsigmap::signal(GDPC1, alpha = x[1], beta = x[2], ...)
#' gapw <- 100 * (GDPC1 - gdpw) / gdpw
#' sqrt(mean((GDPC1_GDPPOT - gapw) ^ 2))
#' }
#'
#' wthr_list <- list(
#' # wd list
#' filter.number = 4:5,
#' bc = c("periodic", "symmetric"),
#' #   type for wd and threshold
#' type = list(c("wavelet", "soft"), c("station", "hard")),
#' # threshold list
#' policy = "BayesThresh",
#' by.level = TRUE
#' )
#'
#' wthr_args <- map_wav_args(wthr_list)
#'
#' # Including the best priors
#'
#' wthr_args2 <- wthr_args %>%
#' dplyr::mutate(alpha_beta = pmap(., ~possibly(optim, NULL)(
#'   c(0.5, 1), best_alpha_beta, lower = c(0, 0), upper = c(3, 3),
#'   method = "L-BFGS-B", ...)$par),
#'   alpha = map(alpha_beta, ~`[`(.x, 1)),
#'   beta = map(alpha_beta, ~`[`(.x, 2))
#' )
#'
#' GDP_wavelet2 <- wthr_args2 %>%
#' dplyr::select(-alpha_beta) %>%
#'   map_wav_signal(x = GDPC1, args = .)
#'
#' GDP_gap_wav2 <- GDP_wavelet2 %>%
#' dplyr::mutate(gap = purrr::map(GDPC1_signal, ~ 100 * (GDPC1 - .x) / .x),
#'               rmse = purrr::map(gap, ~sqrt(mean((GDPC1_GDPPOT - .x) ^ 2))))
#'
#' # The best model to replicate GDPC1_GDPPOT
#'
#' gap_wav_best2 <- GDP_gap_wav2[which.min(GDP_gap_wav2$rmse), ]
#'
#' # Graph of GDP_gap_wav and GDPC1_GDPPOT
#'
#' gap_wav_best2 %>%
#'   `[`(1 ,) %>%
#'   add_column(date = list(time(GDPC1)), GDPC1_GDPPOT = list(GDPC1_GDPPOT)) %>%
#'   unnest(date, gap, GDPC1_GDPPOT) %>%
#'   ggplot(aes(x = date, y = gap, color = "Wavelet")) +
#'   geom_line() +
#'   geom_line(aes(y = GDPC1_GDPPOT, color = "GDPC1_GDPPOT")) +
#'   xlab("") +
#'   ylab("%") +
#'   labs(colour = "Output GAP")
#'
map_wav_signal <- function(x, args) {
  x_name <- rlang::enexpr(x)
  if (!is.name(x_name)) {
    x_name <- "x"
  }
  x_smooth_name <- rlang::expr(!!paste0(rlang::expr(!!x_name), "_signal"))
  wsmooth_all <- purrr::pmap(args, function(...) {
    args_list <- list(...)
    args_list$x <- x
    do.call(purrr::possibly(signal, NULL), args_list)
  })
  wsmooth <- wsmooth_all[!sapply(wsmooth_all, is.null)]
  args_wsmooth <- args[!sapply(wsmooth_all, is.null), ]
  #attr(wsmooth, "class") <- "wsmoothmap"
  dplyr::as_tibble(args_wsmooth) %>% dplyr::mutate(!!x_smooth_name := wsmooth)
}
