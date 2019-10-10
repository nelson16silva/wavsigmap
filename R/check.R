# #' Check Dots (...)
# #'
# #' @param ... arguments to pass through dots
# #'
# #'
check_dot_args <- function(...) {
  args_names <- names(rlang::enexprs(...))

  # named argument check

  if (length(args_names) > 0) {
    if (any(args_names == "")) {
      stop("unnamed argument: all argument passed on ... must be a named one.")
    }
  }

  # definitions for unkown and incompatible checks

  wshrformals <- names(formals(wmtsa::wavShrink))
  any_wshr <- any(args_names %in% wshrformals)

  wdformals <- names(formals(wavethresh::wd))
  thrformals <- c(names(formals(wavethresh::threshold.wd)), "j0")
  all_wthr <- c(wdformals, thrformals)
  any_wthr <- any(args_names %in% all_wthr)
  any_wthr_less_boundary <- any(args_names %in% all_wthr[!all_wthr == "boundary"])

  dwtformals <- names(formals(waveslim::dwt))
  ebthrformals <- names(formals(EbayesThresh::ebayesthresh.wavelet))
  all_ebthr <- c(dwtformals, ebthrformals)
  any_ebthr <- any(args_names %in% all_ebthr)
  any_ebthr_less_boundary <- any(args_names %in% all_ebthr[!all_ebthr == "boundary"])

  all_formals <- c(wshrformals, wdformals, thrformals, dwtformals, ebthrformals)

  # Unkown argument check

  if (length(args_names) > 0) {
    if (!all(args_names %in% all_formals)) {
      unkown_args <- paste(unique(args_names[!(args_names %in% all_formals)]), collapse = ", ")
      stop_unkwon_messsage <- paste0("unknown argument(s): ", unkown_args)
      stop(stop_unkwon_messsage)
    }
  }

  # Incompatible arguments check
  if (length(args_names) > 1) {
    if ((any_wshr & any_wthr) || (any_wshr & any_ebthr)) {
      stop("incompatible arguments")
    }
    if (any_wthr_less_boundary & any_ebthr_less_boundary) {
      stop("incompatible arguments")
    }
  }
}
