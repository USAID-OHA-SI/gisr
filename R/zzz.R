.onAttach <- function(...) {
  if(requireNamespace("gagglr", quietly = TRUE))
    packageStartupMessage(gagglr::oha_check("gisr", suppress_success = TRUE))
}

