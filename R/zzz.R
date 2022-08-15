.onAttach <- function(...) {
  if(requireNamespace("gagglr", quietly = TRUE))
    gagglr::oha_check("gisr", suppress_success = TRUE)
}
