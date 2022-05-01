#' get_context 
#'
#' @description gets the context around a word for each word in a vector of words
#'
#' @return a vector of contexts
#'
#' @noRd
get_context <- function(words, window_width = 100) {
  res <- character(length = length(words))
  for (i in 1:length(words)) {
    max_indx <- i + window_width
    min_indx <- i - window_width
    if (min_indx < 1) min_indx <- 1
    if (max_indx > length(words)) max_indx <- length(words)
    res[i] <- stringr::str_c(words[min_indx:max_indx], collapse = " ")
  }
  res
}
