
#' Replace strings containing patterns with new strings
#'
#' @param key_word
#' @param replace_key
#'
#' @return
#' @export
#'
#' @examples


return_f <- function(key_word,replace_key) {
  ifelse(Reduce("+", list_vector <- lapply(DF[,list_cols_D], function(x) {
    grepl(key_word, x, ignore.case = TRUE)
  })) >0, replace_key, FALSE)}

