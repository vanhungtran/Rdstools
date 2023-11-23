#' Extract columns containing the parterns
#'
#' @param DF
#' @param list_columns
#' @param keywords
#'
#' @return
#' @export
#'
#' @examples

extract_df <- function(DF, list_columns, keywords){
  tt <-  Reduce("+", list_vector <-
                  lapply(DF[,list_columns], function(x) {
                    grepl(keywords, x, ignore.case = TRUE)
                  }))

  DF1 <- DF[tt >0,]
  return(DF1)

}
