#' Generate AUC table from matrix of biomarkers and outcome
#'
#' @param X
#' @param y
#' @param round.digit
#' @param rank
#'
#' @return
#' @export
#'
#' @examples
tableROC <- function(X,
                     y,
                     round.digit = 4,
                     rank = FALSE) {

  X1 <- dplyr::select_if(X, is.numeric)
  roc1 <- list()
  sum.roc <- list()

  for (i in colnames(X1)) {
    roc1[[i]] <- pROC::roc(y, X1[, i, drop = TRUE], warning = FALSE, message = FALSE)
    sum.roc[[i]] <- summary(pROC::ci.auc(roc1[[i]]))
  }

  table_out <- round(do.call(rbind, sum.roc),  round.digit)
  Biomarker <- rownames(table_out)
  table_out <- cbind.data.frame(Biomarker, table_out)

  if (rank) {
    table_out = table_out %>% as.tibble() %>% arrange(desc(Mean)) %>% mutate(N = 1:dim(table_out)[1])
     }

  return(table_out)

}




test <- PLASMA %>% filter(Index == 6)



