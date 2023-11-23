#' Anonymized the PatientID
#'
#' @param x
#' @param prefix
#' @param subfix
#' @param seed_num
#'
#' @return
#' @importFrom digest
#' @export
#'
#' @examples
#'
#'
#'
anonymizer <- function(x, prefix, subfix, seed_num)
{

  set.seed(seed_num)
  x1 <-  sample(x)
  x2 <- NULL
  for (i in 1:length(x1)) {
    x2 <- c(x2, paste0(prefix, i, subfix))
  }
  sha <- sapply(unique(x1), digest, algo = "sha1")
  DF <- data.frame(as.vector(x1), x2, sha)
  colnames(DF) <- c("OriginalID", "NewID", "sha")
  writexl::write_xlsx(DF,
                      paste0("ID_Anony_", prefix, "_", Sys.Date(), "_", seed_num,  ".xlsx"))
  return(DF)
}
