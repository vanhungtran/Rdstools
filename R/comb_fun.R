#' Formulas for biomarker combination
#'
#' @param n
#' @param list_vars
#' @param target
#'
#' @return
#' @export
#'
#' @examples
#' set.seed(15272)
#' parallel::makeCluster(parallel::detectCores())
#' suppressPackageStartupMessages(library(ggplot2))
#' vars <- c("MUC16_2","ENRAGE","tPA","KLRD1_2","FASLG_2","FCRL6","LTBP2","OPG_2","LAG3_2" ,'CDH17',"CEACAM5","TLR3","TLR3","NDRG1","S100A11","CDH17","VIM","GBP2","ESM1","PRCP")
#' N <- list(1,2,3,4,5)
#' target = "y"
#' COMB_L <- comb_fun(n = N, list_vars = vars, target = "y")



comb_fun <- function(n, list_vars, target){

  #N <- 5
  COMB <- sapply(N, function(m) combn(x=list_vars, m))
  COMB2 <- list()
  k= 0


  for(i in seq(COMB)){
    tmp <- COMB[[i]]
    for(j in seq(ncol(tmp))){
      k <- k + 1
      COMB2[[k]] <- formula(paste(target, "~", paste(tmp[,j], collapse=" + ")))
    }
  }



return(invisible(COMB))

}


#
#
# pROC_test <- predicted <- res <- vector(mode="list", length(COMB_L))
# for(i in seq(COMB_L)){
#
#   #for(i in 1:2000){
#
#   res[[i]] <- glm(COMB_L[[i]], family="binomial", data=train)
#   #  print(res[[i]] %>% tab::tabglm()%>% DT::datatable())
#   #use model to predict probability of default
#   predicted[[i]] <- predict(res[[i]], DFVALID, type="response")
#
#   #create confusion matrix
#   #print(DT::datatable(confusionMatrix(test$Endo, predicted)))
#
#   # print(tab_model(res[[i]]))
#
#   #   print(tab_model(res[[i]]))
#
#
#
#
#   par(pty = "s")
#   pROC_test[[i]] <- roc(DFVALID$Endo, predicted[[i]], asp = NA,
#                         smoothed = TRUE, col="#1c61b6", add=FALSE,
#                         # arguments for ci
#                         ci=TRUE, ci.alpha=0.6, stratified=FALSE,
#                         # arguments for plot
#                         plot=FALSE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
#                         print.auc=TRUE, show.thres=TRUE)
#
#
#
#   #title(COMB2[[i]])
#   #print(COMB2[[i]], newpage = F)
#   #sens.ci <- ci.se(pROC_test)
#   #sens.ci
#   #plot(sens.ci, type="shape", col="lightblue")
#   #sp.ci[[i]] <- ci.sp(pROC_test[[i]])
#   #cat('Value of Specificity at a Sensitivity of 80% : ')
#   #print(sp.ci[[i]][9,])
# }



