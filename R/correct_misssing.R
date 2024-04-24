correct_misssing <- function(df, list_col){
  # check list of columns name

  df1 <- df[,list_col] %>% janitor::clean_names()
  imp <- mice::mice(df1, print = FALSE, m =5, maxit = 100, seed = 24415, method = "pmm")
  infor_imp <- complete(imp, 5)
  df[,list_col] <- infor_imp

  return(df)
}
