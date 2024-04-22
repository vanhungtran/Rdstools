format_datatype <- function(df, list_continuous, list_cate){
  df[list_continuous] <- lapply(df[list_continuous], as.numeric)
  df[list_cate] <- lapply(df[list_cate], function (x)
  {
    if (is.factor(x))
      x
    else if (!is.object(x) && is.integer(x)) {
      levels <- sort(unique.default(x))
      f <- match(x, levels)
      levels(f) <- as.character(levels)
      if (!is.null(nx <- names(x)))
        names(f) <- nx
      class(f) <- "factor"
      f
    }
    else factor(x)
  }
  )
  return(df)}
