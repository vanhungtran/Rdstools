library(nnet)
# install.packages("gtsummary")
library(gtsummary)
#> #BlackLivesMatter
library(tidyverse)
library(magrittr)
#simulated data

load("C:/PROJECTS1/PROG/Rdstools/data/ST.RData")


ST <- ST %>% dplyr::select("Gender", "Agegroup", "BMIgroup", "Education", "Smoking", "SCORADSeverity")


multinom_pivot_wider <- function(x) {
  # check inputs match expectatations
  if (!inherits(x, "tbl_regression") &
      !inherits(x$model_obj, "multinom") & !inherits(x, "tbl_stack")) {
    stop("`x=` must be class 'tbl_regression' summary of a `nnet::multinom()` model.")
  }

  # create tibble of results
  df <-
    tibble::tibble(outcome_lev = unique(x$table_body$groupname_col))
  df$tbl <-
    purrr::map(df$outcome_lev,
               function(k) {
                 gtsummary::modify_table_body(
                   x,
                   ~ dplyr::filter(.x, groupname_col %in% k) %>%
                     dplyr::ungroup() %>%
                     dplyr::select(-groupname_col)
                 )
               })

  tbl_merge(df$tbl, tab_spanner = paste0("**", df$outcome_lev, "**"))
}



ivars = c("Gender", "Agegroup", "BMIgroup", "Education", "Smoking")
formulae <- lapply(ivars, function(x) reformulate(x, response="SCORADSeverity"))
test <- lapply(formulae, function(x) tbl_regression(do.call("multinom", list(x, quote(ST))), exponentiate = TRUE))
Tab_uni <- tbl_stack(test) %>%
  multinom_pivot_wider()



mod <- nnet::multinom(reformulate(ivars, response="SCORADSeverity"), data = ST)


for (j in (1:5)){
  paste0(c("estimate_", "ci_", "p.value_"), j)
}


Tab_uni <- Mod6 %>%
  multinom_pivot_wider() %>%
  modify_spanning_header(
    paste0(c("estimate_", "ci_", "p.value_"), 1) ~
      "**moderate/mild**",
    paste0(c("estimate_", "ci_", "p.value_"), 2) ~
      "**severe/mild**"


  )




Tab_mul = mod %>% tbl_regression(exponentiate = TRUE) %>%
  multinom_pivot_wider() |>
  modify_spanning_header(
    paste0(c("estimate_", "ci_", "p.value_"), 1) ~
      "**moderate/mild**",
    paste0(c("estimate_", "ci_", "p.value_"), 2) ~
      "**severe/mild**"
  )



TAB  <-
  tbl_merge(tbls = list(Tab_uni, Tab_mul)) |>

  modify_spanning_header(
    list(
      c(all_stat_cols(), estimate_1_1, ci_1_1, p.value_1_1) ~ "**moderate/mild**",
      c(all_stat_cols(), estimate_2_1, ci_2_1, p.value_2_1) ~ "**severe/mild**",
      c(all_stat_cols(), estimate_1_2, ci_1_2, p.value_1_2) ~ "**moderate/mild**",
      c(all_stat_cols(), estimate_2_2, ci_2_2, p.value_2_2) ~ "**severe/mild**"
    )
  ) %>%   as_gt() |>
  gt::tab_spanner(
    label =  gt::md("**Univariate**"),
    columns = ends_with("_1") ,
    level = 2
  ) |>
  gt::tab_spanner(
    label =  gt::md("**Multivariate**"),
    columns = ends_with("_2"),
    level = 2
  )














