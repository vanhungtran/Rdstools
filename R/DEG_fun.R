
#
#
#
#
#
#
#' Multiple contrast DEG
#'
#' @param dds
#' @param list_contrast
#' @param padj
#' @param lfc.cutoff
#'
#' @return
#' @export
#'
#' @examples
#' contrast = c("AD_severity_scorad",  "mild", "healthy")
#' contrast1 = c("AD_severity_scorad",  "moderate", "healthy")
#' contrast2 = c("AD_severity_scorad",  "severe", "healthy")
#' contrast3 = c("AD_severity_scorad",  "moderate", "mild")
#' contrast4 = c("AD_severity_scorad",  "severe", "mild")
#' contrast5 = c("AD_severity_scorad",  "severe", "moderate")
#' list_contrast <- list("contrast", "contrast1", "contrast2", "contrast3", "contrast4", "contrast5")
#' res_tot <-DEG_multi_contrast(dds, list_contrast = list_contrast, lfc.cutoff = log2(1.5))
#' res_tot %>% arrange(expression) %>%
#' mutate_if(is.numeric, round, 3) %>%
#' tableHTML(width = c(30,rep(100, dim(res_tot)[2]))) %>%
#'   add_theme('rshiny-blue') %>%
#'  add_theme('colorize', color = 'turquoise4') %>%
#'  write_tableHTML(file = 'RNA_DEGs_all.html')
#'

DEG_multi_contrast <- function(dds, list_contrast, padj = 0.05, lfc.cutoff){

  RES_TABLE <- list()
  for (i in list_contrast){

    ctrast <- eval(parse(text=i))

    res = results(dds, filterFun=ihw, contrast = ctrast, alpha = 0.05, lfcThreshold = lfc.cutoff)



    png(filename =paste0(ctrast[2],"_vs_" ,ctrast[3],".png"), width = 1000, height = 800)


    library(EnhancedVolcano)
    p <-   EnhancedVolcano(res,
                           lab = rownames(res),
                           x = 'log2FoldChange',
                           y = 'pvalue')
    print(p)

    dev.off()

    res_table <- res %>%
      data.frame() %>%
      rownames_to_column(var="gene") %>%
      arrange(padj)   %>%
      as_tibble()

    res_table$contrast <- paste0(ctrast[2],"_vs_" ,ctrast[3])


    res_table =  res_table %>% filter(padj < 0.05)

    res_table$expression <- ifelse(res_table$log2FoldChange > lfc.cutoff, "UP" , ifelse(res_table$log2FoldChange < - lfc.cutoff, "DOWN", "None"))




    png(filename =paste0(ctrast[2],"_vs_" ,ctrast[3],"_corr.png"), width = 800, height = 800, res = 350)

    p1 <- corrplot::corrplot(cor(t(log2((counts(dds[res_table$gene,], normalized=TRUE, replaced=FALSE)+.5)))),
                             method="circle",
                             #order = "hclust",
                             addrect = 10,
                             tl.col = "black",  insig='blank',
                             sig.level = 0.05)

    #print(p1)
    dev.off()



    RES_TABLE[[i]] <- res_table %>% dplyr::arrange(desc(log2FoldChange))

    #print(res_table %>% filter(padj < 0.05))
  }

  res_out <- do.call("rbind", RES_TABLE)

  return(res_out) %>% invisible()
}




