

#' Setup the theme for plots
#'
#' @param base_size
#' @param base_family
#'
#' @return
#' @export
#'
#' @examples
#'
#'
#'
my_theme <- function(base_size = 12, base_family = "Chivo") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(


      # colors for the background/panel elements
      plot.background = element_rect(fill = "#f0f0f0", color = NA),
      panel.background = element_rect(fill = "#f0f0f0", color = NA),
      panel.grid.major =  element_blank(),
      panel.border = element_blank(),


      # drop minor gridlines and axis-ticks
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      # change font elements/family

      if(utils::packageVersion("ggplot2") >= package_version("3.4.0")){
      axis.line = element_line(linewidth = 0.5)} else {
        axis.line = element_line(size = 0.5)
      },
      text = element_text(family = "Chivo", size = base_size),
      axis.text = element_text(face = "bold", color = "grey", size = base_size),
      axis.title = element_text(face = "bold", size = rel(1.33)),
      axis.title.x = element_text(margin = margin(0.5, 0, 0, 0, unit = "cm")),
      axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, unit = "cm"), angle =90),
      plot.title = element_text(face = "bold", size = rel(1.67), hjust = 0),
      plot.title.position = "plot",
      plot.subtitle = element_text(size = 16, margin = margin(0.2, 0, 1, 0, unit = "cm"), hjust = 0),
      plot.caption = element_text(size = 10, margin = margin(1, 0, 0, 0, unit = "cm"), hjust = 1),



      # shrinks margin and simplify the strip/facet elements
      plot.margin = margin(0.5, 1, 0.5, 1, unit = "cm"),
      strip.background = element_blank(),
      strip.text = element_text(size = rel(1.33), face = "bold")
    )
}
