#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param df
#' @param label
#' @param dir
#' @param type
#' @return
#' @author geryan
#' @export
save_dancing_boxplots <- function(
  df,
  label,
  dir = "outputs",
  type = "upright",
  width = 200,
  height = 150,
  dpi = 300,
  units = "mm"
) {

  for (ttiq_plot in c("partial", "optimal")) {
    
    dancing_boxplot(
      df = df,
      ttiq_plot = ttiq_plot
    )
    
    ggsave(
      sprintf(
        "%s/%s_%s.png",
        dir,
        label,
        ttiq_plot
      ),
      bg = "white",
      width = width,
      height = height,
      dpi = dpi,
      units = units
    )
    
  }

}
