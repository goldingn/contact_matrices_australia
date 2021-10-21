#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param df
#' @param label
#' @param dir
#' @param type
#' @param width
#' @param height
#' @param dpi
#' @param units
#' @param vacc_from
#' @param fade
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
  units = "mm",
  vacc_from = "twelve",
  fade = FALSE,
  wfh = FALSE
) {

  for (ttiq_plot in c("partial", "optimal")) {
    
    dancing_boxplot(
      df = df,
      ttiq_plot = ttiq_plot,
      vacc_from = vacc_from,
      fade = fade,
      wfh = wfh
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
