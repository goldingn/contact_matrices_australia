#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data
#' @param ylab
#' @return
#' @author Nick Golding
#' @export
control_base_plot <- function(
  data,
  ylab = "Transmission potential"
) {
  data %>%
    ggplot(
      aes(
        x = scenario,
        middle = as.numeric(NA),
        ymin = as.numeric(NA),
        ymax = as.numeric(NA),
        width = 0.6
      )
    ) +
    scale_x_discrete(position = "top") +
    xlab("") +
    scale_y_continuous(
      position = "right",
      breaks = c(0.8, 1, 2, 4, 6, 8, 10),
      trans = 'log'
    ) +
    coord_cartesian(clip = "off") +
    ylab(ylab) +
    control_plot_theme()
}
