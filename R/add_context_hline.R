#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param p
#' @param at
#' @param label
#' @param colour
#' @param linetype
#' @param text_size
#' @return
#' @author Nick Golding
#' @export
add_context_hline <- function(p, at, label,
                              colour = grey(0),
                              linetype = 2,
                              text_size = 2.5) {
  p +
    geom_hline(
      yintercept = at,
      col = colour,
      linetype = linetype
    ) +
    annotate(
      "text",
      label = label,
      x = -0.5,
      y = at,
      hjust = 0,
      vjust = -0.5,
      col = colour,
      size = text_size
    )
}
