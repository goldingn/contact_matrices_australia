#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param p
#' @param r0
#' @param end
#' @param at
#' @param size
#' @param colour
#' @return
#' @author Nick Golding
#' @export
add_arrow <- function(
  p,
  r0,
  end = 1.05,
  at = 0.35,
  size = 3,
  colour = grey(0.8)
) {
  
  r0 <- enquo(r0)
  p +
    geom_segment(
      aes(
        x = at,
        xend = at,
        y = max(!!r0),
        yend = end
      ),
      data = p$data,
      size = size,
      linejoin = "mitre",
      colour = colour,
      arrow = arrow(
        type = "closed",
        length = unit(7.5, "point")
      )
    )
}
