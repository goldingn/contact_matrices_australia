#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param p
#' @param multiplier
#' @param fade_colours
#' @return
#' @author geryan
#' @export
add_fading_box <- function(p, multiplier, fade_colours) {
  
  p <- p %>%
    add_stacked_box(
      top = tp_coverage_0.8*multiplier[1],
      bottom = tp_coverage_0.8*multiplier[2],
      box_colour = fade_colours[1],
      border_colour = fade_colours[1]
    ) %>%
    add_stacked_box(
      top = tp_coverage_0.8*multiplier[2],
      bottom = tp_coverage_0.8*multiplier[3],
      box_colour = fade_colours[2],
      border_colour = fade_colours[2]
    ) %>%
    add_stacked_box(
      top = tp_coverage_0.8*multiplier[3],
      bottom = tp_coverage_0.8*multiplier[4],
      box_colour = fade_colours[3],
      border_colour = fade_colours[3]
    ) %>%
    add_stacked_box(
      top = tp_coverage_0.8*multiplier[4],
      bottom = tp_coverage_0.8*multiplier[5],
      box_colour = fade_colours[4],
      border_colour = fade_colours[4]
    ) %>%
    add_stacked_box(
      top = tp_coverage_0.8*multiplier[5],
      bottom = tp_coverage_0.8*multiplier[6],
      box_colour = fade_colours[5],
      border_colour = fade_colours[5]
    ) %>%
    add_stacked_box(
      top =    tp_coverage_0.8*multiplier[6],
      bottom = tp_coverage_0.8*multiplier[7],
      reference = tp_baseline_vacc,
      box_colour           = fade_colours[6],
      border_colour        = fade_colours[6]
    ) %>%
    add_stacked_box(
      top =    tp_coverage_0.8*multiplier[7],
      bottom = tp_coverage_0.8*multiplier[8],
      reference = tp_baseline_vacc,
      box_colour           = fade_colours[7],
      border_colour        = fade_colours[7]
    ) %>%
    add_stacked_box(
      top =    tp_coverage_0.8*multiplier[8],
      bottom = tp_coverage_0.8*multiplier[9],
      reference = tp_baseline_vacc,
      box_colour           = fade_colours[8],
      border_colour        = fade_colours[8]
    ) %>%
    add_stacked_box(
      top =    tp_coverage_0.8*multiplier[9],
      bottom = tp_coverage_0.8*multiplier[10],
      reference = tp_baseline_vacc,
      box_colour           = fade_colours[9],
      border_colour        = fade_colours[9]
    ) %>%
    add_stacked_box(
      top =    tp_coverage_0.8*multiplier[10],
      bottom = tp_coverage_0.8*multiplier[11],
      reference = tp_baseline_vacc,
      box_colour           = fade_colours[10],
      border_colour        = fade_colours[10]
    ) %>%
    add_stacked_box(
      top =    tp_coverage_0.8*multiplier[11],
      bottom = tp_coverage_0.8*multiplier[12],
      reference = tp_baseline_vacc,
      box_colour           = fade_colours[11],
      border_colour        = fade_colours[11]
    ) %>%
    add_stacked_box(
      top =    tp_coverage_0.8*multiplier[12],
      bottom = tp_coverage_0.8*multiplier[13],
      reference = tp_baseline_vacc,
      box_colour           = fade_colours[12],
      border_colour        = fade_colours[12]
    ) %>%
    add_stacked_box(
      top =    tp_coverage_0.8*multiplier[13],
      bottom = tp_coverage_0.8*multiplier[14],
      reference = tp_baseline_vacc,
      box_colour           = fade_colours[13],
      border_colour        = fade_colours[13]
    ) %>%
    add_stacked_box(
      top =    tp_coverage_0.8*multiplier[14],
      bottom = tp_coverage_0.8*multiplier[15],
      reference = tp_baseline_vacc,
      box_colour           = fade_colours[14],
      border_colour        = fade_colours[14]
    ) %>%
    add_stacked_box(
      top =    tp_coverage_0.8*multiplier[15],
      bottom = tp_coverage_0.8*multiplier[16],
      reference = tp_baseline_vacc,
      box_colour           = fade_colours[15],
      border_colour        = fade_colours[15]
    ) %>%
    add_stacked_box(
      top =    tp_coverage_0.8*multiplier[16],
      bottom = tp_coverage_0.8*multiplier[17],
      reference = tp_baseline_vacc,
      box_colour           = fade_colours[16],
      border_colour        = fade_colours[16]
    ) %>%
    add_stacked_box(
      top =    tp_coverage_0.8*multiplier[17],
      bottom = tp_coverage_0.8*multiplier[18],
      reference = tp_baseline_vacc,
      box_colour           = fade_colours[17],
      border_colour        = fade_colours[17]
    ) %>%
    add_stacked_box(
      top =    tp_coverage_0.8*multiplier[18],
      bottom = tp_coverage_0.8*multiplier[19],
      reference = tp_baseline_vacc,
      box_colour           = fade_colours[18],
      border_colour        = fade_colours[18]
    ) %>%
    add_stacked_box(
      top =    tp_coverage_0.8*multiplier[19],
      bottom = tp_coverage_0.8*multiplier[20],
      reference = tp_baseline_vacc,
      box_colour           = fade_colours[19],
      border_colour        = fade_colours[19]
    ) %>%
    add_stacked_box(
      top =    tp_coverage_0.8*multiplier[20],
      bottom = tp_coverage_0.8*multiplier[21],
      reference = tp_baseline_vacc,
      box_colour           = fade_colours[20],
      border_colour        = fade_colours[20]
    ) %>%
    add_stacked_box(
      top =    tp_coverage_0.8*multiplier[21],
      bottom = tp_coverage_0.8*multiplier[22],
      reference = tp_baseline_vacc,
      box_colour           = fade_colours[21],
      border_colour        = fade_colours[21]
    ) %>%
    add_stacked_box(
      top =    tp_coverage_0.8*multiplier[22],
      bottom = tp_coverage_0.8*multiplier[23],
      reference = tp_baseline_vacc,
      box_colour           = fade_colours[22],
      border_colour        = fade_colours[22]
    ) %>%
    add_stacked_box(
      top =    tp_coverage_0.8*multiplier[23],
      bottom = tp_coverage_0.8*multiplier[24],
      reference = tp_baseline_vacc,
      box_colour           = fade_colours[23],
      border_colour        = fade_colours[23]
    ) %>%
    add_stacked_box(
      top =    tp_coverage_0.8*multiplier[24],
      bottom = tp_coverage_0.8*multiplier[25],
      reference = tp_baseline_vacc,
      box_colour           = fade_colours[24],
      border_colour        = fade_colours[24]
    ) %>%
    add_stacked_box(
      top =    tp_coverage_0.8*multiplier[25],
      bottom = tp_coverage_0.8*multiplier[26],
      reference = tp_baseline_vacc,
      box_colour           = fade_colours[25],
      border_colour        = fade_colours[25]
    )
  
  
  # tried looping instead of this horrendous long list but can't get it to work
  # just moves single box down and down and down
  
  # for(i in 1:25){
  #   p <- p %>%
  #     add_stacked_box(
  #       top = tp_coverage_0.8*multiplier[i],
  #       bottom = tp_coverage_0.8*multiplier[i+1],
  #       box_colour = fade_colours[i],
  #       border_colour = fade_colours[i]
  #     )
  # }
  
  p

}
