#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title National-plan-style boxplot (AKA Golding Plot)
#' @param df
#' @param ttiq
#' @return
#' @author geryan
#' @export
dancing_boxplot <- function(df, ttiq_plot) {
  
  colours <- RColorBrewer::brewer.pal(4, "Set2")
  
  baseline_colour <- washout(colours[2], 0.8)
  vaccine_colours <- washout(colours[3], c(0.7, 0.65, 0.5, 0.35, 0.2, 0.1))
  
  
  border_colour <- grey(0.6)
  r0_colour <- grey(0.5)
  label_colour <- grey(0.3)
  text_size <- 2.5
  

  first_scenario <- levels(df$scenario)[1]
  
  p <- df %>%
    filter(
      ttiq == ttiq_plot
    ) %>%
    pivot_wider(
      names_from = vacc_coverage,
      values_from = post_vacc_tp,
      names_prefix = "tp_coverage_"
    ) %>%
    control_base_plot() %>%
    add_context_hline(
      label = "Control",
      at = 1,
      linetype = 2,
      text_size = text_size * 1.3
    ) %>%
    add_context_hline(
      label = "Delta R0\n(for Australia)",
      at = 8,
      linetype = 2,
      text_size = text_size * 1.3
    ) %>%
    # add the vaccination + ttiq effect as a box
    add_single_box(
      top = r0,
      bottom = tp_baseline,
      box_colour = baseline_colour,
      only_scenarios = first_scenario,
      text_main = paste0(
        "baseline\nPHSM\n&\n",
        ttiq_plot,
        "\nTTIQ"
      )
    ) %>%
    add_single_box(
      top = tp_baseline,
      bottom = tp_coverage_0.5,
      box_colour = vaccine_colours[1],
      text_main = "50%\nvaccination\ncoverage",
      only_scenarios = first_scenario
    ) %>%
    add_stacked_box(
      top = tp_coverage_0.5,
      bottom = tp_coverage_0.6,
      reference = tp_baseline_vacc,
      text_main = "60%",
      only_scenarios = first_scenario,
      box_colour = vaccine_colours[2]
    ) %>%
    add_stacked_box(
      top = tp_coverage_0.6,
      bottom = tp_coverage_0.7,
      reference = tp_baseline_vacc,
      text_main = "70%",
      only_scenarios = first_scenario,
      box_colour = vaccine_colours[3]
    ) %>%
    add_stacked_box(
      top = tp_coverage_0.7,
      bottom = tp_coverage_0.8,
      reference = tp_baseline_vacc,
      text_main = "80%",
      only_scenarios = first_scenario,
      box_colour = vaccine_colours[4]
    ) %>%
    # add_stacked_box(
    #   top = tp_coverage_0.8,
    #   bottom = tp_coverage_0.9,
    #   reference = tp_baseline_vacc,
    #   text_main = "90%",
    #   only_scenarios = first_scenario,
    #   box_colour = vaccine_colours[5]
    # ) %>%
    # add_stacked_box(
    #   top = tp_coverage_0.9,
    #   bottom = tp_coverage_1,
    #   reference = tp_baseline_vacc,
    #   text_main = "100%",
    #   only_scenarios = first_scenario,
    #   box_colour = vaccine_colours[6]
    # ) %>%
    add_arrow(8) +
    theme(
      axis.text.x = element_text(
        size = 10,
        colour = grey(0.1)
      )
    )
  
  p

}
