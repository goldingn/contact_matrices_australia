#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param remote
#' @return
#' @author Nick Golding
#' @export
vino_household_matrix_broad <- function(remote = TRUE) {
  
  remote_label <- ifelse(
    remote,
    "ABC remote",
    "ABC urban"
  )
  
  vino_household_sizes() %>%
    group_by(
      idnum, 
      shire,
      rem_urb,
      respondent_age,
      age
    ) %>%
    summarise(
      count = sum(count),
      across(
        c(overall_no, n_rooms),
        first
      ),
      .groups = "drop"
    ) %>%
    filter(
      rem_urb == remote_label
    ) %>%
    # matrix in original broad groups
    select(
      -rem_urb,
      -shire,
      -n_rooms,
      -overall_no
    ) %>%
    rename(
      age_to = age
    ) %>%
    right_join(
      expand_grid(
        age_from = c("baby", "child", "adult"),
        age_to = c("baby", "child", "adult")
      ),
      by = "age_to"
    ) %>%
    relocate(idnum, age_from, age_to, count) %>%
    arrange(idnum, age_from, age_to) %>%
    mutate(
      count = ifelse(
        age_from == age_to,
        pmax(0, count - 1),
        count
      )
    ) %>%
    mutate(
      across(
        c(age_from, age_to),
        ~factor(.x, levels = c("baby", "child", "adult"))
      )
    ) %>%
    group_by(
      age_from, age_to
    ) %>%
    summarise(
      count = mean(count),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = age_from,
      values_from = count
    ) %>%
    tibble::column_to_rownames(
      "age_to"
    ) %>%
    as.matrix()
  
}
