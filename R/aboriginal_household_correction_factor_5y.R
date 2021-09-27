#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param contact_model
#' @param age_breaks
#' @param remote
#' @return
#' @author Nick Golding
#' @export
aboriginal_household_correction_factor_5y <- function(contact_model, age_breaks, remote = TRUE) {
  
  # get population age distribution
  if (remote) {
    
    # aboriginal population in all remote districts
    population <- nt_lhd_aboriginal_pop %>%
      filter(
        !(district %in% c("Alice Springs Urban", "Darwin/Casuarina"))
      ) %>%
      group_by(
        lower.age.limit
      ) %>%
      summarise(
        population = sum(population)
      )
    
  } else {
    
    # aboriginal population in urban alice springs
    population <- nt_lhd_aboriginal_pop %>%
      filter(
        district == "Alice Springs Urban"
      ) %>%
      group_by(
        lower.age.limit
      ) %>%
      summarise(
        population = sum(population)
      )
    
  }
  
  # convert to a function (1y age bins)
  pop_fun <- get_age_population_function(population)
  
  # age limits for broad age categories in Vino et al.
  age_limits_broad <- c(0, 5, 16, Inf)
  
  # lookup from broad age groups to integer years, to reaggregate correction
  # factors at 5y intervals
  age_group_lookup_broad <- tibble(
    age =  0:100
  ) %>%
    mutate(
      age_group = case_when(
        age < 5 ~ "baby",
        age < 16 ~ "child",
        TRUE ~ "adult"
      )
    )
  
  # empirical contact matrix in broad age groups
  household_matrix_broad <- vino_household_matrix_broad(
    remote = remote
  )
  
  # synthetic contact matrix in broad age groups  
  household_matrix_broad_naive <- conmat::predict_setting_contacts(
    population = population,
    contact_model = contact_model,
    age_breaks = age_limits_broad
  )$home
  
  # correction factor matrix in broad age groups
  correction_factor_broad <- household_matrix_broad / household_matrix_broad_naive
  
  # convert ratio to 5y age bands
  correction_factor_5y <- correction_factor_broad %>%
    conmat::matrix_to_predictions() %>%
    rename(
      correction = contacts
    ) %>%
    right_join(
      age_group_lookup_broad,
      by = c(age_group_from = "age_group")
    ) %>%
    rename(
      age_from = age
    ) %>%
    right_join(
      age_group_lookup_broad,
      by = c(age_group_to = "age_group")
    ) %>%
    rename(
      age_to = age
    ) %>%
    select(
      -starts_with("age_group")
    ) %>%
    mutate(
      age_group_to = cut(
        pmax(0.1, age_to),
        age_breaks,
        right = FALSE
      ),
      age_group_from = cut(
        pmax(0.1, age_from),
        age_breaks,
        right = FALSE
      ),
      population_from = pop_fun(age_from),
      population_to = pop_fun(age_to),
      population_interaction = population_from + population_to
    ) %>%
    group_by(age_group_to, age_group_from) %>%
    summarise(
      correction = weighted.mean(
        x = correction,
        w = population_interaction
      ),
      .groups = "drop"
    ) %>%
    rename(
      contacts = correction
    ) %>%
    conmat::predictions_to_matrix()
  
  correction_factor_5y
  
}
