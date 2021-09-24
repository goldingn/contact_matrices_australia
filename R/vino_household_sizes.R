#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param file
#' @param by_room
#' @return
#' @author Nick Golding
#' @export
vino_household_sizes <- function(file = "data/vino_household.xlsx", by_room = FALSE) {

  # download the data if not available
  if (!file.exists(file)) {
    download.file("https://dfzljdn9uc3pi.cloudfront.net/2017/3958/1/ABC_household_raw_de-identified.xlsx",
                  destfile = file)
  }

  data <- readxl::read_excel(file) %>%
    # split out rooms and demographics
    pivot_longer(
      starts_with("rm"),
      names_to = c("room", "demographic"),
      names_sep = "_",
      values_to = "count"
    ) %>%
    relocate(
      c("room", "demographic", "count"),
      .after = "overall_no"
    ) %>%
    # remove room totals
    filter(
      demographic != "no"
    ) %>%
    # rename some variables
    rename(
      n_rooms = `No of Rooms in a house hold`,
      respondent_sex = sex,
      respondent_age = age_yrs
    ) %>%
    # split contact age group and sex
    mutate(
      sex = case_when(
        grepl("female", demographic) ~ "female",
        grepl("male", demographic) ~ "male",
        TRUE ~ NA_character_
      ),
      age = case_when(
        grepl("baby", demographic) ~ "baby",
        grepl("child", demographic) ~ "child",
        TRUE ~ "adult"
      )
    ) %>%
    # remove unneeded columns
    select(
      # now split out
      -demographic,
      # can be recomputed
      -`Average no of ppl in a room`,
      # remove summaries
      -starts_with("all"),
      # same as overall_no
      -house_c
    ) %>%
    relocate(
      room, sex, age, count,
      .after = respondent_age
    )
  
  # if not needed by room, aggregate over households
  if (!by_room) {
    
    data <- data %>%
      group_by(
        idnum, shire, rem_urb, 
        respondent_sex,
        respondent_age,
        sex,
        age
      ) %>%
      summarise(
        across(
          count,
          sum
        ),
        across(
          c(overall_no, n_rooms),
          first
        ),
        .groups = "drop"
      )

  }
    
  data

}
