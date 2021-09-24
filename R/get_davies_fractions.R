#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param table_image
#' @param csv
#' @return
#' @author geryan
#' @export
get_davies_fractions <- function(
  table_image = "data/susceptibility_clinical_fraction_age_Davies.jpg",
  csv = "data/susceptibility_clinical_fraction_age_Davies.csv"
) {
  
  # table ext 4 in:
  # Davies, N.G., Klepac, P., Liu, Y. et al.
  # Age-dependent effects in the transmission and control of COVID-19 epidemics.
  # Nat Med 26, 1205–1211 (2020).
  # https://doi.org/10.1038/s41591-020-0962-9

  if (!file.exists(table_image)) {
    download.file(
      url = "https://media.springernature.com/full/springer-static/esm/art%3A10.1038%2Fs41591-020-0962-9/MediaObjects/41591_2020_962_Fig8_ESM.jpg",
      destfile = table_image
    )
  }
  
  library(tesseract) 
  # requires tesseract installed from source outside R
  # https://github.com/ropensci/tesseract
  
  text <- tesseract::ocr(table_image)
  
  textlist <- text %>%
    strsplit(split = "\\n") %>%
    unlist %>%
    strsplit(split = " ")
  
  
  textmat <- lapply(
    X = textlist[-10],
    FUN = function(x){
      if (x[1] == "Parameter") {
        lapply(
          X = list(x[1], x[2:3], x[4], x[5:6], x[7:8], x[9:10], x[11:12], x[13:14]),
          FUN = function(x){if(length(x) == 1){x}else{paste(x, collapse = " ")}}
        ) %>%
          unlist
      } else if (x[1] == "Susceptibility") {
        c(x[1], x[3:9])
      } else if (x[1] == "Clinical") {
        c(
          paste(x[1:2], collapse = " "),
          x[4:10]
        )
      }
    }
  ) %>%
    sapply(FUN = c) %>%
    t
  
  df <- as_tibble(textmat[2:17,])
  
  names(df) <- textmat[1,]
  
  df %>%
    rename(
      "age_group" = `Age Group`,
      "mean" = Mean,
      "median" = `Quantile 50%`
    ) %>%
    mutate(
      param = ifelse(
        Parameter == "Susceptibility",
        "rel_susceptibility",
        "clinical_fraction"
      )
    ) %>%
    pivot_longer(
      cols = c(mean, median)
    ) %>%
    dplyr::select(age_group, param, name, value) %>%
    pivot_wider(
      names_from = c(param, name),
      values_from = value,
      names_sep = "_"
    ) %>%
    mutate(
      age_group = str_replace(
        string = age_group,
        pattern = "-",
        replace = "_"
      )
    ) %>%
    write_csv(
      file = csv
    )

}

