# library(remotes)
# #remotes::install_github("njtierney/conmat")
# install.packages("conmat", repos = "https://njtierney.r-universe.dev")
# # library(fnmate)

library(conmat)
library(tidyverse)
library(janitor)
library(readxl)
library(patchwork)
library(magrittr)
library(purrr)


invisible(
  sapply(
    list.files("R/", full.names = TRUE),
    source
  )
)