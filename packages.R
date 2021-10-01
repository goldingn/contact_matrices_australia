invisible(
  sapply(
    X = list.files(
      path = "R",
      full.names = TRUE
    ),
    FUN = source,
    simplify = FALSE
  )
)


library(conmat)
library(tidyverse)
library(janitor)
library(readxl)
library(patchwork)
library(data.table)
library(future)
library(RColorBrewer)
#library(furrr)