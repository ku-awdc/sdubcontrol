## code to prepare `size_type_distr` dataset goes here
CattleDenmark <- readRDS("data-raw/size_type_distr.rds")
usethis::use_data(CattleDenmark, overwrite = TRUE)
