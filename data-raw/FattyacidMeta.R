## code to prepare `FattyacidMeta` dataset goes here
ttyacidMeta <- read.table(text = gsub("=", ":",
                                      readLines("inst/extdata/A100_README_Cant et al_cowFattyAcid_1997_20210922.txt")), sep =  ":")


usethis::use_data(FattyacidMeta,
                  overwrite = TRUE,
                  internal = TRUE)
