## code to prepare `MilkMeta` dataset goes here
MilkMeta <- read.table(text = gsub("=", ":",
                                   readLines("inst/extdata/C100_README_Cant et al_cowMilk_1997_20210922.txt")),
                       sep =  ":")


usethis::use_data(MilkMeta,
                  overwrite = TRUE,
                  internal=TRUE)
