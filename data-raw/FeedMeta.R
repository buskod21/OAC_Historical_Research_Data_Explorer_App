## code to prepare `FeedMeta` dataset goes here

FeedMeta <- read.table(text = gsub("=", ":",
                                   readLines("inst/extdata/B100_README_Cant et al_cowFeed_1997_20210922.txt")), sep =  ":")

usethis::use_data(FeedMeta, overwrite = TRUE)
