## code to prepare `Feed` dataset goes here

Feed <-read.csv("inst/extdata/cowFeedCant1997.csv")


usethis::use_data(Feed, overwrite = TRUE)
