## code to prepare `Milk` dataset goes here
Milk<-read.csv("inst/extdata/cowMilkCant1997.csv")


usethis::use_data(Milk,
                  overwrite = TRUE,
                  internal=TRUE)
