## code to prepare `Fattyacid` dataset goes here

Fattyacid <-read.csv("inst/extdata/cowFattyAcidCant1997.csv")
usethis::use_data(Fattyacid, overwrite = TRUE)
