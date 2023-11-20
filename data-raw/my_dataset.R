## code to prepare `my_dataset` dataset goes here


Fattyacid <-read.csv("inst/extdata/cowFattyAcidCant1997.csv")
Feed <-read.csv("inst/extdata/cowFeedCant1997.csv")
Milk<-read.csv("inst/extdata/cowMilkCant1997.csv")
FattyacidMeta <- read.table(text = gsub("=", ":",
                                        readLines("inst/extdata/A100_README_Cant et al_cowFattyAcid_1997_20210922.txt")), sep =  ":")
FeedMeta <- read.table(text = gsub("=", ":",
                                   readLines("inst/extdata/B100_README_Cant et al_cowFeed_1997_20210922.txt")), sep =  ":")
MilkMeta <- read.table(text = gsub("=", ":",
                                   readLines("inst/extdata/C100_README_Cant et al_cowMilk_1997_20210922.txt")),
                       sep =  ":")




usethis::use_data(Fattyacid, overwrite = TRUE)
usethis::use_data(Feed)
usethis::use_data(Milk)
usethis::use_data(FattyacidMeta)
usethis::use_data(FeedMeta)
usethis::use_data(MilkMeta)
