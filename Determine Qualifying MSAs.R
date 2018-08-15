rm(list=ls())
# Because the Census API is so tricky,
# I want to make sure I grab what
# OVERLAPS between these transit accessibility files
# (see https://conservancy.umn.edu/handle/11299/168064)
# and the HOLC shapefiles
# (see https://dsl.richmond.edu/panorama/redlining)

setwd("D:/AP LARSON/DallasCommutingV2")
xwalk <- read.csv("cbsatocountycrosswalk.csv")
HOLC <- read.csv("HOLC List of Cities.csv")

# field CBSA in xwalk matches first string of files in
# D:\AP LARSON\DallasCommutingV2\ALL_REGIONS_CSV_tr_2014_0700-0859
setwd("D:/AP LARSON/DallasCommutingV2/ALL_REGIONS_CSV_tr_2014_0700-0859")
commuteMSAs <- list.files(".")
MSAid <- strsplit(commuteMSAs, "_")
transitMSA <- list()
for (i in 1:length(MSAid)){
  transitMSA[[i]] <- (MSAid[[i]][1])
}
transitMSA <- do.call(rbind, transitMSA)
# transitMSA is all MSAs present in transit accessibility files.

# Cool. Now, let's find some matches for the HOLC shapefiles.
xwalk$msaname
xwalk$state
testCase <- as.data.frame(HOLC[1,])

matches <- grep(testCase$Keyword, xwalk$msaname)
matchesDf <- xwalk[matches,]

# PERFECT, until you run into a city like Springfield...IN? IL? MO? MA?
# You should append matching row IDs to a vector
