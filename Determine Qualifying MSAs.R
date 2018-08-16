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
transitMSA <- as.numeric(transitMSA)
# transitMSA is all MSAs present in transit accessibility files.

# Cool. Now, let's find some matches for the HOLC shapefiles.
testCase <- as.data.frame(HOLC[1,])

holcMSAl <- list()
for (i in 1:length(HOLC$PlaceName)) {
  testCase <- as.data.frame(HOLC[i,])
  matches <- grep(testCase$PlaceName, xwalk$msaname)
  matchesDf <- xwalk[matches,]
  holcMSAl[[i]] <- matchesDf
}
holcMSAl <- do.call(rbind, holcMSAl)

# Identify overlapping MSA names and IDs
holcMSA <- unique(holcMSAl$cbsa)
overlaps <- intersect(transitMSA, holcMSA)
# Extract corresponding MSA name
overlapsDf <- xwalk[which(xwalk$cbsa %in% overlaps),]
overlapsDf <- overlapsDf[c(7,9,1,2,4,8,13)]
# CBSA names include more counties than MSA names. Filter out those
# That don't actually fall within an MSA.
# If you don't have a comma, it's no good
overlapsDf$MSAtrue <- grepl(",", overlapsDf$msaname)
overlapsDf <- overlapsDf[which(overlapsDf$MSAtrue == TRUE),]
# End result: we have 27 unique MSAs which are present in both datasets.
# Export names as a list.
setwd("D:/AP LARSON/DallasCommutingV2/DallasCommutingV2")
write.csv(overlapsDf$msaname, "qualifyingMSAs.csv", row.names = FALSE)
