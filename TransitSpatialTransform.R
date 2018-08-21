# Must convert block to a higher geography.
# But how? Perhaps just the average is most reliable.
# The original blocks all essentially have a time buffer.
# When you aggregate to tract, block buffers will overlap.

# This is going to be interesting.

# Pseudocode:
# 1.  Upload .csv directory.
# 2.  Filter out .csvs that you don't plan to study.
# 3.  Keep only the 11-digit tract FIPS.
# 4.  Aggregate by tract FIPS, function = mean.
# 5.  Filter out tracts you don't plan to study:
#     these files might have larger extent than MSAs.
# NOTE STEP 5 NOT NECESSARY WITH METHODOLOGY EMPLOYED IN STEP 2
# 6.  Split file into separate data frames by MSA.
# 7.  Upload nationwide tract shapefile.
#     IN A LOOP:
# 8.  Merge MSA data frames to shapefile.
# 9.  Use sp.na.omit to eliminate empty tracts.
# 10. Export resulting shapefile by name.

# 1. Upload .csv directory
rm(list=ls())
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("foreign", "tidycensus", "tidyverse", "rgdal", "stringr")
pack(packages)
setwd("D:/AP LARSON/DallasCommutingV2/ALL_REGIONS_CSV_tr_2014_0700-0859")
fileNames <- list.files(pattern = "*.csv")
transit <- lapply(fileNames, read.csv)
transitDf <- do.call(rbind, transit)
setwd("D:/AP LARSON/DallasCommutingV2")
# 2. Filter out .csvs that you don't plan to study
xwalk <- read.csv("cbsatocountycrosswalk.csv")
statexwalk <- read.csv("ssa_fips_state_county2017.csv")
qualifyingMSAs <- read.csv("qualifyingMSAs.csv")
mynames <- unique(qualifyingMSAs$x)
mynames <- as.character(mynames)
countyList <- subset(xwalk, msaname %in% mynames)
countyList$newfips <- as.numeric(as.character(countyList$fipscounty))
statexwalk <- statexwalk[c(4,8)]
countyList <- merge(countyList, statexwalk, by = "fipscounty")
countyList$cty <- str_sub(countyList$fipscounty, -3, -1)
countyList$cty <- as.numeric(as.character(countyList$cty))
countyList <- countyList[c("msaname", "fipsstate", "cty")]
colnames(countyList) <- c("city", "st", "cty")
countyList$city <- as.factor(as.character(countyList$city))
# Now, countyList are the ONLY states and counties you keep.
transitDf$FIPSchar <- as.character(transitDf$geoid)
transitDf$stcty <- str_sub(transitDf$FIPSchar, -15, -11)
transitDf$cty <- str_sub(transitDf$stcty, -3, -1)
transitDf$st <- str_sub(transitDf$stcty, -5, -4)
transitDf$st <- as.numeric(transitDf$st)
transitDf$cty <- as.numeric(transitDf$cty)
# Create a unique key to link state-county pairs.
countyList$uniqueKey <- paste(countyList$st, countyList$cty, sep = "_")
transitDf$uniqueKey <- paste(transitDf$st, transitDf$cty, sep = "_")
uniqueKeyL <- unique(countyList$uniqueKey)
transitDf <- subset(transitDf, uniqueKey %in% uniqueKeyL)
# 3.  Keep only the 11-digit tract FIPS.
transitDf$tract <- str_sub(transitDf$FIPSchar, 1, str_length(transitDf$FIPSchar)-4)
transitDf$tract <- as.numeric(transitDf$tract)
transitDf <- transitDf[c(9,5,6,7,3)]
# 4.  Aggregate by tract FIPS, function = mean.
transitCollapse <- aggregate(transitDf$tot_jobs, list(transitDf$tract), FUN = mean)
keepState <- aggregate(transitDf$st, list(transitDf$tract), FUN = min)
keepCounty <- aggregate(transitDf$cty, list(transitDf$tract), FUN = min)
transitCollapse <- merge(transitCollapse, keepState, by = "Group.1")
colnames(transitCollapse) <- c("geoid", "jobs", "st")
transitCollapse <- merge(transitCollapse, keepCounty, by.x = "geoid", by.y = "Group.1")
colnames(transitCollapse)[4] <- "cty"
# 6.  Split file into separate data frames by MSA.
transitCollapse$uniqueKey <- paste(transitCollapse$st, transitCollapse$cty, sep = "_")
transitCollapse <- merge(transitCollapse, countyList, by = "uniqueKey")
transitCollapse <- transitCollapse[c(2:6)]
colnames(transitCollapse) <- c("geoid", "jobs", "st", "cty", "city")
transitList <- split(transitCollapse, transitCollapse$city)
# 7.  Upload nationwide tract shapefile.
# UGH that's not a thing.
unique(transitCollapse$st)
for (i in 1:2){
# for (i in 1:length(transitList)){
  currentCity <- as.data.frame(transitList[i])
  statesNeeded <- unique(currentCity[,3])
  for (j in 1:length(statesNeeded)){
    print(statesNeeded[j])
    statesNeededChar <- as.character(statesNeeded)
    statesNeededChar <- ifelse(length(statesNeededChar) == 1, # isn't working right
                               paste0("0",statesNeededChar),
                               statesNeededChar)
    shpName <- paste0("cb_2017_", statesNeededChar, "_tract_500k")
  }
}

