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
setwd("D:/AP LARSON/DallasCommutingV2/tractShps")
unique(transitCollapse$st)
# for (i in 1:length(transitList)){
#   currentCity <- as.data.frame(transitList[i])
#   statesNeeded <- unique(currentCity[,3])
#   vec <- NULL
#   for (j in 1:length(statesNeeded)){
#     print(statesNeeded[j])
#     addZero <- ifelse(statesNeeded[j] < 10, # append 0 to front of string to align w shp naming scheme
#                               paste0("0",as.character(statesNeeded[j])),
#                               statesNeeded[j])
#     readID <- paste0("cb_2017_", addZero,"_tract_500k")
#     vec <- append(vec, readID)
#     for (k in 1:length(vec)) {
#       assign(vec[k], readOGR(".", vec[k]))
#     }
#   }
# } THIS IS RUNNING INTO PROBLEMS BECAUSE IMPORTING SAME STATE MULT TIMES
vec <- NULL
stateList <- unique(transitCollapse$st)
for (j in 1:length(stateList)){
  addZero <- ifelse(stateList[j] < 10, # append 0 to front of string to align w shp naming scheme
                    paste0("0",as.character(stateList[j])),
                    stateList[j])
  readID <- paste0("cb_2017_", addZero,"_tract_500k")
  vec <- append(vec, readID)
}
for (k in 1:length(vec)) {
  assign(vec[k], readOGR(".", vec[k]))
}
# Time for brute force.
# Why? Because once you use assign(vec[k], readOGR(".", vec[k]))
# it's practically impossible to select this particular shapefile by name
sp.na.omit <- function(x, col.name = NULL, margin = 1) {
  if (!inherits(x, "SpatialPointsDataFrame") & 
      !inherits(x, "SpatialPolygonsDataFrame") & 
      !inherits(x, "SpatialLinesDataFrame") )
    stop("MUST BE sp SpatialPointsDataFrame OR SpatialPolygonsDataFrame class object")
  if(!is.null(col.name)) {
    if(is.na(match(col.name, names(x)))) stop(col.name, "does not exist in data") 
    return( x[-which(is.na(x@data[,col.name])),] )
  } else {    
    na.index <- unique(as.data.frame(which(is.na(x@data), arr.ind = TRUE))[, margin])
    if (margin == 1) {
      cat("Deleting rows: ", na.index, "\n")
      return(x[-na.index, ])
    }
    if (margin == 2) {
      cat("Deleting columns: ", na.index, "\n")
      return(x[, -na.index])
    }
  }
} 

# 1. ATLANTA, GA
currentCity <- as.data.frame(transitList[1])
unique(currentCity[,3]) # 13
colnames(currentCity) <- c("geoid", "jobs", "st", "cty", "city")
currentShp <- merge(cb_2017_13_tract_500k,
                    currentCity,
                    by.x = "GEOID",
                    by.y = "geoid")
currentShp <- sp.na.omit(currentShp)
# CHECK: currentShp has 918 features, same as currentCity
writeOGR(currentShp, ".", "res_ATL_GA", driver = "ESRI Shapefile")

# 2. BALTIMORE, MD
currentCity <- as.data.frame(transitList[2])
unique(currentCity[,3]) # 24
colnames(currentCity) <- c("geoid", "jobs", "st", "cty", "city")
currentShp <- merge(cb_2017_24_tract_500k,
                    currentCity,
                    by.x = "GEOID",
                    by.y = "geoid")
currentShp <- sp.na.omit(currentShp)
writeOGR(currentShp, ".", "res_BAL_MD", driver = "ESRI Shapefile")

# 3. BIRMINGHAM, AL
currentCity <- as.data.frame(transitList[3])
unique(currentCity[,3]) # 1
colnames(currentCity) <- c("geoid", "jobs", "st", "cty", "city")
cb_2017_01_tract_500k$GEOID <- as.numeric(as.character(cb_2017_01_tract_500k$GEOID))
currentShp <- merge(cb_2017_01_tract_500k,
                    currentCity,
                    by.x = "GEOID",
                    by.y = "geoid")
currentShp <- sp.na.omit(currentShp)
currentShp$GEOID <- as.factor(currentShp$GEOID)
writeOGR(currentShp, ".", "res_BIR_AL", driver = "ESRI Shapefile")

# 4. BOSTON, MA
currentCity <- as.data.frame(transitList[4])
unique(currentCity[,3]) # 25,33
colnames(currentCity) <- c("geoid", "jobs", "st", "cty", "city")
currentShp <- merge(cb_2017_25_tract_500k,
                    currentCity,
                    by.x = "GEOID",
                    by.y = "geoid")
currentShp <- sp.na.omit(currentShp)
writeOGR(currentShp, ".", "res_BOS_MA1", driver = "ESRI Shapefile")
currentShp <- merge(cb_2017_33_tract_500k,
                    currentCity,
                    by.x = "GEOID",
                    by.y = "geoid")
currentShp <- sp.na.omit(currentShp)
writeOGR(currentShp, ".", "res_BOS_MA2", driver = "ESRI Shapefile")

# 5. BUFFALO, NY
currentCity <- as.data.frame(transitList[5])
unique(currentCity[,3]) # 36
colnames(currentCity) <- c("geoid", "jobs", "st", "cty", "city")
currentShp <- merge(cb_2017_36_tract_500k,
                    currentCity,
                    by.x = "GEOID",
                    by.y = "geoid")
currentShp <- sp.na.omit(currentShp)
writeOGR(currentShp, ".", "res_BUF_NY", driver = "ESRI Shapefile")

# 6. CHARLOTTE, NC
currentCity <- as.data.frame(transitList[6])
unique(currentCity[,3]) # 37, 45
colnames(currentCity) <- c("geoid", "jobs", "st", "cty", "city")
currentShp <- merge(cb_2017_37_tract_500k,
                    currentCity,
                    by.x = "GEOID",
                    by.y = "geoid")
currentShp <- sp.na.omit(currentShp)
writeOGR(currentShp, ".", "res_CHA_NC1", driver = "ESRI Shapefile")
currentShp <- merge(cb_2017_45_tract_500k,
                    currentCity,
                    by.x = "GEOID",
                    by.y = "geoid")
currentShp <- sp.na.omit(currentShp)
writeOGR(currentShp, ".", "res_CHA_NC2", driver = "ESRI Shapefile")

# 7. CINCINNATI, OH
currentCity <- as.data.frame(transitList[7])
unique(currentCity[,3]) # 18, 21, 39
colnames(currentCity) <- c("geoid", "jobs", "st", "cty", "city")
currentShp <- merge(cb_2017_18_tract_500k,
                    currentCity,
                    by.x = "GEOID",
                    by.y = "geoid")
currentShp <- sp.na.omit(currentShp)
writeOGR(currentShp, ".", "res_CIN_OH1", driver = "ESRI Shapefile")
currentShp <- merge(cb_2017_21_tract_500k,
                    currentCity,
                    by.x = "GEOID",
                    by.y = "geoid")
currentShp <- sp.na.omit(currentShp)
writeOGR(currentShp, ".", "res_CIN_OH2", driver = "ESRI Shapefile")
currentShp <- merge(cb_2017_39_tract_500k,
                    currentCity,
                    by.x = "GEOID",
                    by.y = "geoid")
currentShp <- sp.na.omit(currentShp)
writeOGR(currentShp, ".", "res_CIN_OH3", driver = "ESRI Shapefile")

# 8. CLEVELAND, OH
currentCity <- as.data.frame(transitList[8])
unique(currentCity[,3]) # 39
colnames(currentCity) <- c("geoid", "jobs", "st", "cty", "city")
currentShp <- merge(cb_2017_39_tract_500k,
                    currentCity,
                    by.x = "GEOID",
                    by.y = "geoid")
currentShp <- sp.na.omit(currentShp)
writeOGR(currentShp, ".", "res_CLE_OH", driver = "ESRI Shapefile")

# 9. COLUMBUS, OH
currentCity <- as.data.frame(transitList[9])
unique(currentCity[,3]) # 39
colnames(currentCity) <- c("geoid", "jobs", "st", "cty", "city")
currentShp <- merge(cb_2017_39_tract_500k,
                    currentCity,
                    by.x = "GEOID",
                    by.y = "geoid")
currentShp <- sp.na.omit(currentShp)
writeOGR(currentShp, ".", "res_COL_OH", driver = "ESRI Shapefile")

# 10. DENVER, CO
currentCity <- as.data.frame(transitList[10])
unique(currentCity[,3]) # 8
colnames(currentCity) <- c("geoid", "jobs", "st", "cty", "city")
cb_2017_08_tract_500k$GEOID <- as.numeric(as.character(cb_2017_08_tract_500k$GEOID))
currentShp <- merge(cb_2017_08_tract_500k,
                    currentCity,
                    by.x = "GEOID",
                    by.y = "geoid")
currentShp <- sp.na.omit(currentShp)
currentShp$GEOID <- as.factor(currentShp$GEOID)
writeOGR(currentShp, ".", "res_DEN_CO", driver = "ESRI Shapefile")

# 11. HAMILTON, OH
currentCity <- as.data.frame(transitList[11])
unique(currentCity[,3]) # 39
colnames(currentCity) <- c("geoid", "jobs", "st", "cty", "city")
currentShp <- merge(cb_2017_39_tract_500k,
                    currentCity,
                    by.x = "GEOID",
                    by.y = "geoid")
currentShp <- sp.na.omit(currentShp)
writeOGR(currentShp, ".", "res_HAM_OH", driver = "ESRI Shapefile")

# 12. INDIANAPOLIS, IN
currentCity <- as.data.frame(transitList[12])
unique(currentCity[,3]) # 18
colnames(currentCity) <- c("geoid", "jobs", "st", "cty", "city")
currentShp <- merge(cb_2017_18_tract_500k,
                    currentCity,
                    by.x = "GEOID",
                    by.y = "geoid")
currentShp <- sp.na.omit(currentShp)
writeOGR(currentShp, ".", "res_IND_IN", driver = "ESRI Shapefile")

# 13. KANSAS CITY, MO
currentCity <- as.data.frame(transitList[13])
unique(currentCity[,3]) # 20, 29
colnames(currentCity) <- c("geoid", "jobs", "st", "cty", "city")
currentShp <- merge(cb_2017_20_tract_500k,
                    currentCity,
                    by.x = "GEOID",
                    by.y = "geoid")
currentShp <- sp.na.omit(currentShp)
writeOGR(currentShp, ".", "res_KAN_MO1", driver = "ESRI Shapefile")
currentShp <- merge(cb_2017_29_tract_500k,
                    currentCity,
                    by.x = "GEOID",
                    by.y = "geoid")
currentShp <- sp.na.omit(currentShp)
writeOGR(currentShp, ".", "res_KAN_MO2", driver = "ESRI Shapefile")

# 14. LOUISVILLE, KY
currentCity <- as.data.frame(transitList[14])
unique(currentCity[,3]) # 18, 21
colnames(currentCity) <- c("geoid", "jobs", "st", "cty", "city")
currentShp <- merge(cb_2017_18_tract_500k,
                    currentCity,
                    by.x = "GEOID",
                    by.y = "geoid")
currentShp <- sp.na.omit(currentShp)
writeOGR(currentShp, ".", "res_LOU_KY1", driver = "ESRI Shapefile")
currentShp <- merge(cb_2017_21_tract_500k,
                    currentCity,
                    by.x = "GEOID",
                    by.y = "geoid")
currentShp <- sp.na.omit(currentShp)
writeOGR(currentShp, ".", "res_LOU_KY2", driver = "ESRI Shapefile")

# 15. MINNEAPOLIS, MN
currentCity <- as.data.frame(transitList[15])
unique(currentCity[,3]) # 27, 55
colnames(currentCity) <- c("geoid", "jobs", "st", "cty", "city")
currentShp <- merge(cb_2017_27_tract_500k,
                    currentCity,
                    by.x = "GEOID",
                    by.y = "geoid")
currentShp <- sp.na.omit(currentShp)
writeOGR(currentShp, ".", "res_MIN_MN1", driver = "ESRI Shapefile")
currentShp <- merge(cb_2017_55_tract_500k,
                    currentCity,
                    by.x = "GEOID",
                    by.y = "geoid")
currentShp <- sp.na.omit(currentShp)
writeOGR(currentShp, ".", "res_MIN_MN2", driver = "ESRI Shapefile")

# 16. NEW ORLEANS, LA
currentCity <- as.data.frame(transitList[16])
unique(currentCity[,3]) # 22
colnames(currentCity) <- c("geoid", "jobs", "st", "cty", "city")
currentShp <- merge(cb_2017_22_tract_500k,
                    currentCity,
                    by.x = "GEOID",
                    by.y = "geoid")
currentShp <- sp.na.omit(currentShp)
writeOGR(currentShp, ".", "res_NEW_LA", driver = "ESRI Shapefile")

# 17. NORFOLK, VA
currentCity <- as.data.frame(transitList[17])
unique(currentCity[,3]) # 37, 51
colnames(currentCity) <- c("geoid", "jobs", "st", "cty", "city")
currentShp <- merge(cb_2017_37_tract_500k,
                    currentCity,
                    by.x = "GEOID",
                    by.y = "geoid")
currentShp <- sp.na.omit(currentShp)
writeOGR(currentShp, ".", "res_NOR_VA1", driver = "ESRI Shapefile")
currentShp <- merge(cb_2017_51_tract_500k,
                    currentCity,
                    by.x = "GEOID",
                    by.y = "geoid")
currentShp <- sp.na.omit(currentShp)
writeOGR(currentShp, ".", "res_NOR_VA2", driver = "ESRI Shapefile")

# 18. PITTSBURGH, PA
currentCity <- as.data.frame(transitList[18])
unique(currentCity[,3]) # 42
colnames(currentCity) <- c("geoid", "jobs", "st", "cty", "city")
currentShp <- merge(cb_2017_42_tract_500k,
                    currentCity,
                    by.x = "GEOID",
                    by.y = "geoid")
currentShp <- sp.na.omit(currentShp)
writeOGR(currentShp, ".", "res_PIT_PA", driver = "ESRI Shapefile")

# 19. PORTLAND, OR
currentCity <- as.data.frame(transitList[19])
unique(currentCity[,3]) # 41, 53
colnames(currentCity) <- c("geoid", "jobs", "st", "cty", "city")
currentShp <- merge(cb_2017_41_tract_500k,
                    currentCity,
                    by.x = "GEOID",
                    by.y = "geoid")
currentShp <- sp.na.omit(currentShp)
writeOGR(currentShp, ".", "res_POR_OR1", driver = "ESRI Shapefile")
currentShp <- merge(cb_2017_53_tract_500k,
                    currentCity,
                    by.x = "GEOID",
                    by.y = "geoid")
currentShp <- sp.na.omit(currentShp)
writeOGR(currentShp, ".", "res_POR_OR2", driver = "ESRI Shapefile")

# 20. PROVIDENCE, RI
currentCity <- as.data.frame(transitList[20])
unique(currentCity[,3]) # 44
colnames(currentCity) <- c("geoid", "jobs", "st", "cty", "city")
currentShp <- merge(cb_2017_44_tract_500k,
                    currentCity,
                    by.x = "GEOID",
                    by.y = "geoid")
currentShp <- sp.na.omit(currentShp)
writeOGR(currentShp, ".", "res_PRO_RI", driver = "ESRI Shapefile")

# 21. RALEIGH-DURHAM, NC
currentCity <- as.data.frame(transitList[21])
unique(currentCity[,3]) # 37
colnames(currentCity) <- c("geoid", "jobs", "st", "cty", "city")
currentShp <- merge(cb_2017_37_tract_500k,
                    currentCity,
                    by.x = "GEOID",
                    by.y = "geoid")
currentShp <- sp.na.omit(currentShp)
writeOGR(currentShp, ".", "res_RAL_NC", driver = "ESRI Shapefile")

# 22. SACRAMENTO, CA
currentCity <- as.data.frame(transitList[22])
unique(currentCity[,3]) # 6
colnames(currentCity) <- c("geoid", "jobs", "st", "cty", "city")
cb_2017_06_tract_500k$GEOID <- as.numeric(as.character(cb_2017_06_tract_500k$GEOID))
currentShp <- merge(cb_2017_06_tract_500k,
                    currentCity,
                    by.x = "GEOID",
                    by.y = "geoid")
currentShp <- sp.na.omit(currentShp)
currentShp$GEOID <- as.factor(currentShp$GEOID)
writeOGR(currentShp, ".", "res_SAC_CA", driver = "ESRI Shapefile")

# 23. SAN DIEGO, CA
currentCity <- as.data.frame(transitList[23])
unique(currentCity[,3]) # 6
colnames(currentCity) <- c("geoid", "jobs", "st", "cty", "city")
currentShp <- merge(cb_2017_06_tract_500k,
                    currentCity,
                    by.x = "GEOID",
                    by.y = "geoid")
currentShp <- sp.na.omit(currentShp)
currentShp$GEOID <- as.factor(currentShp$GEOID)
writeOGR(currentShp, ".", "res_SAND_CA", driver = "ESRI Shapefile")

# 24. SAN JOSE, CA
currentCity <- as.data.frame(transitList[24])
unique(currentCity[,3]) # 6
colnames(currentCity) <- c("geoid", "jobs", "st", "cty", "city")
currentShp <- merge(cb_2017_06_tract_500k,
                    currentCity,
                    by.x = "GEOID",
                    by.y = "geoid")
currentShp <- sp.na.omit(currentShp)
currentShp$GEOID <- as.factor(currentShp$GEOID)
writeOGR(currentShp, ".", "res_SANJ_CA", driver = "ESRI Shapefile")

# 25. ST. LOUIS, MO
currentCity <- as.data.frame(transitList[25])
unique(currentCity[,3]) # 17, 29
colnames(currentCity) <- c("geoid", "jobs", "st", "cty", "city")
currentShp <- merge(cb_2017_17_tract_500k,
                    currentCity,
                    by.x = "GEOID",
                    by.y = "geoid")
currentShp <- sp.na.omit(currentShp)
writeOGR(currentShp, ".", "res_STL_MO1", driver = "ESRI Shapefile")
currentShp <- merge(cb_2017_29_tract_500k,
                    currentCity,
                    by.x = "GEOID",
                    by.y = "geoid")
currentShp <- sp.na.omit(currentShp)
writeOGR(currentShp, ".", "res_STL_MO2", driver = "ESRI Shapefile")

# 26. TAMPA, FL
currentCity <- as.data.frame(transitList[26])
unique(currentCity[,3]) # 12
colnames(currentCity) <- c("geoid", "jobs", "st", "cty", "city")
currentShp <- merge(cb_2017_12_tract_500k,
                    currentCity,
                    by.x = "GEOID",
                    by.y = "geoid")
currentShp <- sp.na.omit(currentShp)
writeOGR(currentShp, ".", "res_TAM_FL", driver = "ESRI Shapefile")

# 27. YOLO, CA
currentCity <- as.data.frame(transitList[27])
unique(currentCity[,3]) # 6
colnames(currentCity) <- c("geoid", "jobs", "st", "cty", "city")
currentShp <- merge(cb_2017_06_tract_500k,
                    currentCity,
                    by.x = "GEOID",
                    by.y = "geoid")
currentShp <- sp.na.omit(currentShp)
currentShp$GEOID <- as.factor(currentShp$GEOID)
writeOGR(currentShp, ".", "res_YOL_CA", driver = "ESRI Shapefile")
