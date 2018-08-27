# The regressions show that educational attainment makes a bigger difference
# than HOLC rating on earnings. But how do these educational attainments shake out?
rm(list=ls())
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("foreign", "tidycensus", "tidyverse", "rgdal", "stringr")
pack(packages)
setwd("C:/Users/alarson/Downloads/DallasCommutingV2-master/DallasCommutingV2-master")
xwalk <- read.csv("cbsatocountycrosswalk.csv")
statexwalk <- read.csv("ssa_fips_state_county2017.csv")
qualifyingMSAs <- read.csv("qualifyingMSAs.csv")

mynames <- unique(qualifyingMSAs$x)
mynames <- as.character(mynames)

countyList <- subset(xwalk, msaname %in% mynames)
countyList$newfips <- as.numeric(as.character(countyList$fipscounty))

# Need either state code or state fips separate
# in order to talk to tidycensus
statexwalk <- statexwalk[c(4,8)]
countyList <- merge(countyList, statexwalk, by = "fipscounty")

countyList$cty <- str_sub(countyList$fipscounty, -3, -1)
countyList$cty <- as.numeric(as.character(countyList$cty))
countyList <- countyList[c("msaname", "fipsstate", "cty")]
colnames(countyList) <- c("city", "st", "cty")

# This helps you get rid of factors that were once in the data frame
countyList$city <- as.factor(as.character(countyList$city))

# Instead of running separate queries for each MSA, run in a clump and split later
# listOfCities <- split(countyList, countyList$city)

setwd("C:/Users/alarson/Downloads/DallasCommutingV2-master/DallasCommutingV2-master/educationByRace")

# https://stackoverflow.com/questions/45109241/r-tidycensus-download-all-block-groups
# "tidycensus can't yet handle multi-state and multi-county calls simultaneously"
# https://api.census.gov/data/2016/acs/acs5/variables.html
collect <- get_acs(geography = "tract",
                   state = unique(countyList$st),
                   geometry = FALSE,
                   output = "wide",
                   variables = c(whtEdU <- "C15002A_001E",
                                 whtEdhsMal <- "C15002A_004E",
                                 whtEdhsFem <- "C15002A_009E",
                                 whtEdSmclMal <- "C15002A_005E",
                                 whtEdSmclFem <- "C15002A_010E",
                                 whtEdBachMal <- "C15002A_006E",
                                 whtEdBachFem <- "C15002A_011E",
                                 blkEdU <- "C15002B_001E",
                                 blkEdhsMal <- "C15002B_004E",
                                 blkEdhsFem <- "C15002B_009E",
                                 blkEdSmclMal <- "C15002B_005E",
                                 blkEdSmclFem <- "C15002B_010E",
                                 blkEdBachMal <- "C15002B_006E",
                                 blkEdBachFem <- "C15002B_011E"))

# Clean up fields
collect <- collect[, -( grep("\\M$" , colnames(collect), perl = TRUE))]
# Get state/county info so we can split df by MSA
collect$st <- substr(collect$GEOID, 1, 2)
collect$cty <- substr(collect$GEOID, 3, 5)
collect$st <- as.numeric(collect$st); collect$cty <- as.numeric(collect$cty)

fullCensus <- merge(collect, countyList, by = c("st", "cty"))

fullCensus$whtHS <- (fullCensus$C15002A_004E + fullCensus$C15002A_009E) / fullCensus$C15002A_001E * 100
fullCensus$whtSmcl <- (fullCensus$C15002A_005E + fullCensus$C15002A_010E) / fullCensus$C15002A_001E * 100
fullCensus$whtBach <- (fullCensus$C15002A_006E + fullCensus$C15002A_011E) / fullCensus$C15002A_001E * 100
fullCensus$blkHS <- (fullCensus$C15002B_004E + fullCensus$C15002B_009E) / fullCensus$C15002B_001E * 100
fullCensus$blkSmcl <- (fullCensus$C15002B_005E + fullCensus$C15002B_010E) / fullCensus$C15002B_001E * 100
fullCensus$blkBach <- (fullCensus$C15002B_006E + fullCensus$C15002B_011E) / fullCensus$C15002B_001E * 100
fullCensus$whtEdU <- fullCensus$C15002A_001E; fullCensus$blkEdU <- fullCensus$C15002B_001E

# Remove original estimates columns
fullCensus <- fullCensus[, !(grepl("^C15002A_\\d+" , names(fullCensus), perl = TRUE))]
fullCensus <- fullCensus[, !(grepl("^C15002B_\\d+" , names(fullCensus), perl = TRUE))]

listOfCities <- split(fullCensus, fullCensus$city)
for (i in 1:length(listOfCities)){
  myItem <- listOfCities[[i]]
  myCity <- myItem$city[[i]]
  write.csv(listOfCities[[i]], file = paste0("d",myCity,".csv"), row.names = FALSE)
}

setwd("C:/Users/alarson/Downloads/DallasCommutingV2-master/DallasCommutingV2-master/tractShps")
MSAshps <- as.data.frame(list.files(pattern = "\\.shp$")) # list all shapefiles
colnames(MSAshps) <- "id"; MSAshps$id <- as.character(MSAshps$id)
MSAshps <- MSAshps[grepl("res", MSAshps$id),] # keep only the MSA ones
# Got some to remove. yes it's ugly
drops <- c("res_BOS_MA1.shp", "res_BOS_MA2.shp", "res_CHA_NC1.shp",
           "res_CHA_NC2.shp", "res_CIN_OH1.shp", "res_CIN_OH2.shp",
           "res_CIN_OH3.shp", "res_KAN_MO1.shp", "res_KAN_MO2.shp",
           "res_LOU_KY1.shp", "res_LOU_KY2.shp", "res_MIN_MN1.shp",
           "res_MIN_MN2.shp", "res_NOR_VA1.shp", "res_NOR_VA2.shp",
           "res_POR_OR1.shp", "res_POR_OR2.shp", "res_STL_MO1.shp",
           "res_STL_MO2.shp") 
MSAshps <- as.data.frame(MSAshps); colnames(MSAshps) <- "id"
MSAshps <- subset(MSAshps, !(id %in% drops))
# Must remove .shp extension from end
MSAshps <- sub("\\.shp$", "", MSAshps$id) 

readShps <- lapply(MSAshps, shapefile)
datalist <- list()
for (i in 1:length(readShps)) {
  dat <- readShps[[i]]@data
  datalist[[i]] <- dat
}
datalist <- lapply(datalist, function(x) { x["layer"] <- NULL; x })
datalist <- lapply(datalist, function(x) { x["path"] <- NULL; x })
allJobs <- do.call(rbind, datalist)

allJobs <- allJobs[c(1,8,10)]

# Read in all Census datasets
setwd("C:/Users/alarson/Downloads/DallasCommutingV2-master/DallasCommutingV2-master/educationByRace")
MSAcsvs <- list.files(pattern = "*.csv")
datalist <- lapply(MSAcsvs, read.csv)
allCensus <- do.call(rbind, datalist)

# Read in all HOLC data
setwd("C:/Users/alarson/Downloads/DallasCommutingV2-master/DallasCommutingV2-master/housingWeightedScr")
MSAcsvs <- list.files(pattern = "*.csv")
datalist <- lapply(MSAcsvs, read.csv)
allHous <- do.call(rbind, datalist)

fullMerge <- merge(allCensus, allJobs, by = "GEOID")
fullMerge <- merge(fullMerge, allHous, by = "GEOID")

housOnly <- fullMerge[!is.na(fullMerge$quantScore),] # 393 obs

# NOW, among this study area, take a look at educational attainment.
# Does it hold steady?
housOnly$grade <- NA
housOnly$grade <- cut(housOnly$quantScore,
                      breaks = c(0, 1.5, 2.5, 3.5, 4.5),
                      labels = c("D", "C", "B", "A"))
byGrade <- split(housOnly, housOnly$grade)
for (i in 1:4){
  testDf <- byGrade[[i]]
  print(paste("Class", i, sep = " "))
  # print(length(testDf$GEOID))
  print(weighted.mean(testDf$whtHS, w = testDf$whtEdU))
  # print(mean(testDf$whtHS, na.rm=TRUE))
  print(weighted.mean(testDf$whtSmcl, w = testDf$whtEdU))
  # print(mean(testDf$whtSmcl, na.rm=TRUE))
  print(weighted.mean(testDf$whtBach, w = testDf$whtEdU))
  # print(mean(testDf$whtBach, na.rm=TRUE))
  print(weighted.mean(testDf$blkHS, w = testDf$blkEdU))
  # print(mean(testDf$blkHS, na.rm=TRUE))
  print(weighted.mean(testDf$blkSmcl, w = testDf$blkEdU))
  # print(mean(testDf$blkSmcl, na.rm=TRUE))
  print(weighted.mean(testDf$blkBach, w = testDf$blkEdU))
  # print(mean(testDf$blkBach, na.rm=TRUE))
}
