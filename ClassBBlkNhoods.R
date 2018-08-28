rm(list=ls())
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("foreign", "tidycensus", "tidyverse", "rgdal", "raster", "e1071")
pack(packages)

setwd("D:/AP LARSON/DallasCommutingV2/tractShps")
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
setwd("D:/AP LARSON/DallasCommutingV2/censusData")
MSAcsvs <- list.files(pattern = "*.csv")
datalist <- lapply(MSAcsvs, read.csv)
allCensus <- do.call(rbind, datalist)

# Does education make a difference?
# Read in all Census ed datasets
setwd("D:/AP LARSON/DallasCommutingV2/educationByRace")
MSAcsvs <- list.files(pattern = "*.csv")
datalist <- lapply(MSAcsvs, read.csv)
edCensus <- do.call(rbind, datalist)

# Read in all HOLC data
setwd("D:/AP LARSON/DallasCommutingV2/housingWeightedScr")
MSAcsvs <- list.files(pattern = "*.csv")
datalist <- lapply(MSAcsvs, read.csv)
allHous <- do.call(rbind, datalist)

fullMerge <- merge(allCensus, allJobs, by = "GEOID")
fullMerge <- merge(fullMerge, edCensus, by = "GEOID")
fullMerge <- merge(fullMerge, allHous, by = "GEOID")

housOnly <- fullMerge[!is.na(fullMerge$quantScore),] # 393 obs

housOnly$grade <- NA
housOnly$grade <- cut(housOnly$quantScore,
                      breaks = c(0, 1.5, 2.5, 3.5, 4.5),
                      labels = c("D", "C", "B", "A"))
# housOnly <- subset(housOnly, grade == "B")
# housOnly <- subset(housOnly, pluBlk == 1)
# # It's Baltimore
# housOnly <- subset(housOnly, st == 24)
# # Write these to .csv and do good ol' fashioned spatial join
# setwd("D:/AP LARSON/DallasCommutingV2")
# write.csv(housOnly, file = "BaltimoreFocusTracts.csv", row.names = FALSE)

plot(density(housOnly$blkBach))
ed <- subset(housOnly, blkBach >= 50)
dp1 <- weighted.mean(ed$whtBach, ed$whtEdU)
dp2 <- weighted.mean(ed$blkBach, ed$blkEdU)
dp1 - dp2 # When weighted by obs, there's a 3.6% diff, which is small compared to rest

# How do incomes compare?
dp3 <- weighted.mean(ed$incomeData, ed$popData)
dp4 <- weighted.mean(housOnly$incomeData, housOnly$popData)
# Pretty substantial gain on overall sample

# What about the black-white income differential? Does it exist here?
edW <- subset(housOnly, whtBach >= 50)
dp5 <- weighted.mean(edW$incomeData, edW$popData)
# Offers something like hope

# And what about other things, like housing values and stuff?
mean(ed$thouHousVal, na.rm = TRUE); mean(edW$thouHousVal, na.rm = TRUE)
table(ed$grade) / length(ed$grade) * 100; table(edW$grade) / length(edW$grade) * 100
mean(ed$quantScore); mean(edW$quantScore)
weighted.mean(ed$pctOwn, ed$popData); weighted.mean(edW$pctOwn, edW$popData)
weighted.mean(ed$grapi, ed$popData); weighted.mean(edW$grapi, edW$popData, na.rm = TRUE)

# From here, you've got at least two descriptive analysis options.
# 1. Identify the vars important to you and pull them, but the race-subsetted version, for the 24 tracts.
# 2. Ehhhhh number 1 is better. As a backup, compare the 50+% blk bach to the 50+% wht bach

rm(list=ls())
setwd("D:/AP LARSON/DallasCommutingV2")
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

setwd("D:/AP LARSON/DallasCommutingV2/educationByRace")

# https://stackoverflow.com/questions/45109241/r-tidycensus-download-all-block-groups
# "tidycensus can't yet handle multi-state and multi-county calls simultaneously"
# https://api.census.gov/data/2016/acs/acs5/variables.html
collect <- get_acs(geography = "tract",
                   state = unique(countyList$st),
                   geometry = FALSE,
                   output = "wide",
                   variables = c(whtEdU = "C15002A_001E",
                                 whtEdhsMal = "C15002A_004E",
                                 whtEdhsFem = "C15002A_009E",
                                 whtEdSmclMal = "C15002A_005E",
                                 whtEdSmclFem = "C15002A_010E",
                                 whtEdBachMal = "C15002A_006E",
                                 whtEdBachFem = "C15002A_011E",
                                 blkEdU = "C15002B_001E",
                                 blkEdhsMal = "C15002B_004E",
                                 blkEdhsFem = "C15002B_009E",
                                 blkEdSmclMal = "C15002B_005E",
                                 blkEdSmclFem = "C15002B_010E",
                                 blkEdBachMal = "C15002B_006E",
                                 blkEdBachFem = "C15002B_011E",
                                 whtLabForceMal = "C23002A_004E",
                                 whtUnempMal = "C23002A_008E",
                                 whtLabForceFem = "C23002A_017E",
                                 whtUnempFem = "C23002A_021E",
                                 blkLabForceMal = "C23002B_004E",
                                 blkUnempMal = "C23002B_008E",
                                 blkLabForceFem = "C23002B_017E",
                                 blkUnempFem = "C23002B_021E",
                                 whtInc = "B19013A_001E",
                                 blkInc = "B19013B_001E",
                                 whtPovU = "B17020A_001E",
                                 whtBlPov = "B17020A_002E",
                                 blkPovU = "B17020B_001E",
                                 blkBlPov = "B17020B_002E",
                                 racWht = "B02001_002E",
                                 racBlk = "B02001_003E"))

# Clean up fields
collect <- collect[, -( grep("\\M$" , colnames(collect), perl = TRUE))]
# Get state/county info so we can split df by MSA
collect$st <- substr(collect$GEOID, 1, 2)
collect$cty <- substr(collect$GEOID, 3, 5)
collect$st <- as.numeric(collect$st); collect$cty <- as.numeric(collect$cty)

fullCensus <- merge(collect, countyList, by = c("st", "cty"))

fullCensus$whtHS <- (fullCensus$whtEdhsMal + fullCensus$whtEdhsFem) / fullCensus$whtEdU * 100
fullCensus$whtSmcl <- (fullCensus$whtEdSmclMal + fullCensus$whtEdSmclFem) / fullCensus$whtEdU * 100
fullCensus$whtBach <- (fullCensus$whtEdBachMal + fullCensus$whtEdBachFem) / fullCensus$whtEdU * 100
fullCensus$blkHS <- (fullCensus$blkEdhsMal + fullCensus$blkEdhsFem) / fullCensus$blkEdU * 100
fullCensus$blkSmcl <- (fullCensus$blkEdSmclMal + fullCensus$blkEdSmclFem) / fullCensus$blkEdU * 100
fullCensus$blkBach <- (fullCensus$blkEdBachMal + fullCensus$blkEdBachFem) / fullCensus$blkEdU * 100
fullCensus$whtUnemp <- (fullCensus$whtUnempMal + fullCensus$whtUnempFem) /
  (fullCensus$whtLabForceMal + fullCensus$whtLabForceFem) * 100
fullCensus$blkUnemp <- (fullCensus$blkUnempMal + fullCensus$blkUnempFem) /
  (fullCensus$blkLabForceMal + fullCensus$blkLabForceFem) * 100
fullCensus$whtPctBlPov <- fullCensus$whtBlPov / fullCensus$whtPovU * 100
fullCensus$blkPctBlPov <- fullCensus$blkBlPov / fullCensus$blkPovU * 100

# Remove original estimates columns
#fullCensus <- fullCensus[, !(grepl("^C15002A_\\d+" , names(fullCensus), perl = TRUE))]
#fullCensus <- fullCensus[, !(grepl("^C15002B_\\d+" , names(fullCensus), perl = TRUE))]

listOfCities <- split(fullCensus, fullCensus$city)
for (i in 1:length(listOfCities)){
  myItem <- listOfCities[[i]]
  myCity <- myItem$city[[i]]
  write.csv(listOfCities[[i]], file = paste0("d",myCity,".csv"), row.names = FALSE)
}

setwd("D:/AP LARSON/DallasCommutingV2/tractShps")
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
setwd("D:/AP LARSON/DallasCommutingV2/educationByRace")
MSAcsvs <- list.files(pattern = "*.csv")
datalist <- lapply(MSAcsvs, read.csv)
allCensus <- do.call(rbind, datalist)

# Read in all HOLC data
setwd("D:/AP LARSON/DallasCommutingV2/housingWeightedScr")
MSAcsvs <- list.files(pattern = "*.csv")
datalist <- lapply(MSAcsvs, read.csv)
allHous <- do.call(rbind, datalist)

fullMerge <- merge(allCensus, allJobs, by = "GEOID")
fullMerge <- merge(fullMerge, allHous, by = "GEOID")

housOnly <- fullMerge[!is.na(fullMerge$quantScore),] # 393 obs

ed <- subset(housOnly, blkBach >= 50)

# Now, among ed...
# Unemployment
weighted.mean(ed$whtUnemp, ed$racWht); weighted.mean(ed$blkUnemp, ed$racBlk)
kurtosis(ed$whtUnemp); kurtosis(ed$blkUnemp)
# this is not good
# Income
weighted.mean(ed$whtInc, ed$racWht); weighted.mean(ed$blkInc, ed$racBlk, na.rm = TRUE)
kurtosis(ed$whtInc); kurtosis(ed$blkInc, na.rm = TRUE)
# this is good
# Poverty
weighted.mean(ed$whtPctBlPov, ed$racWht); weighted.mean(ed$blkPctBlPov, ed$racBlk)
kurtosis(ed$whtPctBlPov); kurtosis(ed$blkPctBlPov)
# this is not good
# HS
weighted.mean(ed$whtHS, ed$whtEdU); weighted.mean(ed$blkHS, ed$blkEdU)
kurtosis(ed$whtHS); kurtosis(ed$blkHS)
# this looks okay to me
# Some college
weighted.mean(ed$whtSmcl, ed$whtEdU); weighted.mean(ed$blkSmcl, ed$blkEdU)
kurtosis(ed$whtSmcl); kurtosis(ed$blkSmcl) # like, identical kurtosis
# same here, okay
# Bach +
weighted.mean(ed$whtBach, ed$whtEdU); weighted.mean(ed$blkBach, ed$blkEdU)
kurtosis(ed$whtBach); kurtosis(ed$blkBach)
# Okay

# YOU COULD TEST FOR STATISTICAL DIFFERENCES. Probably should
