rm(list=ls())
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("foreign", "tidycensus", "tidyverse", "rgdal", "stringr")
pack(packages)
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

setwd("D:/AP LARSON/DallasCommutingV2/censusData")

# https://stackoverflow.com/questions/45109241/r-tidycensus-download-all-block-groups
# "tidycensus can't yet handle multi-state and multi-county calls simultaneously"
# https://api.census.gov/data/2016/acs/acs5/variables.html
collect <- get_acs(geography = "tract",
                   state = unique(countyList$st),
                   geometry = FALSE,
                   output = "wide",
                   variables = c(popData = "B01001_001E",
                                 incomeData = "B06011_001E",
                                 povUniverse = "B08122_001E",
                                 pov100 = "B08122_002E",
                                 pov149 = "B08122_003E",
                                 pov150 = "B08122_004E",
                                 racUniverse = "B02001_001E",
                                 racWhite = "B02001_002E",
                                 racBlack = "B02001_003E",
                                 racAsian = "B02001_005E",
                                 hispUniverse = "B03001_001E",
                                 hisp = "B03001_003E",
                                 medRent = "B25064_001E",
                                 # someday incl. hh size here,
                                 medValue = "B25077_001E",
                                 medAge = "B25035_001E",
                                 tenureUniverse = "B25003_001E",
                                 tenureOwn = "B25003_002E",
                                 comUniverse = "B08012_001E",
                                 com4 = "B08012_002E",
                                 com5 = "B08012_003E",
                                 com10 = "B08012_004E",
                                 com15 = "B08012_005E",
                                 com20 = "B08012_006E",
                                 com25 = "B08012_007E",
                                 com30 = "B08012_008E",
                                 com35 = "B08012_009E",
                                 com40 = "B08012_010E",
                                 com45 = "B08012_011E",
                                 com60 = "B08012_012E",
                                 com90 = "B08012_013E"))

# Clean up fields
collect <- collect[, -( grep("\\M$" , colnames(collect), perl = TRUE))]
# Get state/county info so we can split df by MSA
collect$st <- substr(collect$GEOID, 1, 2)
collect$cty <- substr(collect$GEOID, 3, 5)
collect$st <- as.numeric(collect$st); collect$cty <- as.numeric(collect$cty)

fullCensus <- merge(collect, countyList, by = c("st", "cty"))
# Some calculations can be done in full data frame
fullCensus$logInc <- log(fullCensus$incomeData)
fullCensus$thouInc <- fullCensus$incomeData / 1000
fullCensus$pct100 <- fullCensus$pov100 / fullCensus$povUniverse * 100
fullCensus$pct149 <- fullCensus$pov100 / fullCensus$povUniverse * 100
fullCensus$pct150 <- fullCensus$pov100 / fullCensus$povUniverse * 100
fullCensus$pctWht <- fullCensus$racWhite / fullCensus$racUniverse * 100
fullCensus$pctBlk <- fullCensus$racBlack / fullCensus$racUniverse * 100
fullCensus$pctAsn <- fullCensus$racAsian / fullCensus$racUniverse * 100
fullCensus$pctHisp <- fullCensus$hisp / fullCensus$hispUniverse * 100
fullCensus$pluWht <- NA
fullCensus$pluWht <- ifelse(fullCensus$pctWht > fullCensus$pctBlk &
                              fullCensus$pctWht > fullCensus$pctAsn &
                              fullCensus$pctWht > fullCensus$pctHisp,
                            1, 0)
fullCensus$pluBlk <- ifelse(fullCensus$pctBlk > fullCensus$pctWht &
                              fullCensus$pctBlk > fullCensus$pctAsn &
                              fullCensus$pctBlk > fullCensus$pctHisp,
                            1, 0)
fullCensus$pluAsn <- ifelse(fullCensus$pctAsn > fullCensus$pctBlk &
                              fullCensus$pctAsn > fullCensus$pctWht &
                              fullCensus$pctAsn > fullCensus$pctHisp,
                            1, 0)
fullCensus$pluHisp <- ifelse(fullCensus$pctHisp > fullCensus$pctBlk &
                               fullCensus$pctHisp > fullCensus$pctAsn &
                               fullCensus$pctHisp > fullCensus$pctWht,
                             1, 0)
fullCensus$logMedRent <- log(fullCensus$medRent)
fullCensus$hunMedRent <- fullCensus$medRent / 100
fullCensus$logHousVal <- log(fullCensus$medValue)
fullCensus$thouHousVal <- fullCensus$medValue / 1000
fullCensus$medAge <- 2018 - fullCensus$medAge
fullCensus$pctOwn <- fullCensus$tenureOwn / fullCensus$tenureUniverse * 100

fullCensus$comBl10 <- (fullCensus$com4 + fullCensus$com5) / fullCensus$comUniverse * 100
fullCensus$com10 <- (fullCensus$com10 + fullCensus$com15) / fullCensus$comUniverse * 100
fullCensus$com20 <- (fullCensus$com20 + fullCensus$com25) / fullCensus$comUniverse * 100
fullCensus$com30 <- (fullCensus$com30 + fullCensus$com35) / fullCensus$comUniverse * 100
fullCensus$com40 <- (fullCensus$com40 + fullCensus$com45) / fullCensus$comUniverse * 100
fullCensus$com60 <- (fullCensus$com60 + fullCensus$com90) / fullCensus$comUniverse * 100

# Remove unnecessary columns
excludeVars <- names(fullCensus) %in% c("povUniverse",
                                        "racUniverse",
                                        "racWhite",
                                        "racBlack",
                                        "racAsian",
                                        "hispUniverse",
                                        "hisp",
                                        "comUniverse",
                                        "com4",
                                        "com5",
                                        "com15",
                                        "com25",
                                        "com35",
                                        "com45",
                                        "com90",
                                        "NAME1",
                                        "GEOID1")
fullCensus <- fullCensus[!excludeVars]

# Remove rows where population is 0
fullCensus <- fullCensus[apply(fullCensus[c(5)], 1, function(i) ! any(i == 0)),]

listOfCities <- split(fullCensus, fullCensus$city)
for (i in 1:length(listOfCities)){
  myItem <- listOfCities[[i]]
  myCity <- myItem$city[[i]]
  write.csv(listOfCities[[i]], file = paste0("d",myCity,".csv"), row.names = FALSE)
}
