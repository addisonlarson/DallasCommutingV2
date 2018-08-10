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

mynames <- c("ALBANY-SCHENECTADY-TROY, NY",
             "ASHEVILLE, NC",
             "ATLANTA, GA",
             "ATLANTIC-CAPE MAY, NJ",
             "AUGUSTA-AIKEN, GA-SC",
             "BALTIMORE, MD",
             "BIRMINGHAM, AL",
             "BOSTON-WORCESTER-LAWRENCE-LOWELL-BROCKTON, MA",
             "BUFFALO-NIAGARA FALLS, NY",
             "CHARLOTTE-GASTONIA-ROCK HILL, NC-SC",
             "CLEVELAND-LORAIN-ELYRIA, OH",
             "COLUMBUS, OH",
             "DALLAS, TX",
             "DENVER, CO",
             "DULUTH-SUPERIOR, MN-WI",
             "ERIE, PA",
             "FRESNO, CA",
             "HARTFORD, CT",
             "INDIANAPOLIS, IN",
             "JACKSONVILLE, FL",
             "KANSAS CITY, MO-KS",
             "KNOXVILLE, TN",
             "LOS ANGELES-LONG BEACH, CA",
             "LYNCHBURG, VA",
             "MADISON, WI",
             "MIAMI, FL",
             "MINNEAPOLIS-ST. PAUL, MN-WI",
             "MOBILE, AL",
             "MONTGOMERY, AL",
             "NEW HAVEN-BRIDGEPORT-STAMFORD-WATERBURY-DANBU",
             "NEW ORLEANS, LA",
             "NEW YORK-NEWARK, NY-NJ-PA",
             "NORFOLK-VIRGINIA BEACH-NEWPORT NEWS, VA-NC",
             "PHILADELPHIA, PA-NJ",
             "PITTSBURGH, PA",
             "PORTLAND-VANCOUVER,OR-WA",
             "RICHMOND-PETERSBURG, VA",
             "ROANOKE, VA",
             "SACRAMENTO, CA",
             "SAN DIEGO, CA",
             "SAN FRANCISCO, CA",
             "SEATTLE-BELLEVUE-EVERETT, WA",
             "SPOKANE, WA",
             "SPRINGFIELD, IL",
             "SPRINGFIELD, MA",
             "SPRINGFIELD, MO",
             "ST. LOUIS, MO-IL",
             "SYRACUSE, NY",
             "TAMPA-ST. PETERSBURG-CLEARWATER, FL",
             "TRENTON, NJ",
             "WICHITA, KS"
)

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

meep <- split(countyList, countyList$city)
testDf1 <- NULL
testDf2 <- NULL

setwd("D:/AP LARSON/DallasCommutingV2/censusData")
for (i in 1){
  # for (i in 1:length(meep)){
  myItem <- meep[[i]]
  myCity <- myItem$city[[i]]
  stateList <- myItem$st
  countyList <- myItem$cty
  stCty <- as.data.frame(cbind(stateList, countyList))
  
  mycoollist <- list()
  
  # THIS IS PROBABLY WHERE YOU START AN INNER LOOP?????
  # or otherwise keep the state - county linked
  for (j in 1:length(stCty$stateList)){
    popData <- get_acs(geography = "tract",
                       variables = "B01001_001E",
                       state = stCty$stateList[[j]],
                       county = stCty$countyList[[j]],
                       geometry = FALSE)
    mycoollist[[j]] <- popData
    
    # MISSING POPULATION DENSITY. EASY TO PULL LAND AREA FROM ANYWHERE?
    # Percentage residents in different FPL categories
    # povUniverse <- get_acs(geography = "tract",
    #                        variables = "B08122_001E", # total obs
    #                        state = stCty$stateList[[j]],
    #                        county = stCty$countyList[[j]],
    #                        geometry = FALSE)
    # povUniverse2 <- rbind(testDf2, povUniverse)
    # testDf <- NULL
    # pov100 <- get_acs(geography = "tract",
    #                   variables = "B08122_002E", # obs below 100% FPL
    #                   state = stateList,
    #                   county = countyList,
    #                   geometry = FALSE)
    # # pov100 <- rbind(testDf, pov100)
    # # testDf <- NULL
    # pov149 <- get_acs(geography = "tract",
    #                   variables = "B08122_003E", # obs 100-149% FPL
    #                   state = stateList,
    #                   county = countyList,
    #                   geometry = FALSE)
    # # pov149 <- rbind(testDf, pov149)
    # # testDf <- NULL
    # pov150 <- get_acs(geography = "tract",
    #                   variables = "B08122_004E", # obs 150%+ FPL
    #                   state = stateList,
    #                   county = countyList,
    #                   geometry = FALSE)
    # # pov150 <- rbind(testDf, pov150)
    # # testDf <- NULL
    # colnames(povUniverse2)[4] <- "universe" # there has to be a better way to merge across datasets...
    # colnames(pov100)[4] <- "p100"
    # colnames(pov149)[4] <- "p149"
    # colnames(pov150)[4] <- "p150"
    # povData <- povUniverse2[c(1,4)]
    # povData <- merge(povUniverse, pov100, by = "GEOID")
    # povData <- povData[c(1,4,8)]
    # povData <- merge(povData, pov149, by = "GEOID")
    # povData <- povData[c(1:3,6)]
    # povData <- merge(povData, pov150, by = "GEOID")
    # povData <- povData[c(1:4,7)]
    # povData[povData == 0] <- NA; povData <- na.omit(povData)
    
    # Percent in each bracket
    # povData$pct100 <- povData$p100 / povData$universe * 100
    # povData$pct149 <- povData$p149 / povData$universe * 100
    # povData$pct150 <- povData$p150 / povData$universe * 100
    # povData <- povData[c(1,6:8)]
    
    # popData, povData, raceData, incomeData, tenureData, housSize, medianHousAge, medValue, medRent
  }
  fullDemographic <- do.call(rbind, mycoollist)
  # fullDemographic <- merge(popData, povData, by.x = "GEOID", by.y = "GEOID")
  write.csv(fullDemographic, file = paste0("d",myCity,".csv"), row.names = FALSE)
}
