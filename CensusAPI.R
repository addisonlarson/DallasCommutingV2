rm(list=ls())
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("foreign", "tidycensus", "tidyverse", "rgdal")
pack(packages)
setwd("D:/AP LARSON/DallasCommuting")
# census_api_key("2f424d72af52408f49ed24e0608d6f5c583af625", install = TRUE)
countyList <- c(113, 321, 251, 425, 367, 379, 467,
                181, 139, 439, 257, 349, 497, 213,
                217, 85, 147, 97, 121, 397)
popData <- get_acs(geography = "tract",
                   variables = "B01001_001E",
                   state = "TX",
                   county = countyList,
                   geometry = FALSE)
# Population density
tx <- readOGR(dsn = ".", layer = "tl_2015_48_tract")
txdf <- as.data.frame(tx)
txdf$ALAND <- as.numeric(as.character(txdf$ALAND))
txdf$landArea <- txdf$ALAND * 3.861022e-07 # Convert sq meters to sq miles
txdf <- txdf[c(4,13)]
popData <- merge(popData, txdf, by = "GEOID")
popData$popDens <- popData$estimate / popData$landArea
popData <- popData[c(1,4,6,7)]
popData[popData == 0] <- NA; popData <- na.omit(popData)
colnames(popData)[2] <- "pop"
popData$pop1000 <- popData$pop / 1000 # Population, thousands
popData$popDens1000 <- popData$popDens / 1000 # Population density, 1000s/sq mi
popData$logPopDens <- log(popData$popDens) # Log density, population / sq mi

# Log median income
incomeData <- get_acs(geography = "tract",
                      variables = "B06011_001E",
                      state = "TX",
                      county = countyList,
                      geometry = FALSE)
incomeData$loginc <- log(incomeData$estimate)
incomeData <- incomeData[c(1,6)]

# Percentage residents in different FPL categories
povUniverse <- get_acs(geography = "tract",
                       variables = "B08122_001E", # total obs
                       state = "TX",
                       county = countyList,
                       geometry = FALSE)
pov100 <- get_acs(geography = "tract",
                  variables = "B08122_002E", # obs below 100% FPL
                  state = "TX",
                  county = countyList,
                  geometry = FALSE)
pov149 <- get_acs(geography = "tract",
                  variables = "B08122_003E", # obs 100-149% FPL
                  state = "TX",
                  county = countyList,
                  geometry = FALSE)
pov150 <- get_acs(geography = "tract",
                  variables = "B08122_004E", # obs 150%+ FPL
                  state = "TX",
                  county = countyList,
                  geometry = FALSE)
colnames(povUniverse)[4] <- "universe" # there has to be a better way to merge across datasets...
colnames(pov100)[4] <- "p100"
colnames(pov149)[4] <- "p149"
colnames(pov150)[4] <- "p150"
povData <- merge(povUniverse, pov100, by = "GEOID")
povData <- povData[c(1,4,8)]
povData <- merge(povData, pov149, by = "GEOID")
povData <- povData[c(1:3,6)]
povData <- merge(povData, pov150, by = "GEOID")
povData <- povData[c(1:4,7)]
povData[povData == 0] <- NA; povData <- na.omit(povData)

# Percent in each bracket
povData$pct100 <- povData$p100 / povData$universe * 100
povData$pct149 <- povData$p149 / povData$universe * 100
povData$pct150 <- povData$p150 / povData$universe * 100
povData <- povData[c(1,6:8)]

# Median rent
# Feeling impatient.
# Table was B25064, median gross rent

# Mean household size
setwd("D:/AP LARSON/wtf/for git") # there is a story as to why this file is called wtf
# I messed up a GitHub push and deleted all the work below...which was already done once
hhSize <- read.csv("ACS_16_5YR_S1101_with_ann.csv")
hhSize <- hhSize[c(2,4)]
colnames(hhSize) < - c("GEOID", "medHHsize")

# Mean home value
# Feeling impatient.
# Table was B25077, median home value

# Median year structure built
medianAge <- get_acs(geography = "tract",
                     variables = "B25035_001E",
                     state = "TX",
                     county = countyList,
                     geometry = FALSE)
medianAge$estimate <- 2018 - medianAge$estimate
medianAge <- medianAge[c(1,4)]
colnames(medianAge)[2] <- "medAge"
medianAge <- medianAge[complete.cases(medianAge),]

# Tenure
tenureUniverse <- get_acs(geography = "tract",
                          variables = "B25003_001E",
                          state = "TX",
                          county = countyList,
                          geometry = FALSE)
tenureOwn <- get_acs(geography = "tract",
                     variables = "B25003_002E",
                     state = "TX",
                     county = countyList,
                     geometry = FALSE)
colnames(tenureUniverse)[4] <- "universe" # there has to be a better way to merge across datasets...
colnames(tenureOwn)[4] <- "own"
tenureData <- merge(tenureUniverse, tenureOwn, by = "GEOID")
tenureData <- tenureData[c(1,4,8)]
tenureData$pctOwn <- tenureData$own / tenureData$universe * 100
tenureData <- tenureData[c(1,4)]

# Race/ethnicity
# Feeling impatient.
# Tables were B02001 (Race) and B03001 (Hispanic or Latino origin)
# Adopted a plurality rule: Boolean 1-0 for largest
# Racial or ethnic group in dataset.

# All data was exported into fullDemographic.csv
