# Goal here: combine information on commuting distance, HOLC, and demographic info
rm(list=ls())
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("foreign", "tidycensus", "tidyverse", "rgdal")
pack(packages)

setwd("D:/AP LARSON/DallasCommutingV2")
dem <- read.csv("fullDemographic.csv")
qualifiers <- read.csv("qualifyingTracts.csv")
holcIntersect <- readOGR(dsn = ".", layer = "HOLC_Dallas_overlay")
holcDf <- as.data.frame(holcIntersect)
length(unique(holcDf$GEOID)) # only 122 unique ids. Means that you need to collapse?

holcDf$quantScore <- NA
holcDf$quantScore <- ifelse(holcDf$holc_grade == "A", 4, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "B", 3, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "C", 2, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "D", 1, holcDf$quantScore)
resGEOID <- aggregate(holcDf$quantScore, by = list(holcDf$GEOID), FUN = mean)
colnames(resGEOID) <- c("GEOID", "quantScore")

holcID <- merge(holcDf, resGEOID, by = "GEOID")
holcID <- holcID[!duplicated(holcID$GEOID), ]

s000 <- read.csv("S000dirdist.csv"); s000 <- s000[c(1,3)]; colnames(s000)[2] <- "s000dist"
se01 <- read.csv("SE01dirdist.csv"); se01 <- se01[c(1,3)]; colnames(se01)[2] <- "se01dist"
se03 <- read.csv("SE03dirdist.csv"); se03 <- se03[c(1,3)]; colnames(se03)[2] <- "se03dist"

fullData <- merge(dem, holcID, by = "GEOID")
fullData <- merge(fullData, s000, by.x = "GEOID", by.y = "idnum")
fullData <- merge(fullData, se01, by.x = "GEOID", by.y = "idnum")
fullData <- merge(fullData, se03, by.x = "GEOID", by.y = "idnum")

testModel <- lm(s000dist ~ logPopDens + pct100 + pluWht + pluBlk + pluAsn + pluHisp +
                  loginc + pctRent + meanHHsz + housAge + logHousVal + logMedianRent +
                  quantScore.y, data = fullData)
summary(testModel)

testModel2 <- lm(s000dist ~ logPopDens + pct100 + pluWht + pluBlk + loginc +
                   pctRent + logHousVal + quantScore.y, data = fullData)
summary(testModel2)

fullData$logPctRent <- log(fullData$pctRent)
testModel3 <- lm(s000dist ~ logPopDens + pct100 + pluWht + pluBlk + loginc +
                   logPctRent + logHousVal + quantScore.y, data = fullData)
summary(testModel3) # A winner!
