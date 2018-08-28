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

housOnly$pluWht <- ifelse(housOnly$pctWht > housOnly$pctBlk &
                            housOnly$pctWht > housOnly$pctAsn, 1, 0)
dWht <- subset(housOnly, grade == "D" & pluWht == 1)
dBlk <- subset(housOnly, grade == "D" & pluBlk == 1)

# Among D tracts, white plurality vs. Black plurality...
weighted.mean(dWht$incomeData, dWht$popData); weighted.mean(dBlk$incomeData, dBlk$popData)
weighted.mean(dWht$pctUnemp, dWht$popData); weighted.mean(dBlk$pctUnemp, dBlk$popData, na.rm = TRUE)
weighted.mean(dWht$pct100, dWht$popData); weighted.mean(dBlk$pct100, dBlk$popData, na.rm = TRUE)
weighted.mean(dWht$pct149, dWht$popData); weighted.mean(dBlk$pct149, dBlk$popData, na.rm = TRUE)
weighted.mean(dWht$singParentHH, dWht$popData); weighted.mean(dBlk$singParentHH, dBlk$popData, na.rm = TRUE)
weighted.mean(dWht$edHighSchool, dWht$popData); weighted.mean(dBlk$edHighSchool, dBlk$popData, na.rm = TRUE)
weighted.mean(dWht$edSomeColl, dWht$popData); weighted.mean(dBlk$edSomeColl, dBlk$popData, na.rm = TRUE)
weighted.mean(dWht$edBach, dWht$popData); weighted.mean(dBlk$edBach, dBlk$popData, na.rm = TRUE)
weighted.mean(dWht$edGrad, dWht$popData); weighted.mean(dBlk$edGrad, dBlk$popData, na.rm = TRUE)
weighted.mean(dWht$jobs, dWht$popData); weighted.mean(dBlk$jobs, dBlk$popData, na.rm = TRUE)
weighted.mean(dWht$zeroCar, dWht$popData); weighted.mean(dBlk$zeroCar, dBlk$popData, na.rm = TRUE)
mean(dWht$thouHousVal, na.rm = TRUE); mean(dBlk$thouHousVal, na.rm = TRUE)
weighted.mean(dWht$grapi, dWht$popData); weighted.mean(dBlk$grapi, dBlk$popData, na.rm = TRUE)
# rental burdens similar
weighted.mean(dWht$hunMedRent, dWht$popData); weighted.mean(dBlk$hunMedRent, dBlk$popData, na.rm = TRUE)
# but rents different
weighted.mean(dWht$pctOwn, dWht$popData); weighted.mean(dBlk$pctOwn, dBlk$popData, na.rm = TRUE)
# ownership patterns similar

# BUT WHERE ARE THESE TRACTS?
todayW <- as.data.frame(table(as.character(dWht$city.x)))
todayB <- as.data.frame(table(as.character(dBlk$city.x)))
sumFreqW <- sum(todayW$Freq)
todayW$pct <- todayW$Freq / sumFreqW * 100
sumFreqB <- sum(todayB$Freq)
todayB$pct <- todayB$Freq / sumFreqB * 100
