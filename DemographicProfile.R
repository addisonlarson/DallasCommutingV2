# Demographic Profile
rm(list=ls())
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("foreign", "tidycensus", "tidyverse", "rgdal", "raster")
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

# Read in all HOLC data
setwd("D:/AP LARSON/DallasCommutingV2/housingWeightedScr")
MSAcsvs <- list.files(pattern = "*.csv")
datalist <- lapply(MSAcsvs, read.csv)
allHous <- do.call(rbind, datalist)

fullMerge <- merge(allCensus, allJobs, by = "GEOID")
fullMerge <- merge(fullMerge, allHous, by = "GEOID")

housOnly <- fullMerge[!is.na(fullMerge$quantScore),] # 393 obs

housOnly$grade <- NA
housOnly$grade <- cut(housOnly$quantScore,
                      breaks = c(0, 1.5, 2.5, 3.5, 4.5),
                      labels = c("D", "C", "B", "A"))
byGrade <- split(housOnly, housOnly$grade)

for (i in 4:1){
  testDf <- byGrade[[i]]
  print(paste("Class", i, sep = " "))
  print(mean(testDf$popData, na.rm=TRUE))
  # print(mean(testDf$thouInc, na.rm=TRUE))
  print(weighted.mean(testDf$thouInc, testDf$popData, na.rm=TRUE))
  # print(mean(testDf$pct100, na.rm=TRUE))
  print(weighted.mean(testDf$pct100, testDf$popData, na.rm=TRUE))
  # print(mean(testDf$pct149, na.rm=TRUE))
  print(weighted.mean(testDf$pct149, testDf$popData, na.rm=TRUE))
  # print(mean(testDf$singParentHH, na.rm=TRUE))
  print(weighted.mean(testDf$singParentHH, testDf$popData, na.rm=TRUE))
  # print(mean(testDf$hunMedRent, na.rm=TRUE))
  print(weighted.mean(testDf$hunMedRent, testDf$popData, na.rm=TRUE))
  # print(mean(testDf$thouHousVal, na.rm=TRUE))
  print(weighted.mean(testDf$thouHousVal, testDf$popData, na.rm=TRUE))
  # print(mean(testDf$medAge, na.rm=TRUE))
  print(weighted.mean(testDf$medAge, testDf$popData, na.rm=TRUE))
  # print(mean(testDf$edHighSchool, na.rm=TRUE))
  print(weighted.mean(testDf$edHighSchool, testDf$popData, na.rm=TRUE))
  # print(mean(testDf$edSomeColl, na.rm=TRUE))
  print(weighted.mean(testDf$edSomeColl, testDf$popData, na.rm=TRUE))
  # print(mean(testDf$edBach, na.rm=TRUE))
  print(weighted.mean(testDf$edBach, testDf$popData, na.rm=TRUE))
  # print(mean(testDf$edGrad, na.rm=TRUE))
  print(weighted.mean(testDf$edGrad, testDf$popData, na.rm=TRUE))
  # print(mean(testDf$pctWht, na.rm=TRUE))
  print(weighted.mean(testDf$pctWht, testDf$popData, na.rm=TRUE))
  # print(mean(testDf$pctBlk, na.rm=TRUE))
  print(weighted.mean(testDf$pctBlk, testDf$popData, na.rm=TRUE))
  # print(mean(testDf$pctAsn, na.rm=TRUE))
  print(weighted.mean(testDf$pctAsn, testDf$popData, na.rm=TRUE))
  # print(mean(testDf$pctHisp, na.rm=TRUE))
  print(weighted.mean(testDf$pctHisp, testDf$popData, na.rm=TRUE))
  # print(mean(testDf$pluWht, na.rm=TRUE))
  # print(mean(testDf$pluBlk, na.rm=TRUE))
  # print(mean(testDf$pluAsn, na.rm=TRUE))
  # print(mean(testDf$pluHisp, na.rm=TRUE))
  # print(mean(testDf$pctUnemp, na.rm=TRUE))
  print(weighted.mean(testDf$pctUnemp, testDf$popData, na.rm=TRUE))
  # print(mean(testDf$pctOwn, na.rm=TRUE))
  print(weighted.mean(testDf$pctOwn, testDf$popData, na.rm=TRUE))
  # print(mean(testDf$jobs, na.rm=TRUE))
  print(weighted.mean(testDf$jobs, testDf$popData, na.rm=TRUE))
  # print(mean(testDf$zeroCar, na.rm=TRUE))
  print(weighted.mean(testDf$zeroCar, testDf$popData, na.rm=TRUE))
  # print(mean(testDf$grapi, na.rm=TRUE))
  print(weighted.mean(testDf$grapi, testDf$popData, na.rm=TRUE))
}
