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

# Get back the white and black raw numbers
fullMerge$blk <- fullMerge$pctBlk * fullMerge$popData
fullMerge$wht <- fullMerge$pctWht * fullMerge$popData
fullMerge$city <- as.character(fullMerge$city)
# Must drop San Jose and Sacramento: They don't exist in HOLC
fullMerge <- fullMerge[which(fullMerge$city != "SACRAMENTO, CA" &
                               fullMerge$city != "SAN JOSE, CA"), ]

housOnly <- fullMerge[!is.na(fullMerge$quantScore),] # 393 obs

# fullMerge is the entire MSA
# housOnly is just the area covered by HOLC
# Do index of dissimilarity for both

MSAsplit <- split(fullMerge, fullMerge$city)
housOnly <- split(housOnly, housOnly$city)

for (i in 1:17){
  mydf <- MSAsplit[[i]]
  totwht <- sum(mydf$wht)
  totblk <- sum(mydf$blk)
  mydf$newwht <- mydf$wht / totwht
  mydf$newblk <- mydf$blk / totblk
  mydf$absdiff <- abs(mydf$newwht - mydf$newblk)
  totdiff <- sum(mydf$absdiff) / 2
  print(unique(mydf$city))
  print(totdiff)
}

for (i in 1:17){
  mydf <- housOnly[[i]]
  totwht <- sum(mydf$wht)
  totblk <- sum(mydf$blk)
  mydf$newwht <- mydf$wht / totwht
  mydf$newblk <- mydf$blk / totblk
  mydf$absdiff <- abs(mydf$newwht - mydf$newblk)
  totdiff <- sum(mydf$absdiff) / 2
  print(unique(mydf$city))
  print(totdiff)
}

# What's up with Portland and Norfolk in HousOnly?
# They are both only one tract............
