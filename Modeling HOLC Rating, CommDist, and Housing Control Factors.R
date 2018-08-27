# Goal here: combine information on commuting distance, HOLC, and demographic info
rm(list=ls())
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("foreign", "tidycensus", "tidyverse", "rgdal", "car")
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

# NEW
# Read in all shapefiles, collapse into a single data frame, OLS
rm(list=ls())
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("foreign", "tidycensus", "tidyverse", "rgdal", "raster", "car")
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

# SET UP FOR PANEL WOOT!
namevector <- as.character(unique(housOnly$city))
for (i in namevector){
  housOnly[,namevector] <- NA
}
for (i in 1:length(namevector)){
  housOnly[i + 60] <- ifelse(housOnly$city == namevector[[i]], 1, 0)
}

# add city boolean as controls
housingValue <- lm(thouHousVal ~ quantScore +
                     `BIRMINGHAM, AL` +
                     `SAN DIEGO, CA` +
                     `TAMPA-ST. PETERSBURG-CLEARWATER, FL` +
                     `ATLANTA, GA` +
                     `ST. LOUIS, MO-IL` +
                     `INDIANAPOLIS, IN` +
                     `KANSAS CITY, MO-KS` +
                     `LOUISVILLE, KY-IN` +
                     `NEW ORLEANS, LA` +
                     `BALTIMORE, MD` +
                     `MINNEAPOLIS-ST. PAUL, MN-WI` +
                     `BUFFALO-NIAGARA FALLS, NY` +
                     `CHARLOTTE-GASTONIA-ROCK HILL, NC-SC` +
                     `COLUMBUS, OH` +
                     `PORTLAND-VANCOUVER,OR-WA` +
                     `PITTSBURGH, PA` +
                     # `NORFOLK-VIRGINIA BEACH-NEWPORT NEWS, VA-NC` +
                     bed0 +
                     bed1 +
                     bed2 +
                     bed3 +
                     bed4 +
                     medAge +
                     completePlumb +
                     completeKitch, data = housOnly)
summary(housingValue)

myCols <- c("quantScore", "thouHousVal", "bed0", "bed1",
            "bed2", "bed3", "bed4", "medAge", "completePlumb", "completeKitch")
testCase <- housOnly[myCols]
cor(testCase, method = "pearson", use = "complete.obs")
vif(housingValue)

tenure <- lm(pctOwn ~ quantScore +
               `BIRMINGHAM, AL` +
               `SAN DIEGO, CA` +
               `TAMPA-ST. PETERSBURG-CLEARWATER, FL` +
               `ATLANTA, GA` +
               `ST. LOUIS, MO-IL` +
               `INDIANAPOLIS, IN` +
               `KANSAS CITY, MO-KS` +
               `LOUISVILLE, KY-IN` +
               `NEW ORLEANS, LA` +
               `BALTIMORE, MD` +
               `MINNEAPOLIS-ST. PAUL, MN-WI` +
               `BUFFALO-NIAGARA FALLS, NY` +
               `CHARLOTTE-GASTONIA-ROCK HILL, NC-SC` +
               `COLUMBUS, OH` +
               `PORTLAND-VANCOUVER,OR-WA` +
               `PITTSBURGH, PA`,
             # `NORFOLK-VIRGINIA BEACH-NEWPORT NEWS, VA-NC`,
             data = housOnly)
summary(tenure)
vif(tenure)

housOnly$thouJobs <- housOnly$jobs / 1000
jobAccess <- lm(thouJobs ~ quantScore +
                  `BIRMINGHAM, AL` +
                  `SAN DIEGO, CA` +
                  `TAMPA-ST. PETERSBURG-CLEARWATER, FL` +
                  `ATLANTA, GA` +
                  `ST. LOUIS, MO-IL` +
                  `INDIANAPOLIS, IN` +
                  `KANSAS CITY, MO-KS` +
                  `LOUISVILLE, KY-IN` +
                  `NEW ORLEANS, LA` +
                  `BALTIMORE, MD` +
                  `MINNEAPOLIS-ST. PAUL, MN-WI` +
                  `BUFFALO-NIAGARA FALLS, NY` +
                  `CHARLOTTE-GASTONIA-ROCK HILL, NC-SC` +
                  `COLUMBUS, OH` +
                  `PORTLAND-VANCOUVER,OR-WA` +
                  `PITTSBURGH, PA` +
                  `NORFOLK-VIRGINIA BEACH-NEWPORT NEWS, VA-NC`, data = housOnly)
summary(jobAccess)

income <- lm(thouInc ~ quantScore +
               `BIRMINGHAM, AL` +
               `SAN DIEGO, CA` +
               `TAMPA-ST. PETERSBURG-CLEARWATER, FL` +
               `ATLANTA, GA` +
               `ST. LOUIS, MO-IL` +
               `INDIANAPOLIS, IN` +
               `KANSAS CITY, MO-KS` +
               `LOUISVILLE, KY-IN` +
               `NEW ORLEANS, LA` +
               `BALTIMORE, MD` +
               `MINNEAPOLIS-ST. PAUL, MN-WI` +
               `BUFFALO-NIAGARA FALLS, NY` +
               `CHARLOTTE-GASTONIA-ROCK HILL, NC-SC` +
               `COLUMBUS, OH` +
               `PORTLAND-VANCOUVER,OR-WA` +
               `PITTSBURGH, PA` +
               `NORFOLK-VIRGINIA BEACH-NEWPORT NEWS, VA-NC` +
               edHighSchool, data = housOnly)
summary(income)

myCols <- c("quantScore", "thouInc", "edHighSchool",
            "edSomeColl", "edBach", "edGrad")
testCase <- housOnly[myCols]
cor(testCase, method = "pearson", use = "complete.obs")

unemp <- lm(pctUnemp ~ quantScore +
              `BIRMINGHAM, AL` +
              `SAN DIEGO, CA` +
              `TAMPA-ST. PETERSBURG-CLEARWATER, FL` +
              `ATLANTA, GA` +
              `ST. LOUIS, MO-IL` +
              `INDIANAPOLIS, IN` +
              `KANSAS CITY, MO-KS` +
              `LOUISVILLE, KY-IN` +
              `NEW ORLEANS, LA` +
              `BALTIMORE, MD` +
              `MINNEAPOLIS-ST. PAUL, MN-WI` +
              `BUFFALO-NIAGARA FALLS, NY` +
              `CHARLOTTE-GASTONIA-ROCK HILL, NC-SC` +
              `COLUMBUS, OH` +
              `PORTLAND-VANCOUVER,OR-WA` +
              `PITTSBURGH, PA` +
              `NORFOLK-VIRGINIA BEACH-NEWPORT NEWS, VA-NC` +
              edHighSchool, data = housOnly)
print(summary(unemp), digits = 3)

myCols <- c("quantScore", "pctUnemp", "edHighSchool",
            "edSomeColl", "edBach", "edGrad")
testCase <- housOnly[myCols]
cor(testCase, method = "pearson", use = "complete.obs")

zeroCar <- lm(zeroCar ~ quantScore +
                `BIRMINGHAM, AL` +
                `SAN DIEGO, CA` +
                `TAMPA-ST. PETERSBURG-CLEARWATER, FL` +
                `ATLANTA, GA` +
                `ST. LOUIS, MO-IL` +
                `INDIANAPOLIS, IN` +
                `KANSAS CITY, MO-KS` +
                `LOUISVILLE, KY-IN` +
                `NEW ORLEANS, LA` +
                `BALTIMORE, MD` +
                `MINNEAPOLIS-ST. PAUL, MN-WI` +
                `BUFFALO-NIAGARA FALLS, NY` +
                `CHARLOTTE-GASTONIA-ROCK HILL, NC-SC` +
                `COLUMBUS, OH` +
                `PORTLAND-VANCOUVER,OR-WA` +
                `PITTSBURGH, PA` +
                `NORFOLK-VIRGINIA BEACH-NEWPORT NEWS, VA-NC`, data = housOnly)
print(summary(zeroCar), digits = 3)

myCols <- c("zeroCar", "incomeData")
testCase <- housOnly[myCols]
cor(testCase, method = "pearson", use = "complete.obs")

singParent <- lm(singParentHH ~ quantScore +
                   `BIRMINGHAM, AL` +
                   `SAN DIEGO, CA` +
                   `TAMPA-ST. PETERSBURG-CLEARWATER, FL` +
                   `ATLANTA, GA` +
                   `ST. LOUIS, MO-IL` +
                   `INDIANAPOLIS, IN` +
                   `KANSAS CITY, MO-KS` +
                   `LOUISVILLE, KY-IN` +
                   `NEW ORLEANS, LA` +
                   `BALTIMORE, MD` +
                   `MINNEAPOLIS-ST. PAUL, MN-WI` +
                   `BUFFALO-NIAGARA FALLS, NY` +
                   `CHARLOTTE-GASTONIA-ROCK HILL, NC-SC` +
                   `COLUMBUS, OH` +
                   `PORTLAND-VANCOUVER,OR-WA` +
                   `PITTSBURGH, PA` +
                   `NORFOLK-VIRGINIA BEACH-NEWPORT NEWS, VA-NC`, data = housOnly)
summary(singParent)

comBl10 <- lm(comBl10 ~ quantScore +
                `BIRMINGHAM, AL` +
                `SAN DIEGO, CA` +
                `TAMPA-ST. PETERSBURG-CLEARWATER, FL` +
                `ATLANTA, GA` +
                `ST. LOUIS, MO-IL` +
                `INDIANAPOLIS, IN` +
                `KANSAS CITY, MO-KS` +
                `LOUISVILLE, KY-IN` +
                `NEW ORLEANS, LA` +
                `BALTIMORE, MD` +
                `MINNEAPOLIS-ST. PAUL, MN-WI` +
                `BUFFALO-NIAGARA FALLS, NY` +
                `CHARLOTTE-GASTONIA-ROCK HILL, NC-SC` +
                `COLUMBUS, OH` +
                `PORTLAND-VANCOUVER,OR-WA` +
                `PITTSBURGH, PA` +
                `NORFOLK-VIRGINIA BEACH-NEWPORT NEWS, VA-NC`, data = housOnly)
summary(comBl10)

com10 <- lm(com10 ~ quantScore +
              `BIRMINGHAM, AL` +
              `SAN DIEGO, CA` +
              `TAMPA-ST. PETERSBURG-CLEARWATER, FL` +
              `ATLANTA, GA` +
              `ST. LOUIS, MO-IL` +
              `INDIANAPOLIS, IN` +
              `KANSAS CITY, MO-KS` +
              `LOUISVILLE, KY-IN` +
              `NEW ORLEANS, LA` +
              `BALTIMORE, MD` +
              `MINNEAPOLIS-ST. PAUL, MN-WI` +
              `BUFFALO-NIAGARA FALLS, NY` +
              `CHARLOTTE-GASTONIA-ROCK HILL, NC-SC` +
              `COLUMBUS, OH` +
              `PORTLAND-VANCOUVER,OR-WA` +
              `PITTSBURGH, PA` +
              `NORFOLK-VIRGINIA BEACH-NEWPORT NEWS, VA-NC`, data = housOnly)
summary(com10)

com20 <- lm(com20 ~ quantScore +
              `BIRMINGHAM, AL` +
              `SAN DIEGO, CA` +
              `TAMPA-ST. PETERSBURG-CLEARWATER, FL` +
              `ATLANTA, GA` +
              `ST. LOUIS, MO-IL` +
              `INDIANAPOLIS, IN` +
              `KANSAS CITY, MO-KS` +
              `LOUISVILLE, KY-IN` +
              `NEW ORLEANS, LA` +
              `BALTIMORE, MD` +
              `MINNEAPOLIS-ST. PAUL, MN-WI` +
              `BUFFALO-NIAGARA FALLS, NY` +
              `CHARLOTTE-GASTONIA-ROCK HILL, NC-SC` +
              `COLUMBUS, OH` +
              `PORTLAND-VANCOUVER,OR-WA` +
              `PITTSBURGH, PA` +
              `NORFOLK-VIRGINIA BEACH-NEWPORT NEWS, VA-NC`, data = housOnly)
summary(com20)

com30 <- lm(com30 ~ quantScore +
              `BIRMINGHAM, AL` +
              `SAN DIEGO, CA` +
              `TAMPA-ST. PETERSBURG-CLEARWATER, FL` +
              `ATLANTA, GA` +
              `ST. LOUIS, MO-IL` +
              `INDIANAPOLIS, IN` +
              `KANSAS CITY, MO-KS` +
              `LOUISVILLE, KY-IN` +
              `NEW ORLEANS, LA` +
              `BALTIMORE, MD` +
              `MINNEAPOLIS-ST. PAUL, MN-WI` +
              `BUFFALO-NIAGARA FALLS, NY` +
              `CHARLOTTE-GASTONIA-ROCK HILL, NC-SC` +
              `COLUMBUS, OH` +
              `PORTLAND-VANCOUVER,OR-WA` +
              `PITTSBURGH, PA` +
              `NORFOLK-VIRGINIA BEACH-NEWPORT NEWS, VA-NC`, data = housOnly)
summary(com30)

com40 <- lm(com40 ~ quantScore +
              `BIRMINGHAM, AL` +
              `SAN DIEGO, CA` +
              `TAMPA-ST. PETERSBURG-CLEARWATER, FL` +
              `ATLANTA, GA` +
              `ST. LOUIS, MO-IL` +
              `INDIANAPOLIS, IN` +
              `KANSAS CITY, MO-KS` +
              `LOUISVILLE, KY-IN` +
              `NEW ORLEANS, LA` +
              `BALTIMORE, MD` +
              `MINNEAPOLIS-ST. PAUL, MN-WI` +
              `BUFFALO-NIAGARA FALLS, NY` +
              `CHARLOTTE-GASTONIA-ROCK HILL, NC-SC` +
              `COLUMBUS, OH` +
              `PORTLAND-VANCOUVER,OR-WA` +
              `PITTSBURGH, PA` +
              `NORFOLK-VIRGINIA BEACH-NEWPORT NEWS, VA-NC`, data = housOnly)
summary(com40)

com60 <- lm(com60 ~ quantScore +
              `BIRMINGHAM, AL` +
              `SAN DIEGO, CA` +
              `TAMPA-ST. PETERSBURG-CLEARWATER, FL` +
              `ATLANTA, GA` +
              `ST. LOUIS, MO-IL` +
              `INDIANAPOLIS, IN` +
              `KANSAS CITY, MO-KS` +
              `LOUISVILLE, KY-IN` +
              `NEW ORLEANS, LA` +
              `BALTIMORE, MD` +
              `MINNEAPOLIS-ST. PAUL, MN-WI` +
              `BUFFALO-NIAGARA FALLS, NY` +
              `CHARLOTTE-GASTONIA-ROCK HILL, NC-SC` +
              `COLUMBUS, OH` +
              `PORTLAND-VANCOUVER,OR-WA` +
              `PITTSBURGH, PA` +
              `NORFOLK-VIRGINIA BEACH-NEWPORT NEWS, VA-NC`, data = housOnly)
summary(com60)

housOnly$allBl149 <- housOnly$pct100 + housOnly$pct149
poverty <- lm(allBl149 ~ quantScore +
                `BIRMINGHAM, AL` +
                `SAN DIEGO, CA` +
                `TAMPA-ST. PETERSBURG-CLEARWATER, FL` +
                `ATLANTA, GA` +
                `ST. LOUIS, MO-IL` +
                `INDIANAPOLIS, IN` +
                `KANSAS CITY, MO-KS` +
                `LOUISVILLE, KY-IN` +
                `NEW ORLEANS, LA` +
                `BALTIMORE, MD` +
                `MINNEAPOLIS-ST. PAUL, MN-WI` +
                `BUFFALO-NIAGARA FALLS, NY` +
                `CHARLOTTE-GASTONIA-ROCK HILL, NC-SC` +
                `COLUMBUS, OH` +
                `PORTLAND-VANCOUVER,OR-WA` +
                `PITTSBURGH, PA` +
                `NORFOLK-VIRGINIA BEACH-NEWPORT NEWS, VA-NC`, data = housOnly)
print(summary(poverty), digits = 3)

deepPoverty <- lm(pct100 ~ quantScore +
                    `BIRMINGHAM, AL` +
                    `SAN DIEGO, CA` +
                    `TAMPA-ST. PETERSBURG-CLEARWATER, FL` +
                    `ATLANTA, GA` +
                    `ST. LOUIS, MO-IL` +
                    `INDIANAPOLIS, IN` +
                    `KANSAS CITY, MO-KS` +
                    `LOUISVILLE, KY-IN` +
                    `NEW ORLEANS, LA` +
                    `BALTIMORE, MD` +
                    `MINNEAPOLIS-ST. PAUL, MN-WI` +
                    `BUFFALO-NIAGARA FALLS, NY` +
                    `CHARLOTTE-GASTONIA-ROCK HILL, NC-SC` +
                    `COLUMBUS, OH` +
                    `PORTLAND-VANCOUVER,OR-WA` +
                    `PITTSBURGH, PA` +
                    `NORFOLK-VIRGINIA BEACH-NEWPORT NEWS, VA-NC`, data = housOnly)
print(summary(deepPoverty), digits = 3)

nWht <- lm(pctWht ~ quantScore +
             `BIRMINGHAM, AL` +
             `SAN DIEGO, CA` +
             `TAMPA-ST. PETERSBURG-CLEARWATER, FL` +
             `ATLANTA, GA` +
             `ST. LOUIS, MO-IL` +
             `INDIANAPOLIS, IN` +
             `KANSAS CITY, MO-KS` +
             `LOUISVILLE, KY-IN` +
             `NEW ORLEANS, LA` +
             `BALTIMORE, MD` +
             `MINNEAPOLIS-ST. PAUL, MN-WI` +
             `BUFFALO-NIAGARA FALLS, NY` +
             `CHARLOTTE-GASTONIA-ROCK HILL, NC-SC` +
             `COLUMBUS, OH` +
             `PORTLAND-VANCOUVER,OR-WA` +
             `PITTSBURGH, PA` +
             `NORFOLK-VIRGINIA BEACH-NEWPORT NEWS, VA-NC`, data = housOnly)
print(summary(nWht), digits = 4)

nBlk <- lm(pctBlk ~ quantScore +
             `BIRMINGHAM, AL` +
             `SAN DIEGO, CA` +
             `TAMPA-ST. PETERSBURG-CLEARWATER, FL` +
             `ATLANTA, GA` +
             `ST. LOUIS, MO-IL` +
             `INDIANAPOLIS, IN` +
             `KANSAS CITY, MO-KS` +
             `LOUISVILLE, KY-IN` +
             `NEW ORLEANS, LA` +
             `BALTIMORE, MD` +
             `MINNEAPOLIS-ST. PAUL, MN-WI` +
             `BUFFALO-NIAGARA FALLS, NY` +
             `CHARLOTTE-GASTONIA-ROCK HILL, NC-SC` +
             `COLUMBUS, OH` +
             `PORTLAND-VANCOUVER,OR-WA` +
             `PITTSBURGH, PA` +
             `NORFOLK-VIRGINIA BEACH-NEWPORT NEWS, VA-NC`, data = housOnly)
print(summary(nBlk), digits = 4)

nHisp <- lm(pctHisp ~ quantScore +
              `BIRMINGHAM, AL` +
              `SAN DIEGO, CA` +
              `TAMPA-ST. PETERSBURG-CLEARWATER, FL` +
              `ATLANTA, GA` +
              `ST. LOUIS, MO-IL` +
              `INDIANAPOLIS, IN` +
              `KANSAS CITY, MO-KS` +
              `LOUISVILLE, KY-IN` +
              `NEW ORLEANS, LA` +
              `BALTIMORE, MD` +
              `MINNEAPOLIS-ST. PAUL, MN-WI` +
              `BUFFALO-NIAGARA FALLS, NY` +
              `CHARLOTTE-GASTONIA-ROCK HILL, NC-SC` +
              `COLUMBUS, OH` +
              `PORTLAND-VANCOUVER,OR-WA` +
              `PITTSBURGH, PA` +
              `NORFOLK-VIRGINIA BEACH-NEWPORT NEWS, VA-NC`, data = housOnly)
print(summary(nHisp), digits = 4)

rentCost <- lm(hunMedRent ~ quantScore +
                 `BIRMINGHAM, AL` +
                 `SAN DIEGO, CA` +
                 `TAMPA-ST. PETERSBURG-CLEARWATER, FL` +
                 `ATLANTA, GA` +
                 `ST. LOUIS, MO-IL` +
                 `INDIANAPOLIS, IN` +
                 `KANSAS CITY, MO-KS` +
                 `LOUISVILLE, KY-IN` +
                 `NEW ORLEANS, LA` +
                 `BALTIMORE, MD` +
                 `MINNEAPOLIS-ST. PAUL, MN-WI` +
                 `BUFFALO-NIAGARA FALLS, NY` +
                 `CHARLOTTE-GASTONIA-ROCK HILL, NC-SC` +
                 `COLUMBUS, OH` +
                 `PORTLAND-VANCOUVER,OR-WA` +
                 `PITTSBURGH, PA` +
                 `NORFOLK-VIRGINIA BEACH-NEWPORT NEWS, VA-NC`, data = housOnly)
summary(rentCost)

grapi <- lm(grapi ~ quantScore +
              hunMedRent +
              `BIRMINGHAM, AL` +
              `SAN DIEGO, CA` +
              `TAMPA-ST. PETERSBURG-CLEARWATER, FL` +
              `ATLANTA, GA` +
              `ST. LOUIS, MO-IL` +
              `INDIANAPOLIS, IN` +
              `KANSAS CITY, MO-KS` +
              `LOUISVILLE, KY-IN` +
              `NEW ORLEANS, LA` +
              `BALTIMORE, MD` +
              `MINNEAPOLIS-ST. PAUL, MN-WI` +
              `BUFFALO-NIAGARA FALLS, NY` +
              `CHARLOTTE-GASTONIA-ROCK HILL, NC-SC` +
              `COLUMBUS, OH` +
              `PORTLAND-VANCOUVER,OR-WA` +
              `PITTSBURGH, PA` +
              `NORFOLK-VIRGINIA BEACH-NEWPORT NEWS, VA-NC`, data = housOnly)
print(summary(grapi), digits = 3)
myCols <- c("grapi", "hunMedRent")
testCase <- housOnly[myCols]
cor(testCase, method = "pearson", use = "complete.obs")
