rm(list=ls())
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("foreign", "stringr", "withr", "rgdal",
              "geosphere", "moments")
pack(packages)

setwd("D:/AP LARSON/DallasCommuting")
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

mytest <- merge(holcDf, resGEOID, by = "GEOID")
mytest <- mytest[!duplicated(mytest$GEOID), ]

# Read in the commdist data and see if there's any relationship
s000 <- read.csv("S000dirdist.csv")

mytest <- merge(mytest, s000, by.x = "GEOID", by.y = "idnum")

cor(mytest$quantScore.y, mytest$distance)
# as HOLC score increases, commuting distance decreases

fit <- lm(distance ~ quantScore.y, data = mytest)
summary(fit) # significant negative relationship
