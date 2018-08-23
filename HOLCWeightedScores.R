rm(list=ls())
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("foreign", "rgdal")
pack(packages)

setwd("D:/AP LARSON/DallasCommutingV2/housingOverlayShps")
MSAshps <- list.files(pattern = "\\.shp$")

# 1. ATLANTA, GA
setwd("D:/AP LARSON/DallasCommutingV2/housingOverlayShps")
HOLC_ATL_GA <- readOGR(".", "HOLC_ATL_GA")
holcDf <- as.data.frame(HOLC_ATL_GA)
holcDf$quantScore <- NA
holcDf$quantScore <- ifelse(holcDf$holc_grade == "A", 4, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "B", 3, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "C", 2, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "D", 1, holcDf$quantScore)
resGEOID <- aggregate(holcDf$quantScore, by = list(holcDf$GEOID), FUN = mean)
colnames(resGEOID) <- c("GEOID", "quantScore")
setwd("D:/AP LARSON/DallasCommutingV2/housingWeightedScr")
write.csv(resGEOID, file = "HOLC_ATL_GA.csv", row.names = FALSE)

# 2. BALTIMORE, MD
setwd("D:/AP LARSON/DallasCommutingV2/housingOverlayShps")
HOLC_BAL_MD <- readOGR(".", "HOLC_BAL_MD")
holcDf <- as.data.frame(HOLC_BAL_MD)
holcDf$quantScore <- NA
holcDf$quantScore <- ifelse(holcDf$holc_grade == "A", 4, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "B", 3, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "C", 2, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "D", 1, holcDf$quantScore)
resGEOID <- aggregate(holcDf$quantScore, by = list(holcDf$GEOID), FUN = mean)
colnames(resGEOID) <- c("GEOID", "quantScore")
setwd("D:/AP LARSON/DallasCommutingV2/housingWeightedScr")
write.csv(resGEOID, file = "HOLC_BAL_MD.csv", row.names = FALSE)

# 3. BIRMINGHAM, AL
setwd("D:/AP LARSON/DallasCommutingV2/housingOverlayShps")
HOLC_BIR_AL <- readOGR(".", "HOLC_BIR_AL")
holcDf <- as.data.frame(HOLC_BIR_AL)
holcDf$quantScore <- NA
holcDf$quantScore <- ifelse(holcDf$holc_grade == "A", 4, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "B", 3, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "C", 2, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "D", 1, holcDf$quantScore)
resGEOID <- aggregate(holcDf$quantScore, by = list(holcDf$GEOID), FUN = mean)
colnames(resGEOID) <- c("GEOID", "quantScore")
setwd("D:/AP LARSON/DallasCommutingV2/housingWeightedScr")
write.csv(resGEOID, file = "HOLC_BIR_AL.csv", row.names = FALSE)

# 4. BOSTON, MA
setwd("D:/AP LARSON/DallasCommutingV2/housingOverlayShps")
HOLC_BOS_MA <- readOGR(".", "HOLC_BOS_MA")
holcDf <- as.data.frame(HOLC_BOS_MA)
holcDf$quantScore <- NA
holcDf$quantScore <- ifelse(holcDf$holc_grade == "A", 4, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "B", 3, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "C", 2, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "D", 1, holcDf$quantScore)
resGEOID <- aggregate(holcDf$quantScore, by = list(holcDf$GEOID), FUN = mean)
colnames(resGEOID) <- c("GEOID", "quantScore")
setwd("D:/AP LARSON/DallasCommutingV2/housingWeightedScr")
write.csv(resGEOID, file = "HOLC_BOS_MA.csv", row.names = FALSE)

# 5. BUFFALO, NY
setwd("D:/AP LARSON/DallasCommutingV2/housingOverlayShps")
HOLC_BUF_NY <- readOGR(".", "HOLC_BUF_NY")
holcDf <- as.data.frame(HOLC_BUF_NY)
holcDf$quantScore <- NA
holcDf$quantScore <- ifelse(holcDf$holc_grade == "A", 4, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "B", 3, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "C", 2, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "D", 1, holcDf$quantScore)
resGEOID <- aggregate(holcDf$quantScore, by = list(holcDf$GEOID), FUN = mean)
colnames(resGEOID) <- c("GEOID", "quantScore")
setwd("D:/AP LARSON/DallasCommutingV2/housingWeightedScr")
write.csv(resGEOID, file = "HOLC_BUF_NY.csv", row.names = FALSE)

# 6. CHARLOTTE, NC
setwd("D:/AP LARSON/DallasCommutingV2/housingOverlayShps")
HOLC_CHA_NC <- readOGR(".", "HOLC_CHA_NC")
holcDf <- as.data.frame(HOLC_CHA_NC)
holcDf$quantScore <- NA
holcDf$quantScore <- ifelse(holcDf$holc_grade == "A", 4, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "B", 3, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "C", 2, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "D", 1, holcDf$quantScore)
resGEOID <- aggregate(holcDf$quantScore, by = list(holcDf$GEOID), FUN = mean)
colnames(resGEOID) <- c("GEOID", "quantScore")
setwd("D:/AP LARSON/DallasCommutingV2/housingWeightedScr")
write.csv(resGEOID, file = "HOLC_CHA_NC.csv", row.names = FALSE)

# 7. CLEVELAND, OH
setwd("D:/AP LARSON/DallasCommutingV2/housingOverlayShps")
HOLC_CLE_OH <- readOGR(".", "HOLC_CLE_OH")
holcDf <- as.data.frame(HOLC_CLE_OH)
holcDf$quantScore <- NA
holcDf$quantScore <- ifelse(holcDf$holc_grade == "A", 4, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "B", 3, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "C", 2, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "D", 1, holcDf$quantScore)
resGEOID <- aggregate(holcDf$quantScore, by = list(holcDf$GEOID), FUN = mean)
colnames(resGEOID) <- c("GEOID", "quantScore")
setwd("D:/AP LARSON/DallasCommutingV2/housingWeightedScr")
write.csv(resGEOID, file = "HOLC_CLE_OH.csv", row.names = FALSE)

# 8. COLUMBUS, OH
setwd("D:/AP LARSON/DallasCommutingV2/housingOverlayShps")
HOLC_COL_OH <- readOGR(".", "HOLC_COL_OH")
holcDf <- as.data.frame(HOLC_COL_OH)
holcDf$quantScore <- NA
holcDf$quantScore <- ifelse(holcDf$holc_grade == "A", 4, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "B", 3, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "C", 2, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "D", 1, holcDf$quantScore)
resGEOID <- aggregate(holcDf$quantScore, by = list(holcDf$GEOID), FUN = mean)
colnames(resGEOID) <- c("GEOID", "quantScore")
setwd("D:/AP LARSON/DallasCommutingV2/housingWeightedScr")
write.csv(resGEOID, file = "HOLC_COL_OH.csv", row.names = FALSE)

# 9. DENVER, CO
setwd("D:/AP LARSON/DallasCommutingV2/housingOverlayShps")
HOLC_DEN_CO <- readOGR(".", "HOLC_DEN_CO")
holcDf <- as.data.frame(HOLC_DEN_CO)
holcDf$quantScore <- NA
holcDf$quantScore <- ifelse(holcDf$holc_grade == "A", 4, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "B", 3, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "C", 2, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "D", 1, holcDf$quantScore)
resGEOID <- aggregate(holcDf$quantScore, by = list(holcDf$GEOID), FUN = mean)
colnames(resGEOID) <- c("GEOID", "quantScore")
setwd("D:/AP LARSON/DallasCommutingV2/housingWeightedScr")
write.csv(resGEOID, file = "HOLC_DEN_CO.csv", row.names = FALSE)

# 10. INDIANAPOLIS, IN
setwd("D:/AP LARSON/DallasCommutingV2/housingOverlayShps")
HOLC_IND_IN <- readOGR(".", "HOLC_IND_IN")
holcDf <- as.data.frame(HOLC_IND_IN)
holcDf$quantScore <- NA
holcDf$quantScore <- ifelse(holcDf$holc_grade == "A", 4, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "B", 3, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "C", 2, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "D", 1, holcDf$quantScore)
resGEOID <- aggregate(holcDf$quantScore, by = list(holcDf$GEOID), FUN = mean)
colnames(resGEOID) <- c("GEOID", "quantScore")
setwd("D:/AP LARSON/DallasCommutingV2/housingWeightedScr")
write.csv(resGEOID, file = "HOLC_IND_IN.csv", row.names = FALSE)

# 11. KANSAS CITY, MO
setwd("D:/AP LARSON/DallasCommutingV2/housingOverlayShps")
HOLC_KAN_MO <- readOGR(".", "HOLC_KAN_MO")
holcDf <- as.data.frame(HOLC_KAN_MO)
holcDf$quantScore <- NA
holcDf$quantScore <- ifelse(holcDf$holc_grade == "A", 4, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "B", 3, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "C", 2, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "D", 1, holcDf$quantScore)
resGEOID <- aggregate(holcDf$quantScore, by = list(holcDf$GEOID), FUN = mean)
colnames(resGEOID) <- c("GEOID", "quantScore")
setwd("D:/AP LARSON/DallasCommutingV2/housingWeightedScr")
write.csv(resGEOID, file = "HOLC_KAN_MO.csv", row.names = FALSE)

# 12. LOUISVILLE, KY
setwd("D:/AP LARSON/DallasCommutingV2/housingOverlayShps")
HOLC_LOU_KY <- readOGR(".", "HOLC_LOU_KY")
holcDf <- as.data.frame(HOLC_LOU_KY)
holcDf$quantScore <- NA
holcDf$quantScore <- ifelse(holcDf$holc_grade == "A", 4, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "B", 3, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "C", 2, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "D", 1, holcDf$quantScore)
resGEOID <- aggregate(holcDf$quantScore, by = list(holcDf$GEOID), FUN = mean)
colnames(resGEOID) <- c("GEOID", "quantScore")
setwd("D:/AP LARSON/DallasCommutingV2/housingWeightedScr")
write.csv(resGEOID, file = "HOLC_LOU_KY.csv", row.names = FALSE)

# 13. MINNEAPOLIS, MN
setwd("D:/AP LARSON/DallasCommutingV2/housingOverlayShps")
HOLC_MIN_MN <- readOGR(".", "HOLC_MIN_MN")
holcDf <- as.data.frame(HOLC_MIN_MN)
holcDf$quantScore <- NA
holcDf$quantScore <- ifelse(holcDf$holc_grade == "A", 4, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "B", 3, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "C", 2, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "D", 1, holcDf$quantScore)
resGEOID <- aggregate(holcDf$quantScore, by = list(holcDf$GEOID), FUN = mean)
colnames(resGEOID) <- c("GEOID", "quantScore")
setwd("D:/AP LARSON/DallasCommutingV2/housingWeightedScr")
write.csv(resGEOID, file = "HOLC_MIN_MN.csv", row.names = FALSE)

# 14. NEW ORLEANS, LA
setwd("D:/AP LARSON/DallasCommutingV2/housingOverlayShps")
HOLC_NEW_LA <- readOGR(".", "HOLC_NEW_LA")
holcDf <- as.data.frame(HOLC_NEW_LA)
holcDf$quantScore <- NA
holcDf$quantScore <- ifelse(holcDf$holc_grade == "A", 4, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "B", 3, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "C", 2, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "D", 1, holcDf$quantScore)
resGEOID <- aggregate(holcDf$quantScore, by = list(holcDf$GEOID), FUN = mean)
colnames(resGEOID) <- c("GEOID", "quantScore")
setwd("D:/AP LARSON/DallasCommutingV2/housingWeightedScr")
write.csv(resGEOID, file = "HOLC_NEW_LA.csv", row.names = FALSE)

# 15. NORFOLK, VA
setwd("D:/AP LARSON/DallasCommutingV2/housingOverlayShps")
HOLC_NOR_VA <- readOGR(".", "HOLC_NOR_VA")
holcDf <- as.data.frame(HOLC_NOR_VA)
holcDf$quantScore <- NA
holcDf$quantScore <- ifelse(holcDf$holc_grade == "A", 4, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "B", 3, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "C", 2, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "D", 1, holcDf$quantScore)
resGEOID <- aggregate(holcDf$quantScore, by = list(holcDf$GEOID), FUN = mean)
colnames(resGEOID) <- c("GEOID", "quantScore")
setwd("D:/AP LARSON/DallasCommutingV2/housingWeightedScr")
write.csv(resGEOID, file = "HOLC_NOR_VA.csv", row.names = FALSE)

# 16. PITTSBURGH, PA
setwd("D:/AP LARSON/DallasCommutingV2/housingOverlayShps")
HOLC_PIT_PA <- readOGR(".", "HOLC_PIT_PA")
holcDf <- as.data.frame(HOLC_PIT_PA)
holcDf$quantScore <- NA
holcDf$quantScore <- ifelse(holcDf$holc_grade == "A", 4, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "B", 3, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "C", 2, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "D", 1, holcDf$quantScore)
resGEOID <- aggregate(holcDf$quantScore, by = list(holcDf$GEOID), FUN = mean)
colnames(resGEOID) <- c("GEOID", "quantScore")
setwd("D:/AP LARSON/DallasCommutingV2/housingWeightedScr")
write.csv(resGEOID, file = "HOLC_PIT_PA.csv", row.names = FALSE)

# 17. PORTLAND, OR
setwd("D:/AP LARSON/DallasCommutingV2/housingOverlayShps")
HOLC_POR_OR <- readOGR(".", "HOLC_POR_OR")
holcDf <- as.data.frame(HOLC_POR_OR)
holcDf$quantScore <- NA
holcDf$quantScore <- ifelse(holcDf$holc_grade == "A", 4, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "B", 3, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "C", 2, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "D", 1, holcDf$quantScore)
resGEOID <- aggregate(holcDf$quantScore, by = list(holcDf$GEOID), FUN = mean)
colnames(resGEOID) <- c("GEOID", "quantScore")
setwd("D:/AP LARSON/DallasCommutingV2/housingWeightedScr")
write.csv(resGEOID, file = "HOLC_POR_OR.csv", row.names = FALSE)

# 18. SACRAMENTO, CA
setwd("D:/AP LARSON/DallasCommutingV2/housingOverlayShps")
HOLC_SAC_CA <- readOGR(".", "HOLC_SAC_CA")
holcDf <- as.data.frame(HOLC_SAC_CA)
holcDf$quantScore <- NA
holcDf$quantScore <- ifelse(holcDf$holc_grade == "A", 4, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "B", 3, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "C", 2, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "D", 1, holcDf$quantScore)
resGEOID <- aggregate(holcDf$quantScore, by = list(holcDf$GEOID), FUN = mean)
colnames(resGEOID) <- c("GEOID", "quantScore")
setwd("D:/AP LARSON/DallasCommutingV2/housingWeightedScr")
write.csv(resGEOID, file = "HOLC_SAC_CA.csv", row.names = FALSE)

# 19. SAN DIEGO, CA
setwd("D:/AP LARSON/DallasCommutingV2/housingOverlayShps")
HOLC_SAND_CA <- readOGR(".", "HOLC_SAND_CA")
holcDf <- as.data.frame(HOLC_SAND_CA)
holcDf$quantScore <- NA
holcDf$quantScore <- ifelse(holcDf$holc_grade == "A", 4, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "B", 3, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "C", 2, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "D", 1, holcDf$quantScore)
resGEOID <- aggregate(holcDf$quantScore, by = list(holcDf$GEOID), FUN = mean)
colnames(resGEOID) <- c("GEOID", "quantScore")
setwd("D:/AP LARSON/DallasCommutingV2/housingWeightedScr")
write.csv(resGEOID, file = "HOLC_SAND_CA.csv", row.names = FALSE)

# 20. SAN JOSE, CA
setwd("D:/AP LARSON/DallasCommutingV2/housingOverlayShps")
HOLC_SANJ_CA <- readOGR(".", "HOLC_SANJ_CA")
holcDf <- as.data.frame(HOLC_SANJ_CA)
holcDf$quantScore <- NA
holcDf$quantScore <- ifelse(holcDf$holc_grade == "A", 4, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "B", 3, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "C", 2, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "D", 1, holcDf$quantScore)
resGEOID <- aggregate(holcDf$quantScore, by = list(holcDf$GEOID), FUN = mean)
colnames(resGEOID) <- c("GEOID", "quantScore")
setwd("D:/AP LARSON/DallasCommutingV2/housingWeightedScr")
write.csv(resGEOID, file = "HOLC_SANJ_CA.csv", row.names = FALSE)

# 21. ST. LOUIS, MO
setwd("D:/AP LARSON/DallasCommutingV2/housingOverlayShps")
HOLC_STL_MO <- readOGR(".", "HOLC_STL_MO")
holcDf <- as.data.frame(HOLC_STL_MO)
holcDf$quantScore <- NA
holcDf$quantScore <- ifelse(holcDf$holc_grade == "A", 4, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "B", 3, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "C", 2, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "D", 1, holcDf$quantScore)
resGEOID <- aggregate(holcDf$quantScore, by = list(holcDf$GEOID), FUN = mean)
colnames(resGEOID) <- c("GEOID", "quantScore")
setwd("D:/AP LARSON/DallasCommutingV2/housingWeightedScr")
write.csv(resGEOID, file = "HOLC_STL_MO.csv", row.names = FALSE)

# 22. TAMPA, FL
setwd("D:/AP LARSON/DallasCommutingV2/housingOverlayShps")
HOLC_TAM_FL <- readOGR(".", "HOLC_TAM_FL")
holcDf <- as.data.frame(HOLC_TAM_FL)
holcDf$quantScore <- NA
holcDf$quantScore <- ifelse(holcDf$holc_grade == "A", 4, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "B", 3, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "C", 2, holcDf$quantScore)
holcDf$quantScore <- ifelse(holcDf$holc_grade == "D", 1, holcDf$quantScore)
resGEOID <- aggregate(holcDf$quantScore, by = list(holcDf$GEOID), FUN = mean)
colnames(resGEOID) <- c("GEOID", "quantScore")
setwd("D:/AP LARSON/DallasCommutingV2/housingWeightedScr")
write.csv(resGEOID, file = "HOLC_TAM_FL.csv", row.names = FALSE)
