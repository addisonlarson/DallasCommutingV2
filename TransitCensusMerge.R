rm(list=ls())
library(rgdal)
# The goal here:
# Merge transit shps with Census data.
# Obtain population density using the ALAND field.

# 1. Read in all the files you need
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

for (i in 1:length(MSAshps)) {
  assign(MSAshps[i], readOGR(".", MSAshps[i]))
}

setwd("D:/AP LARSON/DallasCommutingV2/censusData")
MSAcsvs <- list.files(pattern = "\\.csv$")
for (i in 1:length(MSAcsvs)){
  assign(MSAcsvs[i], read.csv(MSAcsvs[i]))
}

setwd("D:/AP LARSON/DallasCommutingV2/tractShpsCensus")
# 1. ATLANTA, GA
currentShp <- merge(res_ATL_GA, `dATLANTA, GA.csv`,
                    by = "GEOID")
currentShp$ALAND <- as.numeric(as.character(currentShp$ALAND))
currentShp$popDens <- currentShp$popData / (currentShp$ALAND * 0.00000038610)
writeOGR(currentShp, ".", "mer_ATL_GA", driver = "ESRI Shapefile")

# 2. BALTIMORE, MD
currentShp <- merge(res_BAL_MD, `dBALTIMORE, MD.csv`,
                    by = "GEOID")
currentShp$ALAND <- as.numeric(as.character(currentShp$ALAND))
currentShp$popDens <- currentShp$popData / (currentShp$ALAND * 0.00000038610)
writeOGR(currentShp, ".", "mer_BAL_MD", driver = "ESRI Shapefile")

# 3. BIRMINGHAM, AL
currentShp <- merge(res_BIR_AL, `dBIRMINGHAM, AL.csv`,
                    by = "GEOID")
currentShp$ALAND <- as.numeric(as.character(currentShp$ALAND))
currentShp$popDens <- currentShp$popData / (currentShp$ALAND * 0.00000038610)
writeOGR(currentShp, ".", "mer_BIR_AL", driver = "ESRI Shapefile")

# 4. BOSTON, MA
currentShp <- merge(res_BOS_MA, `dBOSTON-WORCESTER-LAWRENCE-LOWELL-BROCKTON, MA.csv`,
                    by = "GEOID")
currentShp$ALAND <- as.numeric(as.character(currentShp$ALAND))
currentShp$popDens <- currentShp$popData / (currentShp$ALAND * 0.00000038610)
writeOGR(currentShp, ".", "mer_BOS_MA", driver = "ESRI Shapefile")

# 5. BUFFALO, NY
currentShp <- merge(res_BUF_NY, `dBUFFALO-NIAGARA FALLS, NY.csv`,
                    by = "GEOID")
currentShp$ALAND <- as.numeric(as.character(currentShp$ALAND))
currentShp$popDens <- currentShp$popData / (currentShp$ALAND * 0.00000038610)
writeOGR(currentShp, ".", "mer_BUF_NY", driver = "ESRI Shapefile")

# 6. CHARLOTTE, NC
currentShp <- merge(res_CHA_NC, `dCHARLOTTE-GASTONIA-ROCK HILL, NC-SC.csv`,
                    by = "GEOID")
currentShp$ALAND <- as.numeric(as.character(currentShp$ALAND))
currentShp$popDens <- currentShp$popData / (currentShp$ALAND * 0.00000038610)
writeOGR(currentShp, ".", "mer_CHA_NC", driver = "ESRI Shapefile")

# 7. CINCINNATI, OH
currentShp <- merge(res_CIN_OH, `dCINCINNATI, OH-KY-IN.csv`,
                    by = "GEOID")
currentShp$ALAND <- as.numeric(as.character(currentShp$ALAND))
currentShp$popDens <- currentShp$popData / (currentShp$ALAND * 0.00000038610)
writeOGR(currentShp, ".", "mer_CIN_OH", driver = "ESRI Shapefile")

# 8. CLEVELAND, OH
currentShp <- merge(res_CLE_OH, `dCLEVELAND-LORAIN-ELYRIA, OH.csv`,
                    by = "GEOID")
currentShp$ALAND <- as.numeric(as.character(currentShp$ALAND))
currentShp$popDens <- currentShp$popData / (currentShp$ALAND * 0.00000038610)
writeOGR(currentShp, ".", "mer_CLE_OH", driver = "ESRI Shapefile")

# 9. COLUMBUS, OH
currentShp <- merge(res_COL_OH, `dCOLUMBUS, OH.csv`,
                    by = "GEOID")
currentShp$ALAND <- as.numeric(as.character(currentShp$ALAND))
currentShp$popDens <- currentShp$popData / (currentShp$ALAND * 0.00000038610)
writeOGR(currentShp, ".", "mer_COL_OH", driver = "ESRI Shapefile")

# 10. DENVER, CO
currentShp <- merge(res_DEN_CO, `dDENVER, CO.csv`,
                    by = "GEOID")
currentShp$ALAND <- as.numeric(as.character(currentShp$ALAND))
currentShp$popDens <- currentShp$popData / (currentShp$ALAND * 0.00000038610)
writeOGR(currentShp, ".", "mer_DEN_CO", driver = "ESRI Shapefile")

# 11. HAMILTON, OH
currentShp <- merge(res_HAM_OH, `dHAMILTON-MIDDLETOWN, OH.csv`,
                    by = "GEOID")
currentShp$ALAND <- as.numeric(as.character(currentShp$ALAND))
currentShp$popDens <- currentShp$popData / (currentShp$ALAND * 0.00000038610)
writeOGR(currentShp, ".", "mer_HAM_OH", driver = "ESRI Shapefile")

# 12. INDIANAPOLIS, IN
currentShp <- merge(res_IND_IN, `dINDIANAPOLIS, IN.csv`,
                    by = "GEOID")
currentShp$ALAND <- as.numeric(as.character(currentShp$ALAND))
currentShp$popDens <- currentShp$popData / (currentShp$ALAND * 0.00000038610)
writeOGR(currentShp, ".", "mer_IND_IN", driver = "ESRI Shapefile")

# 13. KANSAS CITY, MO
currentShp <- merge(res_KAN_MO, `dKANSAS CITY, MO-KS.csv`,
                    by = "GEOID")
currentShp$ALAND <- as.numeric(as.character(currentShp$ALAND))
currentShp$popDens <- currentShp$popData / (currentShp$ALAND * 0.00000038610)
writeOGR(currentShp, ".", "mer_KAN_MO", driver = "ESRI Shapefile")

# 14. LOUISVILLE, KY
currentShp <- merge(res_LOU_KY, `dLOUISVILLE, KY-IN.csv`,
                    by = "GEOID")
currentShp$ALAND <- as.numeric(as.character(currentShp$ALAND))
currentShp$popDens <- currentShp$popData / (currentShp$ALAND * 0.00000038610)
writeOGR(currentShp, ".", "mer_LOU_KY", driver = "ESRI Shapefile")

# 15. MINNEAPOLIS, MN
currentShp <- merge(res_MIN_MN, `dMINNEAPOLIS-ST. PAUL, MN-WI.csv`,
                    by = "GEOID")
currentShp$ALAND <- as.numeric(as.character(currentShp$ALAND))
currentShp$popDens <- currentShp$popData / (currentShp$ALAND * 0.00000038610)
writeOGR(currentShp, ".", "mer_MIN_MN", driver = "ESRI Shapefile")

# 16. NEW ORLEANS, LA
currentShp <- merge(res_NEW_LA, `dNEW ORLEANS, LA.csv`,
                    by = "GEOID")
currentShp$ALAND <- as.numeric(as.character(currentShp$ALAND))
currentShp$popDens <- currentShp$popData / (currentShp$ALAND * 0.00000038610)
writeOGR(currentShp, ".", "mer_NEW_LA", driver = "ESRI Shapefile")

# 17. NORFOLK, VA
currentShp <- merge(res_NOR_VA, `dNORFOLK-VIRGINIA BEACH-NEWPORT NEWS, VA-NC.csv`,
                    by = "GEOID")
currentShp$ALAND <- as.numeric(as.character(currentShp$ALAND))
currentShp$popDens <- currentShp$popData / (currentShp$ALAND * 0.00000038610)
writeOGR(currentShp, ".", "mer_NOR_VA", driver = "ESRI Shapefile")

# 18. PITTSBURGH, PA
currentShp <- merge(res_PIT_PA, `dPITTSBURGH, PA.csv`,
                    by = "GEOID")
currentShp$ALAND <- as.numeric(as.character(currentShp$ALAND))
currentShp$popDens <- currentShp$popData / (currentShp$ALAND * 0.00000038610)
writeOGR(currentShp, ".", "mer_PIT_PA", driver = "ESRI Shapefile")

# 19. PORTLAND, OR
currentShp <- merge(res_POR_OR, `dPORTLAND-VANCOUVER,OR-WA.csv`,
                    by = "GEOID")
currentShp$ALAND <- as.numeric(as.character(currentShp$ALAND))
currentShp$popDens <- currentShp$popData / (currentShp$ALAND * 0.00000038610)
writeOGR(currentShp, ".", "mer_POR_OR", driver = "ESRI Shapefile")

# 20. PROVIDENCE, RI
currentShp <- merge(res_PRO_RI, `dPROVIDENCE-WARWICK-PAWTUCKET, RI.csv`,
                    by = "GEOID")
currentShp$ALAND <- as.numeric(as.character(currentShp$ALAND))
currentShp$popDens <- currentShp$popData / (currentShp$ALAND * 0.00000038610)
writeOGR(currentShp, ".", "mer_PRO_RI", driver = "ESRI Shapefile")

# 21. RALEIGH-DURHAM, NC
currentShp <- merge(res_RAL_NC, `dRALEIGH-DURHAM-CHAPEL HILL, NC.csv`,
                    by = "GEOID")
currentShp$ALAND <- as.numeric(as.character(currentShp$ALAND))
currentShp$popDens <- currentShp$popData / (currentShp$ALAND * 0.00000038610)
writeOGR(currentShp, ".", "mer_RAL_NC", driver = "ESRI Shapefile")

# 22. SACRAMENTO, CA
currentShp <- merge(res_SAC_CA, `dSACRAMENTO, CA.csv`,
                    by = "GEOID")
currentShp$ALAND <- as.numeric(as.character(currentShp$ALAND))
currentShp$popDens <- currentShp$popData / (currentShp$ALAND * 0.00000038610)
writeOGR(currentShp, ".", "mer_SAC_CA", driver = "ESRI Shapefile")

# 23. SAN DIEGO, CA
currentShp <- merge(res_SAND_CA, `dSAN DIEGO, CA.csv`,
                    by = "GEOID")
currentShp$ALAND <- as.numeric(as.character(currentShp$ALAND))
currentShp$popDens <- currentShp$popData / (currentShp$ALAND * 0.00000038610)
writeOGR(currentShp, ".", "mer_SAND_CA", driver = "ESRI Shapefile")

# 24. SAN JOSE, CA
currentShp <- merge(res_SANJ_CA, `dSAN JOSE, CA.csv`,
                    by = "GEOID")
currentShp$ALAND <- as.numeric(as.character(currentShp$ALAND))
currentShp$popDens <- currentShp$popData / (currentShp$ALAND * 0.00000038610)
writeOGR(currentShp, ".", "mer_SANJ_CA", driver = "ESRI Shapefile")

# 25. ST. LOUIS, MO
currentShp <- merge(res_STL_MO, `dST. LOUIS, MO-IL.csv`,
                    by = "GEOID")
currentShp$ALAND <- as.numeric(as.character(currentShp$ALAND))
currentShp$popDens <- currentShp$popData / (currentShp$ALAND * 0.00000038610)
writeOGR(currentShp, ".", "mer_STL_MO", driver = "ESRI Shapefile")

# 26. TAMPA, FL
currentShp <- merge(res_TAM_FL, `dTAMPA-ST. PETERSBURG-CLEARWATER, FL.csv`,
                    by = "GEOID")
currentShp$ALAND <- as.numeric(as.character(currentShp$ALAND))
currentShp$popDens <- currentShp$popData / (currentShp$ALAND * 0.00000038610)
writeOGR(currentShp, ".", "mer_TAM_FL", driver = "ESRI Shapefile")

# 27. YOLO, CA
currentShp <- merge(res_YOL_CA, `dYOLO, CA.csv`,
                    by = "GEOID")
currentShp$ALAND <- as.numeric(as.character(currentShp$ALAND))
currentShp$popDens <- currentShp$popData / (currentShp$ALAND * 0.00000038610)
writeOGR(currentShp, ".", "mer_YOL_CA", driver = "ESRI Shapefile")
