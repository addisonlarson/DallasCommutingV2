# Good to explain: why rhumb bearing? Why great-circle distance?
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
# Read in LEHD data, TIGER/Line shapefiles, TX urbanized area/cluster tracts
# Texas origin-destination jobs records: tx_od_main_JT00_2015.csv
# See line 48
# Texas census tract shapefile: tl_2015_48_tract.shp
# See lines 24 and 78
# Texas urbanized area/cluster shapefile: ua_TX_tracts.shp
# See line 27

# Determine eligible urban populations in region.
# Census 2010 released guidance on this, available in Federal Register 76:164.
tx <- readOGR(dsn = ".", layer = "tl_2015_48_tract")
txdf <- as.data.frame(tx)
txdf$ALAND <- as.numeric(as.character(txdf$ALAND))
ua <- readOGR(dsn = ".", layer = "ua_TX_tracts")
uadf <- as.data.frame(ua)
txdf <- txdf[c("GEOID", "ALAND", "COUNTYFP")]
txdf$ALAND <- txdf$ALAND * 3.861022e-07 # Convert sq meters to sq miles
countyList <- c(113, 321, 251, 425, 367, 379, 467,
                181, 139, 439, 257, 349, 497, 213,
                217, 85, 147, 97, 121, 397)
# Dallas, Hunt, Johnson, Somervell, Parker, Rains, Van Zandt,
# Grayson, Ellis, Tarrant, Kaufman, Navarro, Wise, Henderson,
# Hill, Collin, Fannin, Cooke, Denton, Hood, Rockwall
# Pare down txdf by countyList and tracts intersecting with UA shapefile
uadf$COUNTYFP <- as.numeric(as.character(uadf$COUNTYFP))
uadf <- subset(uadf, COUNTYFP %in% countyList)
tractList <- uadf$GEOID # 1348 tracts intersect with UA or UC
write.csv(tractList, file = "qualifyingTracts.csv", row.names = FALSE)
countyList2 <- c(113, 321, 251, 425, 367, 379, 467,
                 181, 139, 439, 257, 349, 497, 213,
                 217, "085", 147, "097", 121, 397)
rm(tx); rm(txdf)

brackets <- c("S000", "SE01", "SE02", "SE03")
for (item in brackets){
  od <- read.csv("tx_od_main_JT00_2015.csv", stringsAsFactors = FALSE)
  # Split H_GEOCODE into component parts
  odsplit <- t(sapply(od$h_geocode, function(x) substring(x, first=c(1,3,6,12), last=c(2,5,11,15))))
  odsplitdf <- cbind(od, odsplit)
  # Rename columns for subset
  keepvars <- c("w_geocode","h_geocode", item, "1", "2", "3", "4")
  odsplitdf <- odsplitdf[keepvars]
  colnames(odsplitdf) <- c("w_geocode","h_geocode", item, "st", "cty", "trct", "bg")
  
  # odsplitdf <- odsplitdf[which(odsplitdf$cty == "025"),]
  odsplitdf <- subset(odsplitdf, cty %in% countyList2)
  
  # Now, do the same odsplit for w_geocode. This will enable us to collapse into tracts
  dalodsplit <- t(sapply(odsplitdf$w_geocode, function(x) substring(x, first=c(1,3,6,12), last=c(2,5,11,15))))
  dalod <- cbind(odsplitdf, dalodsplit)
  colnames(dalod) <- c("h_geocode", "w_geocode", item, "sth", "ctyh",
                       "trcth", "bgh", "stw", "ctyw", "trctw", "bgw")
  dalod$ctyh2 <- with_options(c(scipen = 999), str_pad(dalod$ctyh, 3, pad = "0"))
  dalod$ctyw2 <- with_options(c(scipen = 999), str_pad(dalod$ctyw, 3, pad = "0"))
  dalod$trcth2 <- with_options(c(scipen = 999), str_pad(dalod$trcth, 6, pad = "0"))
  dalod$trctw2 <- with_options(c(scipen = 999), str_pad(dalod$trctw, 6, pad = "0"))
  dalod$stctyh <- do.call(paste, c(dalod[c("sth", "ctyh2")], sep = ""))
  dalod$stctyw <- do.call(paste, c(dalod[c("stw", "ctyw2")], sep = ""))
  dalod$geoidh <- do.call(paste, c(dalod[c("sth", "ctyh2", "trcth2")], sep = ""))
  dalod$geoidw <- do.call(paste, c(dalod[c("stw","ctyw2", "trctw2")], sep = ""))
  odmatrix <- aggregate(dalod[c(3)], list(geoidh = dalod$geoidh, geoidw = dalod$geoidw), sum)
  
  # This loop subsets out for each geoidh and exports a unique
  # file for each geoidh showing all the destinations for a worker
  # originating in geoidh along with related measures.
  tx <- readOGR(dsn = ".", layer = "tl_2015_48_tract")
  txdf <- as.data.frame(tx)
  # Join geoidh with its corresponding census tract info to get lon/lat
  colnames(txdf) <- c("STATEFP","COUNTYFP","TRACTCE",
                        "geoidh","NAME","NAMELSAD","MTFCC",
                        "FUNCSTAT","ALAND","AWATER","INTPTLAT","INTPTLON")
  homecoords <- merge(txdf, odmatrix, by = "geoidh")
  # Rename lon/lat to be geoidh specific, and keep only the relevant columns
  keepvars <- c("geoidh", "INTPTLAT", "INTPTLON", "geoidw", item)
  homecoords <- homecoords[keepvars]
  colnames(homecoords) <- c("geoidh", "lath", "lonh", "geoidw", "x")
  # Now, join geoidw with its corresponding census tract to get its lon/lat
  # Apologies for the misleading name of "homecoords"
  colnames(txdf) <- c("STATEFP","COUNTYFP","TRACTCE",
                        "geoidw","NAME","NAMELSAD","MTFCC",
                        "FUNCSTAT","ALAND","AWATER","INTPTLAT","INTPTLON")
  homecoords <- merge(homecoords, txdf, by = "geoidw")
  # Rename lon/lat to be geoidw specific, and keep only the relevant columns
  keepvars <- c("geoidw", "geoidh", "lath", "lonh", "INTPTLON", "INTPTLAT", "x")
  homecoords <- homecoords[keepvars]
  colnames(homecoords) <- c("geoidw", "geoidh", "lath", "lonh", "lonw", "latw", "x")
  homecoords[] <- lapply(homecoords, function(x) as.character(x))
  homecoords$lath <- as.numeric(as.character(homecoords$lath))
  homecoords$lonh <- as.numeric(as.character(homecoords$lonh))
  homecoords$latw <- as.numeric(as.character(homecoords$latw))
  homecoords$lonw <- as.numeric(as.character(homecoords$lonw))
  # Below spatial operations in WGS84
  resultdf <- NULL
  homegeoids <- unique(odmatrix$geoidh)
  for (idnum in homegeoids){
    # This lets you subset the data out by home census tract, since we
    # care about the point of origination.
    tabular <- homecoords[ which(homecoords$geoidh == idnum), ]
    work <- (data.frame(lonw = tabular$lonw, latw = tabular$latw))
    home <- (data.frame(lonh = tabular$lonh, lath = tabular$lath))
    workm <- data.matrix(work)
    homem <- data.matrix(home)
    tabular$distmet <- distHaversine(homem, workm)
    maxdist = 80467.2 # Threshold distance of 50 miles, in meters
    tabular <- tabular[ which(tabular$distmet < maxdist), ]
    work <- (data.frame(lonw = tabular$lonw, latw = tabular$latw))
    home <- (data.frame(lonh = tabular$lonh, lath = tabular$lath))
    workm <- data.matrix(work)
    homem <- data.matrix(home)
    tabular$distmet <- distHaversine(homem, workm)
    tabular$azimuth <- bearingRhumb(homem, workm)
    # This df expands to accomodate multiple job observations.
    # Was initially made this way for use with the circular package
    azimuthdf <- data.frame(x = tabular$x, azimuth = tabular$azimuth, distmet = tabular$distmet)
    azimuthdf <- azimuthdf[rep(row.names(azimuthdf), azimuthdf$x), ]
    azimuthdf$x[azimuthdf$x != 1] <- 1
    # People who live and work in same tract have -180 recorded as direction.
    # Remove these from the analysis.
    azimuthdf$azimuth[azimuthdf$azimuth == -180.000000] <- NA
    azimuthdf$dmath <- 450-azimuthdf$azimuth
    azimuthdf$dmath <- ifelse(azimuthdf$dmath >= 360, azimuthdf$dmath - 360, azimuthdf$dmath)
    azimuthdf$radmath <- azimuthdf$dmath * pi / 180
    azimuthdf$s <- sin(azimuthdf$radmath)
    azimuthdf$c <- cos(azimuthdf$radmath)
    sbar <- sum(azimuthdf$s, na.rm = TRUE)/length(azimuthdf[!is.na(azimuthdf)])
    cbar <- sum(azimuthdf$c, na.rm = TRUE)/length(azimuthdf[!is.na(azimuthdf)])
    r <- (cbar ^ 2 + sbar ^ 2) ^ 0.5
    avgdirection <- atan(sbar / cbar) # Apply a piecewise function; not all values correct.
    avgdirection <- ifelse(cbar < 0, avgdirection + pi, avgdirection)
    avgdirection <- ifelse(sbar < 0 & cbar > 0, avgdirection + 2 * pi, avgdirection)
    azimuthdf$smi <- azimuthdf$s * azimuthdf$distmet * 0.000621371192 # Going to try to convert rad to mi and see
    azimuthdf$cmi <- azimuthdf$c * azimuthdf$distmet * 0.000621371192
    sbarmi <- sum(azimuthdf$smi, na.rm = TRUE)/length(azimuthdf[!is.na(azimuthdf)])
    cbarmi <- sum(azimuthdf$cmi, na.rm = TRUE)/length(azimuthdf[!is.na(azimuthdf)])
    rmi <- (cbarmi ^ 2 + sbarmi ^ 2) ^ 0.5
    avgthrust <- rmi # Resultant vector length in miles.
    tabular$x <- as.numeric(as.character(tabular$x))
    distance <- sum(tabular$distmet * tabular$x) / sum(tabular$x)
    obs <- sum(tabular$x)
    distance <- distance * 0.000621371192 # Convert meters to miles
    resultdf <- rbind(resultdf, data.frame(idnum,
                                           obs,
                                           distance,
                                           avgdirection,
                                           avgthrust))
  }
  resultdf <- subset(resultdf, resultdf$idnum %in% tractList)
  write.csv(resultdf, file = paste0(item,"dirdist.csv"), row.names = FALSE)
}
