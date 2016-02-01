make.overview.map.plot <- function(r.loc = getwd()){
  ## Create the overview map for the hake assessment
  ## r.loc is necessary so that the code can be run from latex
  ## and a reletive path can be given to locate the r directory
  oldpar <- par()

  ## California coast
  CA <- matrix(c(-124.1829, 42.00354,
                 -123.2375, 42.00927,
                 -122.3036, 42.00927,
                 -120.0060, 42.00927,
                 -119.9946, 38.98978,
                 -119.8685, 38.90956,
                 -117.8460, 37.47717,
                 -114.6374, 35.01918,
                 -114.6087, 34.88740,
                 -114.5515, 34.83583,
                 -114.5744, 34.80719,
                 -114.4483, 34.70978,
                 -114.3738, 34.46914,
                 -114.1389, 34.29725,
                 -114.1332, 34.26287,
                 -114.4197, 34.07953,
                 -114.4540, 33.99932,
                 -114.5400, 33.92483,
                 -114.5228, 33.73003,
                 -114.5572, 33.57533,
                 -114.6202, 33.52949,
                 -114.6087, 33.50084,
                 -114.7291, 33.40344,
                 -114.7405, 33.31176,
                 -114.6890, 33.26593,
                 -114.7004, 33.08258,
                 -114.5400, 33.04247,
                 -114.4827, 32.99664,
                 -114.4770, 32.93362,
                 -114.5515, 32.76173,
                 -114.7348, 32.73308,
                 -117.1126, 32.53827),
               ncol = 2,
               dimnames = list(NULL, c("x", "y")),
               byrow = TRUE)

  ## Oregon coast
  OR <- matrix(c(-123.4323, 46.22623,
                 -123.3693, 46.14029,
                 -123.1172, 46.16894,
                 -122.9109, 46.07153,
                 -122.7906, 45.87673,
                 -122.7619, 45.64182,
                 -122.3208, 45.53296,
                 -121.7765, 45.69339,
                 -121.3696, 45.69339,
                 -121.1634, 45.60171,
                 -121.0545, 45.64755,
                 -120.9629, 45.64182,
                 -120.6535, 45.72776,
                 -120.4472, 45.68765,
                 -119.6966, 45.85381,
                 -119.5935, 45.91684,
                 -119.3185, 45.93402,
                 -119.2497, 45.92830,
                 -118.9575, 45.99705,
                 -116.9292, 45.99705,
                 -116.7459, 45.80798,
                 -116.7000, 45.83662,
                 -116.5453, 45.76214,
                 -116.4651, 45.61317,
                 -116.6829, 45.23502,
                 -116.8490, 44.99438,
                 -116.8318, 44.94854,
                 -116.9178, 44.81676,
                 -117.0381, 44.74800,
                 -117.2100, 44.43861,
                 -117.2042, 44.30683,
                 -117.1584, 44.26099,
                 -117.0954, 44.27245,
                 -117.0496, 44.23234,
                 -117.0037, 44.25526,
                 -116.9521, 44.22088,
                 -116.9063, 44.14640,
                 -116.9579, 44.08910,
                 -116.9350, 44.00889,
                 -117.0266, 43.79116,
                 -117.0266, 42.00927,
                 -119.3070, 42.00927,
                 -120.0060, 42.00927,
                 -122.3036, 42.00927,
                 -123.2375, 42.00927,
                 -124.1829, 42.00354),
               ncol = 2,
               dimnames = list(NULL, c("x", "y")),
               byrow = TRUE)

  ## Washington coast
  WA <- matrix(c(-122.7104, 48.98789,
                 -122.0916, 48.99362,
                 -117.0209, 48.99362,
                 -117.0209, 48.99362,
                 -117.0266, 46.53563,
                 -117.0266, 46.42677,
                 -117.0324, 46.38666,
                 -116.9235, 46.16894,
                 -116.9292, 45.99705,
                 -118.9575, 45.99705,
                 -119.2497, 45.92830,
                 -119.3185, 45.93402,
                 -119.5935, 45.91684,
                 -119.6966, 45.85381,
                 -120.4472, 45.68765,
                 -120.6535, 45.72776,
                 -120.9629, 45.64182,
                 -121.0545, 45.64755,
                 -121.1634, 45.60171,
                 -121.3696, 45.69339,
                 -121.7765, 45.69339,
                 -122.3208, 45.53296,
                 -122.7619, 45.64182,
                 -122.7906, 45.87673,
                 -122.9109, 46.07153,
                 -123.1172, 46.16894,
                 -123.3693, 46.14029,
                 -123.4323, 46.22623),
               ncol = 2,
               dimnames = list(NULL, c("x", "y")),
               byrow = TRUE)


  wc.states.inland.pbs <- as.PolySet(data.frame(PID = c(rep(1, nrow(CA)),
                                                        rep(3, nrow(OR)),
                                                        rep(4, nrow(WA))),
                                                SID = c(rep(1, nrow(CA)),
                                                        rep(3, nrow(OR)),
                                                        rep(4, nrow(WA))),
                                                POS = c(1:nrow(CA),
                                                        1:nrow(OR),
                                                        1:nrow(WA)),
                                                X = c(CA[,"x"], OR[,"x"], WA[,"x"]),
                                                Y = c(CA[,"y"], OR[,"y"], WA[,"y"])), projection = "LL")

  ## Only the inland borders, this allows different packages, such as PBSmapping to be used for coast
  wc.states.inland <- list(CA = CA, OR = OR, WA = WA)

  ## worldLLhigh and nepacLL are from the PBSmapping package
  data(worldLLhigh)
  data(nepacLL)

  ## Go to latitude of 34.1 to get the islands that PBSmapping cuts off
  tmp <- worldLLhigh[worldLLhigh$Y > 21 &
                     worldLLhigh$Y <= 34.1 &
                     worldLLhigh$X > 360 - 126 &
                     worldLLhigh$X < 360 - 103,]

  ## PID = 1 is the west coast. there are also some islands included
  tmp$X <- tmp$X - 360

  ## This also gets the channel islands
  tmp.islands <- tmp[tmp$PID!=1,]

  ## Now cut off the little bit that nepacLL has
  tmp <- tmp[tmp$PID == 1 & tmp$Y <= 34,]
  wc <- nepacLL[nepacLL$Y >= 34 &
                nepacLL$Y < 65 &
                nepacLL$X < -112 &
                nepacLL$X >= -145,]

  ## Grab islands from nepacLL, but omit the southernmost that are cutoff
  wc.islands <- wc[wc$PID != 1 & wc$Y > 34.1,]
  tmp <- rbind(wc[wc$PID == 1,], tmp)

  ## Add in a point offscreen to connect the polygon
  tmp <- rbind(c(1, 1, tmp[1, "X"], max(tmp$Y)), tmp)
  ## Add in a point offscreen to connect the polygon
  tmp <- rbind(c(1, 1, -100, max(tmp$Y)), tmp)
  ## Add in a point offscreen to connect the polygon
  tmp <- rbind(c(1, 1, -100, min(tmp$Y)), tmp)
  tmp$POS <- 1:nrow(tmp)

  ## Just in case some of the PID numbers are the same, renumber them
  ## This renumbers the PID to be between 1 and whatever, and keeps the correct groups,
  ##  example: x <- c(11,12,12,13,13,13,14,14,14,14,15,15,15,15,15); as.numeric(factor(x))
  tmpPID <- as.numeric(factor(tmp.islands$PID)) + 1
  ## Add to it so that other PIDs are not the same
  wcPID <- as.numeric(factor(wc.islands$PID)) + max(tmpPID)
  tmp.islands$PID <- tmpPID
  wc.islands$PID <- wcPID
  westCoastLL <- rbind(tmp, wc.islands, tmp.islands)
  rm(tmp, wc, tmp.islands, wc.islands, tmpPID, wcPID)

  ## Remove puget sound border
  wc <- westCoastLL[westCoastLL$PID == 1, c("X", "Y")]
  wc <- wc[!(wc$Y > 46.9 & wc$X > -123.3),]

  ## PSMFC boundaries
  PSMFC <- data.frame(boundary = c("Start", "1A", "1B", "1C", "2A", "2B", "2C", "3A"),
                      lat = c(32.5, 36, 40.5, 42, 42.933333, 44.3, 45.7667, 47.3333))

  PSMFCareasPBS <- PSMFCareaNames <- NULL
  for(i in 1:(nrow(PSMFC) - 1)) {
    S <- i
    N <- i + 1
    tmp <- wc[wc$Y >= PSMFC$lat[S] &
              wc$Y <= PSMFC$lat[N],
              c("X", "Y")]
    tmp <- rbind(c(-140, PSMFC$lat[N]),
                 c(tmp$X[1], PSMFC$lat[N]),
                 tmp,
                 c(tmp$X[nrow(tmp)], PSMFC$lat[S]),
                 c(-140, PSMFC$lat[S]))
    tmp <- data.frame(PID = rep(i,nrow(tmp)),
                      POS = 1:nrow(tmp),
                      X = tmp$X,
                      Y = tmp$Y)
    PSMFCareasPBS <- as.PolySet(rbind(tmp, PSMFCareasPBS), projection="LL")
    PSMFCareaNames <- as.PolyData(rbind(data.frame(PID = i,
                                                   label = PSMFC$boundary[N],
                                                   X = -128,
                                                   Y = (PSMFC$lat[S] + PSMFC$lat[N]) / 2),
                                        PSMFCareaNames),
                                  projection = "LL")
  }

  PSMFC <- data.frame(boundary = c("Start", "1A", "1B", "1C", "2A", "2B", "2C", "3A", "3S/3B"),
                      lat = c(30, 36, 40.5, 42, 42.933333, 44.3, 45.7667, 47.3333, 48.7))
  xlabel <- c(-123, -127, -128, -128, -128, -128, -128, -125.2)
  PSMFCareasPBS <- PSMFCareaNames <- NULL
  for(i in 1:(nrow(PSMFC)-1)) {
    S <- i
    N <- i + 1
    tmp <- wc[wc$Y >= PSMFC$lat[S] &
              wc$Y <= PSMFC$lat[N],
              c("X", "Y")]
    tmp <- rbind(c(-140, PSMFC$lat[N]),
                 c(tmp$X[1], PSMFC$lat[N]),
                 tmp,
                 c(tmp$X[nrow(tmp)], PSMFC$lat[S]),
                 c(-140, PSMFC$lat[S]))
    tmp <- data.frame(PID = rep(i, nrow(tmp)),
                      POS = 1:nrow(tmp),
                      X = tmp$X,
                      Y = tmp$Y)
    PSMFCareasPBS <- as.PolySet(rbind(tmp, PSMFCareasPBS), projection = "LL")
    PSMFCareaNames <- as.PolyData(rbind(data.frame(PID = i,
                                                   label = PSMFC$boundary[N],
                                                   X = xlabel[i],
                                                   Y = (PSMFC$lat[S] + PSMFC$lat[N]) / 2),
                                        PSMFCareaNames),
                                  projection = "LL")
  }
  PSMFCareaNames[1, c("X", "Y")] <- c(-126.2, 47.7)

  ## INPFC boundaries (no EEZ)
  INPFC <- data.frame(boundary = c("Start", "CP", "MT", "EK", "CL"),
                      lat = c(30, 36, 40.5, 43, 47.5))

  ## Could probably use joinPolys
  INPFCareasPBS <- INPFCareaNames <- NULL
  for(i in 1:(nrow(INPFC)-1)) {
    S <- i
    N <- i + 1
    tmp <- wc[wc$Y >= INPFC$lat[S] &
              wc$Y <= INPFC$lat[N],
              c("X", "Y")]
    tmp <- rbind(c(-140, INPFC$lat[N]),
                 c(tmp$X[1], INPFC$lat[N]),
                 tmp,
                 c(tmp$X[nrow(tmp)], INPFC$lat[S]),
                 c(-140, INPFC$lat[S]))
    tmp <- data.frame(PID = rep(i, nrow(tmp)),
                      POS = 1:nrow(tmp),
                      X = tmp$X,
                      Y = tmp$Y)
    INPFCareasPBS <- as.PolySet(rbind(tmp, INPFCareasPBS), projection = "LL")
    INPFCareaNames <- as.PolyData(rbind(data.frame(PID = i,
                                                   label = INPFC$boundary[N],
                                                   X = -128,
                                                   Y = (INPFC$lat[S] + INPFC$lat[N]) / 2),
                                        INPFCareaNames),
                                  projection = "LL")
  }

  ## INPFC boundaries (with EEZ)
  INPFC <- data.frame(boundary = c("Start", "CP", "MT", "EK", "CL", "VN"),
                      lat = c(30.5, 36, 40.5, 43, 47.5, 48.7))

    ## Could probably use joinPolys
  INPFCareasPBS <- INPFCareaNames <- NULL
  xlabel <- c(-123, -127, -128, -128, -125.2)
  for(i in 1:(nrow(INPFC)-1)) {
    S <- i
    N <- i + 1
    tmp <- wc[wc$Y >= INPFC$lat[S] &
              wc$Y <= INPFC$lat[N],
              c("X", "Y")]
    tmp <- rbind(c(-140, INPFC$lat[N]),
                 c(tmp$X[1], INPFC$lat[N]),
                 tmp,
                 c(tmp$X[nrow(tmp)], INPFC$lat[S]),
                 c(-140, INPFC$lat[S]))
    tmp <- data.frame(PID = rep(i, nrow(tmp)),
                      POS = 1:nrow(tmp),
                      X = tmp$X,
                      Y = tmp$Y)
    INPFCareasPBS <- as.PolySet(rbind(tmp, INPFCareasPBS), projection = "LL")
    INPFCareaNames <- as.PolyData(rbind(data.frame(PID = i,
                                                   label = INPFC$boundary[N],
                                                   X = xlabel[i],
                                                   Y = (INPFC$lat[S] + INPFC$lat[N]) / 2),
                                        INPFCareaNames),
                                  projection = "LL")
  }
  INPFCareaNames[1,c("X", "Y")] <- c(-126.15, 47.75)

  print("Created PSMFCareasPBS, PSMFCareaNames, INPFCareasPBS, and INPFCareaNames")
  EEZ <- importShapefile(file.path(r.loc, "map-data", "eez", "Pacific-EEZ-dissolve-dd.shp"), readDBF = TRUE)
  ## tmp <- importShapefile("C:\\Mapping\\Shapefiles\\DepthContours_4Survey_dd.shp",readDBF=T)
  ## depth30f <- tmp[tmp$PID==1,]
  ## depth100f <- tmp[tmp$PID==2,]
  ## depth300f <- tmp[tmp$PID==3,]
  ## depth700f <- tmp[tmp$PID==4,]
  ##print("Created depth contours depth30f, depth100f, depth300f, and depth700f")

  INPFCareasEEZ.PBS <- joinPolys(EEZ, INPFCareasPBS, operation = "INT")

  ##tmp <- importShapefile("C:\\Mapping\\Shapefiles\\contour4allan_ETsmoothBezier5_diss.shp",readDBF=T)
  ##depth30m <- tmp[tmp$PID==3,]
  ##depth50m <- tmp[tmp$PID==2,]
  ##depth1500m <- tmp[tmp$PID==1,]
  ##print("Created depth contours depth30m, depth50m, and depth1500m")

  tmp <- joinPolys(EEZ, INPFCareasPBS[INPFCareasPBS$PID == 4,], operation = "INT")
  tmp <- joinPolys(EEZ, INPFCareasPBS, operation = "INT")

  LMEoffshore <- importShapefile(file.path(r.loc, "map-data", "lme-66-offshore", "LME66-Offshore.shp"), readDBF = TRUE)
  LME <- importShapefile(file.path(r.loc, "map-data", "lme-66", "LME66.shp"), readDBF = TRUE)
  province <- importShapefile(file.path(r.loc, "map-data", "province", "province.shp"), readDBF = TRUE)
  alberta <- attributes(province)$PolyData[attributes(province)$PolyData$NAME == "Alberta", "PID"]
  CCLME <- attributes(LME)$PolyData[attributes(LME)$PolyData$LME_NAME == "California Current", "PID"]
  GOALME <- attributes(LME)$PolyData[attributes(LME)$PolyData$LME_NAME == "Gulf of Alaska", "PID"]

  port.lats <- read.csv(file.path(r.loc, "map-data","port-lats.csv"))
  cities <- c("Newport", "Westport", "Astoria", "Eureka", "Charleston (Coos Bay)")
  cities <- port.lats[port.lats$Name %in% cities,]

  ## par(mar = c(4, 4, 0, 0) + 0.1, las = 1)
  plotMap(westCoastLL, tck = c(-0.02), xlim = c(-140, -113.0), ylim = c(29.9, 59.1), col = gray(0.8))
  addLines(wc.states.inland.pbs)
  map("state", add = TRUE, region = c("Idaho", "Montana", "Nevada", "Arizona"))
  addLines(province, polyProps = data.frame(PID = alberta))
  addLines(province, polyProps = data.frame(PID = 12, SID = c(123)))
  ## Found this by x <- addLines(LMEoffshore) then looking at x
  addLines(LMEoffshore, polyProps = data.frame(PID = 76))
  addLines(LMEoffshore, polyProps = data.frame(PID = 169))
  points(-cities$Lon, cities$Lat, pch = 16, col = gray(0.4))
  text(-cities$Lon, cities$Lat, cities$Name, pos = 4, cex = 0.7, col = gray(0.4))

  text(-123, 56, "BC", cex = 1.2)
  text(-120, 47.1, "WA", cex = 1.1)
  text(-120, 44.3, "OR", cex = 1.2)
  text(-118.8, 35.9, "CA", cex = 1.2)
  text(-122.8, 51, "Strait of\nGeorgia", adj = 0, cex = 0.7)
  arrows(-122.8, 51, -123.55, 49.67, length = 0.05)
  text(-120.7, 48.4, "Puget\nSound", adj = 0, cex = 0.7)
  arrows(-120.7, 48.4, -122.1, 47.7, length = 0.05)
  text(-133.2, 52, "Haida\nGwaii", adj = 1, cex = 0.7)
  arrows(-133.2, 52, -132.2, 52.5, length = 0.05)
  text(-117.8, 30.5,"Baja\nCalifornia", adj = 1, cex = 0.7)
  arrows(-117.8, 30.5, -115.5, 30.5, length = 0.05)
  text(-131.2, 58, "SE\nAlaska", adj = 0, cex = 0.7)
  arrows(-131.2, 58, -132.3, 57.5, length = 0.05)
  text(-128.3, 49.9, "Vancouver\nIsland", adj = 1, cex = 0.7)
  arrows(-128.3, 49.9, -127.4, 49.8, length = 0.05)
  text(-129.5, 51.6, "Queen\nCharlotte\nSound", adj = 0.5, cex = 0.6)
  text(-133.5, 54.5, "Dixon Entrance", adj = 1, cex = 0.7)
  arrows(-133.5, 54.45, -132.5, 54.45, length = 0.05)
  text(-126.2, 48.5, "Strait of\nJuan de Fuca", adj = 1, cex = 0.7)
  arrows(-126.2, 48.5, -124.7, 48.5, length = 0.05)

  text(-128, 39, "California Current LME", cex = 1.4, srt = 285)
  text(-138, 56.8, "Gulf Of Alaska\nLME", cex = 0.9, srt = 300)

  par <- oldpar
}


