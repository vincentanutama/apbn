
  
  # Raster
  library(raster)
  basin <- raster('/Users/vincent/Dropbox (Personal)/Research/vincent/idnresources/sediment-raster.tif')
  e <- c(141, 2420, 1245, 2190)
  idns <- c(94.328, 141.042, -10.837, 7.794)
  basin <- crop(basin, e)
  basin[which(getValues(basin) == 255) ] <- NA
  basin[which(!is.na(getValues(basin)))] <- 1
  extent(basin) <- idns
  crs(basin) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  saveRDS(basin, '01_Data/01_Raw/sedimentary.rds')
  basinsh    <- rasterToPolygons(basin)
  basinsf    <- st_as_sf(basinsh)
  saveRDS(basinsf, '01_Data/01_Raw/sedimentarysh.rds')
  
  
  # Crop 
  library(sf)
  idn0 <- st_read('01_Data/01_Raw/idnshape/idn_admbnda_adm0_bps_20200401.shp')
  idn2 <- st_read('01_Data/01_Raw/idnshape/idn_admbnda_adm2_bps_20200401.shp')
  idn2 <- idn2[-which(substr(idn2$ADM2_PCODE, 5,5) %in% c('8', '9')),]
  bpsk <- sort(gsub('^ID', '', idn2$ADM2_PCODE))
  pb   <- txtProgressBar(min = 0, max = nrow(idn2), initial = 0, style = 3)
  am2  <- c()
  am2  <- c(am2, NA)
  
  for (i in 1:nrow(idn2)) {
    
    a   <- st_intersection(idn2[i,], basinsf)
    am2 <- c(am2, sum(st_area(a)))
    setTxtProgressBar(pb, i)
    
  }
  saveRDS(am2,  '01_Data/02_Temp/sedimentkabarea.rds')
  
  ar <- st_area(idn2)
  w <- which(as.numeric(am2/ar) >1)
  nw <- setdiff(1:length(am2), w)
  am2  <- as.numeric(am2)
  ar   <- as.numeric(ar)
  over <- mean(abs(am2[w]-ar[w]), na.rm = T)
  am2[w] <- ar[w]
  am2[nw] <- am2[nw] - over
  am2[nw][which(am2[nw] <0)] <- 0
  saveRDS(am2,  '01_Data/02_Temp/sedimentkabarea_edit.rds')
  
  sedidf <- as.data.frame(idn2)[,1:(ncol(idn2)-1)]
  sedidf$CalculatedKM2 <- ar/1000000
  sedidf$SedimentKM2   <- am2/1000000
  sedidf$ShareSediment <- with(sedidf, SedimentKM2/CalculatedKM2)
  w <- which(is.na(sedidf$SedimentKM2))
  sedidf$SedimentKM2[w] <- sedidf$ShareSediment[w] <- 0
  saveRDS(sedidf, '01_Data/02_Temp/TertiarySedi.rds')
  
  
  
  # Make buffer: 0-4mile is kab, 4-12 is prov
  idn1 <- st_read('01_Data/01_Raw/idnshape/idn_admbnda_adm1_bps_20200401.shp')
  idn <- st_union(idn2)
  
  # Note that only 372 kabupatens have sea borders
  kab2  <- st_buffer(idn2, 4/60)
  kab  <- st_difference(kab2, idn)
  # not perfect sea border yet
  pb   <- txtProgressBar(min = 0, max = nrow(kab), initial = 0, style = 3)
  kam2  <- c()
  
  for (i in 1:nrow(kab)) {
    
    a   <- st_intersection(kab[i,], basinsf)
    kam2 <- c(kam2, sum(st_area(a)))
    setTxtProgressBar(pb, i)
    
  }
  saveRDS(kam2,  '01_Data/02_Temp/sedimentkabsea.rds')
  kam2 <- as.numeric(kam2)
  kabsea <- data.frame(ADM2_PCODE = kab$ADM2_PCODE, 
                       Sediment4Mile = kam2/1000000,
                       stringsAsFactors = F)
  kabsea$Buffer4Mile <- as.numeric(st_area(kab))/1000000
  kabsea$ShareBuff4  <- with(kabsea, Sediment4Mile/Buffer4Mile)
  kabsea$ShareBuff4[which(kabsea$ShareBuff4 > 1)] <- 1
  saveRDS(kabsea, '01_Data/02_Temp/sedimentkabsea.rds')
  
  # Prov buffer
  prov <- st_buffer(idn1, 12/60)
  # combine by province
  np <- nrow(prov)
  am2 <- c()
  pb <- txtProgressBar(min=0, max=np, initial = 0, style=3)
  for (i in 1:np) {
    
    ki <- which(kab2$ADM1_EN == prov$ADM1_EN[i])
    kibuf <- st_union(st_geometry(kab2[ki,]))
    pbuf <- st_difference(st_geometry(prov[i,]), kibuf)
    st_geometry(prov[i,]) <- pbuf
    a   <- st_intersection(prov[i,], basinsf)
    am2 <- c(am2, sum(st_area(a)))
    
    setTxtProgressBar(pb, i)
  
  }
  am2 <- as.numeric(am2)
  provsea <- data.frame(ADM1_PCODE = prov$ADM1_PCODE, 
                       Sediment12Mile = am2/1000000,
                       stringsAsFactors = F)
  provsea$Buffer12Mile <- as.numeric(st_area(prov))/1000000
  provsea$ShareBuff12  <- with(provsea, Sediment12Mile/Buffer12Mile)
  saveRDS(provsea,  '01_Data/02_Temp/sedimentprovsea.rds')
  
    # Merge prov and kab buffer
      sedidf <- merge(sedidf, provsea, all.x = T)
      sedidf <- merge(sedidf, kabsea , all.x = T)
      wna    <- which(is.na(sedidf$Sediment4Mile))
      sedidf$Sediment4Mile[wna] <- sedidf$Buffer4Mile[wna] <- 0      
      saveRDS(sedidf, '01_Data/04_Output/basin.rds')
  
      # Create basinprov
      bp  <- aggregate(. ~ ADM1_PCODE,
                       data = sedidf[,c(2, 15:16, 21:22)],
                       sum)
      bpm <- aggregate(. ~ ADM1_PCODE,
                       data = sedidf[,c(2, 18:20)],
                       mean)
      bp$ShareSediment <- with(bp, SedimentKM2/CalculatedKM2)
      bp$ShareBuff4    <- with(bp, Sediment4Mile/Buffer4Mile)
      bp <- merge(bp, bpm)
      saveRDS(bp,  '01_Data/04_Output/basinprov.rds')
  
  