   
  library(data.table)

  apbn  <- readRDS('01_Data/04_Output/apbd1020tkdd1820.rds')
  price <- readRDS('01_Data/02_Temp/annualprices.rds')
  basin <- readRDS('01_Data/04_Output/basin.rds')
  bprov <- readRDS('01_Data/04_Output/basinprov.rds')
  over  <- readRDS('01_Data/04_Output/paneloverlap.rds')
  
    # Merge price and basin
    colnames(price)[1] <- 'Tahun'
    apbn  <- merge(apbn, price, all.x = T)    
    
    # Aggregate basin to province and all
    colnames(bprov)[1] <- 'ADM2_PCODE'
    bprov$ADM2_PCODE   <- paste0(bprov$ADM2_PCODE, '00')
    basin <- basin[,colnames(bprov)]
    basin <- rbind(basin, bprov)    
    colnames(basin)[1] <- 'bpscode'
    basin$bpscode  <- as.numeric(gsub('ID', '', basin$bpscode))
    
    # Merge basin and apbn
    apbn  <- merge(apbn, basin, all.x = T)
    
    # Select relevant rows and reshape into panel
    ucode <- unique(apbn$Akuncode)
    codes <- c('APBD.REV.TOTAL', 'APBD.REV.OWN.SUM',
               'APBD.REV.TRA.SUM', 'APBD.REV.TRA.DBH', 'APBD.REV.TRA.DAU', 'APBD.REV.TRA.DAK',
               grep('TKDD.DBH', ucode, value = T),
               'TKDD.DAU.DAU')
    apbn  <- apbn[which(apbn$Akuncode %in% codes),]
    apbn  <- apbn[,-c(3,6:10,12)]
    colnames(apbn)[c(3,4)] <- c('A', 'R')
    cols  <- colnames(apbn)[setdiff(1:ncol(apbn), 3:5)]
    apbn  <- apbn[-which(duplicated(apbn)),]
    cast.apbn <- reshape(apbn, 
                         idvar=cols, 
                         timevar='Akuncode', 
                         direction='wide')
    cast.apbn$prov <- substr(as.character(cast.apbn$bpscode), 1, 2)
    
    colnames(over) <- c('prov', 'bpscode', 'Tahun', 'daysover', 'yearover')
    over$prov <- as.character(over$prov)
    cast.apbn <- merge(cast.apbn, over, all.x = T)  
    
  # Regress
      # 1. Level vs log
      # 2. Land  vs sea vs total
      # 3. Kind of budget
      # 4. Fixed effects and controls
    
    provonly <- which(substr(as.character(cast.apbn$bpscode), 3, 4) == '00')
    summary(lm(R.TKDD.DBH.SUM ~ OIL * SedimentKM2 + daysover + as.factor(prov), 
               data = cast.apbn))
    
    summary(lm(log(R.APBD.REV.TRA.DAU) ~ OIL * SedimentKM2 + daysover + as.factor(prov), 
               data = cast.apbn))
    
    
    # RUN this
    
    # Report overlap
    
    # Conclude
    
    
    
    
    