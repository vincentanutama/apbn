
  # Construct simple overlap
  apbn   <- readRDS('01_Data/04_Output/apbd1020tkdd1820.rds')
  ap     <- unique(apbn[, c('Provinsi', 'Provcode')])
  tenure <- readRDS('01_Data/01_Raw/simple-tenure.rds')
  sumat  <- grep('Sumatra', tenure$province)
  tenure$province[sumat] <- gsub('Sumatra', 'Sumatera', 
                                 grep('Sumatra', tenure$province, value = T))
  tp     <- unique(tenure$province)[-1]
  op     <- match(tp, ap$Provinsi)
  op[which(is.na(op))] <- c(30, 13, 14, 21, 22)
  tpdf   <- data.frame(province = tp,
                       Provinsi = ap$Provinsi[op],
                       stringsAsFactors = F)
  tpdf   <- merge(tpdf, ap)
  tenure <- merge(tenure, tpdf, all.x = T)
  
  # Separate province
  w <- which(tenure$province=='')
  tenprov <- tenure[ w,]
  tenure  <- tenure[-w,]
  op     <- match(tenprov$admin, ap$Provinsi)
  op[which(is.na(op))] <- c(5, 8, 21, 22, 30, 3, 4)
  tenprov$Provinsi     <- ap$Provinsi[op]
  tenprov$Provcode     <- ap$Provcode[op]
  
  # Merge kabupaten
  kablist <- unique(apbn[,c('Provcode', 'Kabupaten', 'bpscode')])
  not     <- which(substr(as.character(kablist$bpscode), 3, 4) %in% c('00', '99'))
  kablist <- kablist[-not,]
  kabupaten <- grep('^Kab\\.', kablist$Kabupaten )
  kablist$Kabupaten[kabupaten] <- gsub('^Kab\\.', 'Kabupaten',
                                       kablist$Kabupaten[kabupaten])
  colnames(tenure)[2] <- 'Kabupaten'
    am <- match(tenure$Kabupaten, kablist$Kabupaten)
    am[which(is.na(am))] <- c(317, 110, 174, 286, 293, 473, 490, 147,   7, 344, 
                              498, 493, 494,   6, 366, 380, 381, 378, 421, 411,
                              412, 410, 409, 413, 391,  86,  73, 118, 119, 115,
                              122, 121,  46,  48)
    tenure$bpscode <- kablist$bpscode[am]
    provtenure <- tenprov[, c('tenureEnd', 'electionDate', 'Provcode')]
    colnames(provtenure)[1:2] <- c('tprov', 'eprov')
    tenure <- merge(tenure, provtenure, all.x = T)    
  
  ## construct overlap panel
  ## Simplistic, because we ignore reelection
    
    w <- which(is.na(tenure$tenureEnd))
    tenure$tenureEnd[w] <- tenure$electionDate[w]
    tenure$Year <- year(tenure$tenureEnd)
    ddays <- as.numeric(tenure$tenureEnd - tenure$tprov)
    w <- which(ddays<0)
    ddays[w] <- 365*5 + ddays[w]    
    w <- which(ddays < 365)
    ddays[w] <- 365 * 5 - ddays[w]
    tenure$daysover <- ddays
    tenure$daysover[which(is.na(tenure$daysover) |
                          tenure$daysover > 365 * 5)] <- 365*5
    tenure$yearover <- tenure$daysover %/% 365
    
    # Create tenure panel
    tenpan <- tenure[,c('Provcode', 'bpscode', 'Year', 'daysover', 'yearover')]
    tenpan$Yearnna <- tenpan$Year
    tenpan$daysnna <- tenpan$daysover
    tenpan <- apply(tenpan, 1, function(x) {
      y    <- setdiff(2005:2020, x[3])
      N    <- length(y)
      df   <- data.frame(Provcode = rep(x[1], N),
                         bpscode  = rep(x[2], N),
                         Year     = y,
                         daysover = NA,
                         yearover = NA,
                         Yearnna  = rep(x[3], N),
                         daysnna  = rep(x[4], N),
                         stringsAsFactors = F)
      return(rbind(df,x))
    })
    tenpan <- do.call('rbind', tenpan)
    tenpan <- with(tenpan, tenpan[order(Provcode, bpscode, Year),])
    rownames(tenpan) <- 1:nrow(tenpan)
    tenpan$Yearnna   <- with(tenpan, Year - Yearnna)
    tenpan$daysover  <- with(tenpan, daysnna + Yearnna * 365 )    
    tenpan$Yearnna   <- tenpan$daysnna <- NULL
    tenpan <- tenpan[-which(duplicated(tenpan)),]
    
    w <- which(tenpan$daysover < 0)
    tenpan$daysover[w] <- 365*5 + tenpan$daysover[w]
    w <- which(tenpan$daysover < 0)
    tenpan$daysover[w] <- 365*5 + tenpan$daysover[w]
    
    w <- setdiff(which(tenpan$daysover > 365 * 5), w)
    tenpan$daysover[w] <- tenpan$daysover[w] - 365 * 5
    w <- setdiff(which(tenpan$daysover > 365 * 5), w)
    tenpan$daysover[w] <- tenpan$daysover[w] - 365 * 5
    w <- setdiff(which(tenpan$daysover > 365 * 5), w)
    tenpan$daysover[w] <- tenpan$daysover[w] - 365 * 5
    
    tenpan$yearover    <- tenpan$daysover %/% 365 + 1
    tenpan$yearover[which(tenpan$yearover > 5)] <- 5    
    
    saveRDS(tenpan, '01_Data/04_Output/paneloverlap.rds')
    
        