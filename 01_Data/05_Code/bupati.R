
  # Just regress mayor, idiot!
  library(data.table)
  
  apbn  <- readRDS('01_Data/04_Output/apbd1020tkdd1820.rds')
  price <- readRDS('01_Data/02_Temp/annualprices.rds')
  basin <- readRDS('01_Data/04_Output/basin.rds')
  bprov <- readRDS('01_Data/04_Output/basinprov.rds')
  over  <- readRDS('01_Data/04_Output/paneloverlap.rds')
  bdf   <- readRDS('02_Analysis/01_Raw/Bupati.rds') %>%
    dplyr::ungroup()
  gdf   <- readRDS('02_Analysis/01_Raw/Gubernur.rds') %>%
    dplyr::ungroup()
  lhkpn <- readRDS('02_Analysis/01_Raw/lhkpn.rds')
  
  
  BDF <- bdf
  
  # Combine bupati and prices etc
  bdf <- bdf[-which(bdf$year < 2005),]
  bdf$logidr  <- log(bdf$idr)
  basin$idkab <- gsub('^ID', '', basin$ADM2_PCODE)
  over$idkab  <- as.character(over$bpscode)
  bdf$year    <- bdf$yearreport
  
  bdf <- merge(bdf, basin, all.x = T)  
  bdf <- merge(bdf, price, all.x = T)
  overlap <- apply(bdf, 1, function(x) {
    which(over$Year == x[1] &
          over$idkab == x[2])
  })
  odf <- lapply(overlap, function(x) over[x, c('daysover', 'yearover')])
  wo  <- which(lengths(overlap)==0)
  odf[wo] <- replicate(length(wo), data.frame(daysover = NA, 
                                                  yearover = NA), simplify = F)
  
  bdf <- cbind(bdf, do.call('rbind', odf))
  bdf$pulau <- as.numeric(substr(as.character( bdf$idprov), 1, 1))
  bdf$pulau[which(bdf$pulau == 2)] <- 1
  
  # Reporting year issue
  y1 <- with(bdf, which(yearreport < Year1))
  y2 <- with(bdf, which(yearreport >= Year1 &
                        yearreport <= Year2))
  y3 <- setdiff(1:nrow(bdf), c(y1,y2))
  bdf$yyy <- bdf$yearreport - bdf$Year2
  
  # Create time trend
  time <- bdf$year
  matime <- 2005:2017
  wmatime<- lapply(bdf$year,
                   function(x) {
                     y <- match(matime, x)
                     y[which(is.na(y))] <- 0
                     y[which(y!=0)] <- 1
                     return(y)
                   })
  ttrend <- do.call('rbind', wmatime)
  ttrend <- as.data.frame(ttrend)
  bdf <- cbind(bdf, ttrend)
  
  
  # Regress
  library(lfe)
  library(stargazer)
  
  
  
  
  summary(reg1 <- felm(logidr ~ yearover + SedimentKM2 * OIL +
                         yyy + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8  + V9 + V10 + V11 + V12 
                  | idprov | 0 | 0, data = bdf), 
          robust = T)
  summary(reg2 <- felm(logidr ~ yearover + SedimentKM2 * LNG +
                         yyy + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8  + V9 + V10 + V11 + V12 
                       | idprov | 0 | 0, data = bdf), 
          robust = T)
  summary(reg3 <- felm(logidr ~ yearover + SedimentKM2 * COAL +
                         yyy + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8  + V9 + V10 + V11 + V12 
                       | idprov | 0 | 0, data = bdf), 
          robust = T)
  summary(reg4 <- felm(logidr ~ yearover * SedimentKM2 * OIL +
                         yyy + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8  + V9 + V10 + V11 + V12 
                       | idprov | 0 | 0, data = bdf), 
          robust = T)
  summary(reg5 <- felm(logidr ~ yearover * SedimentKM2 * LNG +
                         yyy + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8  + V9 + V10 + V11 + V12 
                       | idprov | 0 | 0, data = bdf), 
          robust = T)
  summary(reg6 <- felm(logidr ~ yearover * SedimentKM2 * COAL +
                         yyy + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8  + V9 + V10 + V11 + V12 
                       | idprov | 0 | 0, data = bdf), 
          robust = T)
  
  
  
  # save robust standard errors
  robse <- function(mod) as.vector(summary(mod, robust = T)$coefficients[,2])
  
  # print stargazer output with robust standard errors
  stargazer(reg1, reg2, reg3, reg4, reg5, reg6,
            type = "latex", se = list(robse(reg1),
                                    robse(reg2),
                                    robse(reg3),
                                    robse(reg4),
                                    robse(reg5),
                                    robse(reg6)),
            omit.stat = 'ser',
            covariate.labels = c("Overlap", "Sedi. Basin", "Oil (USD)", "LNG (USD)", "Coal (USD)",
                                 "Year", paste0('V', 1:12), 
                                 "Overlap * Basin", 
                                 "Overlap * Oil", "Basin * Oil",
                                 "Overlap * LNG", "Basin * LNG",
                                 "Overlap * Coal", "Basin * Coal",
                                 "O * B * Oil",
                                 "O * B * LNG",
                                 "O * B * Coal"),
            dep.var.labels  = "Log IDR Reported Asset")
  
    
    