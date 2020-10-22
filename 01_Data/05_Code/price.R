
  p <- read.csv('01_Data/01_Raw/rawprices.csv')
  p$year <- as.numeric(substr(p$month, 1, 4))
  
  library(corrplot)
  w <- which(is.na(p$LNG))
  M <- cor(p[-w,2:(ncol(p)-1)])
  corrplot(M, method = 'number')
  
  # just do oilgascoal for now
  # others might not have been copied right
  price <- p[-w, c('OIL','COAL','LNG', 'year')]
  price <- aggregate(. ~ year, data = price, mean)
  saveRDS(price, '01_Data/02_Temp/annualprices.rds')
  