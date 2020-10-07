
  folder <- c('01_Data/02_Temp/apbd')
  files  <- list.files(folder)
  pcode  <- as.numeric(substr(files, 1, 2))
  rds    <- file.path(folder, files)
  data   <- lapply(rds, readRDS)
  provcode <- read.csv('01_Data/01_Raw/provcode.csv')
  bpscode  <- provcode$bpscode[pcode]
  provname <- provcode$prov[pcode]
  data     <- Map(function(x, y, z) {
    x$Prov <- y
    x$bpscode <- z
    return(x)
  }, data, as.list(provname), as.list(bpscode))
  cname    <- colnames(data[[1]])
  data     <- lapply(data, function(x) {
    colnames(x) <- cname
    return(x)
  })
  data     <- do.call('rbind', data)
  
  # Clean numbers
  data <- data[-which(is.na(data$Akun)),]
  connum <- function(x)  as.numeric(gsub('\\,', '.', 
                                    gsub('\\.|\\-', '', x)))
  data$Anggaran  <- connum(data$Anggaran)
  data$Realisasi <- connum(data$Realisasi)
  data$Persen    <- data$Realisasi/data$Anggaran * 100
  data$Persen[which(!is.finite(data$Persen))] <- NA
  data$`%` <- NULL
  
  # Add categories
  code <- read.csv('01_Data/01_Raw/djpkcode.csv')
  # merge now
  
  # Combine with BPS kabupatne code
  # combine provcode
  
  # Check sums
  
  
  
  # Summary statistics
  
  
  