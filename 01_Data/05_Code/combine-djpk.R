
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
  code <- code[-2,-2] # Remove direct CIV for now
  colnames(code) <- c('Akun', 'Code')
  data <- dplyr::left_join(data, code)
  # replace indirect CIV
  civ  <- which(data$Code=='APBD.EXP.DIR.SUM') + 1
  data$Code[civ] <- ifelse(data$Code[civ] == 'APBD.EXP.IND.CIV', 'APBD.EXP.DIR.CIV',
                           data$Code[civ])
  
  # Combine with BPS kabupatne code
  bps  <- read.csv('01_Data/01_Raw/bpsdagri_code_crosswalk.csv')
  bps$dagricode <- sprintf('%.2f', bps$dagricode)
  data <- dplyr::left_join(data, bps)
  colnames(data) <- c('Akun', 'Alokasi', 'Realisasi', 'Tipe', 'Tahun',
                      'Provinsi', 'Kabupaten', 'Provcode', 'Persen', 'Akuncode', 'bpscode', 'dagricode')
    
  # Check sums
  
  # Summary statistics
  
  # Save data
  saveRDS(data, '01_Data/04_Output/apbd1020tkdd1820.rds')
  
  # Nkabkota
  kode <- sort(unique(as.character(data$bpscode)))
  N    <- length(which(!grepl('..(00|99)', kode))) # No jakarta kota/pulauseribu
  
  