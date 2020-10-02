
  Sys.Date() # "2020-10-01"
  
  library(RSelenium)
  library(wdman)
  library(rvest)
  library(XML)
  library(profvis)
  #driver<- rsDriver()
  #remDr <- driver[["client"]]
  
  # or
  selServ <- selenium(jvmargs = c("-Dwebdriver.chrome.verboseLogging=true"))
  remDr <- remoteDriver(port = 4567L, browserName = "chrome")
  remDr$open()
  selServ$log()
  
  # Go to DJPK website
  remDr$navigate("http://www.djpk.kemenkeu.go.id/portal/data/apbd")
  pause(3)
  
  
  ##### 1. APBN/TKDN #####
  
  apbn   <- replicate(11, c())
  names(apbn) <- any <- 2010:2020
  tkdn   <- replicate(3, c())
  names(tkdn) <- tny <- 2018:2020
  
  # Apbn loop
  rdme <- c('APBN Notes:')
  pb   <- txtProgressBar(min = 0, max = length(apbn), initial = 0, style = 3)
  for (t in 1:length(apbn)) {
    
    # Toggle filter
    sel <- remDr$findElement(using = 'xpath', 
                             value = '//*[@id="accordion"]/div/div[1]/h4/a')
    sel$clickElement()
    pause(1)
    
    # Click APBN
    sel <- remDr$findElement(using = 'xpath', 
                             value = '/html/body/div/div/div/section[2]/div[1]/div/div/div/div/div[2]/div/form/div/div[1]/div/span/span[1]/span/span[1]')
    sel$clickElement()
    sel <- remDr$findElement(using = 'xpath',
                             value = '/html/body/span/span/span[2]/ul/li[2]')
    sel$clickElement()
    pause(1)
    
    # Click Year
    sel <- remDr$findElement(using = 'xpath', 
                             value = '/html/body/div/div/div/section[2]/div[1]/div/div/div/div/div[2]/div/form/div/div[2]/div/span/span[1]/span/span[1]')
    sel$clickElement()
    vy  <- paste0('/html/body/span/span/span[2]/ul/li[', t, ']')
    sel <- remDr$findElement(using = 'xpath',
                             value = vy)
    sel$clickElement()
    pause(1)
    
    # Submit
    sub <- remDr$findElement(using = 'xpath',
                             value = '//*[@id="filter"]/div/div[5]/div/button')
    sub$clickElement()
    pause(5)
    
    # Grab data
    source <- remDr$getPageSource()[[1]]
    data <- read_html(source) %>% 
      html_node('table') %>% 
      html_table(fill = T)
    wna  <- apply(data, 2, function(x) length(which(is.na(x))))
    r <- which(wna == nrow(data))
    if (length(r)!=0) {
      data <- data[,-r]
    }
    data$Tipe  <- 'APBN'
    data$Tahun <- any[t]
    apbn[[t]] <- data
    
    txt <- read_html(source) %>%
      html_node(xpath = '/html/body/div/div/div/section[2]/div[2]/div/div/p/label') %>%
      html_text()
    rdme <- c(rdme, paste0(any[t], ': ', txt))
    
    # Toggle filter off
    sel <- remDr$findElement(using = 'xpath', 
                             value = '//*[@id="accordion"]/div/div[1]/h4/a')
    sel$clickElement()
    pause(1)
    setTxtProgressBar(pb, t)
    
  }
  
    apbn <- do.call('rbind', apbn)
    saveRDS(apbn, '01_Data/02_Temp/apbn1020.rds')
    
  # Tkpd loop
  rdme <- c(rdme, 'TKDN Notes:')
  pb   <- txtProgressBar(min = 0, max = length(tkdn), initial = 0, style = 3)
  for (t in 1:length(tkdn)) {
    
    # Toggle filter
    sel <- remDr$findElement(using = 'xpath', 
                             value = '//*[@id="accordion"]/div/div[1]/h4/a')
    sel$clickElement()
    pause(1)
    
    # Click APBN
    sel <- remDr$findElement(using = 'xpath', 
                             value = '/html/body/div/div/div/section[2]/div[1]/div/div/div/div/div[2]/div/form/div/div[1]/div/span/span[1]/span/span[1]')
    sel$clickElement()
    sel <- remDr$findElement(using = 'xpath',
                             value = '/html/body/span/span/span[2]/ul/li[1]')
    sel$clickElement()
    pause(1)
    
    # Click Year
    sel <- remDr$findElement(using = 'xpath', 
                             value = '/html/body/div/div/div/section[2]/div[1]/div/div/div/div/div[2]/div/form/div/div[2]/div/span/span[1]/span/span[1]')
    sel$clickElement()
    vy  <- paste0('/html/body/span/span/span[2]/ul/li[', tny[t]-min(any)+1, ']')
    sel <- remDr$findElement(using = 'xpath',
                             value = vy)
    sel$clickElement()
    pause(1)
    
    # Submit
    sub <- remDr$findElement(using = 'xpath',
                             value = '//*[@id="filter"]/div/div[5]/div/button')
    sub$clickElement()
    pause(5)
    
    # Grab data
    source <- remDr$getPageSource()[[1]]
    data <- read_html(source) %>% 
      html_node('table') %>% 
      html_table(fill = T)
    wna  <- apply(data, 2, function(x) length(which(is.na(x))))
    r <- which(wna == nrow(data))
    if (length(r)!=0) {
      data <- data[,-r]
    }
    data$Tipe  <- 'TKDN'
    data$Tahun <- tny[t]
    tkdn[[t]] <- data
    
    txt <- read_html(source) %>%
      html_node(xpath = '/html/body/div/div/div/section[2]/div[2]/div/div/p/label') %>%
      html_text()
    rdme <- c(rdme, paste0(tny[t], ': ', txt))
    
    # Toggle filter off
    sel <- remDr$findElement(using = 'xpath', 
                             value = '//*[@id="accordion"]/div/div[1]/h4/a')
    sel$clickElement()
    pause(1)
    setTxtProgressBar(pb, t)
    
  }
  
    tkdn <- do.call('rbind', tkdn)
    saveRDS(tkdn, '01_Data/02_Temp/tkdn1820.rds')
    
    fileConn<-file("01_Data/02_Temp/readme.txt")
    writeLines(rdme, fileConn)
    close(fileConn)
    
  
    
  ##### 2. APBD/TKDD #####
    
  # list is by province
  N <- 34
  remDr$navigate("http://www.djpk.kemenkeu.go.id/portal/data/apbd")
  pause(3)
    
    
  # Apbn loop
  for (t in 1:N) {
    
    # Toggle filter
    sel <- remDr$findElement(using = 'xpath', 
                             value = '//*[@id="accordion"]/div/div[1]/h4/a')
    sel$clickElement()
    pause(1)
    
    # Click APBN
    sel <- remDr$findElement(using = 'xpath', 
                             value = '/html/body/div/div/div/section[2]/div[1]/div/div/div/div/div[2]/div/form/div/div[1]/div/span/span[1]/span/span[1]')
    sel$clickElement()
    sel <- remDr$findElement(using = 'xpath',
                             value = '/html/body/span/span/span[2]/ul/li[2]')
    sel$clickElement()
    pause(1)
    
    # Click daearah
    sel <- remDr$findElement(using = 'xpath', 
                             value = '/html/body/div/div/div/section[2]/div[1]/div/div/div/div/div[2]/div/form/div/div[3]/div/span/span[1]/span/span[1]')
    sel$clickElement()
    vp  <- paste0('/html/body/span/span/span[2]/ul/li[', t+1, ']')
    sel <- remDr$findElement(using = 'xpath',
                             value = vp)
    sel$clickElement()
    pause(1)
    
    # kabupaten list
    kab <- remDr$findElement(using = 'xpath',
                             value = '/html/body/div/div/div/section[2]/div[1]/div/div/div/div/div[2]/div/form/div/div[4]/div/span/span[1]/span/span[1]')
    kab$clickElement()
    kabname     <- kab$getElementText()[[1]]
    
    kablist <- remDr$findElement(using = 'xpath',
                                 value = '/html/body/span/span/span[2]/ul')
    kablistname <- kablist$getElementText()[[1]]
    kablistname <- strsplit(kablistname, '\\n')[[1]]
    
    # Loop for Kabupaten Here
    print(paste0('>>>>> ', kabname, ' ====='))
    for (k in 1:length(kablistname)) {
      
      # Click kabupaten
      if (k != 1) {
        kab$clickElement()
      }
      print(paste0(' - ', kablistname[k]))
      vk  <- paste0('/html/body/span/span/span[2]/ul/li[', k, ']')
      pem <- remDr$findElement(using = 'xpath',
                               value = vk)
      pem$clickElement()
      
      # APBD
      sel <- remDr$findElement(using = 'xpath', 
                               value = '/html/body/div/div/div/section[2]/div[1]/div/div/div/div/div[2]/div/form/div/div[1]/div/span/span[1]/span/span[1]')
      sel$clickElement()
      sel <- remDr$findElement(using = 'xpath',
                               value = '/html/body/span/span/span[2]/ul/li[2]')
      sel$clickElement()
      pause(1)
      
      ylist <- list()
      print('      + APBD: ')
      pb   <- txtProgressBar(min = 0, max = length(any), initial = 0, style = 3)
      for (y in 1:length(any)) {
        
        # Click Year
        sel <- remDr$findElement(using = 'xpath', 
                                 value = '/html/body/div/div/div/section[2]/div[1]/div/div/div/div/div[2]/div/form/div/div[2]/div/span/span[1]/span/span[1]')
        sel$clickElement()
        vy  <- paste0('/html/body/span/span/span[2]/ul/li[', y, ']')
        sel <- remDr$findElement(using = 'xpath',
                                 value = vy)
        sel$clickElement()
        pause(1)
        
        # Submit
        sub <- remDr$findElement(using = 'xpath',
                                 value = '//*[@id="filter"]/div/div[5]/div/button')
        sub$clickElement()
        pause(7)
        
        # Grab data
        source <- remDr$getPageSource()[[1]]
        data <- read_html(source) %>% 
          html_node('table') %>% 
          html_table(fill = T)
        wna  <- apply(data, 2, function(x) length(which(is.na(x))))
        r <- which(wna == nrow(data))
        if (length(r)!=0) {
          data <- data[,-r]
        }
        data$Tipe  <- 'APBD'
        data$Tahun <- any[y]
        data$Prov  <- kabname
        data$Kab   <- kablistname[k]
        ylist <- append(ylist, list(data))
        setTxtProgressBar(pb, y)
        
      } 
      print("")
      
      # Bind APBD
      coln  <- colnames(ylist[[1]])
      ylist <- lapply(ylist, function(x) {
        colnames(x) <- coln
        return(x)
      })
      apbd <- do.call('rbind', ylist)
      
      # TKDD
      sel <- remDr$findElement(using = 'xpath', 
                               value = '/html/body/div/div/div/section[2]/div[1]/div/div/div/div/div[2]/div/form/div/div[1]/div/span/span[1]/span/span[1]')
      sel$clickElement()
      sel <- remDr$findElement(using = 'xpath',
                               value = '/html/body/span/span/span[2]/ul/li[1]')
      sel$clickElement()
      pause(1)
      
      ylist <- list()
      print('      + TKDD: ')
      pb   <- txtProgressBar(min = 0, max = length(tny), initial = 0, style = 3)
      for (y in 1:length(tny)) {
        
        # Click Year
        sel <- remDr$findElement(using = 'xpath', 
                                 value = '/html/body/div/div/div/section[2]/div[1]/div/div/div/div/div[2]/div/form/div/div[2]/div/span/span[1]/span/span[1]')
        sel$clickElement()
        vy  <- paste0('/html/body/span/span/span[2]/ul/li[', tny[y]-min(any)+1, ']')
        sel <- remDr$findElement(using = 'xpath',
                                 value = vy)
        sel$clickElement()
        pause(1)
        
        # Submit
        sub <- remDr$findElement(using = 'xpath',
                                 value = '//*[@id="filter"]/div/div[5]/div/button')
        sub$clickElement()
        pause(7)
        
        # Grab data
        source <- remDr$getPageSource()[[1]]
        data <- read_html(source) %>% 
          html_node('table') %>% 
          html_table(fill = T)
        wna  <- apply(data, 2, function(x) length(which(is.na(x))))
        r <- which(wna == nrow(data))
        if (length(r)!=0) {
          data <- data[,-r]
        }
        data$Tipe  <- 'TKDD'
        data$Tahun <- tny[y]
        data$Prov  <- kabname
        data$Kab   <- kablistname[k]
        ylist <- append(ylist, list(data))
        setTxtProgressBar(pb, y)
        
      } 
      print("")
      
      # Bind TKDD
      coln  <- colnames(ylist[[1]])
      ylist <- lapply(ylist, function(x) {
        colnames(x) <- coln
        return(x)
      })
      tkdd <- do.call('rbind', ylist)
      colnames(tkdd) <- colnames(apbd)
      
      # Bind both APBD and TKDD
      dana <- rbind(apbd, tkdd)
      saveRDS(dana, file.path('01_Data/02_Temp/apbd', 
                              paste0(sprintf('%02d', t), sprintf('%02d', k), '.rds')))
      
    }
    
    # Toggle filter off
    sel <- remDr$findElement(using = 'xpath', 
                             value = '//*[@id="accordion"]/div/div[1]/h4/a')
    sel$clickElement()
    pause(1)
    
  }
  
  
  
  