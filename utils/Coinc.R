## Script to calculate the coincidence within and between years

# Try to find the correct working dir
if(file.exists("utils/Coinc_plots_data.RData")) {
  print("The coincidence file is present")
  load("utils/Coinc_plots_data.RData")
  
} else {

#data_coinc <- read.csv("data/DataBean_MET_GYv2.csv",h=T, stringsAsFactors = TRUE)
data_coinc <- data_beans
#str(data_coinc)

tibble(data_coinc)
data_coinc$year <- as.factor(data_coinc$year)

data_coinc.filter<- data_coinc %>%
  unite(name_year_loc, c(name, year_loc), remove = F) %>%
  group_by(name_year_loc) %>%
  dplyr::summarise(Mean = mean(gy_kg_ha, na.rm = TRUE)) %>%
  filter(!is.na(Mean))

data_coinc<- data_coinc %>%
  unite(name_year_loc, c(name, year_loc), remove = F)

data_coinc<- data_coinc %>%
  filter((name_year_loc %in% data_coinc.filter$name_year_loc))


######Loop
## Market classes
mkt <- nlevels(data_coinc$mkt)
## Years
year1 <- nlevels(data_coinc$year)
year2 <- nlevels(data_coinc$year)
year3 <- nlevels(data_coinc$year)
year4 <- nlevels(data_coinc$year)
year5 <- nlevels(data_coinc$year)
year6 <- nlevels(data_coinc$year)

## Locations
loc1 <- nlevels(data_coinc$loc)
loc2 <- nlevels(data_coinc$loc)
loc3 <- nlevels(data_coinc$loc)
loc4 <- nlevels(data_coinc$loc)

for (k in 1:mkt) {
  #k=1    
  ci <- levels(data_coinc$mkt)
  cj <- ci[k]
  
  data_coinc.sub <- droplevels(subset(data_coinc, mkt==cj))
  # Suppress summarise info
  options(dplyr.summarise.inform = FALSE)
  
  #####################
  ## 1 AND 2 YEARS 
  #####################
  
  for (y1 in 1:year1) {
    #y1=1    
    b17 <- levels(data_coinc.sub$year)
    c17 <- b17[y1]
    
    data_coinc.sub.temp1 <- droplevels(subset(data_coinc.sub, year==c17)) %>% 
      group_by(name, year, loc) %>% 
      dplyr::summarise(rep = mean(rep))
    
    ##Locations
    data_loc_BA_Y1 <- droplevels(subset(data_coinc.sub.temp1, loc=="BA"))
    data_loc_SA_Y1 <- droplevels(subset(data_coinc.sub.temp1, loc=="SA"))
    data_loc_TU_Y1 <- droplevels(subset(data_coinc.sub.temp1, loc=="TU"))
    data_loc_HU_Y1 <- droplevels(subset(data_coinc.sub.temp1, loc=="HU"))
    
    for (y2 in 1:year2) {
      #y2=1 
      b18 <- levels(data_coinc.sub$year)
      c18 <- b18[y2]
      
      data_coinc.sub.temp2 <- droplevels(subset(data_coinc.sub, year==c18)) %>% 
        group_by(name, year, loc) %>% 
        dplyr::summarise(rep = mean(rep))
      
      ##Locations
      data_loc_BA_Y2 <- droplevels(subset(data_coinc.sub.temp2, loc=="BA"))
      data_loc_SA_Y2 <- droplevels(subset(data_coinc.sub.temp2, loc=="SA"))
      data_loc_TU_Y2 <- droplevels(subset(data_coinc.sub.temp2, loc=="TU"))
      data_loc_HU_Y2 <- droplevels(subset(data_coinc.sub.temp2, loc=="HU"))
      
      ###################    
      ### Coincidence
      data_coinc_BA<- data_loc_BA_Y1 %>% 
        filter((name %in% data_loc_BA_Y2$name)) %>% 
        group_by(name) %>% 
        dplyr::summarise(count = n()) %>%
        dplyr::summarise(count = length(name)) %>% 
        dplyr::mutate(loc = "BA") %>% 
        dplyr::mutate(year1 = c17) %>% 
        dplyr::mutate(year2 = c18) %>% 
        dplyr::mutate(mkt = cj)
      
      data_coinc_SA<- data_loc_SA_Y1 %>% 
        filter((name %in% data_loc_SA_Y2$name)) %>% 
        group_by(name) %>% 
        dplyr::summarise(count = n()) %>%
        dplyr::summarise(count = length(name)) %>% 
        dplyr::mutate(loc = "SA") %>% 
        dplyr::mutate(year1 = c17) %>% 
        dplyr::mutate(year2 = c18) %>% 
        dplyr::mutate(mkt = cj)
      
      data_coinc_TU<- data_loc_TU_Y1 %>% 
        filter((name %in% data_loc_TU_Y2$name)) %>% 
        group_by(name) %>% 
        dplyr::summarise(count = n()) %>%
        dplyr::summarise(count = length(name)) %>% 
        dplyr::mutate(loc = "TU") %>% 
        dplyr::mutate(year1 = c17) %>% 
        dplyr::mutate(year2 = c18) %>% 
        dplyr::mutate(mkt = cj)
      
      data_coinc_HU<- data_loc_HU_Y1 %>% 
        filter((name %in% data_loc_HU_Y2$name)) %>% 
        group_by(name) %>% 
        dplyr::summarise(count = n()) %>%
        dplyr::summarise(count = length(name)) %>% 
        dplyr::mutate(loc = "HU") %>% 
        dplyr::mutate(year1 = c17) %>% 
        dplyr::mutate(year2 = c18) %>% 
        dplyr::mutate(mkt = cj)
      
      
      data_coinc_final <- rbind(data_coinc_BA,
                                data_coinc_SA,
                                data_coinc_TU,
                                data_coinc_HU )
      
      if(y2==1){data_coinc_final1<-data_coinc_final}else{data_coinc_final1<-rbind(data_coinc_final, data_coinc_final1)}
    }
    
    if(y1==1){data_coinc_final2<-data_coinc_final1}else{data_coinc_final2<-rbind(data_coinc_final2, data_coinc_final1)}
    
  }
  
  data_coinc_final_1Y<- data_coinc_final2 %>% 
    dplyr::mutate(year_final = year1 == year2) %>% 
    filter(year_final == TRUE) %>% 
    dplyr::select(-year_final) %>% 
    unite(years, c('year1','year2'),remove = FALSE)
  
  data_coinc_final_1Y<- data_coinc_final_1Y %>% 
    dplyr::mutate(year1 = as.numeric(year1)) %>% 
    dplyr::mutate(year2 = as.numeric(year2)) 
  
  #data_coinc_final_1Y$years <- as.factor(data_coinc_final_1Y$years)
  
  rem_rep <- c(10, 14, 15, 18, 19, 20, 22, 23, 24, 25, 26, 27, 28, 29, 30)
  
  data_coinc_final_2Y<- data_coinc_final2 %>%
    unite(years, c('year1','year2'),remove = FALSE) %>% 
    filter(!(years %in% data_coinc_final_1Y$years)) %>% 
    arrange(loc) %>% 
    dplyr::group_by(loc) %>% 
    dplyr::mutate(filter=row_number()) %>% 
    filter(!(filter %in% rem_rep)) %>%
    dplyr::select(-c("filter"))
  
  data_coinc_final_2Y<- data_coinc_final_2Y %>% 
    dplyr::mutate(year1 = as.numeric(year1)) %>% 
    dplyr::mutate(year2 = as.numeric(year2)) 
  
  
  #####################
  #####################
  
  #####################
  ## 3 YEARS 
  #####################
  
  for (y1 in 1:year1) {
    #y1=1    
    b17 <- levels(data_coinc.sub$year)
    c17 <- b17[y1]
    
    data_coinc.sub.temp1 <- droplevels(subset(data_coinc.sub, year==c17)) %>% 
      group_by(name, year, loc) %>% 
      dplyr::summarise(rep = mean(rep))
    
    ##Locations
    data_loc_BA_Y1 <- droplevels(subset(data_coinc.sub.temp1, loc=="BA"))
    data_loc_SA_Y1 <- droplevels(subset(data_coinc.sub.temp1, loc=="SA"))
    data_loc_TU_Y1 <- droplevels(subset(data_coinc.sub.temp1, loc=="TU"))
    data_loc_HU_Y1 <- droplevels(subset(data_coinc.sub.temp1, loc=="HU"))
    
    for (y2 in 2:year2) {
      #y2=2
      b18 <- levels(data_coinc.sub$year)
      c18 <- b18[y2]
      
      data_coinc.sub.temp2 <- droplevels(subset(data_coinc.sub, year==c18)) %>% 
        group_by(name, year, loc) %>% 
        dplyr::summarise(rep = mean(rep))
      
      ##Locations
      data_loc_BA_Y2 <- droplevels(subset(data_coinc.sub.temp2, loc=="BA"))
      data_loc_SA_Y2 <- droplevels(subset(data_coinc.sub.temp2, loc=="SA"))
      data_loc_TU_Y2 <- droplevels(subset(data_coinc.sub.temp2, loc=="TU"))
      data_loc_HU_Y2 <- droplevels(subset(data_coinc.sub.temp2, loc=="HU"))
      
      for (y3 in 3:year3) {
        
        b19 <- levels(data_coinc.sub$year)
        c19 <- b19[y3]
        
        data_coinc.sub.temp3 <- droplevels(subset(data_coinc.sub, year==c19)) %>% 
          group_by(name, year, loc) %>% 
          dplyr::summarise(rep = mean(rep))
        
        ##Locations
        data_loc_BA_Y3 <- droplevels(subset(data_coinc.sub.temp3, loc=="BA"))
        data_loc_SA_Y3 <- droplevels(subset(data_coinc.sub.temp3, loc=="SA"))
        data_loc_TU_Y3 <- droplevels(subset(data_coinc.sub.temp3, loc=="TU"))
        data_loc_HU_Y3 <- droplevels(subset(data_coinc.sub.temp3, loc=="HU"))
        
        ###################    
        ### Coincidence
        data_coinc_BA<- data_loc_BA_Y1 %>% 
          filter((name %in% data_loc_BA_Y2$name)) %>% 
          filter((name %in% data_loc_BA_Y3$name)) %>% 
          group_by(name) %>% 
          dplyr::summarise(count = n()) %>%
          dplyr::summarise(count = length(name)) %>% 
          dplyr::mutate(loc = "BA") %>% 
          dplyr::mutate(year1 = c17) %>% 
          dplyr::mutate(year2 = c18) %>% 
          dplyr::mutate(year3 = c19) %>% 
          dplyr::mutate(mkt = cj)
        
        data_coinc_SA<- data_loc_SA_Y1 %>% 
          filter((name %in% data_loc_SA_Y2$name)) %>% 
          filter((name %in% data_loc_SA_Y3$name)) %>% 
          group_by(name) %>% 
          dplyr::summarise(count = n()) %>%
          dplyr::summarise(count = length(name)) %>% 
          dplyr::mutate(loc = "SA") %>% 
          dplyr::mutate(year1 = c17) %>% 
          dplyr::mutate(year2 = c18) %>% 
          dplyr::mutate(year3 = c19) %>% 
          dplyr::mutate(mkt = cj)
        
        data_coinc_TU <- data_loc_TU_Y1 %>% 
          filter((name %in% data_loc_TU_Y2$name)) %>% 
          filter((name %in% data_loc_TU_Y3$name)) %>% 
          group_by(name) %>% 
          dplyr::summarise(count = n()) %>%
          dplyr::summarise(count = length(name)) %>% 
          dplyr::mutate(loc = "TU") %>% 
          dplyr::mutate(year1 = c17) %>% 
          dplyr::mutate(year2 = c18) %>% 
          dplyr::mutate(year3 = c19) %>% 
          dplyr::mutate(mkt = cj)
        
        data_coinc_HU <-  data_loc_HU_Y1 %>% 
          filter((name %in% data_loc_HU_Y2$name)) %>% 
          filter((name %in% data_loc_HU_Y3$name)) %>% 
          group_by(name) %>% 
          dplyr::summarise(count = n()) %>%
          dplyr::summarise(count = length(name)) %>% 
          dplyr::mutate(loc = "HU") %>% 
          dplyr::mutate(year1 = c17) %>% 
          dplyr::mutate(year2 = c18) %>% 
          dplyr::mutate(year3 = c19) %>% 
          dplyr::mutate(mkt = cj)
        
        
        data_coinc_final1 <- rbind(data_coinc_BA,
                                   data_coinc_SA,
                                   data_coinc_TU,
                                   data_coinc_HU )
        
        if(y3==3){data_coinc_final2<-data_coinc_final1}else{data_coinc_final2<-rbind(data_coinc_final2, data_coinc_final1)}
        
      }
      if(y2==2){data_coinc_final3<-data_coinc_final2}else{data_coinc_final3<-rbind(data_coinc_final3, data_coinc_final2)} 
    }
    
    if(y1==1){data_coinc_final4<-data_coinc_final3}else{data_coinc_final4<-rbind(data_coinc_final4, data_coinc_final3)} 
    
  }
  
  
  data_coinc_final_3Y <- data_coinc_final4 %>% 
    dplyr::mutate(year1 = as.numeric(year1)) %>% 
    dplyr::mutate(year2 = as.numeric(year2)) %>% 
    dplyr::mutate(year3 = as.numeric(year3)) %>% 
    arrange(count, loc) %>% 
    rowwise() %>% 
    dplyr::mutate(filter1 = (year1 * year2 * year3)) %>% 
    group_by(loc) %>% 
    distinct(filter1, .keep_all = TRUE) %>% 
    rowwise() %>% 
    dplyr::mutate(filter2 = year2 == year3) %>% 
    dplyr::mutate(filter3 = year1 == year2) %>% 
    dplyr::mutate(filter4 = year1 == year3) %>% 
    filter(filter2 == F & filter3 == F & filter4 == F) %>% 
    dplyr::select(-c('filter1', 'filter2', 'filter3', 'filter4'))
  
  
  #####################
  #####################
  
  #####################
  ## 4 YEARS 
  #####################
  
  for (y1 in 1:year1) {
    #y1=1    
    b17 <- levels(data_coinc.sub$year)
    c17 <- b17[y1]
    
    data_coinc.sub.temp1 <- droplevels(subset(data_coinc.sub, year==c17)) %>% 
      group_by(name, year, loc) %>% 
      dplyr::summarise(rep = mean(rep))
    
    ##Locations
    data_loc_BA_Y1 <- droplevels(subset(data_coinc.sub.temp1, loc=="BA"))
    data_loc_SA_Y1 <- droplevels(subset(data_coinc.sub.temp1, loc=="SA"))
    data_loc_TU_Y1 <- droplevels(subset(data_coinc.sub.temp1, loc=="TU"))
    data_loc_HU_Y1 <- droplevels(subset(data_coinc.sub.temp1, loc=="HU"))
    
    for (y2 in 2:year2) {
      #y2=2
      b18 <- levels(data_coinc.sub$year)
      c18 <- b18[y2]
      
      data_coinc.sub.temp2 <- droplevels(subset(data_coinc.sub, year==c18)) %>% 
        group_by(name, year, loc) %>% 
        dplyr::summarise(rep = mean(rep))
      
      ##Locations
      data_loc_BA_Y2 <- droplevels(subset(data_coinc.sub.temp2, loc=="BA"))
      data_loc_SA_Y2 <- droplevels(subset(data_coinc.sub.temp2, loc=="SA"))
      data_loc_TU_Y2 <- droplevels(subset(data_coinc.sub.temp2, loc=="TU"))
      data_loc_HU_Y2 <- droplevels(subset(data_coinc.sub.temp2, loc=="HU"))
      
      for (y3 in 3:year3) {
        #y3=3
        b19 <- levels(data_coinc.sub$year)
        c19 <- b19[y3]
        
        data_coinc.sub.temp3 <- droplevels(subset(data_coinc.sub, year==c19)) %>% 
          group_by(name, year, loc) %>% 
          dplyr::summarise(rep = mean(rep))
        
        ##Locations
        data_loc_BA_Y3 <- droplevels(subset(data_coinc.sub.temp3, loc=="BA"))
        data_loc_SA_Y3 <- droplevels(subset(data_coinc.sub.temp3, loc=="SA"))
        data_loc_TU_Y3 <- droplevels(subset(data_coinc.sub.temp3, loc=="TU"))
        data_loc_HU_Y3 <- droplevels(subset(data_coinc.sub.temp3, loc=="HU"))
        
        for (y4 in 4:year4) {
          #y3=3
          b20 <- levels(data_coinc.sub$year)
          c20 <- b20[y4]
          
          data_coinc.sub.temp4 <- droplevels(subset(data_coinc.sub, year==c20)) %>% 
            group_by(name, year, loc) %>% 
            dplyr::summarise(rep = mean(rep))
          
          ##Locations
          data_loc_BA_Y4 <- droplevels(subset(data_coinc.sub.temp4, loc=="BA"))
          data_loc_SA_Y4 <- droplevels(subset(data_coinc.sub.temp4, loc=="SA"))
          data_loc_TU_Y4 <- droplevels(subset(data_coinc.sub.temp4, loc=="TU"))
          data_loc_HU_Y4 <- droplevels(subset(data_coinc.sub.temp4, loc=="HU"))
          
          ###################    
          ### Coincidence
          data_coinc_BA<- data_loc_BA_Y1 %>% 
            filter((name %in% data_loc_BA_Y2$name)) %>% 
            filter((name %in% data_loc_BA_Y3$name)) %>% 
            filter((name %in% data_loc_BA_Y4$name)) %>% 
            group_by(name) %>% 
            dplyr::summarise(count = n()) %>%
            dplyr::summarise(count = length(name)) %>% 
            dplyr::mutate(loc = "BA") %>% 
            dplyr::mutate(year1 = c17) %>% 
            dplyr::mutate(year2 = c18) %>% 
            dplyr::mutate(year3 = c19) %>% 
            dplyr::mutate(year4 = c20) %>% 
            dplyr::mutate(mkt = cj)
          
          data_coinc_SA<- data_loc_SA_Y1 %>% 
            filter((name %in% data_loc_SA_Y2$name)) %>% 
            filter((name %in% data_loc_SA_Y3$name)) %>% 
            filter((name %in% data_loc_SA_Y4$name)) %>%
            group_by(name) %>% 
            dplyr::summarise(count = n()) %>%
            dplyr::summarise(count = length(name)) %>% 
            dplyr::mutate(loc = "SA") %>% 
            dplyr::mutate(year1 = c17) %>% 
            dplyr::mutate(year2 = c18) %>% 
            dplyr::mutate(year3 = c19) %>% 
            dplyr::mutate(year4 = c20) %>% 
            dplyr::mutate(mkt = cj)
          
          data_coinc_TU <- data_loc_TU_Y1 %>% 
            filter((name %in% data_loc_TU_Y2$name)) %>% 
            filter((name %in% data_loc_TU_Y3$name)) %>% 
            filter((name %in% data_loc_TU_Y4$name)) %>%
            group_by(name) %>% 
            dplyr::summarise(count = n()) %>%
            dplyr::summarise(count = length(name)) %>% 
            dplyr::mutate(loc = "TU") %>% 
            dplyr::mutate(year1 = c17) %>% 
            dplyr::mutate(year2 = c18) %>% 
            dplyr::mutate(year3 = c19) %>% 
            dplyr::mutate(year4 = c20) %>% 
            dplyr::mutate(mkt = cj)
          
          data_coinc_HU <-  data_loc_HU_Y1 %>% 
            filter((name %in% data_loc_HU_Y2$name)) %>% 
            filter((name %in% data_loc_HU_Y3$name)) %>%
            filter((name %in% data_loc_HU_Y4$name)) %>% 
            group_by(name) %>% 
            dplyr::summarise(count = n()) %>%
            dplyr::summarise(count = length(name)) %>% 
            dplyr::mutate(loc = "HU") %>% 
            dplyr::mutate(year1 = c17) %>% 
            dplyr::mutate(year2 = c18) %>% 
            dplyr::mutate(year3 = c19) %>% 
            dplyr::mutate(year4 = c20) %>% 
            dplyr::mutate(mkt = cj)
          
          
          data_coinc_final1 <- rbind(data_coinc_BA,
                                     data_coinc_SA,
                                     data_coinc_TU,
                                     data_coinc_HU )
          
          if(y4==4){data_coinc_final2<-data_coinc_final1}else{data_coinc_final2<-rbind(data_coinc_final2, data_coinc_final1)}
          
        }
        
        if(y3==3){data_coinc_final3<-data_coinc_final2}else{data_coinc_final3<-rbind(data_coinc_final3, data_coinc_final2)}
      }
      
      if(y2==2){data_coinc_final4<-data_coinc_final3}else{data_coinc_final4<-rbind(data_coinc_final4, data_coinc_final3)}}
    
    if(y1==1){data_coinc_final5<-data_coinc_final4}else{data_coinc_final5<-rbind(data_coinc_final5, data_coinc_final4)}
  }
  
  data_coinc_final_4Y <- data_coinc_final5 %>% 
    dplyr::mutate(year1 = as.numeric(year1)) %>% 
    dplyr::mutate(year2 = as.numeric(year2)) %>% 
    dplyr::mutate(year3 = as.numeric(year3)) %>% 
    dplyr::mutate(year4 = as.numeric(year4)) %>% 
    arrange(count, loc) %>% 
    rowwise() %>% 
    dplyr::mutate(filter1 = (year1 * year2 * year3 * year4)) %>% 
    group_by(loc) %>% 
    distinct(filter1, .keep_all = TRUE) %>% 
    rowwise() %>% 
    dplyr::mutate(filter2 = year1 == year2) %>% 
    dplyr::mutate(filter3 = year1 == year3) %>% 
    dplyr::mutate(filter4 = year2 == year3) %>% 
    dplyr::mutate(filter5 = year2 == year4) %>% 
    dplyr::mutate(filter6 = year3 == year4) %>% 
    dplyr::mutate(filter7 = year1 == year4) %>% 
    filter(filter2 == F & filter3 == F & filter4 == F & filter5 == F & filter6 == F & filter7 == F) %>% 
    dplyr::select(-c('filter1', 'filter2', 'filter3', 'filter4',
                     'filter5', 'filter6', 'filter7'))
  
  
  #####################
  
  ## 5 YEARS 
  #####################
  for (y1 in 1:year1) {
    
    b17 <- levels(data_coinc.sub$year)
    c17 <- b17[y1]
    
    data_coinc.sub.temp1 <- droplevels(subset(data_coinc.sub, year==c17)) %>% 
      group_by(name, year, loc) %>% 
      dplyr::summarise(rep = mean(rep))
    
    ##Locations
    data_loc_BA_Y1 <- droplevels(subset(data_coinc.sub.temp1, loc=="BA"))
    data_loc_SA_Y1 <- droplevels(subset(data_coinc.sub.temp1, loc=="SA"))
    data_loc_TU_Y1 <- droplevels(subset(data_coinc.sub.temp1, loc=="TU"))
    data_loc_HU_Y1 <- droplevels(subset(data_coinc.sub.temp1, loc=="HU"))
    
    for (y2 in 2:year2) {
      
      b18 <- levels(data_coinc.sub$year)
      c18 <- b18[y2]
      
      data_coinc.sub.temp2 <- droplevels(subset(data_coinc.sub, year==c18)) %>% 
        group_by(name, year, loc) %>% 
        dplyr::summarise(rep = mean(rep))
      
      ##Locations
      data_loc_BA_Y2 <- droplevels(subset(data_coinc.sub.temp2, loc=="BA"))
      data_loc_SA_Y2 <- droplevels(subset(data_coinc.sub.temp2, loc=="SA"))
      data_loc_TU_Y2 <- droplevels(subset(data_coinc.sub.temp2, loc=="TU"))
      data_loc_HU_Y2 <- droplevels(subset(data_coinc.sub.temp2, loc=="HU"))
      
      for (y3 in 3:year3) {
        
        b19 <- levels(data_coinc.sub$year)
        c19 <- b19[y3]
        
        data_coinc.sub.temp3 <- droplevels(subset(data_coinc.sub, year==c19)) %>% 
          group_by(name, year, loc) %>% 
          dplyr::summarise(rep = mean(rep))
        
        ##Locations
        data_loc_BA_Y3 <- droplevels(subset(data_coinc.sub.temp3, loc=="BA"))
        data_loc_SA_Y3 <- droplevels(subset(data_coinc.sub.temp3, loc=="SA"))
        data_loc_TU_Y3 <- droplevels(subset(data_coinc.sub.temp3, loc=="TU"))
        data_loc_HU_Y3 <- droplevels(subset(data_coinc.sub.temp3, loc=="HU"))
        
        for (y4 in 4:year4) {
          
          b20 <- levels(data_coinc.sub$year)
          c20 <- b20[y4]
          
          data_coinc.sub.temp4 <- droplevels(subset(data_coinc.sub, year==c20)) %>% 
            group_by(name, year, loc) %>% 
            dplyr::summarise(rep = mean(rep))
          
          ##Locations
          data_loc_BA_Y4 <- droplevels(subset(data_coinc.sub.temp4, loc=="BA"))
          data_loc_SA_Y4 <- droplevels(subset(data_coinc.sub.temp4, loc=="SA"))
          data_loc_TU_Y4 <- droplevels(subset(data_coinc.sub.temp4, loc=="TU"))
          data_loc_HU_Y4 <- droplevels(subset(data_coinc.sub.temp4, loc=="HU"))
          
          for (y5 in 5:year5) {
            
            b21 <- levels(data_coinc.sub$year)
            c21 <- b21[y5]
            
            data_coinc.sub.temp5 <- droplevels(subset(data_coinc.sub, year==c21)) %>% 
              group_by(name, year, loc) %>% 
              dplyr::summarise(rep = mean(rep))
            
            ##Locations
            data_loc_BA_Y5 <- droplevels(subset(data_coinc.sub.temp5, loc=="BA"))
            data_loc_SA_Y5 <- droplevels(subset(data_coinc.sub.temp5, loc=="SA"))
            data_loc_TU_Y5 <- droplevels(subset(data_coinc.sub.temp5, loc=="TU"))
            data_loc_HU_Y5 <- droplevels(subset(data_coinc.sub.temp5, loc=="HU"))
            
            ###################    
            ### Coincidence
            data_coinc_BA<- data_loc_BA_Y1 %>% 
              filter((name %in% data_loc_BA_Y2$name)) %>% 
              filter((name %in% data_loc_BA_Y3$name)) %>% 
              filter((name %in% data_loc_BA_Y4$name)) %>% 
              filter((name %in% data_loc_BA_Y5$name)) %>% 
              group_by(name) %>% 
              dplyr::summarise(count = n()) %>%
              dplyr::summarise(count = length(name)) %>% 
              dplyr::mutate(loc = "BA") %>% 
              dplyr::mutate(year1 = c17) %>% 
              dplyr::mutate(year2 = c18) %>% 
              dplyr::mutate(year3 = c19) %>% 
              dplyr::mutate(year4 = c20) %>% 
              dplyr::mutate(year5 = c21) %>% 
              dplyr::mutate(mkt = cj)
            
            data_coinc_SA<- data_loc_SA_Y1 %>% 
              filter((name %in% data_loc_SA_Y2$name)) %>% 
              filter((name %in% data_loc_SA_Y3$name)) %>% 
              filter((name %in% data_loc_SA_Y4$name)) %>% 
              filter((name %in% data_loc_SA_Y5$name)) %>% 
              group_by(name) %>% 
              dplyr::summarise(count = n()) %>%
              dplyr::summarise(count = length(name)) %>% 
              dplyr::mutate(loc = "SA") %>% 
              dplyr::mutate(year1 = c17) %>% 
              dplyr::mutate(year2 = c18) %>% 
              dplyr::mutate(year3 = c19) %>% 
              dplyr::mutate(year4 = c20) %>% 
              dplyr::mutate(year5 = c21) %>% 
              dplyr::mutate(mkt = cj)
            
            data_coinc_TU <-  data_loc_TU_Y1 %>% 
              filter((name %in% data_loc_TU_Y2$name)) %>% 
              filter((name %in% data_loc_TU_Y3$name)) %>% 
              filter((name %in% data_loc_TU_Y4$name)) %>% 
              filter((name %in% data_loc_TU_Y5$name)) %>% 
              group_by(name) %>% 
              dplyr::summarise(count = n()) %>%
              dplyr::summarise(count = length(name)) %>% 
              dplyr::mutate(loc = "TU") %>% 
              dplyr::mutate(year1 = c17) %>% 
              dplyr::mutate(year2 = c18) %>% 
              dplyr::mutate(year3 = c19) %>% 
              dplyr::mutate(year4 = c20) %>% 
              dplyr::mutate(year5 = c21) %>% 
              dplyr::mutate(mkt = cj)
            
            data_coinc_HU <-  data_loc_HU_Y1 %>% 
              filter((name %in% data_loc_HU_Y2$name)) %>% 
              filter((name %in% data_loc_HU_Y3$name)) %>% 
              filter((name %in% data_loc_HU_Y4$name)) %>% 
              filter((name %in% data_loc_HU_Y5$name)) %>% 
              group_by(name) %>% 
              dplyr::summarise(count = n()) %>%
              dplyr::summarise(count = length(name)) %>% 
              dplyr::mutate(loc = "HU") %>% 
              dplyr::mutate(year1 = c17) %>% 
              dplyr::mutate(year2 = c18) %>% 
              dplyr::mutate(year3 = c19) %>% 
              dplyr::mutate(year4 = c20) %>% 
              dplyr::mutate(year5 = c21) %>% 
              dplyr::mutate(mkt = cj)
            
            
            data_coinc_final1 <- rbind(data_coinc_BA,
                                       data_coinc_SA,
                                       data_coinc_TU,
                                       data_coinc_HU )
            
            if(y5==5){data_coinc_final2<-data_coinc_final1}else{data_coinc_final2<-rbind(data_coinc_final2, data_coinc_final1)}
            
          }
          
          if(y4==4){data_coinc_final3<-data_coinc_final2}else{data_coinc_final3<-rbind(data_coinc_final3, data_coinc_final2)}
        }
        
        if(y3==3){data_coinc_final4<-data_coinc_final3}else{data_coinc_final4<-rbind(data_coinc_final4, data_coinc_final3)}
      }
      
      if(y2==2){data_coinc_final5<-data_coinc_final4}else{data_coinc_final5<-rbind(data_coinc_final5, data_coinc_final4)}
    }
    
    if(y1==1){data_coinc_final6<-data_coinc_final5}else{data_coinc_final6<-rbind(data_coinc_final6, data_coinc_final5)}
    
  }
  
  
  data_coinc_final_5Y <- data_coinc_final6 %>% 
    dplyr::mutate(year1 = as.numeric(year1)) %>% 
    dplyr::mutate(year2 = as.numeric(year2)) %>% 
    dplyr::mutate(year3 = as.numeric(year3)) %>% 
    dplyr::mutate(year4 = as.numeric(year4)) %>% 
    dplyr::mutate(year5 = as.numeric(year5)) %>% 
    arrange(count, loc) %>% 
    rowwise() %>% 
    dplyr::mutate(filter1 = (year1 * year2 * year3 * year4 * year5)) %>% 
    group_by(loc) %>% 
    distinct(filter1, .keep_all = TRUE) %>% 
    rowwise() %>% 
    dplyr::mutate(filter2 = year1 == year2) %>% 
    dplyr::mutate(filter3 = year1 == year3) %>% 
    dplyr::mutate(filter4 = year2 == year3) %>% 
    dplyr::mutate(filter5 = year2 == year4) %>% 
    dplyr::mutate(filter6 = year3 == year4) %>% 
    dplyr::mutate(filter7 = year1 == year4) %>% 
    dplyr::mutate(filter8 = year1 == year5) %>% 
    dplyr::mutate(filter9 = year2 == year5) %>% 
    dplyr::mutate(filter10 = year3 == year5) %>% 
    dplyr::mutate(filter11 = year4 == year5) %>% 
    filter(filter2 == F & filter3 == F & filter4 == F & filter5 == F & filter6 == F & filter7 == F & 
             filter8 == F & filter9 == F & filter10 == F & filter11 == F) %>% 
    dplyr::select(-c('filter1', 'filter2', 'filter3', 'filter4',
                     'filter5', 'filter6', 'filter7',
                     'filter8', 'filter9', 'filter10', "filter11"))
  
  #####################
  #####################
  
  #####################
  ## 6 YEARS 
  #####################
  
  
  b17 <- levels(data_coinc.sub$year)
  c17 <- b17[1]
  
  data_coinc.sub.temp1 <- droplevels(subset(data_coinc.sub, year==c17)) %>% 
    group_by(name, year, loc) %>% 
    dplyr::summarise(rep = mean(rep))
  
  ##Locations
  data_loc_BA_Y1 <- droplevels(subset(data_coinc.sub.temp1, loc=="BA"))
  data_loc_SA_Y1 <- droplevels(subset(data_coinc.sub.temp1, loc=="SA"))
  data_loc_TU_Y1 <- droplevels(subset(data_coinc.sub.temp1, loc=="TU"))
  data_loc_HU_Y1 <- droplevels(subset(data_coinc.sub.temp1, loc=="HU"))
  
  b18 <- levels(data_coinc.sub$year)
  c18 <- b18[2]
  
  data_coinc.sub.temp2 <- droplevels(subset(data_coinc.sub, year==c18)) %>% 
    group_by(name, year, loc) %>% 
    dplyr::summarise(rep = mean(rep))
  
  ##Locations
  data_loc_BA_Y2 <- droplevels(subset(data_coinc.sub.temp2, loc=="BA"))
  data_loc_SA_Y2 <- droplevels(subset(data_coinc.sub.temp2, loc=="SA"))
  data_loc_TU_Y2 <- droplevels(subset(data_coinc.sub.temp2, loc=="TU"))
  data_loc_HU_Y2 <- droplevels(subset(data_coinc.sub.temp2, loc=="HU"))
  
  b19 <- levels(data_coinc.sub$year)
  c19 <- b19[3]
  
  data_coinc.sub.temp3 <- droplevels(subset(data_coinc.sub, year==c19)) %>% 
    group_by(name, year, loc) %>% 
    dplyr::summarise(rep = mean(rep))
  
  ##Locations
  data_loc_BA_Y3 <- droplevels(subset(data_coinc.sub.temp3, loc=="BA"))
  data_loc_SA_Y3 <- droplevels(subset(data_coinc.sub.temp3, loc=="SA"))
  data_loc_TU_Y3 <- droplevels(subset(data_coinc.sub.temp3, loc=="TU"))
  data_loc_HU_Y3 <- droplevels(subset(data_coinc.sub.temp3, loc=="HU"))
  
  b20 <- levels(data_coinc.sub$year)
  c20 <- b20[4]
  
  data_coinc.sub.temp4 <- droplevels(subset(data_coinc.sub, year==c20)) %>% 
    group_by(name, year, loc) %>% 
    dplyr::summarise(rep = mean(rep))
  
  ##Locations
  data_loc_BA_Y4 <- droplevels(subset(data_coinc.sub.temp4, loc=="BA"))
  data_loc_SA_Y4 <- droplevels(subset(data_coinc.sub.temp4, loc=="SA"))
  data_loc_TU_Y4 <- droplevels(subset(data_coinc.sub.temp4, loc=="TU"))
  data_loc_HU_Y4 <- droplevels(subset(data_coinc.sub.temp4, loc=="HU"))
  
  b21 <- levels(data_coinc.sub$year)
  c21 <- b21[5]
  
  data_coinc.sub.temp5 <- droplevels(subset(data_coinc.sub, year==c21)) %>% 
    group_by(name, year, loc) %>% 
    dplyr::summarise(rep = mean(rep))
  
  ##Locations
  data_loc_BA_Y5 <- droplevels(subset(data_coinc.sub.temp5, loc=="BA"))
  data_loc_SA_Y5 <- droplevels(subset(data_coinc.sub.temp5, loc=="SA"))
  data_loc_TU_Y5 <- droplevels(subset(data_coinc.sub.temp5, loc=="TU"))
  data_loc_HU_Y5 <- droplevels(subset(data_coinc.sub.temp5, loc=="HU"))
  
  for (y6 in 6:6) {
    #y3=3
    b22 <- levels(data_coinc.sub$year)
    c22 <- b22[y6]
    
    data_coinc.sub.temp6 <- droplevels(subset(data_coinc.sub, year==c22)) %>% 
      group_by(name, year, loc) %>% 
      dplyr::summarise(rep = mean(rep))
    
    ##Locations
    data_loc_BA_Y6 <- droplevels(subset(data_coinc.sub.temp6, loc=="BA"))
    data_loc_SA_Y6 <- droplevels(subset(data_coinc.sub.temp6, loc=="SA"))
    data_loc_TU_Y6 <- droplevels(subset(data_coinc.sub.temp6, loc=="TU"))
    data_loc_HU_Y6 <- droplevels(subset(data_coinc.sub.temp6, loc=="HU"))
    
    ###################    
    ### Coincidence
    data_coinc_BA<- data_loc_BA_Y1 %>% 
      filter((name %in% data_loc_BA_Y2$name)) %>% 
      filter((name %in% data_loc_BA_Y3$name)) %>% 
      filter((name %in% data_loc_BA_Y4$name)) %>% 
      filter((name %in% data_loc_BA_Y5$name)) %>% 
      filter((name %in% data_loc_BA_Y6$name)) %>%
      group_by(name) %>% 
      dplyr::summarise(count = n()) %>%
      dplyr::summarise(count = length(name)) %>% 
      dplyr::mutate(loc = "BA") %>% 
      dplyr::mutate(year1 = c17) %>% 
      dplyr::mutate(year2 = c18) %>% 
      dplyr::mutate(year3 = c19) %>% 
      dplyr::mutate(year4 = c20) %>% 
      dplyr::mutate(year5 = c21) %>% 
      dplyr::mutate(year6 = c22) %>% 
      dplyr::mutate(mkt = cj)
    
    data_coinc_SA<- data_loc_SA_Y1 %>% 
      filter((name %in% data_loc_SA_Y2$name)) %>% 
      filter((name %in% data_loc_SA_Y3$name)) %>% 
      filter((name %in% data_loc_SA_Y4$name)) %>% 
      filter((name %in% data_loc_SA_Y5$name)) %>% 
      filter((name %in% data_loc_SA_Y6$name)) %>% 
      group_by(name) %>% 
      dplyr::summarise(count = n()) %>%
      dplyr::summarise(count = length(name)) %>% 
      dplyr::mutate(loc = "SA") %>% 
      dplyr::mutate(year1 = c17) %>% 
      dplyr::mutate(year2 = c18) %>% 
      dplyr::mutate(year3 = c19) %>% 
      dplyr::mutate(year4 = c20) %>% 
      dplyr::mutate(year5 = c21) %>% 
      dplyr::mutate(year6 = c22) %>% 
      dplyr::mutate(mkt = cj)
    
    data_coinc_TU <-  data_loc_TU_Y1 %>% 
      filter((name %in% data_loc_TU_Y2$name)) %>% 
      filter((name %in% data_loc_TU_Y3$name)) %>% 
      filter((name %in% data_loc_TU_Y4$name)) %>% 
      filter((name %in% data_loc_TU_Y5$name)) %>% 
      filter((name %in% data_loc_TU_Y6$name)) %>% 
      group_by(name) %>% 
      dplyr::summarise(count = n()) %>%
      dplyr::summarise(count = length(name)) %>% 
      dplyr::mutate(loc = "TU") %>% 
      dplyr::mutate(year1 = c17) %>% 
      dplyr::mutate(year2 = c18) %>% 
      dplyr::mutate(year3 = c19) %>% 
      dplyr::mutate(year4 = c20) %>% 
      dplyr::mutate(year5 = c21) %>% 
      dplyr::mutate(year6 = c22) %>% 
      dplyr::mutate(mkt = cj)
    
    data_coinc_HU <-  data_loc_HU_Y1 %>% 
      filter((name %in% data_loc_HU_Y2$name)) %>% 
      filter((name %in% data_loc_HU_Y3$name)) %>% 
      filter((name %in% data_loc_HU_Y4$name)) %>% 
      filter((name %in% data_loc_HU_Y5$name)) %>% 
      filter((name %in% data_loc_HU_Y6$name)) %>% 
      group_by(name) %>% 
      dplyr::summarise(count = n()) %>%
      dplyr::summarise(count = length(name)) %>% 
      dplyr::mutate(loc = "TU") %>% 
      dplyr::mutate(year1 = c17) %>% 
      dplyr::mutate(year2 = c18) %>% 
      dplyr::mutate(year3 = c19) %>% 
      dplyr::mutate(year4 = c20) %>% 
      dplyr::mutate(year5 = c21) %>% 
      dplyr::mutate(year6 = c22) %>% 
      dplyr::mutate(mkt = cj)
    
    
    data_coinc_final1 <- rbind(data_coinc_BA,
                               data_coinc_SA,
                               data_coinc_TU,
                               data_coinc_HU )
    
    if(y6==6){data_coinc_final_6Y<-data_coinc_final1}else{data_coinc_final_6Y<-rbind(data_coinc_final_6Y, data_coinc_final1)}
    
  }
  
  data_coinc_final_6Y <- data_coinc_final_6Y %>% 
    dplyr::mutate(year1 = as.numeric(year1)) %>% 
    dplyr::mutate(year2 = as.numeric(year2)) %>% 
    dplyr::mutate(year3 = as.numeric(year3)) %>% 
    dplyr::mutate(year4 = as.numeric(year4)) %>% 
    dplyr::mutate(year5 = as.numeric(year5)) %>% 
    dplyr::mutate(year6 = as.numeric(year6))
  
  #####################
  ##################### 
  
  data_coinc_table<- bind_rows(data_coinc_final_1Y,
                               data_coinc_final_2Y,
                               data_coinc_final_3Y,
                               data_coinc_final_4Y,
                               data_coinc_final_5Y,
                               data_coinc_final_6Y)         
  
  
  data_coinc_table <- data_coinc_table %>% 
    unite(year, c('year1', 'year2', 'year3', 'year4', 'year5', 'year6'), na.rm = TRUE) %>% 
    dplyr::select(-years)
  
  if(k==1){data_coinc_all<-data_coinc_table}else{data_coinc_all<-rbind(data_coinc_all, data_coinc_table)}
  
}

rm(data_coinc_BA, data_coinc_SA, data_coinc_TU, data_coinc_HU,
   data_loc_BA_Y6, data_loc_SA_Y6, data_loc_TU_Y6, data_loc_HU_Y6,
   data_loc_BA_Y1, data_loc_SA_Y1, data_loc_TU_Y1, data_loc_HU_Y1,
   data_loc_BA_Y2, data_loc_SA_Y2, data_loc_TU_Y2, data_loc_HU_Y2,
   data_loc_BA_Y3, data_loc_SA_Y3, data_loc_TU_Y3, data_loc_HU_Y3,
   data_loc_BA_Y4, data_loc_SA_Y4, data_loc_TU_Y4, data_loc_HU_Y4,
   data_loc_BA_Y5, data_loc_SA_Y5, data_loc_TU_Y5, data_loc_HU_Y5,
   data_coinc_final1,
   data_coinc_final2,
   data_coinc_final3,
   data_coinc_final4,
   data_coinc_final5,
   data_coinc_final6,
   data_coinc.sub.temp1,
   data_coinc.sub.temp2,
   data_coinc.sub.temp3,
   data_coinc.sub.temp4,
   data_coinc.sub.temp5,
   data_coinc.sub.temp6)


#####################
#####################

str(data_coinc_all)
data_coinc_all2<- data.table(data_coinc_all)

data_coinc_all2$loc<-as.factor(data_coinc_all2$loc)
data_coinc_all2$year<-as.character(data_coinc_all2$year)
data_coinc_all2$mkt<-as.factor(data_coinc_all2$mkt)
str(data_coinc_all2)

data_coinc_all2<- data_coinc_all2 %>% 
  dplyr::mutate(year = replace(year, year == '17_17', '17')) %>% 
  dplyr::mutate(year = replace(year, year == '18_18', '18')) %>% 
  dplyr::mutate(year = replace(year, year == '19_19', '19')) %>% 
  dplyr::mutate(year = replace(year, year == '20_20', '20')) %>% 
  dplyr::mutate(year = replace(year, year == '21_21', '21')) %>% 
  dplyr::mutate(year = replace(year, year == '22_22', '22'))


data_coinc_all2$year<-as.factor(data_coinc_all2$year)
row_num = length(levels(data_coinc_all2$loc))

data_coinc_all.BB <- data_coinc_all2 %>%
  filter(mkt == "BB") %>% 
  filter(loc == "BA") 

data_coinc_all.BB$year <- as.character(data_coinc_all.BB$year)
str(data_coinc_all.BB)

names_year<- data_coinc_all.BB$year
class(names_year)

print(names_year)
#unique(unlist(strsplit(names_year, " ")))

##### Figure - Black beans
#####

data_coinc_all.BB <- data_coinc_all2 %>%
  filter(mkt == "BB") 

str(data_coinc_all.BB)

plotB = ggplot(data_coinc_all.BB,aes(x=year,y=as.numeric(loc),fill=count)) + 
  xlim(c("",names_year)) + ylim(c(0.5,row_num+0.5)) +
  geom_tile(color = "white",
            lwd = 0.1) + 
  ylab("") + scale_fill_gradient (low = "#edf2f5",
                                  high = "#031c2b") +
  annotate(x="",y=1:row_num,label=levels(data_coinc_all.BB$loc),size=4,geom="text") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x=element_text(),
        axis.title.y=element_text(size=14,face="bold"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(legend.position = "right", axis.title.x=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  labs(title = "Black beans", fill= "Coinc.#") + theme(plot.title = element_text(hjust = 0.5))



#labs (fill = "Geno#")

##Number of genotypes evaluated in common between each pair, three, four and all environments
##represented in shades of gray in the pie chart.
# plotB<- plotB + coord_polar(start=-0.15) + theme_bw() + 
#   theme(legend.position = "none")
#theme( legend.position = c(0.5, 0.5),legend.key.size = unit(0.6, "cm")) + 
#labs(x="", y="") 


## Navy beans

data_coinc_all.NB <- data_coinc_all2 %>%
  filter(mkt == "NB") 

str(data_coinc_all.NB)

plotN = ggplot(data_coinc_all.NB,aes(x=year,y=as.numeric(loc),fill=count)) + 
  xlim(c("",names_year)) + ylim(c(0.5,row_num+0.5)) +
  geom_tile(color = "white",
            lwd = 0.1) + 
  ylab("") + scale_fill_gradient (low = "#edf2f5",
                                  high = "#031c2b") +
  annotate(x="",y=1:row_num,label=levels(data_coinc_all.NB$loc),size=4,geom="text") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x=element_text(),
        axis.title.y=element_text(size=14,face="bold"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(legend.position = "right", axis.title.x=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  labs(title = "Navy beans", fill= "Coinc.#") + theme(plot.title = element_text(hjust = 0.5))

##Number of genotypes evaluated in common between each pair, three, four and all environments
##represented in shades of gray in the pie chart.
# plotN<- plotN + coord_polar(start=-0.15) + theme_bw() +
#   #theme(legend.position = "none")#+ 
#   theme( legend.position = "right",legend.key.size = unit(0.6, "cm")) + 
#   labs(x="", y="") 


## Small Red Beans
data_coinc_all.SR <- data_coinc_all2 %>%
  filter(mkt == "SR") 

str(data_coinc_all.SR)

plotSR = ggplot(data_coinc_all.SR,aes(x=year,y=as.numeric(loc),fill=count)) + 
  xlim(c("",names_year)) + ylim(c(0.5,row_num+0.5)) +
  geom_tile(color = "white",
            lwd = 0.1) + 
  ylab("") + scale_fill_gradient (low = "#edf2f5",
                                  high = "#031c2b") +
  annotate(x="",y=1:row_num,label=levels(data_coinc_all.SR$loc),size=4,geom="text") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x=element_text(size=14,face="bold"),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_text(face="bold"),
        axis.ticks.y=element_blank())+
  labs(x= "Year", title = "Small Red beans", fill= "Coinc.#") + theme(plot.title = element_text(hjust = 0.5))

##Number of genotypes evaluated in common between each pair, three, four and all environments
##represented in shades of gray in the pie chart.
# plotSR<- plotSR + coord_polar(start=-0.15) + theme_bw() +
#   theme(legend.position = "none")# + 
#theme( legend.position = c(0.5, 0.5),legend.key.size = unit(0.6, "cm")) + 
#labs(x="", y="") 


rm (data_coinc_final_1Y,
    data_coinc_final_2Y,
    data_coinc_final_3Y,
    data_coinc_final_4Y,
    data_coinc_final_5Y,
    data_coinc_final_6Y,
    data_coinc_all.BB,
    data_coinc_all.NB,
    data_coinc_all.SR)
#arrange_ggplot(plotB, plotN, plotSR,ncol = 1)
save(plotB, plotN, plotSR, file = "utils/Coinc_plots_data.RData")

}
