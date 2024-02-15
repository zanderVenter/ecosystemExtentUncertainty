# Script to perform area estimation, compare with pixel counting, and make plots

#### Data import and wrangling --------------------------------------------------------

strataAreas <- read_csv('./DATA/From_GEE/strataAreas.csv')%>%
  mutate(product = str_split(batch, '_') %>% map_chr(4),
         batch = str_sub(batch, -9,-1),
         areaList = str_sub(areaList, 2,-2),
         areaList = str_split(areaList, ',')) %>%
  dplyr::select(batch, product, areaList)
strataAreas
strataAreas$areaList[1][[1]]

samplers <- c('Zander', 'Megan', 'Trond', 'Erik', 'Anders')

rawDat <- tibble()
for (s in samplers){
  rawDat <- rawDat %>% bind_rows(readMultiFiles(paste0('./DATA/reference_data_collection/',s,'/')))
}
rawDat
unique(rawDat$groundTruth)
rawDat <- rawDat %>%
  filter(!groundTruth %in% c('Mixed', 'Skip')) %>%
  mutate(groundTruth = ifelse(groundTruth %in% c('Water'), 'water',
                              ifelse(groundTruth %in% c('Built', 'Bare'), 'impervious',
                                     ifelse(groundTruth %in% c('Tree', 'Shrub'), 'tall veg', 'short veg')))) %>%
  mutate(product = str_split(batch, '_') %>% map_chr(4),
         batch = str_sub(batch, -9,-1))%>%
  distinct(PLOTID, groundTruth, year, batch, product, .keep_all=TRUE) %>%
  filter(!PLOTID %in% c('id_49121541', 'id_94064681', 'id_48210159'))
rawDat
nrow(rawDat)


strataAreasFormatted <- strataAreas %>% 
  unnest_wider(areaList, names_sep='') %>%
  gather(changeLC, area, -batch, -product) %>%
  mutate(changeLC = recode_factor(factor(changeLC), 
                                  `areaList1` = 'water - water',
                                  `areaList2` = 'water - impervious',
                                  `areaList3` = 'water - tall veg',
                                  `areaList4` = 'water - short veg',
                                  `areaList5` = 'impervious - water',
                                  `areaList6` = 'impervious - impervious',
                                  `areaList7` = 'impervious - tall veg',
                                  `areaList8` = 'impervious - short veg',
                                  `areaList9` = 'tall veg - water',
                                  `areaList10` = 'tall veg - impervious',
                                  `areaList11` = 'tall veg - tall veg',
                                  `areaList12` = 'tall veg - short veg',
                                  `areaList13` = 'short veg - water',
                                  `areaList14` = 'short veg - impervious',
                                  `areaList15` = 'short veg - tall veg',
                                  `areaList16` = 'short veg - short veg',
  ))  %>%
  mutate(area = as.numeric(area))%>%
  mutate(area = area/10000)

strataAreasFormatted %>%
  ggplot(aes(y=changeLC, x=area, fill=product)) +
  geom_bar(stat='identity', position='dodge') +
  facet_wrap(~batch) 



# Re-import reference data with doubled size from new samples
mappedNew <- read_csv('./DATA/From_GEE/reference_data_doubled.csv')
mapped <- mappedNew %>%
  dplyr::select(PLOTID:me_2018_2021) %>%
  gather(key, val, -PLOTID) %>%
  mutate(batch = str_sub(key,4,12),
         product = str_sub(key, 1,2),
         year = str_sub(batch, 1,4)) %>%
  arrange(PLOTID)%>%
  mutate(changeLC = recode_factor(factor(val), 
                                  `1` = 'water - water',
                                  `2` = 'water - impervious',
                                  `3` = 'water - tall veg',
                                  `4` = 'water - short veg',
                                  `5` = 'impervious - water',
                                  `6` = 'impervious - impervious',
                                  `7` = 'impervious - tall veg',
                                  `8` = 'impervious - short veg',
                                  `9` = 'tall veg - water',
                                  `10` = 'tall veg - impervious',
                                  `11` = 'tall veg - tall veg',
                                  `12` = 'tall veg - short veg',
                                  `13` = 'short veg - water',
                                  `14` = 'short veg - impervious',
                                  `15` = 'short veg - tall veg',
                                  `16` = 'short veg - short veg',
  )) %>%
  dplyr::select(PLOTID, changeLC, batch, product)



rawDatWide <- rawDat %>% 
  distinct(PLOTID, year, .keep_all = TRUE) %>%
  group_by(PLOTID) %>% 
  mutate(n=n())%>%
  filter(n == 3) %>%
  dplyr::select(PLOTID, groundTruth, batch, year)%>%
  pivot_wider(values_from=groundTruth, names_from=year) %>%
  arrange(PLOTID)

t <- tibble()
for (b in c('2015_2018','2018_2021','2015_2021')){
  
  if (b == '2015_2018'){
    tSub <- rawDatWide %>%
      mutate(changeLC =paste0(`2015`,' - ', `2018`)) %>%
      dplyr::select(PLOTID,changeLC)
  } else if (b == '2018_2021'){
    tSub <- rawDatWide %>%
      mutate(changeLC =paste0(`2018`,' - ', `2021`)) %>%
      dplyr::select(PLOTID,changeLC)
  } else {
    tSub <- rawDatWide %>%
      mutate(changeLC =paste0(`2015`,' - ', `2021`)) %>%
      dplyr::select(PLOTID,changeLC)
  }
  
  t <- t %>% bind_rows(tSub %>% mutate(batch = b))
  
}

truth <- t %>% mutate(product = 'dw') %>%
  bind_rows(t %>% mutate(product = 'me') ) %>%
  arrange(PLOTID)
truth

mapped <- mapped %>% filter(PLOTID %in% unique(truth$PLOTID))


#### Area estimation ----------------------------------------------------------------------------

accountFinal <- tibble()
desbsdFinal <- tibble()
mapAccFinal <- tibble()

batchSel <- '2018_2021'
productSel <- 'me'

for (batchSel in c('2015_2018','2018_2021','2015_2021')){
  for (productSel in c('me','dw')){
    
    
    truthChng1 <- truth %>%
      filter(batch == batchSel & product %in% productSel)
    
    mappedChng1 <- mapped %>%
      filter(batch == batchSel & product %in% productSel) %>%
      dplyr::select(PLOTID, batch,product,  changeLC)
    
    truthChng1 %>% mutate(type = 'truth') %>%
      bind_rows(mappedChng1 %>% mutate( type = 'mapped')) %>%
      group_by(type, changeLC) %>%
      summarise(n = n() ) %>%
      ggplot(aes(y=changeLC,x=n, fill=type)) +
      geom_bar(stat='identity', position='dodge')
    
    areaListSelect <- strataAreas %>%
      filter(batch == batchSel & product  %in% productSel)
    
    
    Nh <- c('water - water'=as.numeric(areaListSelect$areaList[1][[1]][1]),
            'water - impervious'=as.numeric(areaListSelect$areaList[1][[1]][2]),
            'water - tall veg'=as.numeric(areaListSelect$areaList[1][[1]][3]),
            'water - short veg'=as.numeric(areaListSelect$areaList[1][[1]][4]),
            'impervious - water'=as.numeric(areaListSelect$areaList[1][[1]][5]),
            'impervious - impervious'=as.numeric(areaListSelect$areaList[1][[1]][6]),
            'impervious - tall veg'=as.numeric(areaListSelect$areaList[1][[1]][7]),
            'impervious - short veg'=as.numeric(areaListSelect$areaList[1][[1]][8]),
            'tall veg - water'=as.numeric(areaListSelect$areaList[1][[1]][9]),
            'tall veg - impervious'=as.numeric(areaListSelect$areaList[1][[1]][10]),
            'tall veg - tall veg'=as.numeric(areaListSelect$areaList[1][[1]][11]),
            'tall veg - short veg'=as.numeric(areaListSelect$areaList[1][[1]][12]),
            'short veg - water'=as.numeric(areaListSelect$areaList[1][[1]][13]),
            'short veg - impervious'=as.numeric(areaListSelect$areaList[1][[1]][14]),
            'short veg - tall veg'=as.numeric(areaListSelect$areaList[1][[1]][15]),
            'short veg - short veg'=as.numeric(areaListSelect$areaList[1][[1]][16]))
    
    
    r <- truthChng1$changeLC
    length(r)
    m <- mappedChng1$changeLC
    length(m)
    e <- olofsson2(r,m, Nh)
    #View(e$matrix)
    
    # Map accuracy
    accDF <- tibble()
    i <- 1
    for (i in seq(1, length(e$UA))){
      subAccDF <- tibble(class = rownames(e$matrix)[i],
                         PA = e$PA[i]*100,
                         PA_error = qnorm(0.975)*e$SEpa[i]*100,
                         UA = e$UA[i]*100,
                         UA_error = qnorm(0.975)*e$SEua[i]*100,
                         OA = e$OA*100,
                         OA_error = qnorm(0.975)*e$SEoa*100)
      
      accDF <- accDF %>% bind_rows(subAccDF)
    }
    
    mapAccFinal <- mapAccFinal %>%
      bind_rows(accDF %>% 
                  mutate(batch = batchSel, 
                         product = productSel))
    
    # Area estimation
    
    changeDF <- tibble()
    for (i in seq(1, length(e$area))){
      subChangeDF <- tibble(
        classFrom = str_split(rownames(e$matrix)[i], ' - ')[[1]][1],
        classTo = str_split(rownames(e$matrix)[i], ' - ')[[1]][2],
        area = e$area[i]*sum(Nh)/10000 ,
        prop =  e$area[i],
        ciArea = (qnorm(0.975)*e$SEa[i]*sum(Nh)/10000), #/2,
        ciProp = (qnorm(0.975)*e$SEa[i]) #/2
      )
      changeDF <- changeDF %>% bind_rows(subChangeDF)
    }
    
    # accounting table 4-class area estimates
    
    areaDFout <- tibble(type = c('opening', 'change', 'closing'))
    propDFout <- tibble(type = c('opening', 'change', 'closing'))
    
    for (x in unique(changeDF$classFrom)){
      selClass <- x
      print(selClass)
      opening <- changeDF %>% filter(classFrom == selClass)  %>% 
        summarise(area = sum(area), prop = sum(prop),
                  ciArea = sum(ciArea), ciProp = sum(ciProp))
      opening
      closing <- changeDF %>% filter(classTo == selClass)  %>% 
        summarise(area = sum(area), prop = sum(prop),
                  ciArea = sum(ciArea), ciProp = sum(ciProp))
      closing
      change <- changeDF %>% filter(classTo == selClass | classFrom == selClass) %>% 
        filter(classTo != classFrom)  %>%
        summarise(ciArea= sqrt(sum(ciArea^2)),
                  ciProp= sqrt(sum(ciProp^2)))
      change
      
      areaDF <- tibble(class = c(opening$area, NA, closing$area), 
                       error = c(opening$ciArea, change$ciArea, closing$ciArea))
      names(areaDF) <- c(selClass, paste0(selClass,'_error'))
      areaDF[2,1] <- areaDF[3,1] - areaDF[1,1]
      areaDFout <- areaDFout %>% bind_cols(areaDF)
      
      propDF <- tibble(class = c(opening$prop, NA, closing$prop), 
                       error = c(opening$ciProp, change$ciProp, closing$ciProp))
      
      names(propDF) <- c(selClass, paste0(selClass,'_error'))
      propDF[2,1] <- propDF[3,1] - propDF[1,1]
      propDFout <- propDFout %>% bind_cols(propDF)
    }
    areaDFout <- areaDFout %>%
      mutate(batch = batchSel, 
             product = productSel)
    propDFout
    
    
    changeDFlong <- tibble(class = c('water', 'impervious', 'tall veg', 'short veg'),
                           area = as.numeric(areaDFout[2,c(2,4,6,8)]),
                           error = as.numeric(areaDFout[2,c(3,5,7,9)]),
                           type = 'change')
    
    openingDFlong <- tibble(class = c('water', 'impervious', 'tall veg', 'short veg'),
                            area = as.numeric(areaDFout[1,c(2,4,6,8)]),
                            error = as.numeric(areaDFout[1,c(3,5,7,9)]),
                            type = 'opening')
    
    closingDFlong <- tibble(class = c('water', 'impervious', 'tall veg', 'short veg'),
                            area = as.numeric(areaDFout[3,c(2,4,6,8)]),
                            error = as.numeric(areaDFout[3,c(3,5,7,9)]),
                            type = 'closing')
    
    changeDFlong <- tibble(class = c('water', 'impervious', 'tall veg', 'short veg'),
                           area = as.numeric(areaDFout[2,c(2,4,6,8)]),
                           error = as.numeric(areaDFout[2,c(3,5,7,9)]),
                           type = 'change')
    
    
    accountDFlong <- openingDFlong %>% bind_rows(changeDFlong) %>% bind_rows(closingDFlong) %>%
      mutate(batch = batchSel, 
             product = productSel,
             method = 'design based')
    
    
    pc_openingDFlong <- tibble(class = c('water', 'impervious', 'tall veg', 'short veg'),
                               area = c(Nh[1] + Nh[2] + Nh[3] + Nh[4], 
                                        Nh[5] + Nh[6] + Nh[7] + Nh[8], 
                                        Nh[9] + Nh[10] + Nh[11] + Nh[12], 
                                        Nh[13] + Nh[14] + Nh[15] + Nh[16]),
                               error =  c(NA, NA, NA, NA),
                               type = 'opening')
    
    pc_closingDFlong <- tibble(class = c('water', 'impervious', 'tall veg', 'short veg'),
                               area = c(Nh[1] + Nh[5] + Nh[9] + Nh[13], 
                                        Nh[2] + Nh[6] + Nh[10] + Nh[14], 
                                        Nh[3] + Nh[7] + Nh[11] + Nh[15], 
                                        Nh[4] + Nh[8] + Nh[12] + Nh[16]),
                               error =  c(NA, NA, NA, NA),
                               type = 'closing')
    

    pc_changeDFlong <- tibble(class = c('water', 'impervious', 'tall veg', 'short veg'),
                              area = pc_closingDFlong$area - pc_openingDFlong$area,
                              error =  c(NA, NA, NA, NA),
                              type = 'change')
    
    pc_areaDFlong <- pc_openingDFlong %>% bind_rows(pc_changeDFlong) %>% bind_rows(pc_closingDFlong) %>%
      mutate(batch = batchSel, 
             product = productSel,
             method = 'pixel counting') %>%
      mutate(area = area/10000)
    
    accountFinal <- accountFinal %>% bind_rows(accountDFlong) %>% bind_rows(pc_areaDFlong)
    
    
    # 16-class area estimates
    desbsd <- changeDF %>%
      mutate(class = paste0(classFrom, ' - ', classTo),
             error = ciArea,
             batch = batchSel, 
             product = productSel,
             method = 'design based') %>%
      dplyr::select(class, area, error, batch, product,method)
    
    pc_area <- tibble(class = names(Nh), 
                      area = Nh/10000, 
                      error = NA, 
                      batch = batchSel, 
                      product = productSel,
                      method = 'pixel counting')
    
    desbsdFinal <- desbsdFinal %>%
      bind_rows(desbsd) %>%
      bind_rows(pc_area)
    
  }
}

desbsdFinal  <- desbsdFinal %>%
  mutate(class = recode_factor(factor(class),
                               'water - water' = 'W to W',
                               'water - impervious' = 'W to B',
                               'water - tall veg' = 'W to VT',
                               'water - short veg' = 'W to VS',
                               'impervious - water' = 'B to W',
                               'impervious - impervious' = 'B to B',
                               'impervious - tall veg' = 'B to VT',
                               'impervious - short veg' = 'B to VS',
                               'tall veg - water' = 'VT to W',
                               'tall veg - impervious' = 'VT to B',
                               'tall veg - tall veg' = 'VT to VT',
                               'tall veg - short veg' = 'VT to VS',
                               'short veg - water' = 'VS to W',
                               'short veg - impervious' = 'VS to B',
                               'short veg - tall veg' = 'VS to VT',
                               'short veg - short veg' = 'VS to VS'))

mapAccFinal<- mapAccFinal%>%
  mutate(class = recode_factor(factor(class),
                               'water - water' = 'W to W',
                               'water - impervious' = 'W to B',
                               'water - tall veg' = 'W to VT',
                               'water - short veg' = 'W to VS',
                               'impervious - water' = 'B to W',
                               'impervious - impervious' = 'B to B',
                               'impervious - tall veg' = 'B to VT',
                               'impervious - short veg' = 'B to VS',
                               'tall veg - water' = 'VT to W',
                               'tall veg - impervious' = 'VT to B',
                               'tall veg - tall veg' = 'VT to VT',
                               'tall veg - short veg' = 'VT to VS',
                               'short veg - water' = 'VS to W',
                               'short veg - impervious' = 'VS to B',
                               'short veg - tall veg' = 'VS to VT',
                               'short veg - short veg' = 'VS to VS'))

#### Construct figures -------------------------------------------------------------

# Plot 16-class area estimates and uncertainty compared to pixel counting

makeSubFig4 <- function(t, label, legPos, ytitle){
  
  levelNames <- c('DW pix cnt', 'DW des-bsd', 'ELC10 pix cnt', 'ELC10 des-bsd')
  p <- desbsdFinal %>%
    mutate(type = ifelse(class %in% c('W to W', 'B to B', 'VS to VS', 'VT to VT'), 'Stable', 'Change')) %>%
    filter(type == t) %>%
    mutate(lowCI = area - error,
           lowCI = ifelse(lowCI < 0, 0, lowCI)) %>%
    mutate(error = ifelse(is.na(error), 0, error)) %>%
    mutate(groupVar = paste0(method, product)) %>%
    mutate(groupVar = factor(groupVar, levels = c( 'pixel countingdw', 'design baseddw', 'pixel countingme','design basedme')))  %>%
    mutate(batch = ifelse(batch == '2015_2018', '2015 to 2018',
                          ifelse(batch == '2018_2021', '2018 to 2021', '2015 to 2021')),
           batch = factor(batch, levels = c('2015 to 2018', '2018 to 2021', '2015 to 2021')))  %>%
    ggplot(aes(x=class, y=area, fill=groupVar)) +
    geom_bar_pattern(aes(pattern = groupVar),
                     stat='identity',
                     alpha=0.8, 
                     position = 'dodge', 
                     width=0.8,
                     linewidth=0.2,
                     color = NA, 
                     pattern_fill = "#545252",
                     pattern_color = "#545252",
                     pattern_alpha = 0.4,
                     pattern_angle = 45,
                     pattern_density = 0.8,
                     pattern_size = 0.1,
                     pattern_key_scale_factor = 0.2) +
    geom_errorbar(aes(ymax=area+error, ymin=lowCI, alpha=groupVar),
                  linewidth = 0.3,
                  width=0.3, 
                  position = position_dodge(width=0.8)) +
    facet_grid(batch~.) +
    labs(y = 'Area (ha)') +
    scale_pattern_manual(values = c("stripe", "none", "stripe", "none"),
                         labels= levelNames)+
    scale_fill_manual(values = c('#3a81bc','#3a81bc',  '#e69f01', '#e69f01'),
                      labels= levelNames) +
    scale_alpha_manual(values = c(0,1,0,1),
                       labels= levelNames) +
    guides(fill = guide_legend(nrow = 2),color = guide_legend(nrow = 2),pattern = guide_legend(nrow = 2)) + 
    ggtitle(label)+
    theme(axis.title.x = element_blank(),
          plot.title = element_text(size=11),
          legend.title = element_blank(),
          legend.position = legPos,
          axis.text.x = element_text(angle=45, vjust=1, hjust=1))
  
  if(!ytitle){
    p <- p + theme(axis.title.y = element_blank())
  }
  
  if (ytitle){
    p <- p + 
      theme(
        strip.background = element_blank(),
        strip.text = element_blank()
      )
  }
  p
  return (p)
}
makeSubFig4('Stable', 'A) Stable classes', 'none', TRUE)

makeFig4 <- function(){
  p1 <- makeSubFig4('Stable', 'A) Stable classes', 'none', TRUE)
  p2 <- makeSubFig4('Change', 'B) Conversion classes', c(0.3,0.53), FALSE)
  
  pout <-  grid.arrange(p1, p2, nrow=1, widths=c(1,2.1), padding = unit(0, "line"), newpage = T)
  return (pout)
}
makeFig4()
ggsave("fig4.png", makeFig4(), width = 23, height=18, units='cm')



# Plot the same only for DW
makeSubFig4_extra <- function(t, label, legPos, ytitle){
  
  levelNames <- c( 'Design-based', 'Pixel counting')
  p <- desbsdFinal %>%
    filter(product == 'dw') %>%
    mutate(type = ifelse(class %in% c('W to W', 'B to B', 'VS to VS', 'VT to VT'), 'Stable', 'Change')) %>%
    filter(type == t) %>%
    mutate(error = ifelse(is.na(error), 0, error)) %>%
    mutate(batch = ifelse(batch == '2015_2018', '2015 to 2018',
                          ifelse(batch == '2018_2021', '2018 to 2021', '2015 to 2021')),
           batch = factor(batch, levels = c('2015 to 2018', '2018 to 2021', '2015 to 2021')))  %>%
    ggplot(aes(x=class, y=area, fill=method)) +
    geom_bar_pattern(aes(pattern = method),
                     stat='identity',
                     alpha=0.8, 
                     position = 'dodge', 
                     width=0.5,
                     linewidth=0.2,
                     color = "black", 
                     pattern_fill = "#545252",
                     pattern_color = "#545252",
                     pattern_alpha = 0.4,
                     pattern_angle = 45,
                     pattern_density = 0.3,
                     pattern_size = 0.1,
                     pattern_key_scale_factor = 0.2) +
    geom_errorbar(aes(ymax=area+error, ymin=area-error, alpha=method),
                  linewidth = 0.3,
                  width=0.3, 
                  position = position_dodge(width=0.50)) +
    facet_grid(batch~.) +
    labs(y = 'Area (ha)') +
    scale_pattern_manual(values = c("stripe", "none"),
                         labels= levelNames)+
    scale_fill_manual(values = c('#3a81bc', '#e69f01'),
                      labels= levelNames) +
    scale_alpha_manual(values = c(1,0,1,0),
                       labels= levelNames) +
    #guides(fill = guide_legend(nrow = 1),color = guide_legend(nrow = 1),pattern = guide_legend(nrow = 1)) + 
    ggtitle(label)+
    theme(axis.title.x = element_blank(),
          plot.title = element_text(size=11),
          legend.title = element_blank(),
          legend.position = legPos,
          axis.text.x = element_text(angle=45, vjust=1, hjust=1))
  
  if(!ytitle){
    p <- p + theme(axis.title.y = element_blank())
  }
  
  if (ytitle){
    p <- p + 
      theme(
        strip.background = element_blank(),
        strip.text = element_blank()
      )
  }
  p
  return (p)
}

makeFig4_extra <- function(){
  p1 <- makeSubFig4_extra('Stable', 'A) Stable classes', 'none', TRUE)
  p2 <- makeSubFig4_extra('Change', 'B) Conversion classes', c(0.3,0.56), FALSE)
  
  pout <-  grid.arrange(p1, p2, nrow=1, widths=c(1,2.1), padding = unit(0, "line"), newpage = T)
  return (pout)
}
makeFig4_extra()
ggsave("fig4_extra.png", makeFig4_extra(), width = 20, height=16, units='cm')



# Plot bias x class x accouting period x data source

biasDF <- desbsdFinal %>%
  dplyr::select(-error) %>%
  pivot_wider(values_from=c('area'), names_from=method) %>%
  mutate(bias = (`pixel counting`-`design based`)/`design based`*100,
         bias = ifelse(bias == Inf, NA, bias)) %>%
  mutate(accPeriod = ifelse(batch == '2015_2021', '6-yr', '3-yr')) %>%
  group_by(class, accPeriod, product) %>%
  summarise(bias = mean(bias, na.rm=T)) %>%
  mutate(type = ifelse(class %in% c('W to W', 'B to B', 'VS to VS', 'VT to VT'), 'Stable', 'Change'))
biasDF


makeFig5 <- function(){
  
  biasDFtot <- biasDF %>%
    mutate(type = ifelse(type =='Change', 'Conversion', type)) %>%
    group_by(product, accPeriod) %>%
    summarise(bias = median(abs(bias), na.rm=T)) %>%
    mutate(type = 'Total', class='Total') 
  biasDFtot
  
  
  p1 <- biasDF %>%
    mutate(type = ifelse(type =='Change', 'Conversion', type)) %>%
    ggplot(aes(x=class, y=bias, color=product, shape=accPeriod, linetype=type)) +
    geom_point( position = position_dodge(width=0.75), size=1.2) +
    geom_hline(yintercept = 0, linetype=2) +
    geom_line() +
    geom_vline(xintercept=seq(1,16,1)+.5,color="black", linetype=3, alpha=0.5)+
    scale_y_continuous(trans='pseudo_log', breaks=c(-100,-10,10,100,1000, 10000))+
    coord_flip() +
    ggtitle('A)') +
    scale_shape_manual(values = c(19, 2),  name='Acc. period') +
    scale_linetype_manual(values=c(1,2), name='Change type', labels = c('Conversion', 'Stable')) +
    scale_color_manual(values = c('#3a81bc', '#e69f01'), labels = c('DW', 'ELC10'), name='Data source') +
    guides(color = guide_legend(override.aes = list(size = 5)),
           shape = guide_legend(override.aes = list(size = 3)))+
    labs(y='Relative bias (%)') +
    theme(axis.title.y=element_blank(),
          plot.title = element_text(size=11)) +
    facet_grid(type~., space='free', scales='free')
  
  p2 <-   biasDF %>%
    mutate(bias = abs(bias)) %>%
    ggplot(aes(x=accPeriod, y=bias, fill = product, linetype = type)) +
    geom_boxplot(outlier.shape=NA) +
    scale_y_log10()+
    ggtitle('B)') +
    scale_fill_manual(values = c('#3a81bc', '#e69f01'), labels = c('DW', 'ELC10'), name='Data source') +
    theme(legend.position = 'none',
          plot.title = element_text(size=11)) +
    labs(x='Accounting period', y='|Relative bias| (%)')
  
  pout <-  grid.arrange(p1, p2, nrow=1, widths=c(1.6,1), padding = unit(0, "line"), newpage = T)
  return (pout)
}
makeFig5()
ggsave("fig5.png", makeFig5(), width = 22, height=13, units='cm')


# Plot uncertainty x class x accouting period x data source

uncertDF <- desbsdFinal %>%
  mutate(accPeriod = ifelse(batch == '2015_2021', '6-yr', '3-yr')) %>%
  filter(method == 'design based') %>%
  mutate(uncert = error/area*100) %>%
  group_by(class, accPeriod, product) %>%
  summarise(uncert = mean(uncert, na.rm=T)) %>%
  mutate(type = ifelse(class %in% c('W to W', 'B to B', 'VS to VS', 'VT to VT'), 'Stable', 'Change'))

uncertDF %>% ggplot(aes(x=uncert)) + geom_histogram()

View(uncertDF %>%
       group_by(accPeriod) %>%
       summarise(uncert = median(uncert, na.rm=T)) )

View(uncertDF %>%
       group_by(accPeriod,class) %>%
       summarise(uncert = median(uncert, na.rm=T)) %>%
       pivot_wider(values_from=uncert, names_from=accPeriod) %>%
       mutate(diff = `6-yr` - `3-yr`))

View(uncertDF %>%
       group_by(product,class) %>%
       summarise(uncert = median(uncert, na.rm=T)) %>%
       pivot_wider(values_from=uncert, names_from=product) %>%
       mutate(diff = me - dw))

makeFig6 <- function(){
  
  uncertDFtot <- uncertDF%>%
    mutate(type = ifelse(type =='Change', 'Conversion', type)) %>%
    group_by(product, accPeriod) %>%
    summarise(uncert = median(uncert, na.rm=T)) %>%
    mutate(type = 'Total', class='Total') 
  uncertDFtot
  
  
  p1 <- uncertDF %>%
    mutate(type = ifelse(type =='Change', 'Conversion', type))%>%
    ggplot(aes(x=class, y=uncert, color=product, shape=accPeriod, linetype=type)) +
    geom_point( position = position_dodge(width=0.750), size=1.2) +
    geom_vline(xintercept=seq(1,16,1)+.5,color="black", linetype=3, alpha=0.5)+
    geom_line() +
    #scale_y_continuous(trans='pseudo_log', breaks=c(-100,-10,10,100,1000))+
    coord_flip() +
    ggtitle('A)') +
    scale_shape_manual(values = c(19, 2),  name='Acc. period') +
    scale_linetype_manual(values=c(1,2), name='Change type', labels = c('Conversion', 'Stable')) +
    scale_color_manual(values = c('#3a81bc', '#e69f01'), labels = c('DW', 'ELC10'), name='Data source') +
    guides(color = guide_legend(override.aes = list(size = 5)),
           shape = guide_legend(override.aes = list(size = 3)))+
    labs(y='Relative uncertainty (%)') +
    theme(axis.title.y=element_blank(),
          plot.title = element_text(size=11)) +
    facet_grid(type~., space='free', scales='free')
  
  p1
  
  p2 <-   uncertDF  %>%
    ggplot(aes(x=accPeriod, y=uncert, fill = product, linetype=type)) +
    geom_boxplot(outlier.shape=NA) +
    scale_y_log10()+
    ggtitle('B)') +
    scale_fill_manual(values = c('#3a81bc', '#e69f01'), labels = c('DW', 'ELC10'), name='Data source') +
    theme(legend.position = 'none',
          plot.title = element_text(size=11)) +
    labs(x='Accounting period', y='Relative uncertainty (%)')
  p2
  
  pout <-  grid.arrange(p1, p2, nrow=1, widths=c(1.6,1), padding = unit(0, "line"), newpage = T)
  return (pout)
}
makeFig6()
ggsave("fig6.png", makeFig6(), width = 22, height=13, units='cm')

# Map accuracy table
exportAccuracyTable <- function(){
  mapAccOut <- mapAccFinal %>%
    group_by(class, product) %>%
    summarise_at(vars(PA:OA_error), mean, na.rm=T) %>%
    mutate_at(vars(PA:OA_error), round, 1) %>%
    mutate(PAjoin = ifelse(is.na(PA) | PA == 0, '-', paste0(PA, ' (', PA_error, ')')),
           UAjoin = ifelse(is.na(UA) | UA == 0, '-', paste0(UA, ' (', UA_error, ')')),
           OAjoin = ifelse(is.na(OA) | OA == 0, '-', paste0(OA, ' (', OA_error, ')'))) %>%
    dplyr::select(class, product, PAjoin:OAjoin) %>%
    pivot_wider(values_from=c('PAjoin', 'UAjoin', 'OAjoin'), names_from=product)
  mapAccOut
  mapAccOut%>%
    write_csv('map_accuracy_tables.csv')
}


# 4-class accounting tables

exportAccTables <- function(){
  acctableOut <- accountFinal %>%
    pivot_wider(values_from=c('area','error'), names_from=class) %>%
    arrange(product, method) %>%
    mutate_if(is.numeric, round, 0) %>%
    mutate(waterFmtd = paste0(area_water, ' (', error_water, ')'),
           imperviousFmtd = paste0(area_impervious, ' (', error_impervious, ')'),
           tallvegFmtd = paste0(`area_tall veg`, ' (', `error_tall veg`, ')'),
           shortvegFmtd = paste0(`area_short veg`, ' (', `error_short veg`, ')'))
  acctableOut
  acctableOut %>%
    write_csv('accounting_tables.csv')
}
exportAccTables()

makeSuppFigAccounts <- function(){
  accountFinal %>%
    mutate(class = recode_factor(factor(class),
                                 "water" = 'Water',
                                 "impervious" = 'Bare',
                                 "tall veg" = 'Vegetation tall',
                                 "short veg" = 'short veg')) %>%
    mutate(batch = ifelse(batch == '2015_2018', '2015 to 2018',
                          ifelse(batch == '2018_2021', '2018 to 2021', '2015 to 2021')),
           batch = factor(batch, levels = c('2015 to 2018', '2018 to 2021', '2015 to 2021'))) %>%
    filter(method == 'design based', type == 'change') %>%
    ggplot(aes(x=class, y=area, fill=product)) +
    geom_bar(stat='identity',alpha=0.9,width=0.5, position = position_dodge(width=0.50)) +
    geom_hline(yintercept = 0, linetype=2) +
    geom_errorbar(aes(ymax=area+error, ymin=area-error), 
                  position = position_dodge(width=0.50),
                  width=0.15, linewidth=0.55, alpha=0.8) +
    scale_fill_manual(values = c('#3a81bc', '#e69f01'), labels = c('DW', 'ELC10'), name='Data source')+ 
    facet_grid(batch~.) +
    labs(y='Change in extent (ha)', x='Ecosystem type') 
}
makeSuppFigAccounts()
ggsave("suppFig1.png", makeSuppFigAccounts(), width = 16, height=12, units='cm')

