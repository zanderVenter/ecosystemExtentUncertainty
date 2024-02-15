# Script to calculate the correspondence/accuracy between image interpreters in the sampling


list.files('./DATA/reference_data_collection/Zander/Calibration sample/')
plotids <- readMultiFiles('./DATA/reference_data_collection/Zander/Calibration sample/')$PLOTID

names <- c('Zander', 'Megan', 'Erik', 'David', 'Anders' ,'Trond')
calDat <- tibble()
for (n in names){
  dir <- paste0('./DATA/reference_data_collection/',n,'/Calibration sample/')
  calDat <- calDat %>% bind_rows(readMultiFiles(dir) %>% 
                                   mutate(groundTruth = ifelse(groundTruth == 'Mixed', 'Skip', groundTruth)) %>%
                                   filter(PLOTID %in% plotids))
}

calDatWide <- calDat %>%
  dplyr::select(PLOTID, sampler, groundTruth) %>%
  pivot_wider(values_from=groundTruth, names_from=sampler)


numInAgreement <- calDat %>%
  group_by(groundTruth, PLOTID) %>%
  summarise(n=n()) %>%
  group_by(PLOTID) %>%
  summarise(numInAgreement = max(n))

numAgreePlot <-numInAgreement %>% 
  ggplot(aes(x=numInAgreement)) +
  geom_bar() +
  labs(x='Number samplers in agreement', y = 'Count of sample points')
numAgreePlot

disagreements <- numInAgreement %>% filter(numInAgreement < 5) %>% drop_na()
disagreements$PLOTID

consensus <- calDat %>%
  group_by(PLOTID, groundTruth) %>%
  summarise(n=n()) %>%
  filter(n >= 4)
length(unique(consensus$PLOTID))

accDat <- tibble()
cmDat <- tibble()
n <- 'Anders'
for (n in names){
  refSel <- calDat %>% filter(sampler == n) %>% 
    dplyr::select(PLOTID, groundTruth)
  predSel <- consensus %>% 
    mutate(pred = groundTruth) %>%dplyr::select(PLOTID, pred)
  eval <- predSel %>% left_join(refSel) %>%
    mutate_at(vars(pred, groundTruth), as.factor)
  cm <- confusionMatrix(eval$pred, eval$groundTruth)
  
  cmSubDat <- as_tibble(cm$table) 
  cmDat <- cmDat %>% bind_rows(cmSubDat)
  
  accSubDat <- as_tibble(cm$byClass) %>%
    mutate(class = str_remove(rownames(as.data.frame(cm$byClass)), 'Class: '),
           overallAcc = cm$overall[[1]],
           sampler = n) %>%
    dplyr::select(class, sampler,overallAcc, `Balanced Accuracy`)
  accDat <- accDat %>% bind_rows(accSubDat)
  
  
}

accDat %>% summarise(overallAcc = mean(overallAcc))

accDat %>% group_by(sampler) %>% summarise(overallAcc = mean(overallAcc))

accStat <- accDat %>%
  group_by(class) %>%
  summarise(`Balanced Accuracy` = mean(`Balanced Accuracy`))

nCat <- calDat %>% mutate(class=groundTruth) %>%
  group_by(class) %>%
  summarise(n=n())
classAccPlot <- accStat %>%
  left_join(nCat) %>%
  mutate(`Balanced Accuracy` = `Balanced Accuracy`*100) %>%
  mutate(lab = paste0(round(`Balanced Accuracy`, 1), ' (n = ', n, ')')) %>%
  ggplot(aes(y=reorder(class, `Balanced Accuracy`), x=`Balanced Accuracy` )) +
  geom_bar(stat='identity') +
  geom_text(aes(label = lab, x=0), hjust =-0.5, color='white') +
  labs(x='Balanced accuracy (%)', y='')
classAccPlot
#ggsave("classAccPlot.png", classAccPlot, width = 10, height=10, units='cm')



exportSamplerAcc <- function(){
  cmDat %>%
    filter(Reference != 'Skip')%>%
    filter(Prediction != 'Skip')  %>%
    group_by(Reference, Prediction) %>%
    summarise(n=sum(n)) %>%
    pivot_wider(names_from=Reference, values_from=n) %>%
    left_join(accStat %>% mutate(Prediction = class) %>%
                dplyr::select(Prediction, `Balanced Accuracy`) %>%
                mutate(`Balanced Accuracy` = `Balanced Accuracy`*100)) %>%
    write_csv('sampler_accuracy.csv')
}

