
#### Import libraries ---------------------------------------------------------------------------------

library(tidyverse)
library(randomForest)
library(caret)
library(sf)
library(ggalluvial)
library(ggpattern)
library(gridExtra)

#### Themes and objects ---------------------------------------------------------------------------------

theme_set(theme_bw()+ 
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
            theme(strip.background =element_rect(fill="white")))


#### Global functions ---------------------------------------------------------------------------------
# Function to read in multiple files from directory
readMultiFiles <- function(directory){
  
  files <- list.files(directory, pattern='*.csv', full.names=TRUE)
  raw <- files %>% 
    map_df(~read_csv(.) %>% mutate(PLOTID = as.character(PLOTID)))
  return (raw)
  
}

# Function to calculate map accuracy and perform area estimation
# Copied from here: https://rdrr.io/cran/mapaccuracy/src/R/olofsson.R
olofsson2<-function(r, m, Nh, margins=TRUE, csv_filepath){
  
  # check arguments
  r<-unlist(r)
  m<-unlist(m)
  Nh<-unlist(Nh)
  if(is.null(names(Nh))) stop("Nh must be named.", call. = FALSE)
  if(length(names(Nh))>length(unique(names(Nh)))) stop("Repeated names detected in Nh.", call. = FALSE)
  
  # convert arguments
  r<-factor(r, levels = names(Nh))
  m<-factor(m, levels = names(Nh))
  
  
  # error matrix
  matrix<-table(m, r)
  
  # Sampling settings
  q <-length(Nh)       # number of classes
  A <-sum(Nh)          # total area
  nh<-rowSums(matrix)  # stratum sample size
  Wh<-Nh/A             # map class proportions
  
  # confusion matrix (estimated area proportions)
  props<-prop.table(matrix,1)
  for(j in 1:q){
    props[j,]<-Wh[j]*props[j,]
  }
  
  # Accuracy and area estimates
  OA<-sum(diag(props),na.rm=T)
  UA<-diag(props)/rowSums(props)
  PA<-diag(props)/colSums(props)
  # tarea<-A*colSums(props)
  tarea<-colSums(props)
  
  
  # standard error of OA
  VAR_OA<-0
  for(j in 1:q){
    temp<-Wh[j]^2*UA[j]*(1-UA[j])/(nh[j]-1)
    if(is.na(temp)){temp<-0}
    if(is.infinite(temp)){temp<-0}
    VAR_OA<-VAR_OA + temp
  }
  SEoa<-sqrt(VAR_OA)
  names(SEoa)<-NULL
  
  
  # standard error of UA
  VAR_UA<-NULL
  for(j in 1:q){
    temp<-UA[j]*(1-UA[j])/(nh[j]-1)
    if(is.na(temp)){temp<-0}
    if(is.infinite(temp)){temp<-0}
    VAR_UA<-c(VAR_UA, temp)
  }
  SEua<-sqrt(VAR_UA)
  
  
  # standard error of PA
  N.j<-NULL
  for(j in 1:q){
    temp<-0
    for(i in 1:q){
      temp<-sum(temp,Nh[i]/nh[i]*matrix[i,j],na.rm=T)
    }
    N.j<-c(N.j, temp)
  }
  
  VAR_PA<-NULL
  for(j in 1:q){
    # temp1<-N.j[j]^2*(1-PA[j])^2*UA[j]*(1-UA[j])/(nh[j]-1)
    temp1<-Nh[j]^2*(1-PA[j])^2*UA[j]*(1-UA[j])/(nh[j]-1)
    if(is.na(temp1)){temp1<-0}
    if(is.infinite(temp1)){temp1<-0}
    temp2<-0
    seque<-1:q
    seque<-seque[-j]
    for(i in seque){
      temp2<-sum(temp2, Nh[i]^2*matrix[i,j]/nh[i]*(1-matrix[i,j]/nh[i])/(nh[i]-1), na.rm=T)
    }
    if(is.na(temp2)){temp2<-0}
    if(is.infinite(temp2)){temp2<-0}
    
    VAR_PA<-c(VAR_PA, (1/N.j[j]^2)*(temp1+PA[j]^2*temp2))
  }
  SEpa<-sqrt(VAR_PA)
  
  
  # standard error of the area
  VAR_A<-list()
  for(j in 1:q){
    a<-Wh^2
    b<-matrix[,j]/nh
    c<-(1-matrix[,j]/nh)
    d<-nh-1
    VAR_A[[j]]<-sum(a*(b*c/d))
  }
  VAR_A<-unlist(VAR_A)
  SEa<-sqrt(VAR_A)
  # SEa<-A*SEa
  names(SEa)<-names(Nh)
  
  # gather calculations together
  if(!missing(csv_filepath)){
    # export as a table to csv
  }
  
  # Add margins to confusion matrix (potentially useful for exporting only)
  props[matrix==0]<-NA
  if(margins){
    props<-cbind(props,rowSums(props, na.rm = TRUE))
    props<-rbind(props,c(colSums(props, na.rm = TRUE)))
    colnames(props)[ncol(props)]<-"sum"; rownames(props)[nrow(props)]<-"sum"
  }
  
  # return
  list(OA=OA,
       UA=UA,
       PA=PA,
       area=tarea,
       SEoa=SEoa,
       SEua=SEua,
       SEpa=SEpa,
       SEa =SEa,
       matrix=props)
}

