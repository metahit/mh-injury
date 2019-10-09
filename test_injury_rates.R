rm(list=ls())
setwd('~/overflow_dropbox/mh-injury/')
library(stringr)
library(dplyr)
library(stats)
library(tidyr)
library(splines)
model_modes <- c('pedestrian','cyclist','car/taxi','motorcycle')
mh_path <- "~/overflow_dropbox/mh-injury/"
overflow_path <- paste0(mh_path,"/rds_storage/")
if(file.exists(paste0(overflow_path,'codes_for_stats19.Rds'))){
  codes_for_stats19 <- readRDS(paste0(overflow_path,'codes_for_stats19.Rds'))
  city_regions <- readRDS(paste0(overflow_path,'city_regions.Rds'))
}else{
  source('get_area_codes.R')
}
stats19 <- readRDS(paste0(overflow_path,'processed_injuries_3.Rds'))
injuries <- subset(stats19, year>=2010&year<2016&cas_severity%in%c('Fatal','Serious'))
injuries$road <- 'motorway'
injuries$road[injuries$roadtype=='A'&injuries$urban_or_rural_area==1] <- 'urban_A'
injuries$road[injuries$roadtype=='A'&injuries$urban_or_rural_area==2] <- 'rural_A'
injuries$road[injuries$roadtype=='B, C, Unclassified'&injuries$urban_or_rural_area==1] <- 'urban_B'
injuries$road[injuries$roadtype=='B, C, Unclassified'&injuries$urban_or_rural_area==2] <- 'rural_B'

for(ext in c('rts','0_smooth','250_smooth','0_raw','250_raw')){
  mode_road_city_dist <- read.csv(paste0('../mh-execute/outputs/mode_road_city_',ext,'.csv'),stringsAsFactors = F)
  total_mode_city <- rowSums(mode_road_city_dist[,-c(1:2)])
  colnames(mode_road_city_dist) <- c('city','mode','motorway','urban_A','rural_A','urban_B','rural_B')
  mode_road_city_dist$mode[mode_road_city_dist$mode=='lgv'] <- 'light goods'
  mode_road_city_dist$mode[mode_road_city_dist$mode=='hgv'] <- 'heavy goods'
  mode_road_city_dist$mode[mode_road_city_dist$mode=='bicycle'] <- 'cyclist'
  
  roads <- colnames(mode_road_city_dist)[3:7]
  cities <- unique(mode_road_city_dist$city)
  city_order <- c('northeast','bristol','nottingham','greatermanchester','sheffield','leeds','westmidlands','liverpool','london')#
  modes <- unique(injuries$cas_mode)
  
  cols <- rainbow(length(cities))
  {pdf(paste0('cas_rates_',ext,'.pdf'),width=15,height=10); par(mfrow=c(2,3))
    for(i in 1:length(modes)){
      dist_name <- strsplit(modes[i],'/')[[1]][1]
      if(dist_name %in% mode_road_city_dist$mode){
        raw_rates <- t(sapply(cities,function(x)
          sapply(roads,function(y) nrow(subset(injuries,region==x&road==y&cas_mode==modes[i])))
        ))/
          subset(mode_road_city_dist,mode==dist_name)[,3:7]*1e6
        print(dist_name)
        total <- sapply(roads,function(y) nrow(subset(injuries,road==y&cas_mode==modes[i])))/
          colSums(subset(mode_road_city_dist,mode==dist_name)[,3:7]*1e-6)
        print(total)
        raw_rates <- raw_rates[match(city_order,cities),]
        rownames(raw_rates) <- city_order
        raw_rates <- rbind(raw_rates,total=total)
        xlsx::write.xlsx(raw_rates,file=paste0('cas_rates_',ext,'.xlsx'),sheetName=gsub('/','',modes[i]),append=(i!=2))
        raw_rates[raw_rates==0] <- 1e-0
        raw_rates[is.na(raw_rates)] <- 1e-0
        raw_rates[!sapply(raw_rates,is.finite)] <- 1e-0
        barplot(log(as.matrix(raw_rates)),beside=T,col=cols,main=dist_name)
      }
      if(dist_name=='heavy goods') legend(fill=cols,legend=city_order,x=1,y=4,bty='n')
    }
    dev.off()}
  
  {pdf(paste0('strike_rates_',ext,'.pdf'),width=15,height=10); par(mfrow=c(2,3))
    for(i in 1:length(modes)){
      dist_name <- strsplit(modes[i],'/')[[1]][1]
      if(dist_name %in% mode_road_city_dist$mode){
        raw_rates <- t(sapply(cities,function(x)
          sapply(roads,function(y) nrow(subset(injuries,region==x&road==y&strike_mode==modes[i])))
        ))/
          subset(mode_road_city_dist,mode==dist_name)[,3:7]*1e6
        print(dist_name)
        total <- sapply(roads,function(y) nrow(subset(injuries,road==y&strike_mode==modes[i])))/
          colSums(subset(mode_road_city_dist,mode==dist_name)[,3:7]*1e-6)
        print(total)
        raw_rates <- raw_rates[match(city_order,cities),]
        rownames(raw_rates) <- city_order
        raw_rates <- rbind(raw_rates,total=total)
        xlsx::write.xlsx(raw_rates,file=paste0('strike_rates_',ext,'.xlsx'),sheetName=gsub('/','',modes[i]),append=(i!=2))
        raw_rates[raw_rates==0] <- 1e-0
        raw_rates[is.na(raw_rates)] <- 1e-0
        raw_rates[!sapply(raw_rates,is.finite)] <- 1e-0
        barplot(log(as.matrix(raw_rates)),beside=T,col=cols,main=dist_name)
      }
      if(dist_name=='heavy goods') legend(fill=cols,legend=city_order,x=1,y=5.5,bty='n')
    }
    dev.off()
  }
}
#################################################################

## raw injury numbers

# strike ksi
for(i in 1:length(modes)){
  dist_name <- strsplit(modes[i],'/')[[1]][1]
  if(dist_name %in% mode_road_city_dist$mode){
    raw_numbers <- t(sapply(cities,function(x)
      sapply(roads,function(y) nrow(subset(injuries,region==x&road==y&strike_mode==modes[i])))
    ))
    print(dist_name)
    total <- sapply(roads,function(y) nrow(subset(injuries,road==y&strike_mode==modes[i])))
    print(total)
    raw_numbers <- raw_numbers[match(city_order,cities),]
    rownames(raw_numbers) <- city_order
    raw_numbers <- rbind(raw_numbers,total=total)
    xlsx::write.xlsx(raw_numbers,file='strike_numbers.xlsx',sheetName=paste0(gsub('/','',modes[i]),'_ksi'),append=(i!=2))
  }
}

# strike fatal
for(i in 1:length(modes)){
  dist_name <- strsplit(modes[i],'/')[[1]][1]
  if(dist_name %in% mode_road_city_dist$mode){
    raw_numbers <- t(sapply(cities,function(x)
      sapply(roads,function(y) nrow(subset(injuries,region==x&road==y&strike_mode==modes[i]&cas_severity=='Fatal')))
    ))
    print(dist_name)
    total <- sapply(roads,function(y) nrow(subset(injuries,road==y&strike_mode==modes[i]&cas_severity=='Fatal')))
    print(total)
    raw_numbers <- raw_numbers[match(city_order,cities),]
    rownames(raw_numbers) <- city_order
    raw_numbers <- rbind(raw_numbers,total=total)
    xlsx::write.xlsx(raw_numbers,file='strike_numbers.xlsx',sheetName=paste0(gsub('/','',modes[i]),'_fatal'),append=T)
  }
}

# cas ksi
for(i in 1:length(modes)){
  dist_name <- strsplit(modes[i],'/')[[1]][1]
  if(dist_name %in% mode_road_city_dist$mode){
    raw_numbers <- t(sapply(cities,function(x)
      sapply(roads,function(y) nrow(subset(injuries,region==x&road==y&cas_mode==modes[i])))
    ))
    print(dist_name)
    total <- sapply(roads,function(y) nrow(subset(injuries,road==y&cas_mode==modes[i])))
    print(total)
    raw_numbers <- raw_numbers[match(city_order,cities),]
    rownames(raw_numbers) <- city_order
    raw_numbers <- rbind(raw_numbers,total=total)
    xlsx::write.xlsx(raw_numbers,file='cas_numbers.xlsx',sheetName=paste0(gsub('/','',modes[i]),'_ksi'),append=(i!=2))
  }
}

# cas fatal
for(i in 1:length(modes)){
  dist_name <- strsplit(modes[i],'/')[[1]][1]
  if(dist_name %in% mode_road_city_dist$mode){
    raw_numbers <- t(sapply(cities,function(x)
      sapply(roads,function(y) nrow(subset(injuries,region==x&road==y&cas_mode==modes[i]&cas_severity=='Fatal')))
    ))
    print(dist_name)
    total <- sapply(roads,function(y) nrow(subset(injuries,road==y&cas_mode==modes[i]&cas_severity=='Fatal')))
    print(total)
    raw_numbers <- raw_numbers[match(city_order,cities),]
    rownames(raw_numbers) <- city_order
    raw_numbers <- rbind(raw_numbers,total=total)
    xlsx::write.xlsx(raw_numbers,file='cas_numbers.xlsx',sheetName=paste0(gsub('/','',modes[i]),'_fatal'),append=T)
  }
}
