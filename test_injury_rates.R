rm(list=ls())
setwd('~/overflow_dropbox/mh-injury/')
library(stringr)
library(dplyr)
library(stats)
library(tidyr)
library(splines)
library(data.table)
model_modes <- c('pedestrian','cyclist','car/taxi','motorcycle')
mh_path <- "~/overflow_dropbox/mh-injury/"
overflow_path <- paste0(mh_path,"/rds_storage/")
if(file.exists(paste0(overflow_path,'codes_for_stats19.Rds'))&&file.exists(paste0(overflow_path,'city_regions.Rds'))){
  codes_for_stats19 <- readRDS(paste0(overflow_path,'codes_for_stats19.Rds'))
  city_regions <- readRDS(paste0(overflow_path,'city_regions.Rds'))
}else{
  source('get_area_codes.R')
}
stats19 <- readRDS(paste0(overflow_path,'processed_injuries_3.Rds'))
injuries <- subset(stats19, year>=2010&year<2016&cas_severity%in%c('Fatal','Serious'))
injuries$road <- 'motorway'
injuries$road[injuries$roadtype!='Motorway/A(M)'] <- 'other'

all_distances <- readRDS(paste0(overflow_path,'base_injury_distances.Rds'))
mode_road_city_dist <- all_distances$distance_for_cas
roadnames <- c('motorway','other')
total_mode_city <- mode_road_city_dist[,lapply(.SD,sum),by=c('mode_name','city_region'),.SDcols=roadnames]
#total_mode_city <- rowSums(mode_road_city_dist[,-c(1:2)])
colnames(total_mode_city) <- c('mode','city','motorway','other')
total_mode_city$mode[total_mode_city$mode=='walk'] <- 'pedestrian'
total_mode_city$mode[total_mode_city$mode=='cycle'] <- 'cyclist'
total_mode_city$mode[total_mode_city$mode=='cardrive'] <- 'car'
total_mode_city$mode[total_mode_city$mode=='mbikedrive'] <- 'motorcycle'
injuries$cas_mode[injuries$cas_mode=='car/taxi'] <- 'car'

roads <- colnames(total_mode_city)[3:ncol(total_mode_city)]
cities <- unique(total_mode_city$city)
city_order <- c('northeast','bristol','nottingham','greatermanchester','sheffield','leeds','westmidlands','liverpool','london')#
modes <- unique(total_mode_city$mode)

cols <- rainbow(length(cities))
{pdf(paste0('cas_rates.pdf'),width=15,height=10); par(mfrow=c(2,3))
  for(i in 1:length(modes)){
    dist_name <- strsplit(modes[i],'/')[[1]][1]
    #if(dist_name %in% mode_road_city_dist$mode){
      raw_rates <- t(sapply(cities,function(x)
        sapply(roads,function(y) nrow(subset(injuries,region==x&road==y&cas_mode==modes[i])))
      ))/
        subset(total_mode_city,mode==dist_name)[,3:ncol(total_mode_city)]*1e6
      print(dist_name)
      total <- sapply(roads,function(y) nrow(subset(injuries,road==y&cas_mode==modes[i])))/
        colSums(subset(total_mode_city,mode==dist_name)[,3:ncol(total_mode_city)]*1e-6)
      print(total)
      raw_rates <- raw_rates[match(city_order,cities),]
      rownames(raw_rates) <- city_order
      raw_rates_write <- rbind(as.matrix(raw_rates),total=matrix(total,ncol=length(roads)))
      rownames(raw_rates_write) <- c(city_order,'total')
      xlsx::write.xlsx(raw_rates_write,file=paste0('cas_rates.xlsx'),sheetName=gsub('/','',modes[i]),append=(i!=1))
      xlsx::write.xlsx(subset(total_mode_city,mode==dist_name)[,2:ncol(total_mode_city)],file=paste0('cas_distances.xlsx'),sheetName=gsub('/','',modes[i]),append=(i!=1))
      raw_rates[raw_rates==0] <- 1e-0
      raw_rates[is.na(raw_rates)] <- 1e-0
      raw_rates[!sapply(raw_rates,is.finite)] <- 1e-0
      barplot(log(as.matrix(raw_rates)),beside=T,col=cols,main=dist_name)
  }
  plot(0,0,col='white',frame=F,xaxt='n',yaxt='n')
  legend(fill=cols,legend=city_order,x=-1,y=1,bty='n')
  dev.off()
  }


mode_road_city_dist <- all_distances$distance_for_strike
roadnames <- c('motorway','other')
total_mode_city <- mode_road_city_dist[,lapply(.SD,sum),by=c('mode_name','city_region'),.SDcols=roadnames]
#total_mode_city <- rowSums(mode_road_city_dist[,-c(1:2)])
colnames(total_mode_city) <- c('mode','city','motorway','other')
total_mode_city$mode[total_mode_city$mode=='walk'] <- 'pedestrian'
total_mode_city$mode[total_mode_city$mode=='cycle'] <- 'cyclist'
total_mode_city$mode[total_mode_city$mode=='cardrive'] <- 'car'
total_mode_city$mode[total_mode_city$mode=='mbikedrive'] <- 'motorcycle'
injuries$strike_mode[injuries$strike_mode=='car/taxi'] <- 'car'

{pdf(paste0('strike_rates.pdf'),width=15,height=10); par(mfrow=c(2,3))
  for(i in 1:length(modes)){
    dist_name <- strsplit(modes[i],'/')[[1]][1]
      raw_rates <- t(sapply(cities,function(x)
        sapply(roads,function(y) nrow(subset(injuries,region==x&road==y&strike_mode==modes[i])))
      ))/
        subset(total_mode_city,mode==dist_name)[,3:ncol(total_mode_city)]*1e6
      print(dist_name)
      total <- sapply(roads,function(y) nrow(subset(injuries,road==y&strike_mode==modes[i])))/
        colSums(subset(total_mode_city,mode==dist_name)[,3:ncol(total_mode_city)]*1e-6)
      print(total)
      raw_rates <- raw_rates[match(city_order,cities),]
      rownames(raw_rates) <- city_order
      raw_rates_write <- rbind(as.matrix(raw_rates),total=matrix(total,ncol=length(roads)))
      rownames(raw_rates_write) <- c(city_order,'total')
      xlsx::write.xlsx(raw_rates_write,file=paste0('strike_rates.xlsx'),sheetName=gsub('/','',modes[i]),append=(i!=1))
      xlsx::write.xlsx(subset(total_mode_city,mode==dist_name)[,2:ncol(total_mode_city)],file=paste0('strike_distances.xlsx'),sheetName=gsub('/','',modes[i]),append=(i!=1))
      raw_rates[raw_rates==0] <- 1e-0
      raw_rates[is.na(raw_rates)] <- 1e-0
      raw_rates[!sapply(raw_rates,is.finite)] <- 1e-0
      barplot(log(as.matrix(raw_rates)),beside=T,col=cols,main=dist_name)
  }
  plot(0,0,col='white',frame=F,xaxt='n',yaxt='n')
  legend(fill=cols,legend=city_order,x=-1,y=1,bty='n')
  dev.off()
}
#################################################################

## raw injury numbers

# strike ksi
for(i in 1:length(modes)){
  dist_name <- strsplit(modes[i],'/')[[1]][1]
  #if(dist_name %in% mode_road_city_dist$mode){
  raw_numbers <- t(sapply(cities,function(x)
    sapply(roads,function(y) nrow(subset(injuries,region==x&road==y&strike_mode==modes[i])))
  ))
  print(dist_name)
  total <- sapply(roads,function(y) nrow(subset(injuries,road==y&strike_mode==modes[i])))
  print(total)
  raw_numbers <- raw_numbers[match(city_order,cities),]
  rownames(raw_numbers) <- city_order
  raw_numbers <- rbind(raw_numbers,total=total)
  xlsx::write.xlsx(raw_numbers,file='strike_numbers.xlsx',sheetName=paste0(gsub('/','',modes[i]),'_ksi'),append=(i!=1))
  #}
}

cas_modes <- unique(injuries$cas_mode)
raw_numbers <- t(sapply(cities,function(x)
  sapply(cas_modes,function(y) nrow(subset(injuries,region==x&road=='other'&cas_mode==y&strike_mode=='cyclist')))
))
total <- sapply(cas_modes,function(y) nrow(subset(injuries,road=='other'&cas_mode==y&strike_mode=='cyclist')))
print(total)
raw_numbers <- raw_numbers[match(city_order,cities),]
rownames(raw_numbers) <- city_order
raw_numbers <- rbind(raw_numbers,total=total)
xlsx::write.xlsx(raw_numbers,file='cyclist_strike_numbers.xlsx',sheetName=paste0(gsub('/','',modes[i]),'_ksi'),append=(i!=1))
#}

# strike fatal
for(i in 1:length(modes)){
  dist_name <- strsplit(modes[i],'/')[[1]][1]
  #if(dist_name %in% mode_road_city_dist$mode){
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
  #}
}

# cas ksi
for(i in 1:length(modes)){
  dist_name <- strsplit(modes[i],'/')[[1]][1]
  #if(dist_name %in% mode_road_city_dist$mode){
    raw_numbers <- t(sapply(cities,function(x)
      sapply(roads,function(y) nrow(subset(injuries,region==x&road==y&cas_mode==modes[i])))
    ))
    print(dist_name)
    total <- sapply(roads,function(y) nrow(subset(injuries,road==y&cas_mode==modes[i])))
    print(total)
    raw_numbers <- raw_numbers[match(city_order,cities),]
    rownames(raw_numbers) <- city_order
    raw_numbers <- rbind(raw_numbers,total=total)
    xlsx::write.xlsx(raw_numbers,file='cas_numbers.xlsx',sheetName=paste0(gsub('/','',modes[i]),'_ksi'),append=(i!=1))
  #}
}

# cas fatal
for(i in 1:length(modes)){
  dist_name <- strsplit(modes[i],'/')[[1]][1]
  #if(dist_name %in% mode_road_city_dist$mode){
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
  #}
}
