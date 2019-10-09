rm(list=ls())
setwd('~/overflow_dropbox/mh-injury/')
######################################################################


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

######################################################################
## add distance
if(file.exists(paste0(overflow_path,'processed_injuries_8.Rds'))){
  injury_table <- readRDS(paste0(overflow_path,'processed_injuries_8.Rds'))
}else{
  
  ######################################################################
  ## add index to injuries
  library(readxl)
  demography <- readxl::read_xlsx('../mh-execute/inputs/scenarios/190330_sp_ind_codebook.xlsx',sheet=2,col_names=F)
  demogindex_to_numerical <- unlist(demography[,3])
  demography[,3] <- 1:nrow(demography)
  demo_indices <- unlist(demography[,3])
  age_table <- readxl::read_xlsx('../mh-execute/inputs/scenarios/190330_sp_ind_codebook.xlsx',sheet=1,col_names=F)
  age_category <- unlist(age_table[,1])
  age_lower_bounds <- as.numeric(sapply(age_category,function(x)strsplit(x,' to ')[[1]][1]))
  if(file.exists(paste0(overflow_path,'processed_injuries_7.Rds'))){
    injury_table <- readRDS(paste0(overflow_path,'processed_injuries_7.Rds'))
  }else{
    
    ######################################################################
    ## injury data
    
    if(file.exists(paste0(overflow_path,'processed_injuries_6_tabulated_injuries.Rds'))){
      injury_table <- readRDS(paste0(overflow_path,'processed_injuries_6_tabulated_injuries.Rds'))
    }else{
      source('build_injury_data_from_stats19.R')
    }
    
    
    age_match <- demography
    colnames(age_match) <- c('cas_male','cas_age_cat','cas_index')
    age_match$cas_male <- as.numeric(age_match$cas_male=='male')
    for(i in 1:2) injury_table[[1]][[i]] <- left_join(injury_table[[1]][[i]],age_match,by=c('cas_male','cas_age_cat'))
    
    colnames(age_match) <- c('strike_male','strike_age_cat','strike_index')
    for(i in 1:2) injury_table[[i]][[1]] <- left_join(injury_table[[i]][[1]],age_match,by=c('strike_male','strike_age_cat'))
    
    roads <- unique(injury_table[[1]][[1]]$road)
    for(i in 1:2) for(j in 1:2)
      injury_table[[i]][[j]]$road_index <- match(injury_table[[i]][[j]]$road,roads)
    
    saveRDS(injury_table,paste0(overflow_path,'processed_injuries_7.Rds'),version=2)
  }
  ######################################################################
  
  
  ## distance data
  all_distances <- readRDS('../mh-execute/all_distances.Rds')
  distance_for_strike <- all_distances[[1]]$distance_for_strike
  distance_for_cas <- all_distances[[1]]$distance_for_cas
  
  mode_road_city_dist <- read.csv('../mh-execute/outputs/mode_road_city.csv',stringsAsFactors = F)
  total_mode_city <- rowSums(mode_road_city_dist[,-c(1:2)])
  colnames(mode_road_city_dist) <- c('city','mode','motorway','urban_A','rural_A','urban_B','rural_B')
  mode_road_city_dist$mode[mode_road_city_dist$mode=='lgv'] <- 'light goods'
  mode_road_city_dist$mode[mode_road_city_dist$mode=='hgv'] <- 'heavy goods'
  mode_road_city_dist_ordered <- as.matrix(mode_road_city_dist[,match(roads,colnames(mode_road_city_dist))])
  for(i in 3:ncol(mode_road_city_dist)) mode_road_city_dist[,i] <- mode_road_city_dist[,i]/total_mode_city
  
  # road by mode
  roads <- unique(injury_table[[1]][[1]]$road)
  
  # initialise distance
  for(j in 1:2){
    injury_table[[1]][[j]]$cas_distance <- 0
    injury_table[[j]][[1]]$strike_distance <- 0
    injury_table[[2]][[j]]$cas_distance <- 1
    injury_table[[j]][[2]]$strike_distance <- 1
  }
  
  
  distance_for_cas$mode_name[distance_for_cas$mode_name=='cardrive'] <- 'car/taxi'
  distance_for_cas$mode_name[distance_for_cas$mode_name=='cycle'] <- 'cyclist' 
  distance_for_cas$mode_name[distance_for_cas$mode_name=='walk'] <- 'pedestrian'
  distance_for_cas$mode_name[distance_for_cas$mode_name=='mbikedrive'] <- 'motorcycle'
  distance_for_cas$demogindex <- match(distance_for_cas$demogindex,demogindex_to_numerical)
  colnames(distance_for_cas)[c(1,2,4:7)] <- c('cas_mode','cas_index',"rural_B","rural_A","urban_B","urban_A")
  
  distance_for_strike$mode_name[distance_for_strike$mode_name=='cardrive'] <- 'car/taxi'
  distance_for_strike$mode_name[distance_for_strike$mode_name=='cycle'] <- 'cyclist' 
  distance_for_strike$mode_name[distance_for_strike$mode_name=='walk'] <- 'pedestrian'
  distance_for_strike$mode_name[distance_for_strike$mode_name=='mbikedrive'] <- 'motorcycle'
  distance_for_strike$demogindex <- match(distance_for_strike$demogindex,demogindex_to_numerical)
  colnames(distance_for_strike)[c(1,2,4:7)] <- c('strike_mode','strike_index',"rural_B","rural_A","urban_B","urban_A")
  
  for(city_name in names(codes_for_stats19)){
    
    ## primary cas
    la_distances <- subset(distance_for_cas,la%in%codes_for_stats19$bristol)#[[city_name]])
    city_distances <- la_distances[,lapply(.SD,sum),.SDcols=c("rural_B","rural_A","urban_B","urban_A",  'motorway'),by=c('cas_index','cas_mode')]
    melted_city_distances <- melt(city_distances,id.vars=c('cas_mode','cas_index'),variable.name='road',value.name='cas_distance')
    melted_city_distances <- subset(melted_city_distances,cas_mode%in%c("pedestrian","cyclist","car/taxi","motorcycle"))
    for(j in 1:2)
      setDT(injury_table[[1]][[j]])[melted_city_distances,on=c('cas_mode','cas_index','road'),cas_distance:=i.cas_distance]
    
    ## whw strike
    la_distances <- subset(distance_for_strike,la%in%codes_for_stats19$bristol)#[[city_name]])
    city_distances <- la_distances[,lapply(.SD,sum),.SDcols=c("rural_B","rural_A","urban_B","urban_A",  'motorway'),by=c('strike_index','strike_mode')]
    melted_city_distances <- melt(city_distances,id.vars=c('strike_mode','strike_index'),variable.name='road',value.name='strike_distance')
    melted_city_distances <- subset(melted_city_distances,strike_mode%in%c("pedestrian","cyclist","car/taxi","motorcycle"))
    for(j in 1:2)
      setDT(injury_table[[j]][[1]])[melted_city_distances,on=c('strike_mode','strike_index','road'),strike_distance:=i.strike_distance]
    
    # road by year, city
    
    ## bus cas distance should be sum over distance_for_cas
    for(road_type in c('motorway','rural_A','rural_B','urban_A','urban_B'))
      mode_road_city_dist_ordered[mode_road_city_dist$mode==cas_mode&mode_road_city_dist$city==city_name,which(colnames(mode_road_city_dist_ordered)==road_type)] <- sum(subset(distance_for_cas,cas_mode=='bus')[[road_type]])
    
    for(j in 1:2){
      
      ##!! do road and year distances for bus, light goods, heavy goods.
      city_index <- injury_table[[2]][[j]]$region==city_name
      for(cas_mode in c('bus','heavy goods','light goods')) {
        indices <- which(injury_table[[2]][[j]]$cas_mode==cas_mode&city_index)
        injury_table[[2]][[j]]$cas_distance[indices] <- mode_road_city_dist_ordered[mode_road_city_dist$mode==cas_mode&mode_road_city_dist$city==city_name,injury_table[[2]][[j]]$road_index[indices]]
      }
      city_index <- injury_table[[j]][[2]]$region==city_name
      for(strike_mode in c('bus','heavy goods','light goods')) {
        indices <- which(injury_table[[j]][[2]]$strike_mode==strike_mode&city_index)
        injury_table[[j]][[2]]$strike_distance[indices] <- mode_road_city_dist_ordered[mode_road_city_dist$mode==strike_mode&mode_road_city_dist$city==city_name,injury_table[[j]][[2]]$road_index[indices]]
      }
    }
  }
  
  
  for(i in 1:2) for(j in 1:2) injury_table[[i]][[j]] <- subset(injury_table[[i]][[j]],cas_distance>0&strike_distance>0)
  
  ages <- unique(injury_table[[1]][[1]]$cas_age_cat)
  ages <- ages[!is.na(ages)]
  lower_age <- as.numeric(sapply(ages,function(x)strsplit(x,' to ')[[1]][1]))
  upper_age <- as.numeric(sapply(ages,function(x)strsplit(x,' to ')[[1]][2]))
  mid_age <- (lower_age+upper_age)/2
  mid_age[length(mid_age)] <- 97
  mid_ages <- c(mid_age,mid_age)
  
  for(j in 1:2)  injury_table[[1]][[j]]$cas_age <- mid_ages[injury_table[[1]][[j]]$cas_index]
  for(j in 1:2)  injury_table[[j]][[1]]$strike_age <- mid_ages[injury_table[[j]][[1]]$strike_index]
  
  keepnames <- c("year","cas_male","cas_severity","cas_mode","strike_mode","road","region","strike_male","count","cas_distance","strike_distance","cas_age","strike_age","rate",'cas_index','strike_index')
  for(i in 1:2)
    for(j in 1:2)
      injury_table[[i]][[j]] <- setDT(injury_table[[i]][[j]])[,colnames(injury_table[[i]][[j]])%in%keepnames,with=F]
  
  saveRDS(injury_table,paste0(overflow_path,'processed_injuries_8.Rds'),version=2)
}

# get city data
city_table <- injury_table
for(i in 1:2)
  for(j in 1:2)
    city_table[[i]][[j]] <- injury_table[[i]][[j]][injury_table[[i]][[j]]$year==2015,]

saveRDS(city_table,'../mh-execute/inputs/injury/processed_injuries_9.Rds',version=2)


######################################################################
## model

trim_glm_object <- function(obj){
  obj$y <- c()
  obj$model <- c()
  obj$R <- c()
  obj$qr$qr <- c()
  obj$residuals <- c()
  obj$fitted.values <- c()
  obj$effects <- c()
  #obj$linear.predictors <- c()
  obj$weights <- c()
  obj$prior.weights <- c()
  obj$data <- c()
  obj$family$variance = c()
  obj$family$dev.resids = c()
  obj$family$aic = c()
  obj$family$validmu = c()
  obj$family$simulate = c()
  #attr(obj$terms,".Environment") = c()
  attr(obj$formula,".Environment") = c()
  obj
}

test_model <- F
if(test_model){
  form <- 'count~region+offset(log(cas_distance)+log(strike_distance))'
  mod <- glm(as.formula(form),offset=-log(rate),family=poisson(link=log),data=injury_table[[2]][[2]])
  mod1 <- trim_glm_object(mod)
  predict(mod1)
}


#formula_one <- 'count~ns(year,df=2)+cas_severity+cas_mode+strike_mode+road+region+offset(log(cas_distance)+log(strike_distance))'
formula_one <- 'count~year+cas_severity+region+offset(log(cas_distance)+log(strike_distance))'

##!! decide offset, splines, interactions

mod <- list()
for(i in 1:2) {
  mod[[i]] <- list()
  for(j in 1:2) {
    form <- formula_one
    #if(i==1) form <- paste0(form,'+cas_male+ns(cas_age,df=4)')
    #if(j==1) form <- paste0(form,'+strike_male+ns(strike_age,df=4)')
    print(form)
    ##for model build, set rate=1
    injury_table[[i]][[j]]$rate <- 1
    mod[[i]][[j]] <- glm(as.formula(form),family=poisson(link=log),data=injury_table[[i]][[j]])
    #saveRDS(mod[[i]][[j]],paste0('/scratch/rob/city_region',i,j,'.Rds'),version=2)
    trimmed_mod <- trim_glm_object(mod[[i]][[j]])
    print(sapply(mod[[i]][[j]],function(x)length(serialize(x, NULL))))
    print(1)
    predict(trimmed_mod,newdata=injury_table[[i]][[j]],type='response')
    trimmed_mod$offset <- c()
    print(2)
    predict(trimmed_mod,newdata=injury_table[[i]][[j]],type='response')
    trimmed_mod$linear.predictors <- c()
    print(3)
    predict(trimmed_mod,newdata=injury_table[[i]][[j]],type='response')
    saveRDS(trimmed_mod,paste0('../mh-execute/inputs/injury/city_region',i,j,'.Rds'),version=2)
  }
}



common_coefs <- intersect(names(mod[[1]][[1]]$coefficients),names(mod[[2]][[2]]$coefficients))
do.call(cbind,lapply(mod,function(x)sapply(x,function(y)y$coefficients[names(y$coefficients)%in%common_coefs])))
cas_coefs <- names(mod[[1]][[1]]$coefficients)[!names(mod[[1]][[1]]$coefficients)%in%names(mod[[2]][[1]]$coefficients)]
sapply(mod[[1]],function(y)y$coefficients[names(y$coefficients)%in%cas_coefs])
str_coefs <- names(mod[[1]][[1]]$coefficients)[!names(mod[[1]][[1]]$coefficients)%in%names(mod[[1]][[2]]$coefficients)]
sapply(list(mod[[1]][[1]],mod[[2]][[1]]),function(y)y$coefficients[names(y$coefficients)%in%str_coefs])


