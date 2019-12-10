rm(list=ls())
setwd('~/overflow_dropbox/mh-injury/')
######################################################################


library(data.table)
library(stringr)
library(dplyr)
library(stats)
library(tidyr)
library(splines)
#library(biglm)
model_modes <- c('pedestrian','cyclist','car/taxi','motorcycle')

mh_path <- "~/overflow_dropbox/mh-injury/"
overflow_path <- paste0(mh_path,"/rds_storage/")


if(file.exists(paste0(overflow_path,'codes_for_stats19.Rds'))&&file.exists(paste0(overflow_path,'city_regions.Rds'))){
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
  
  for(i in 1:2) for(j in 1:2) injury_table[[i]][[j]] <- copy(setDT(injury_table[[i]][[j]]))
  
  ## distance data
  # scale by year
  scale_by_year_raw <- readxl::read_xls('../mh-scenarios/1_InputData/2_CityRegion_scaling/RTS_NTS_distances_cityreg.xls',sheet=3,col_names=T)
  ## reference year is 2015
  reference_year <- 2015
  reference_col <- which(sapply(colnames(scale_by_year_raw),function(x)grepl(reference_year,x)))
  col_names <- strsplit(colnames(scale_by_year_raw)[reference_col],reference_year)[[1]][1]
  colnames(scale_by_year_raw) <- sapply(colnames(scale_by_year_raw),function(x)gsub(col_names,'',x))
  scale_by_year <- scale_by_year_raw
  years <- unique(injury_table[[2]][[2]]$year)
  for(yr in years) 
    scale_by_year[[as.character(yr)]] <- scale_by_year_raw[[as.character(yr)]]/scale_by_year_raw[[as.character(reference_year)]]
  scale_by_year$modename[scale_by_year$modename=='cartaxi'] <- 'car/taxi'
  scale_by_year$modename[scale_by_year$modename=='cycle'] <- 'cyclist'
  scale_by_year$modename[scale_by_year$modename=='van'] <- 'lgv'
  
  # road by mode
  roads <- unique(injury_table[[1]][[1]]$road)
  
  mode_road_city_dist <- read.csv(paste0(overflow_path,'mode_road_city_year.csv'),stringsAsFactors = F)
  colnames(mode_road_city_dist)[1:3] <- c('road','mode','year')
  mode_road_city_dist$mode[mode_road_city_dist$mode=='lgv'] <- 'light goods'
  mode_road_city_dist$mode[mode_road_city_dist$mode=='hgv'] <- 'heavy goods'
  
  scenarios <- c('base_','scen_')
  all_distances <- list()
  for(scen in 1:length(scenarios))
    all_distances[[scen]] <- readRDS(paste0(overflow_path,scenarios[scen],'injury_distances.Rds'))
  names(all_distances) <- scenarios
  
  for(scen in 1:length(scenarios)){
    scen_pref <- scenarios[scen]
    
    strike_col <- paste0(scen_pref,'strike_distance')
    cas_col <- paste0(scen_pref,'cas_distance')
    strike_sum_col <- paste0(scen_pref,'strike_distance_sum')
    cas_sum_col <- paste0(scen_pref,'cas_distance_sum')
    
    distance_for_strike <- all_distances[[scen]]$distance_for_strike
    distance_for_cas <- all_distances[[scen]]$distance_for_cas
    all_distances[[scen]] <- 0
  
  
    # initialise distance
    for(j in 1:2){
      injury_table[[1]][[j]][[cas_col]] <- 0
      injury_table[[j]][[1]][[strike_col]] <- 0
      injury_table[[2]][[j]][[cas_col]] <- 1
      injury_table[[j]][[2]][[strike_col]] <- 1
      injury_table[[1]][[j]][[cas_sum_col]] <- 0
      injury_table[[j]][[1]][[strike_sum_col]] <- 0
      injury_table[[2]][[j]][[cas_sum_col]] <- 1
      injury_table[[j]][[2]][[strike_sum_col]] <- 1
    }
  
    
    distance_for_cas$mode_name[distance_for_cas$mode_name=='cardrive'] <- 'car/taxi'
    distance_for_cas$mode_name[distance_for_cas$mode_name=='cycle'] <- 'cyclist' 
    distance_for_cas$mode_name[distance_for_cas$mode_name=='walk'] <- 'pedestrian'
    distance_for_cas$mode_name[distance_for_cas$mode_name=='mbikedrive'] <- 'motorcycle'
    distance_for_cas$demogindex <- match(distance_for_cas$demogindex,demogindex_to_numerical)
    colnames(distance_for_cas)[c(1,2)] <- c('cas_mode','cas_index')#,"rural_B","rural_A","urban_B","urban_A")
    
    distance_for_strike$mode_name[distance_for_strike$mode_name=='cardrive'] <- 'car/taxi'
    distance_for_strike$mode_name[distance_for_strike$mode_name=='cycle'] <- 'cyclist' 
    distance_for_strike$mode_name[distance_for_strike$mode_name=='walk'] <- 'pedestrian'
    distance_for_strike$mode_name[distance_for_strike$mode_name=='mbikedrive'] <- 'motorcycle'
    distance_for_strike$demogindex <- match(distance_for_strike$demogindex,demogindex_to_numerical)
    colnames(distance_for_strike)[c(1,2)] <- c('strike_mode','strike_index')#,"rural_B","rural_A","urban_B","urban_A")
  
    for(city_name in names(codes_for_stats19)){
      ##!! scale by year - scale_by_year
      ## primary cas
      la_distances <- subset(distance_for_cas,city_region==city_name)
      city_distances <- la_distances[,lapply(.SD,sum),.SDcols=roads,by=c('cas_index','cas_mode')]
      melted_city_distances <- melt(city_distances,id.vars=c('cas_mode','cas_index'),variable.name='road',value.name=cas_col)
      melted_city_distances <- subset(melted_city_distances,cas_mode%in%c("pedestrian","cyclist","car/taxi","motorcycle"))
      melted_city_distances[,region := city_name]
      melted_city_distances[,paste0(cas_sum_col):=sum(get(..cas_col)),by=c('cas_mode','region','road'),.SDcols=cas_col]
      for(j in 1:2){
        injury_table[[1]][[j]][melted_city_distances,on=c('cas_mode','cas_index','road','region'),c(paste0(cas_col),paste0(cas_sum_col)):=list(get(paste0('i.',cas_col)),get(paste0('i.',cas_sum_col)))]
        for(mode_name in unique(injury_table[[1]][[j]]$cas_mode))
          for(yr in unique(injury_table[[1]][[j]]$year)){
            values <- injury_table[[1]][[j]]$year==yr&injury_table[[1]][[j]]$cas_mode==mode_name&injury_table[[1]][[j]]$region==city_name
            if(mode_name%in%unique(scale_by_year$modename)){
              injury_table[[1]][[j]][[cas_col]][values] <- 
                injury_table[[1]][[j]][[cas_col]][values] * as.numeric(scale_by_year[scale_by_year$home_cityregion==city_name&scale_by_year$modename==mode_name,colnames(scale_by_year)==yr])
              injury_table[[1]][[j]][[cas_sum_col]][values] <- 
                injury_table[[1]][[j]][[cas_sum_col]][values] * as.numeric(scale_by_year[scale_by_year$home_cityregion==city_name&scale_by_year$modename==mode_name,colnames(scale_by_year)==yr])
            }
          }
      }
      
      ## whw strike
      la_distances <- subset(distance_for_strike,city_region==city_name)
      city_distances <- la_distances[,lapply(.SD,sum),.SDcols=roads,by=c('strike_index','strike_mode')]
      melted_city_distances <- melt(city_distances,id.vars=c('strike_mode','strike_index'),variable.name='road',value.name=strike_col)
      melted_city_distances <- subset(melted_city_distances,strike_mode%in%c("pedestrian","cyclist","car/taxi","motorcycle"))
      melted_city_distances[,region := city_name]
      melted_city_distances[,paste0(strike_sum_col):=sum(get(..strike_col)),by=c('strike_mode','region','road'),.SDcols=strike_col]
      for(j in 1:2){
        injury_table[[j]][[1]][melted_city_distances,on=c('strike_mode','strike_index','road','region'),c(paste0(strike_col),paste0(strike_sum_col)):=list(get(paste0('i.',strike_col)),get(paste0('i.',strike_sum_col)))]
        for(mode_name in unique(injury_table[[j]][[1]]$strike_mode))
          for(yr in unique(injury_table[[j]][[1]]$year)){
            values <- injury_table[[j]][[1]]$year==yr&injury_table[[j]][[1]]$strike_mode==mode_name&injury_table[[j]][[1]]$region==city_name
            if(mode_name%in%unique(scale_by_year$modename)){
              injury_table[[j]][[1]][[strike_col]][values] <- 
                injury_table[[j]][[1]][[strike_col]][values] * as.numeric(scale_by_year[scale_by_year$home_cityregion==city_name&scale_by_year$modename==mode_name,colnames(scale_by_year)==yr])
              injury_table[[j]][[1]][[strike_sum_col]][values] <- 
                injury_table[[j]][[1]][[strike_sum_col]][values] * as.numeric(scale_by_year[scale_by_year$home_cityregion==city_name&scale_by_year$modename==mode_name,colnames(scale_by_year)==yr])
            }
          }
      }
      
      # road by year, city
      
      ##!! bus cas distance should be sum over distance_for_cas e.g.
      #for(road_type in roads)
      #  mode_road_city_dist_ordered[mode_road_city_dist$mode=='bus'&mode_road_city_dist$city==city_name,which(colnames(mode_road_city_dist_ordered)==road_type)] <- sum(subset(distance_for_cas,cas_mode=='bus')[[road_type]])
      for(j in 1:2){
        city_index <- injury_table[[2]][[j]]$region==city_name
        for(cas_mode in c('bus','heavy goods','light goods'))
          for(yr in years){
            indices <- which(injury_table[[2]][[j]]$cas_mode==cas_mode&city_index&injury_table[[2]][[j]]$year==yr)
            injury_table[[2]][[j]][[cas_col]][indices] <- c(mode_road_city_dist[mode_road_city_dist$year==yr&mode_road_city_dist$mode==cas_mode,colnames(mode_road_city_dist)==city_name])[injury_table[[2]][[j]]$road_index[indices]]
          }
        injury_table[[2]][[j]][[cas_sum_col]] <- injury_table[[2]][[j]][[cas_col]]
        city_index <- injury_table[[j]][[2]]$region==city_name
        for(strike_mode in c('bus','heavy goods','light goods')) 
          for(yr in years){
            indices <- which(injury_table[[j]][[2]]$strike_mode==strike_mode&city_index&injury_table[[j]][[2]]$year==yr)
            injury_table[[j]][[2]][[strike_col]][indices] <- c(mode_road_city_dist[mode_road_city_dist$year==yr&mode_road_city_dist$mode==strike_mode,colnames(mode_road_city_dist)==city_name])[injury_table[[j]][[2]]$road_index[indices]]
          }
        injury_table[[j]][[2]][[strike_sum_col]] <- injury_table[[j]][[2]][[strike_col]]
      }
    }
    
  
  #for(i in 1:2) for(j in 1:2) injury_table[[i]][[j]] <- subset(injury_table[[i]][[j]],get(cas_col)>0&get(strike_col)>0)
  }
  
  ages <- unique(injury_table[[1]][[1]]$cas_age_cat)
  ages <- ages[!is.na(ages)]
  lower_age <- as.numeric(sapply(ages,function(x)strsplit(x,' to ')[[1]][1]))
  upper_age <- as.numeric(sapply(ages,function(x)strsplit(x,' to ')[[1]][2]))
  mid_age <- (lower_age+upper_age)/2
  mid_age[length(mid_age)] <- 97
  mid_ages <- c(mid_age,mid_age)
  
  for(j in 1:2)  injury_table[[1]][[j]]$cas_age <- mid_ages[injury_table[[1]][[j]]$cas_index]
  for(j in 1:2)  injury_table[[j]][[1]]$strike_age <- mid_ages[injury_table[[j]][[1]]$strike_index]
  
  keepnames <- c("year","cas_male","cas_severity","cas_mode","strike_mode","road","region","strike_male","count","cas_age","strike_age","rate",'cas_index','strike_index',
                 paste0(scenarios,'cas_distance_sum'),paste0(scenarios,'strike_distance_sum'),paste0(scenarios,'cas_distance'),paste0(scenarios,'strike_distance'))
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

for(i in 1:2) for(j in 1:2){
  colnames(injury_table[[i]][[j]]) <- sapply(colnames(injury_table[[i]][[j]]),function(x) gsub('base_','',x))
  injury_table[[i]][[j]] <- subset(injury_table[[i]][[j]],cas_distance>0&strike_distance>0)
}

test_model <- F
if(test_model){
  new_data <- injury_table[[2]][[2]]
  new_data$cas_distance <- new_data$cas_distance*1.5
  new_data$strike_distance <- new_data$strike_distance*0.75
  sums <- seq(0.5,1.5,by=0.1)
  inter <- c()
  base_predictions <- predictions <- list()
  for(i in 1:length(sums)){
    coeff <- sums[i]
    form <- paste0('count~offset(',coeff,'*log(cas_distance_sum)+',coeff,'*log(strike_distance_sum))')
    mod <- glm(as.formula(form),offset=-log(rate),family=poisson(link=log),data=injury_table[[2]][[2]])
    inter[i] <- coef(mod)[1]
    base_predictions[[i]] <- fitted.values(mod)
    predictions[[i]] <- predict(mod,newdata = new_data,type='response')
  }
  form <- paste0('count~offset(',1,'*log(cas_distance)+',1,'*log(strike_distance))')
  mod <- glm(as.formula(form),offset=-log(rate),family=poisson(link=log),data=injury_table[[2]][[2]])
  raw_pred <- predict(mod,type='response')
  approx_predictions <- list()
  for(i in 1:length(sums)){
    coeff <- sums[i]
    approx_predictions[[i]] <- raw_pred*(new_data$cas_distance/injury_table[[2]][[2]]$cas_distance)^coeff*(new_data$strike_distance/injury_table[[2]][[2]]$strike_distance)^coeff
  }
  mod1 <- trim_glm_object(mod)
  predict(mod1)
}


#formula_one <- 'count~ns(year,df=2)+cas_severity+cas_mode+strike_mode+road+region+offset(log(cas_distance)+log(strike_distance))'
CAS_EXPONENT <- 1
STR_EXPONENT <- 1
formula_one <- 'count~year+cas_severity+cas_mode*strike_mode+road+road:(cas_mode+strike_mode)+region+offset(log(cas_distance)+log(strike_distance)-CAS_EXPONENT*log(cas_distance_sum)-STR_EXPONENT*log(strike_distance_sum))'

##!! decide offset, splines, interactions

mod <- list()
for(i in 1:2) {
  mod[[i]] <- list()
  for(j in 1:2) {
    injury_table[[i]][[j]][order(year),]
    form <- formula_one
    if(i==1){
      form <- paste0(form,'+cas_male*ns(cas_age,df=4)+cas_mode:(ns(cas_age,df=4)+cas_male)')
      #injury_table[[i]][[j]] <- injury_table[[i]][[j]][order(cas_male,cas_age),]
    }
    if(j==1){
      form <- paste0(form,'+strike_male*ns(strike_age,df=4)+strike_mode:(strike_male+ns(strike_age,df=4))')
      #injury_table[[i]][[j]] <- injury_table[[i]][[j]][order(strike_male,strike_age),]
    }
    form <- paste0(form,'+cas_mode:cas_severity')
    print(form)
    ##for model build, set rate=1
    injury_table[[i]][[j]]$rate <- 1
    mod[[i]][[j]] <- glm(as.formula(form),family=poisson(link=log),data=injury_table[[i]][[j]])
    print(AIC(mod[[i]][[j]]))
    trimmed_mod <- trim_glm_object(mod[[i]][[j]])
    trimmed_mod$offset <- c()
    trimmed_mod$linear.predictors <- c()
    saveRDS(trimmed_mod,paste0('../mh-execute/inputs/injury/city_region',i,j,'.Rds'),version=2)
  }
}



common_coefs <- intersect(names(mod[[1]][[1]]$coefficients),names(mod[[2]][[2]]$coefficients))
do.call(cbind,lapply(mod,function(x)sapply(x,function(y)y$coefficients[names(y$coefficients)%in%common_coefs])))
cas_coefs <- names(mod[[1]][[1]]$coefficients)[!names(mod[[1]][[1]]$coefficients)%in%names(mod[[2]][[1]]$coefficients)]
sapply(mod[[1]],function(y)y$coefficients[names(y$coefficients)%in%cas_coefs])
str_coefs <- names(mod[[1]][[1]]$coefficients)[!names(mod[[1]][[1]]$coefficients)%in%names(mod[[1]][[2]]$coefficients)]
sapply(list(mod[[1]][[1]],mod[[2]][[1]]),function(y)y$coefficients[names(y$coefficients)%in%str_coefs])

mod <- list()
for(i in 1:2) {
  mod[[i]] <- list()
  for(j in 1:2) {
    form <- formula_one
    if(i==1) form <- paste0(form,'+cas_male*ns(cas_age,df=4)+(cas_male+ns(cas_age,df=4)):cas_mode')
    if(j==1) form <- paste0(form,'+strike_male*ns(strike_age,df=4)+(strike_male+ns(strike_age,df=4)):strike_mode')
    form <- paste0(form,'+strike_mode:cas_severity')
    print(form)
    ##for model build, set rate=1
    injury_table[[i]][[j]]$rate <- 1
    mod[[i]][[j]] <- glm(as.formula(form),family=poisson(link=log),data=injury_table[[i]][[j]])
    print(AIC(mod[[i]][[j]]))
    #saveRDS(mod[[i]][[j]],paste0('/scratch/rob/city_region',i,j,'.Rds'),version=2)
    trimmed_mod <- trim_glm_object(mod[[i]][[j]])
    #print(sapply(mod[[i]][[j]],function(x)length(serialize(x, NULL))))
    #print(1)
    #predict(trimmed_mod,newdata=injury_table[[i]][[j]],type='response')
    trimmed_mod$offset <- c()
    #print(2)
    #predict(trimmed_mod,newdata=injury_table[[i]][[j]],type='response')
    trimmed_mod$linear.predictors <- c()
    #print(3)
    #predict(trimmed_mod,newdata=injury_table[[i]][[j]],type='response')
    saveRDS(trimmed_mod,paste0('../mh-execute/inputs/injury/city_region',i,j,'.Rds'),version=2)
  }
}


