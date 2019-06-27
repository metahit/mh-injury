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
  cities <- list()
  cities[[1]] <- readRDS(paste0('../mh-execute/inputs/scenarios/SPind_E06000022.Rds'))
  cities[[2]] <- readRDS(paste0('../mh-execute/inputs/scenarios/SPind_E06000023.Rds'))
  cities[[3]] <- readRDS(paste0('../mh-execute/inputs/scenarios/SPind_E06000024.Rds'))
  cities[[4]] <- readRDS(paste0('../mh-execute/inputs/scenarios/SPind_E06000025.Rds'))
  
  synth_pop <- do.call(rbind,cities)
  synth_pop$dem_index <- match(synth_pop$demogindex,demogindex_to_numerical)
  
  # year
  break_labels <- sapply(names(synth_pop),function(x)strsplit(x,'_')[[1]])
  base_indices <- sapply(break_labels,function(x)'base'%in%x)
  distdur <- 'wkkm'
  dist_indices <- sapply(break_labels,function(x)distdur%in%x)
  mode_indices <- dist_indices&base_indices
  modenames <- sapply(break_labels[mode_indices],function(x)x[2])
  
  ##!! decide smoothing
  
  travel <- matrix(0,nrow=length(demo_indices),ncol=length(modenames))
  colnames(travel) <- modenames
  for(i in 1:length(demo_indices)){
    subpop <- subset(synth_pop,dem_index==i)
    subpop <- subpop[,mode_indices] 
    travel[i,] <- colSums(subpop)
  }
  
  # age by mode
  cas_travel <- matrix(0,nrow=nrow(travel),ncol=length(model_modes))
  colnames(cas_travel) <- model_modes
  cas_travel[,colnames(cas_travel)%in%'pedestrian'] <- travel[,modenames=='walk']
  cas_travel[,colnames(cas_travel)%in%'cyclist'] <- travel[,modenames=='cycle']
  cas_travel[,colnames(cas_travel)%in%'motorcycle'] <- travel[,modenames=='mbikedrive'] + travel[,modenames=='mbikepass']
  cas_travel[,colnames(cas_travel)%in%'car/taxi'] <- travel[,modenames=='cardrive'] + travel[,modenames=='carpass'] + travel[,modenames=='taxi']
  
  # road by mode
  roads <- unique(injury_table[[1]][[1]]$road)
  mode_proportions <- matrix(0,nrow=length(roads),ncol=length(model_modes))
  colnames(mode_proportions) <- model_modes
  rownames(mode_proportions) <- roads
  mode_proportions[,colnames(mode_proportions)%in%'pedestrian'] <- c(0,  0.05,0.1,0.1,0.75)
  mode_proportions[,colnames(mode_proportions)%in%'cyclist'] <-    c(0,  0.05,0.1,0.1,0.75)
  mode_proportions[,colnames(mode_proportions)%in%'motorcycle'] <- c(0.2,0.1, 0.1,0.3,0.3)
  mode_proportions[,colnames(mode_proportions)%in%'car/taxi'] <-   c(0.2,0.1, 0.1,0.3,0.3)
  
  # road by year, city
  
  for(j in 1:2){
    # initialise distance
    injury_table[[1]][[j]]$cas_distance <- 0
    for(i in 1:ncol(cas_travel)){
      indices <- which(injury_table[[1]][[j]]$cas_mode==colnames(cas_travel)[i])
      injury_table[[1]][[j]]$cas_distance[indices] <- cas_travel[injury_table[[1]][[j]]$cas_index[indices],i] * mode_proportions[injury_table[[1]][[j]]$road_index[indices],i]
    }
  }
  
  # age by mode
  strike_travel <- matrix(0,nrow=nrow(travel),ncol=length(model_modes))
  colnames(strike_travel) <- model_modes
  strike_travel[,colnames(strike_travel)%in%'pedestrian'] <- travel[,modenames=='walk']
  strike_travel[,colnames(strike_travel)%in%'cyclist'] <- travel[,modenames=='cycle']
  strike_travel[,colnames(strike_travel)%in%'motorcycle'] <- travel[,modenames=='mbikedrive'] 
  strike_travel[,colnames(strike_travel)%in%'car/taxi'] <- travel[,modenames=='cardrive']
  
  # road by year, city
  
  for(j in 1:2){
    # initialise distance
    injury_table[[j]][[1]]$strike_distance <- 0
    for(i in 1:ncol(strike_travel)){
      indices <- which(injury_table[[j]][[1]]$strike_mode==colnames(strike_travel)[i])
      injury_table[[j]][[1]]$strike_distance[indices] <- strike_travel[injury_table[[j]][[1]]$strike_index[indices],i] * mode_proportions[injury_table[[j]][[1]]$road_index[indices],i]
    }
  }
  
  ##!! do road and year distances for van, bus, light goods, heavy goods.
  for(j in 1:2)  injury_table[[2]][[j]]$cas_distance <- 1
  for(j in 1:2)  injury_table[[j]][[2]]$strike_distance <- 1
  
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
      injury_table[[i]][[j]] <- injury_table[[i]][[j]][,colnames(injury_table[[i]][[j]])%in%keepnames]
  
  saveRDS(injury_table,paste0(overflow_path,'processed_injuries_8.Rds'),version=2)
}

# get city data
city_table <- injury_table
for(i in 1:2)
  for(j in 1:2)
    city_table[[i]][[j]] <- injury_table[[i]][[j]][injury_table[[i]][[j]]$year==2015,]

saveRDS(city_table,paste0(overflow_path,'processed_injuries_9.Rds'),version=2)


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
    saveRDS(trimmed_mod,paste0('city_region',i,j,'.Rds'),version=2)
  }
}



common_coefs <- intersect(names(mod[[1]][[1]]$coefficients),names(mod[[2]][[2]]$coefficients))
do.call(cbind,lapply(mod,function(x)sapply(x,function(y)y$coefficients[names(y$coefficients)%in%common_coefs])))
cas_coefs <- names(mod[[1]][[1]]$coefficients)[!names(mod[[1]][[1]]$coefficients)%in%names(mod[[2]][[1]]$coefficients)]
sapply(mod[[1]],function(y)y$coefficients[names(y$coefficients)%in%cas_coefs])
str_coefs <- names(mod[[1]][[1]]$coefficients)[!names(mod[[1]][[1]]$coefficients)%in%names(mod[[1]][[2]]$coefficients)]
sapply(list(mod[[1]][[1]],mod[[2]][[1]]),function(y)y$coefficients[names(y$coefficients)%in%str_coefs])


