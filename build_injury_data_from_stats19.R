

######################################################################
## 0_read_Sources.R

# 10-2-2017 MRC-Epid JHZ
# conversion of Anna Goodman's Stata code adding direct Internet access

# function to append files as with dplyr::bind_rows()
# http://stackoverflow.com/questions/8169323/r-concatenate-two-dataframes

fastmerge <- function(d1, d2) {
  d1.names <- names(d1)
  d2.names <- names(d2)
  # columns in d1 but not in d2
  d2.add <- setdiff(d1.names, d2.names)
  # columns in d2 but not in d1
  d1.add <- setdiff(d2.names, d1.names)
  # add blank columns to d2
  if(length(d2.add) > 0) {
    for(i in 1:length(d2.add)) {
      d2[d2.add[i]] <- NA
    }
  }
  # add blank columns to d1
  if(length(d1.add) > 0) {
    for(i in 1:length(d1.add)) {
      d1[d1.add[i]] <- NA
    }
  }
  return(rbind(d1, d2))
}

# function To get and merge 2005-2014 with 2015 datasets 

get.data <- function(){
  # url0514 contains file0514 from 2005-2014
  url0514 <- "http://data.dft.gov.uk.s3.amazonaws.com/road-accidents-safety-data/Stats19_Data_2005-2014.zip"
  # individual files
  file0514 <- c("Vehicles0514.csv","Casualties0514.csv","Accidents0514.csv")
  ## version for 2014 only
  ## file0514 <- paste("DfTRoadSafety_",c("Vehicles_2014.csv","Casualties_2014.csv","Accidents_2014.csv"),sep="")

  temp <- tempfile()
  download.file(url0514,temp)
  ## download.file("http://data.dft.gov.uk/road-accidents-safety-data/DfTRoadSafety_Accidents_2014.zip",temp)
  a0514 <<- read.csv(unz(temp, file0514[3]))
  ## download.file("http://data.dft.gov.uk/road-accidents-safety-data/DfTRoadSafety_Casualties_2014.zip",temp)
  c0514 <<- read.csv(unz(temp, file0514[2]))
  ## download.file("http://data.dft.gov.uk/road-accidents-safety-data/DfTRoadSafety_Vehicles_2014.zip",temp)
  v0514 <<- read.csv(unz(temp, file0514[1]))
  ### under Windows but not Linux
  names(a0514)[names(a0514)=="ï..Accident_Index"] <- "Accident_Index"
  names(c0514)[names(c0514)=="ï..Accident_Index"] <- "Accident_Index"
  names(v0514)[names(v0514)=="ï..Accident_Index"] <- "Accident_Index"

  filenames <- c('Vehicles','Casualties','Accidents')
  assign('Vehicles',v0514)
  assign('Casualties',c0514)
  assign('Accidents',a0514)
  short_name <- c('Veh.csv','Cas.csv','Acc.csv')
  for(year in 2015:2017){
    for(table_name_ind in 1:length(filenames)){
      table_name <- filenames[table_name_ind]
      tryCatch({
        root_name <- ifelse(year==2015,'RoadSafetyData_','dftRoadSafetyData_')
        filename <- paste0("http://data.dft.gov.uk/road-accidents-safety-data/",root_name,table_name,"_",year,".zip")
        download.file(filename,temp)
      }, 
      warning=function(w) {
        root_name <- ifelse(year==2015,'RoadSafety_','dftRoadSafety_')
        filename <- paste0("http://data.dft.gov.uk/road-accidents-safety-data/",root_name,table_name,"_",year,".zip")
        download.file(filename,temp)
      },error=function(e){
        root_name <- ifelse(year==2015,'RoadSafety_','dftRoadSafety_')
        filename <- paste0("http://data.dft.gov.uk/road-accidents-safety-data/",root_name,table_name,"_",year,".zip")
        download.file(filename,temp)
      }
      )
      tryCatch(unzip(temp,exdir=overflow_path),warning=function(w)print(w))
      root_name <- ifelse(year==2015,'','dftRoadSafety_')
      full_name <-  paste0(overflow_path,'/',root_name,table_name,"_",year,".csv")
      file_contents <- if(file.exists(full_name)){
        read.csv(full_name,stringsAsFactors = F)
      }else{
        read.csv( paste0(overflow_path,'/',short_name[table_name_ind]),stringsAsFactors = F)
      }
      print(file_contents[1,])
      assign(table_name,fastmerge(get(table_name),file_contents))
      print(get(table_name)[1,])
      unlink(temp)
    }
  }
  Accidents <<- Accidents
  Casualties <<- Casualties
  Vehicles <<- Vehicles
}




######################################################################

# The URL https://data.gov.uk/dataset/road-accidents-safety-data contains the 
# 2005-2014 and 2015 data, which can be directly accessed as follows,
if(file.exists(paste0(overflow_path,"processed_injuries_1.Rds"))){
  stopped <- readRDS(paste0(overflow_path,"processed_injuries_1.Rds"))
}else{
  get.data()
  
  # rm(v0514,c0514,a0514,v2015,a2015,c2015)
  # For codebook see https://discover.ukdataservice.ac.uk/catalogue?sn=7752
  
  # Optionally, pre-downloaded data can also be processed, assumming in 
  # 'z_ITHIMFILES/Stats19/1a_DataOriginal' while the output is an empty folder
  # 'z_ITHIMFILES/Stats19/1b_DataCreated' containing '0-temp' subdirectory
  
  # get.data(local.dir="z_ITHIMfiles/Stats19")
  
  names(Accidents) <- tolower(names(Accidents))
  names(Casualties) <- tolower(names(Casualties))
  names(Vehicles) <- tolower(names(Vehicles))
  
  # Merge three datasets keeping required variables
  v1 <- c("accident_index","local_authority_.highway.", "local_authority_.district.", "x1st_road_class", "date", "number_of_vehicles","day_of_week",'time','urban_or_rural_area')
  v2 <- c("accident_index","vehicle_reference", "vehicle_type", "sex_of_driver", "age_of_driver") 
  v3 <- c("accident_index","vehicle_reference","casualty_reference", "casualty_class", "casualty_severity", "sex_of_casualty", "age_of_casualty")
  av <- merge(Accidents[v1], Vehicles[v2], by="accident_index",all=TRUE)
  avc <- merge(av,Casualties[v3],by=c("accident_index","vehicle_reference"),all=TRUE)
  
  # Drop Wales and Scotland
  stopped <- subset(avc,local_authority_.district.<=699)
  stopped <- subset(stopped,local_authority_.highway.%in%unlist(codes_for_stats19))
  # accidents_vehicles_casualties_05-15.rds
  saveRDS(stopped,file=paste0(overflow_path,"processed_injuries_1.Rds"),version=2)
  
  
  
}

######################################################################
## 2_build_Injuries.R
# carries on processing object: stopped  

if(file.exists(paste0(overflow_path,'processed_injuries_2.Rds'))){
  stopped <- readRDS(paste0(overflow_path,'processed_injuries_2.Rds'))
}else{
  # PREPARE VARIABLES
  stopped = dplyr::rename(stopped,  cas_severity = casualty_severity )
  stopped$cas_severity = recode(stopped$cas_severity, '1'="Fatal",'2'="Serious", '3'="Slight")
  
  # DATE
  td = str_split(string = stopped$date, pattern = "/",n = 3, simplify = TRUE)
  stopped$year = td[,3]
  rm(td)
  
  # rename ROAD CLASS
  stopped$st_road_class =  recode(stopped$x1st_road_class,'1'=1,'2' = 1, '3'=2, '4'=3,'5'=3, '6'=3) #1 unchanged
  stopped$roadtype = recode(stopped$st_road_class, '1'="Motorway/A(M)", '2'="A", '3'="B, C, Unclassified") 
  
  # VEHICLE MODE AND CASUALTY SEX
  stopped$veh_mode = recode(stopped$vehicle_type,'-1'=99, '1'=2, '2'=3,'3'=3, '4'=3, '5'=3, '8'=4, '9'=4,
                            '10'=6, '11'=6, '16'=99, '17'=99,'18'=99, '19'=5, '20'=7, '21'=7, '22'=99,
                            '23'=3, '90'=99, '97'=3, '98'=7 ) 
  
  #keep integer values for future
  stopped$veh_mode.int = stopped$cas_mode.int = stopped$veh_mode 
  
  stopped$cas_mode.int[stopped$casualty_class==3]   = 1
  stopped$cas_mode.int[ is.na(stopped$cas_severity)]   = NA
  
  #creates veh_mode/cas_mode label vars
  stopped$veh_mode = recode(stopped$veh_mode.int, '1'="pedestrian",'2' ="cyclist",'3'="motorcycle",
                            '4'="car/taxi",'5'="light goods",'6'="bus",'7'="heavy goods",
                            '8' = "NOV", '99' ="other or unknown")
  
  stopped$cas_mode = recode(stopped$cas_mode.int, '1'="pedestrian",'2' ="cyclist",'3'="motorcycle",
                            '4'="car/taxi",'5'="light goods",'6'="bus",'7'="heavy goods",
                            '8' = "NOV", '99' ="other or unknown")
  
  #sex of casualty
  stopped$cas_male = recode(stopped$sex_of_casualty, '-1'=NULL,'1' =1, '2'=0)
  stopped$veh_male = recode(stopped$sex_of_driver, '-1'=NULL, '1'=1, '2'=0, '3' = NULL)
  
  #ages
  stopped$cas_age = stopped$age_of_casualty    #replicate variable
  stopped$cas_age[stopped$cas_age== -1] = NA   
  stopped$veh_age = stopped$age_of_driver
  stopped$veh_age[stopped$veh_age== -1 ] = NA
  
  #RENAMING FOR CONSISTENCY
  stopped = dplyr::rename(stopped,  veh_reference  = vehicle_reference )
  
  
  ############### START ALLOCATION ALGORITHM
  
  ## SELECT A 'STRIKE VEHICLE' PEDESTRIAN AT RANDOM 
  ## (NB ONLY KNOW ABOUT THOSE PEDESTRIANS WHO WERE INJURED...
  ## DON'T NEED TO SPLIT BY VEHICLE AS THIS ONLY BECOMES RELEVANT IF NO OTHER VEHICLE BUT THE PEDESTRIAN)
  
  ## NO. OF PEDESTRIANS IN ACCIDENT    
  stopped$pedflag = 0   
  stopped$pedflag[stopped$cas_mode.int==1] = 1   #  1 if cas_mode=1 | 0: otherwise
  
  # add "numped" column
  stopped= arrange(stopped, accident_index)
  stopped.gr = aggregate(stopped$pedflag, by =list(stopped$accident_index), FUN=sum, na.rm=T)
  names(stopped.gr) = c('accident_index', 'numped')
  
  stopped= inner_join(stopped, stopped.gr, by="accident_index")
  
  # set seed 2010
  set.seed(2010)
  stopped$random0 = runif(n = nrow(stopped), min = 0, max = 1)
  
  #used in next loop
  by_stopped <- stopped %>% group_by(accident_index, cas_mode.int)   # groups by 2 vars
  
  ## LITTLE N's 
  for (x in c('male', 'age')) {
    
    # sorts by 3 vars->generate little_n's, delete intermediate var
    stopped <- mutate(arrange(stopped,accident_index, cas_mode.int, random0),
                      vartemp=unlist(lapply(group_size(by_stopped), FUN=seq_len)))
    stopped[[paste0('littlen_cas', x) ]] = stopped$vartemp 
    stopped$vartemp =NULL
    
    #pedestrians= casualties hurt in mode=1    
    #pedestrian age/sex set equal to one randomly selected pedestrian within the accident
    stopped[[paste0('ped_cas_', x) ]] = stopped[[ paste0('cas_', x) ]]
    sel= (stopped$cas_mode.int!=1 | stopped[[paste0('littlen_cas', x) ]]!=1)
    stopped[[paste0('ped_cas_', x) ]][ sel] = NA
    
    #bysort accident_index: egen ped_cas_`x'=max(ped_cas_`x'_temp)
    vartemp = paste0('ped_cas_', x)
    stopped[[vartemp]][is.na(stopped[[vartemp]])] = 0 #allow grouping for NAs
    stopped.gr  = aggregate(stopped[[vartemp]], by = list(stopped$accident_index), 
                            FUN = max)
    names(stopped.gr) = c('accident_index', paste0('ped_cas_', x,'_max'))
    stopped  = inner_join(stopped, stopped.gr, by= 'accident_index')
    
    stopped[[ paste0('ped_cas_', x) ]] = stopped[[ paste0('ped_cas_', x,'_max') ]] 
    
    #drop littlen_cas`x' ped_cas_`x'_temp
    stopped[[ paste0('ped_cas_', x,'_max')]]  = NULL
    stopped[[paste0('littlen_cas', x)]] = NULL
    
  }
  
  #remove loop components & collect
  rm(stopped.gr, by_stopped)
  # stopped_with_ped_strike.Rds
  saveRDS(stopped, paste0(overflow_path,'processed_injuries_2.Rds'),version=2)   #save for testing
}

######################################################################
## DEFINE LARGEST AND SECOND LARGEST OTHER VEHICLES, TO BECOME STRIKE VEHICLE

if(file.exists(paste0(overflow_path,'processed_injuries_3.Rds'))){
  stopped <- readRDS(paste0(overflow_path,'processed_injuries_3.Rds'))
}else{
  #use stopped1 to merge later
  stopped1 = subset(stopped, select = c(accident_index,veh_mode,veh_mode.int, 
                                        veh_reference, veh_male, veh_age, numped,
                                        ped_cas_male, ped_cas_age ))
  
  # duplicates drop
  stopped1 = stopped1[!duplicated(stopped1),]   
  stopped1$veh_modei = -1 *stopped1$veh_mode.int
  stopped1$veh_modei[stopped1$veh_modei == -99] = 99
  
  set.seed(2011)
  stopped1$random1 = runif(n = nrow(stopped1), min = 0, max = 1)
  
  # by accident_index (veh_modei random1), sort: gen littlen=_n
  by_stopped1 <- stopped1 %>% group_by(accident_index)   # groups by
  stopped1 <- mutate(arrange(stopped1, accident_index, veh_modei, random1),
                     littlen=unlist(lapply(group_size(by_stopped1), FUN= seq_len)))
  
  
  # keep accident_index veh_reference veh_mode veh_male veh_age littlen numped ped_cas_male ped_cas_age  
  stopped1 = subset(x = stopped1, select = c(accident_index, veh_reference, veh_mode, 
                                             veh_male, veh_age, littlen, numped, 
                                             ped_cas_male, ped_cas_age)   )
  
  
  # !! reshape wide veh_reference veh_mode veh_male veh_age, i(accident_index) j(littlen)
  stopped1 = reshape(data = stopped1, v.names = c('veh_reference','veh_mode','veh_male','veh_age'),
                     timevar='littlen' , idvar = c('accident_index'),  direction = "wide")
  
  stopped1$veh_mode.2[is.na(stopped1$veh_mode.2) & stopped1$numped!= 0 ]= 'pedestrian'  # 
  stopped1$veh_mode.2[is.na(stopped1$veh_mode.2)] = 'NOV'
  
  #replace values for age/male second large vehicle
  sel = (stopped1$numped!=0 & stopped1$veh_mode.2 == 'pedestrian')
  stopped1$veh_male.2[sel] = stopped1$ped_cas_male[sel]
  stopped1$veh_age.2[sel] = stopped1$ped_cas_age[sel]
  
  ##RJ
  stopped1 <- subset(stopped1,!duplicated(accident_index))
  for(i in 1:17) stopped1[[paste0('na',i)]] <- stopped1[[paste0('veh_mode.',i)]]!='pedestrian'&!(is.na(stopped1[[paste0('veh_male.',i)]])|is.na(stopped1[[paste0('veh_age.',i)]]))
  na_names <- sapply(1:17,function(x)paste0('na',x))
  na_cols <- match(na_names,names(stopped1))
  stopped1$num_true <- apply(stopped1[,na_cols],1,function(x)sum(x==T))
  stopped1$firstcol <- 1
  stopped1$secondcol <- 2
  stopped1$firstcol[stopped1$num_true==1] <- apply(stopped1[stopped1$num_true==1,na_cols],1,function(x)which(x==T))
  stopped1$secondcol[stopped1$firstcol>1] <- 1
  stopped1$firstcol[stopped1$num_true>1] <- apply(stopped1[stopped1$num_true>1,na_cols],1,function(x)which(x==T)[1])
  stopped1$secondcol[stopped1$num_true>1] <- apply(stopped1[stopped1$num_true>1,na_cols],1,function(x)which(x==T)[2])
  columns <- which(colnames(stopped1)%in%sapply(1:17,function(x)paste0('veh_reference.',x)))
  stopped1$veh_reference_firstlarge <- sapply(1:nrow(stopped1),function(x)stopped1[x,columns[stopped1$firstcol[x]]])
  stopped1$veh_mode_firstlarge <- sapply(1:nrow(stopped1),function(x)stopped1[x,1+columns[stopped1$firstcol[x]]])
  stopped1$veh_male_firstlarge <- sapply(1:nrow(stopped1),function(x)stopped1[x,2+columns[stopped1$firstcol[x]]])
  stopped1$veh_age_firstlarge <- sapply(1:nrow(stopped1),function(x)stopped1[x,3+columns[stopped1$firstcol[x]]])
  stopped1$veh_reference_secondlarge <- sapply(1:nrow(stopped1),function(x)stopped1[x,columns[stopped1$secondcol[x]]])
  stopped1$veh_mode_secondlarge <- sapply(1:nrow(stopped1),function(x)stopped1[x,1+columns[stopped1$secondcol[x]]])
  stopped1$veh_male_secondlarge <- sapply(1:nrow(stopped1),function(x)stopped1[x,2+columns[stopped1$secondcol[x]]])
  stopped1$veh_age_secondlarge <- sapply(1:nrow(stopped1),function(x)stopped1[x,3+columns[stopped1$secondcol[x]]])
  ##
  
  for (x in c('reference','mode','male','age')) {
    names(stopped1)[names(stopped1)==paste0('veh_',x,'.1')] = paste0('veh_',x,'_firstlarge')
    names(stopped1)[names(stopped1)==paste0('veh_',x,'.2')] = paste0('veh_',x,'_secondlarge')
  }
  
  stopped1 = subset(stopped1, select =c(accident_index, veh_reference_firstlarge, veh_reference_secondlarge,
                                        veh_mode_firstlarge, veh_mode_secondlarge, veh_male_firstlarge,
                                        veh_male_secondlarge, veh_age_firstlarge, veh_age_secondlarge)   )
  
  
  #MERGE IN AND DEFINE STRIKE MODE - FOR NON-PEDESTRIANS, THIS IS LARGEST OTHER VEHICLE
  stopped = inner_join(stopped, stopped1, by="accident_index")
  rm(stopped1)
  
  # prepare vars for loop
  stopped= stopped[! is.na(stopped$cas_severity),  ]   # delete undefined severity
  stopped$veh_mode_firstlarge = as.character(stopped$veh_mode_firstlarge)
  stopped$veh_mode_secondlarge = as.character(stopped$veh_mode_secondlarge)
  
  #output: 3 strike* vars w. integers categories
  for (x in c('reference', 'mode','male','age')) {
    
    stopped[[paste0('strike_', x) ]]= NA   #creates the vars (char type imposed by factors treatment)
    
    sel= (stopped$cas_mode.int==1)
    stopped[[paste0('strike_', x) ]][sel] = stopped[[paste0('veh_', x) ]][sel]    # 1 if cas_mode.int==1, 0 otherwise
    
    
    sel= (stopped$cas_mode.int!=1) 
    stopped[[paste0('strike_', x) ]][sel] = stopped[[paste0('veh_', x,'_firstlarge') ]][sel]     
    
    sel= (stopped$veh_reference== stopped$veh_reference_firstlarge  & stopped$cas_mode.int!= 1)
    stopped[[paste0('strike_', x) ]][ sel ] = stopped[[paste0('veh_', x, '_secondlarge') ]][sel]
    
  }  
  
  #recode as integers
  stopped$strike_male = as.numeric(stopped$strike_male)
  stopped$strike_age = as.numeric(stopped$strike_age)
  stopped$strike_mode.int = recode(stopped$strike_mode, "pedestrian"='1', "cyclist"='2' , "motorcycle"='3',
                                   "car/taxi"='4', "light goods"='5', "bus"='6',
                                   "heavy goods"='7', "NOV"= '8',
                                   "other or unknown" = '99')
  
  #IMPUTE AT RANDOM MISSING SEX OF A) CASUALTY AND B) STRIKER, 
  # IN PROPORTION TO OBSERVED SEX RATIO OF STRIKER COLLISIONS FOR EACH MODE [not done for age]
  set.seed(2012)
  stopped$random3 = runif(n = nrow(stopped), min = 0, max = 1)
  table(stopped$cas_male, useNA = "always")   # missing data 0.2% casualty sex
  
  table(stopped$strike_male[stopped$strike_mode.int!=8] , useNA = "always" )  # missing data 6.5% striker sex
  
  
  #to allow means to work operates on the .int variable
  #for (x in c('cas', 'strike')) {
  #  stopped[[paste0(x,'_mode_sexratio')]]= NA  #creates vars
    
  #  for (i in c(1:7,99) ) {
  #    sel= (stopped[[paste0(x, '_mode.int')]]==i)
  #    temp.mean = mean( stopped[[paste0(x, '_male') ]][sel], na.rm = T)  #the mean in the summary
  #    stopped[[ paste0(x, '_mode_sexratio')]][sel]=  temp.mean
  #  }
  #}
  
  #sel= is.na(stopped$cas_male) & (stopped$random3 <= stopped$cas_mode_sexratio) & !is.na(stopped$cas_severity) 
  #stopped$cas_male[ sel ] = 1 
  
  #sel= is.na(stopped$cas_male) & (stopped$random3 > stopped$cas_mode_sexratio) & !is.na(stopped$cas_severity)
  #stopped$cas_male[ sel ] = 0
  
  #sel= is.na(stopped$strike_male) & (stopped$random3 <= stopped$cas_mode_sexratio) & stopped$strike_mode != 8
  #stopped$strike_male[ sel ] = 1 
  
  #sel= is.na(stopped$strike_male) & (stopped$random3 > stopped$cas_mode_sexratio) & stopped$strike_mode != 8
  #stopped$strike_male[ sel ] = 0
  
  #SAVE
  stopped = arrange(stopped, accident_index, year, roadtype, cas_severity, cas_mode,
                    cas_male, cas_age, strike_mode, strike_male, strike_age,local_authority_.highway.)
  
  #save a range of columns
  # ncol1= which(names(stopped)=='accident_index')
  # ncol2= which(names(stopped)=='strike_age')
  # stopped = stopped [, c(ncol1:ncol2) ]
  
  cols =c('accident_index', 'number_of_vehicles', "casualty_reference", "veh_reference",
          'strike_reference', 'year','urban_or_rural_area', 'roadtype', 'cas_severity', 'cas_mode', 'cas_mode.int',
          'cas_male','cas_age', 'strike_mode', 'strike_mode.int', 'strike_male', 'strike_age','veh_reference',"local_authority_.highway.") 
  stopped <- stopped[, cols]
  stopped$region <- sapply(stopped$local_authority_.highway.,function(x)names(codes_for_stats19)[sapply(codes_for_stats19,function(y)x%in%y)])
  saveRDS(stopped, paste0(overflow_path,'processed_injuries_3.Rds'),version=2)  # input for ITHIM conversion
  
  saveRDS(stopped,paste0(overflow_path,"stats19_29.Rds"),version=2)
}

######################################################################

nrow(subset(stopped,cas_severity=='Fatal'))
sapply(c('Fatal','Serious','Slight'),function(y)sapply(names(codes_for_stats19),function(x)nrow(subset(stopped,cas_severity==y&region==x))))

hr <- readRDS(paste0(overflow_path,'hitrun.rds'))
colnames(hr)[2] <- 'veh_reference'
stopped <- left_join(stopped,hr[hr$accident_index%in%stopped$accident_index,],by=c('veh_reference','accident_index'))
severity_tab <- sapply(c('Fatal','Serious','Slight'),function(y)sapply(names(codes_for_stats19),function(x)nrow(subset(stopped,cas_severity==y&region==x))))
sapply(c(1,2,3),function(y)sapply(names(codes_for_stats19),function(x)nrow(subset(stopped,urban_or_rural_area==y&region==x))))
hit_run_tab <- sapply(c(0,1),function(y)sapply(names(codes_for_stats19),function(x)nrow(subset(stopped,hitrun==y&region==x))))
roadtype_tab <- sapply(c('Motorway/A(M)','A','B, C, Unclassified'),function(y)sapply(names(codes_for_stats19),function(x)nrow(subset(stopped,roadtype==y&region==x))))
cas_mode_tab <- sapply(c('pedestrian','cyclist','car/taxi','motorcycle'),function(y)sapply(names(codes_for_stats19),function(x)nrow(subset(stopped,cas_mode==y&region==x))))
strike_mode_tab <- sapply(c('pedestrian','cyclist','car/taxi','motorcycle','bus','heavy goods','light goods','NOV'),function(y)sapply(names(codes_for_stats19),function(x)nrow(subset(stopped,strike_mode==y&region==x))))
year_tab <- sapply(2005:2017,function(y)sapply(names(codes_for_stats19),function(x)nrow(subset(stopped,year==y&region==x))))
cas_male_tab <- sapply(c(0,1),function(y)sapply(names(codes_for_stats19),function(x)nrow(subset(stopped,cas_male==y&region==x))))
strike_male_tab <- sapply(c(0,1),function(y)sapply(names(codes_for_stats19),function(x)nrow(subset(stopped,strike_male==y&region==x))))
cas_age_tab <- sapply(0:100,function(y)sapply(names(codes_for_stats19),function(x)nrow(subset(stopped,cas_age==y&region==x))))
strike_age_tab <- sapply(0:100,function(y)sapply(names(codes_for_stats19),function(x)nrow(subset(stopped,strike_age==y&region==x))))

library(pracma)
cols <- rainbow(length(codes_for_stats19))
par(mar=c(5,5,2,2)); matplot(t(repmat(c(0:100),nrow(cas_age_tab),1)),t(cas_age_tab),lwd=2,typ='l',lty=1,frame=F,xlab='Casualty age',ylab='Count',cex.axis=1.5,cex.lab=1.5,col=cols)
legend(legend=rev(names(codes_for_stats19)),col=rev(cols),x=60,y=8000,bty='n',lwd=2)
par(mar=c(5,5,2,2)); matplot(t(repmat(c(0:100),nrow(strike_age_tab),1)),t(strike_age_tab),lwd=2,typ='l',lty=1,frame=F,xlab='Strike age',ylab='Count',cex.axis=1.5,cex.lab=1.5,col=cols)
legend(legend=rev(names(codes_for_stats19)),col=rev(cols),x=60,y=7000,bty='n',lwd=2)
par(mar=c(5,5,2,2)); matplot(t(repmat(c(2005:2017),nrow(year_tab),1)),t(year_tab),lwd=2,typ='l',lty=1,frame=F,xlab='Year',ylab='Count',cex.axis=1.5,cex.lab=1.5,col=cols)
legend(legend=rev(names(codes_for_stats19)),col=rev(cols),x=2011,y=24000,bty='n',lwd=2)
par(mar=c(9,5,2,2)); barplot(hit_run_tab[,2]/rowSums(hit_run_tab)*100,beside=T,col=cols,xlab='',ylab='Percent hit and run',cex.axis=1.5,cex.lab=1.5,las=2)
par(mar=c(5,5,2,2)); barplot(roadtype_tab/t(repmat(rowSums(roadtype_tab),3,1))*100,beside=T,col=cols,xlab='Road type',names.arg = c('Motorway/A(M)','A','B, C, Unclassified'),ylab='Percent',cex.axis=1.5,cex.lab=1.5)
par(mar=c(5,5,2,2)); barplot(cas_mode_tab/t(repmat(rowSums(cas_mode_tab),4,1))*100,beside=T,col=cols,xlab='Casualty mode',names.arg=c('pedestrian','cyclist','car/taxi','motorcycle'),ylab='Percent',cex.axis=1.5,cex.lab=1.5)
par(mar=c(9,5,2,2)); barplot(cas_male_tab[,2]/rowSums(cas_male_tab)*100,beside=T,col=cols,xlab='',ylab='Percent male casualties',cex.axis=1.5,cex.lab=1.5,las=2)
par(mar=c(9,5,2,2)); barplot(strike_male_tab[,2]/rowSums(strike_male_tab)*100,beside=T,col=cols,xlab='',ylab='Percent male strikers',cex.axis=1.5,cex.lab=1.5,las=2)
par(mar=c(6,6,2,2)); barplot(strike_mode_tab/t(repmat(rowSums(strike_mode_tab),8,1))*100,beside=T,col=cols,ylab='Percent',names.arg=c('pedestrian','cyclist','car/taxi','motorcycle','bus','heavy goods','light goods','NOV'),cex.axis=1.5,cex.lab=1.5,las=2)
par(mar=c(5,5,2,2)); barplot(severity_tab/t(repmat(rowSums(severity_tab),3,1))*100,beside=T,col=cols,xlab='Casualty severity',names.arg = c('Fatal','Serious','Slight'),ylab='Percent',cex.axis=1.5,cex.lab=1.5)
par(mar=c(9,6,2,2)); barplot(severity_tab[,1],beside=T,col=cols,xlab='',cex.axis=1.5,cex.lab=1.5,las=2); mtext(side=2,'Fatalities',line=4,cex=1.5)

strike_age_hitrun_tab <- sapply(0:100,function(y)sapply(names(codes_for_stats19),function(x)nrow(subset(stopped,strike_age==y&region==x&hitrun==0))))
par(mar=c(5,5,2,2)); matplot(t(repmat(c(0:100),nrow(strike_age_hitrun_tab),1)),t(strike_age_hitrun_tab),lwd=2,typ='l',lty=1,frame=F,xlab='Strike age',ylab='Count',cex.axis=1.5,cex.lab=1.5,col=cols)
legend(legend=rev(names(codes_for_stats19)),col=rev(cols),x=60,y=7000,bty='n',lwd=2)



excluded_injuries <- list()
excluded_injuries$child_casualty <- subset(stopped,cas_age<16)
excluded_injuries$child_striker <- subset(stopped,strike_age<16)
excluded_injuries$van_cas <- subset(stopped,cas_mode=='light goods')
excluded_injuries$hgv_cas <- subset(stopped,cas_mode=='heavy goods')
unique(stopped$cas_mode)
lapply(excluded_injuries,nrow)

## omit all unknown urban/rural: 21 slight injuries in 2005 in liverpool and sheffield.
subset(stopped,urban_or_rural_area==3)

## 141 NA cas_male. 5 serious, 2007--2010, nottingham and north east.
subset(stopped,is.na(cas_male)&cas_severity!='Slight')

## 29731 NA cas_age. 6 fatal (Bristol/London, 2005--2007), 2545 serious, 27180 slight.
subset(stopped,is.na(cas_age)&cas_severity=='Fatal')

## greater manchester has half the rate of NA strike age and twice the rate of 30 year olds
sapply(names(city_regions),function(x) nrow(subset(stopped,region==x&is.na(strike_age)))/nrow(subset(stopped,region==x)))
sum(is.na(stopped$strike_age))
sum(is.na(stopped$strike_male))

## omit incomplete rows and re-use as reporting rate?

## use hit run to impute ages and genders of strikers?
## edit hitrun: peds do not hit and run, and cyclists only hit peds?
stopped$hitrun[stopped$strike_mode%in%c('cyclist','pedestrian')&stopped$cas_mode!='pedestrian'] <- 0
# hit and run with no other vehicle must be 'other or unknown'?
stopped$strike_mode[stopped$strike_mode=='NOV'&stopped$hitrun==1] <- 'other or unknown'
hitrun <- sapply(0:1,function(x)c(nrow(subset(stopped,is.na(strike_age)&hitrun==x)),nrow(subset(stopped,!is.na(strike_age)&hitrun==x))))
rownames(hitrun) <- c('na','age')
colnames(hitrun) <- c('not hr','hr')
sapply(unique(stopped$strike_mode),function(x)sum(stopped$strike_mode==x))/nrow(stopped)
sapply(unique(stopped$strike_mode),function(x)sum(stopped$strike_mode==x&stopped$hitrun==0,na.rm=T))/sum(stopped$hitrun==0,na.rm=T)
sapply(unique(stopped$strike_mode),function(x)sum(stopped$strike_mode==x&stopped$hitrun==1,na.rm=T))/sum(stopped$hitrun==1,na.rm=T)


######################################################################

if(file.exists(paste0(overflow_path,'processed_injuries_6_tabulated_injuries.Rds'))){
  injury_table <- readRDS(paste0(overflow_path,'processed_injuries_6_tabulated_injuries.Rds'))
}else{
  ## omit unknown urban/rural (12, slight, 2005)
  stopped <- subset(stopped,urban_or_rural_area!=3)
  
  ## assign road types
  stopped$road <- 'motorway'
  stopped$road[stopped$roadtype=='A'&stopped$urban_or_rural_area==1] <- 'urban_A'
  stopped$road[stopped$roadtype=='A'&stopped$urban_or_rural_area==2] <- 'rural_A'
  stopped$road[stopped$roadtype=='B, C, Unclassified'&stopped$urban_or_rural_area==1] <- 'urban_B'
  stopped$road[stopped$roadtype=='B, C, Unclassified'&stopped$urban_or_rural_area==2] <- 'rural_B'
  
  ## divide into primary and secondary, whw and noov
  
  injury_long <- injury_table <- count_nas <- list()
  for(type in c('primary','secondary')) injury_long[[type]] <- injury_table[[type]] <- count_nas[[type]] <- list()
  injury_long$primary$whw <- subset(stopped,cas_mode%in%model_modes&strike_mode%in%model_modes)
  injury_long$primary$noov <- subset(stopped,cas_mode%in%model_modes&!strike_mode%in%model_modes)
  injury_long$secondary$whw <- subset(stopped,!cas_mode%in%model_modes&strike_mode%in%model_modes)
  injury_long$secondary$noov <- subset(stopped,!cas_mode%in%model_modes&!strike_mode%in%model_modes)
  lapply(injury_long,function(x) lapply(x,function(y)list(unique(y$cas_mode),unique(y$strike_mode))))
  lapply(injury_long,function(x) lapply(x,function(y)c(sum(y$cas_severity=='Fatal'),nrow(y))))
  
  
  
  ## remove NAs for casualty
  cas_na <- lapply(injury_long$primary,function(x)subset(x,is.na(cas_age)|is.na(cas_male)))
  number_na_cas_removed <- lapply(cas_na,function(y)sapply(c('Fatal','Serious','Slight'),function(x)sum(y$cas_severity==x)))
  injury_long$primary <- lapply(injury_long$primary,function(x)subset(x,!is.na(cas_age)&!is.na(cas_male)))
  
  ## remove NAs for striker
  strike_na <- list()
  strike_na$primary <- subset(injury_long$primary$whw,is.na(strike_male)|is.na(strike_age))
  strike_na$secondary <- subset(injury_long$secondary$whw,is.na(strike_male)|is.na(strike_age))
  number_na_strike_removed <- lapply(strike_na,function(y)sapply(c('Fatal','Serious','Slight'),function(x)sum(y$cas_severity==x)))
  injury_long$primary$whw <- subset(injury_long$primary$whw,!is.na(strike_male)&!is.na(strike_age))
  injury_long$secondary$whw <- subset(injury_long$secondary$whw,!is.na(strike_male)&!is.na(strike_age))
  
  ## store removed entries and reuse for reporting rates
  saveRDS(list(cas_na,strike_na),paste0(overflow_path,'processed_injuries_4_removed_NAs.Rds'),version=2)
  
  ## tabulate na
  count_nas <- group_by(rbind(do.call(rbind,cas_na),do.call(rbind,strike_na)),cas_severity,cas_mode,strike_mode,road) %>% 
    summarise(nas=dplyr::n()) %>% 
    droplevels() %>% 
    as.data.frame() %>%    # remove "grouped" class, which breaks filling with zeroes
    complete(cas_severity,cas_mode,strike_mode,road,fill=list(nas=0)) 
  totals <- group_by(stopped,cas_severity,cas_mode,strike_mode,road) %>% 
    summarise(total=dplyr::n()) %>% 
    droplevels() %>% 
    as.data.frame() %>%    # remove "grouped" class, which breaks filling with zeroes
    complete(cas_severity,cas_mode,strike_mode,road,fill=list(total=0)) 
  count_nas <- left_join(count_nas,totals,by=c('cas_severity','cas_mode','strike_mode','road'))
  
  logmod <- glm(I(nas/total)~cas_severity+cas_mode+strike_mode+road,weights=total,data=subset(count_nas,total>0),family=binomial)
  count_nas$rate <- 1-predict(logmod,newdata=mutate(count_nas,total=1),type='response')
  saveRDS(count_nas,paste0(overflow_path,'processed_injuries_5_NA_table_for_reporting_rate.Rds'),version=2)
  
  # remove underage events
  for(j in 1:length(injury_long)) 
    injury_long[[j]]$whw <- subset(injury_long[[j]]$whw,strike_age > 15)
  for(j in 1:2) 
    injury_long[[1]][[j]] <- subset(injury_long[[1]][[j]],cas_age > 15)
  
  ## assign age groups
  age_label <- 'strike_age'
  for(j in 1:length(injury_long)) {
    for(i in 2:length(age_lower_bounds)-1){
      injury_long[[j]]$whw$strike_age_cat[injury_long[[j]]$whw[[age_label]] >= age_lower_bounds[i] & injury_long[[j]]$whw[[age_label]] < age_lower_bounds[i+1]] <- age_category[i]
    }
    injury_long[[j]]$whw$strike_age_cat[injury_long[[j]]$whw[[age_label]] >= age_lower_bounds[length(age_lower_bounds)]] <- age_category[length(age_lower_bounds)]
  }
  age_label <- 'cas_age'
  for(j in 1:2){
    for(i in 2:length(age_lower_bounds)-1){
      injury_long[[1]][[j]]$cas_age_cat[injury_long[[1]][[j]][[age_label]] >= age_lower_bounds[i] & injury_long[[1]][[j]][[age_label]] < age_lower_bounds[i+1]] <- age_category[i]
    }
    injury_long[[1]][[j]]$cas_age_cat[injury_long[[1]][[j]][[age_label]] >= age_lower_bounds[length(age_lower_bounds)]] <- age_category[length(age_lower_bounds)]
  }
  
  ## tabulate
  injury_table$primary$whw <-
    group_by(injury_long$primary$whw,year,cas_male,cas_severity,cas_mode,strike_mode,cas_age_cat,road,region,strike_age_cat,strike_male) %>% 
    summarise(count=dplyr::n()) %>% 
    droplevels() %>% 
    as.data.frame() %>%    # remove "grouped" class, which breaks filling with zeroes
    complete(year,cas_male,cas_severity,cas_mode,strike_mode,cas_age_cat,road,region,strike_age_cat,strike_male,fill=list(count=0)) 
  
  injury_table$primary$noov <-
    group_by(injury_long$primary$noov,year,cas_male,cas_severity,cas_mode,strike_mode,cas_age_cat,road,region) %>% 
    summarise(count=dplyr::n()) %>% 
    droplevels() %>% 
    as.data.frame() %>%    # remove "grouped" class, which breaks filling with zeroes
    complete(year,cas_male,cas_severity,cas_mode,strike_mode,cas_age_cat,road,region,fill=list(count=0)) 
  
  injury_table$secondary$whw <-
    group_by(injury_long$secondary$whw,year,cas_severity,cas_mode,strike_mode,road,region,strike_age_cat,strike_male) %>% 
    summarise(count=dplyr::n()) %>% 
    droplevels() %>% 
    as.data.frame() %>%    # remove "grouped" class, which breaks filling with zeroes
    complete(year,cas_severity,cas_mode,strike_mode,road,region,strike_age_cat,strike_male,fill=list(count=0)) 
  
  injury_table$secondary$noov <-
    group_by(injury_long$secondary$noov,year,cas_severity,cas_mode,strike_mode,road,region) %>% 
    summarise(count=dplyr::n()) %>% 
    droplevels() %>% 
    as.data.frame() %>%    # remove "grouped" class, which breaks filling with zeroes
    complete(year,cas_severity,cas_mode,strike_mode,road,region,fill=list(count=0)) 
  
  for(i in 1:2) for(j in 1:2) injury_table[[i]][[j]]$year <- as.numeric(injury_table[[i]][[j]]$year)
  
  lapply(injury_table,function(x) lapply(x,function(y)c(sum(subset(y,cas_severity=='Fatal')$count),sum(y$count))))
  
  for(i in 1:2) 
    for(j in 1:2) 
      injury_table[[i]][[j]] <- left_join(injury_table[[i]][[j]],count_nas[,names(count_nas)%in%c('cas_severity','cas_mode','strike_mode','road','rate')],by=c('cas_severity','cas_mode','strike_mode','road'))
  
  for(i in 1:2) 
    for(j in 1:2) 
      injury_table[[i]][[j]] <- subset(injury_table[[i]][[j]],!(road=='motorway'&(strike_mode%in%c('cyclist','pedestrian')|cas_mode%in%c('cyclist','pedestrian'))))
  
  saveRDS(injury_table,paste0(overflow_path,'processed_injuries_6_tabulated_injuries.Rds'),version=2)
}
######################################################################



