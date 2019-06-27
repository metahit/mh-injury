
library(xlsx)
library(tidyverse)
library(readODS)

## la codes
lacodes <- read.ods(paste0(overflow_path,'UKLAcodes.ods'))[[1]][-1,]

## populations
pop_la <- read.ods(paste0(overflow_path,'populationUK.ods'))[[2]][-c(1:15),]
pop_la <- pop_la[,-5]
names(pop_la) <- c('code','region','ua','la','number','area','density')
pop_la$number<-as.numeric(gsub(',','',pop_la$number))
pop_la <- drop_na(pop_la)

## road travel distances
raw_la <- read.xlsx(paste0(overflow_path,'VehicleType_LALevel.xlsx'),sheetIndex=1,rowIndex=6:1670,colClasses=c('numeric',rep('character',2),rep('numeric',7)))


## city regions
city_regions <- list()

city_regions$bristol <- c('Bath and North East Somerset', 'Bristol', 'North Somerset', 'South Gloucestershire')
city_regions$nottingham <- c('Ashfield', 'Bassetlaw', 'Broxtowe', 'Gedling', 'Mansfield', 'Nottingham', 'Newark and Sherwood', 'Rushcliffe')
city_regions$sheffield <- c('Barnsley', 'Doncaster', 'Rotherham', 'Sheffield')
city_regions$liverpool <- c('Halton', 'Knowsley', 'Liverpool', 'St. Helens', 'Sefton', 'Wirral')
city_regions$northeast <- c('County Durham', 'Gateshead', 'Newcastle upon Tyne', 'North Tyneside', 'Northumberland', 'South Tyneside', 'Sunderland')
city_regions$greatermanchester <- c('Bolton', 'Bury', 'Manchester', 'Oldham', 'Rochdale', 'Salford', 'Stockport', 'Tameside', 'Trafford','Wigan')
city_regions$westyorkshire <- c('Bradford', 'Calderdale', 'Kirklees','Leeds', 'Wakefield')
city_regions$westmidlands <- c('Birmingham', 'Coventry', 'Dudley','Sandwell', 'Solihull', 'Walsall', 'Wolverhampton')
city_regions$london <- as.character(unique(droplevels(subset(raw_la,Region_Name=='London')$LA_Name)))

lookup_table <- data.frame(la=unlist(city_regions),
                           la_code=sapply(unlist(city_regions),function(x)lacodes$A[which(lacodes$C==x)]),
                           stringsAsFactors = F)
lookup_table$region <- sapply(unlist(city_regions),function(x)names(city_regions)[sapply(city_regions,function(y)x%in%y)])

region_codes <- data.frame(region=c('london','westmidlands','bristol','nottingham','westyorkshire','liverpool','greatermanchester','northeast','sheffield'),
                           region_code=c('E12000007','E11000005',NA,'E10000024','E11000006','E11000002','E11000001','E11000004','E11000003'),
                           stringsAsFactors = F)

lookup_table <- left_join(lookup_table,region_codes,by='region')
lookup_table$stats19_code <- lookup_table$la_code
# nottingham is E06000018
# all other areas in nottinghamshire are E10000024
lookup_table$stats19_code[lookup_table$region=='nottingham'&lookup_table$la!='Nottingham'] <- region_codes$region_code[region_codes$region=='nottingham']

# E11000002 (merseyside) doesn't include Halton (E06000006)
# E11000004 (tyne and wear) doesn't include county durham (E06000047) or northumberland (E06000048)

# check codes are in stats19
#lookup_table$stats19_code%in%stopped$local_authority_.highway.
#lookup_table$la_code[lookup_table$region=='Nottingham'&lookup_table$la!='Nottingham']%in%stopped$local_authority_.highway.


codes_for_stats19 <- lapply(city_regions,function(x)unique(subset(lookup_table,la%in%x)$stats19_code))
saveRDS(codes_for_stats19,paste0(overflow_path,'codes_for_stats19.Rds'),version=2)
saveRDS(lookup_table,paste0(overflow_path,'lookup_table.Rds'),version=2)

