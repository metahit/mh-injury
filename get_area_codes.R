
library(xlsx)
library(tidyverse)
library(readODS)

## la table
la_table <- read.csv('../mh-execute/inputs/mh_regions_lad_lookup.csv',stringsAsFactors = F)[2:4]
names(la_table)[1:2] <- c('la','la_code')
city_region_names <- unique(la_table$cityregion)
city_region_names <- city_region_names[city_region_names!='']
city_regions <- lapply(city_region_names,function(x)la_table$la[la_table$cityregion==x])
names(city_regions) <- city_region_names
saveRDS(city_regions,paste0(overflow_path,'city_regions.Rds'),version=2)

region_codes <- data.frame(cityregion=c('london','westmidlands','bristol','nottingham','westyorkshire','liverpool','greatermanchester','northeast','sheffield'),
                           region_code=c('E12000007','E11000005',NA,'E10000024','E11000006','E11000002','E11000001','E11000004','E11000003'),
                           stringsAsFactors = F)

la_table <- left_join(la_table,region_codes,by='cityregion')
la_table$stats19_code <- la_table$la_code
# nottingham is E06000018
# all other areas in nottinghamshire are E10000024
la_table$stats19_code[la_table$cityregion=='nottingham'&la_table$la!='Nottingham'] <- region_codes$region_code[region_codes$cityregion=='nottingham']

# E11000002 (merseyside) doesn't include Halton (E06000006)
# E11000004 (tyne and wear) doesn't include county durham (E06000047) or northumberland (E06000048)

codes_for_stats19 <- lapply(city_regions,function(x)unique(subset(la_table,la%in%x)$stats19_code))
saveRDS(codes_for_stats19,paste0(overflow_path,'codes_for_stats19.Rds'),version=2)
saveRDS(la_table,paste0(overflow_path,'lookup_table.Rds'),version=2)
