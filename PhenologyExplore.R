library(rnpn)
library(tidyverse)

npn_species_id(ids = 3)

# get buffalo id
gid=npn_groups()%>%
  filter(grepl("Buffalo",network_name))%>%
  select(network_id)%>%
  as.numeric()

#UB_station=stations[grep(".*Elicott Complex at UB North Campus*",stations$station_name),]
#stid=UB_station$station_id

#inds=npn_indsatstations(stationid=stid)
#d=npn_allobssp(speciesid=102,stationid=stid,startdate='2019-01-01', enddate='2019-12-31')
#d$data[[match(stid,d$stations$station_id)]]
#d$phenophase
#length(d)

data_ind=npn_download_individual_phenometrics(request_source = "Adam",
                                              years=c('2019'),
                                              network_ids = gid)

d=npn_download_site_phenometrics(
  request_source="Adam",
  years=c('2019'),
  network_ids = gid,
  additional_fields="Plant_Nickname" )%>%
  mutate_if(is_numeric,function(x)ifelse(x==-9999,NA,x))


d=npn_download_magnitude_phenometrics(
  request_source="Adam",
  years=c('2019'),
  network_ids = gid,
  period_frequency = 1,
  additional_fields="Plant_Nickname" )%>%
  mutate_if(is_numeric,function(x)ifelse(x==-9999,NA,x))


library(tidyverse)
library(rnpn)
npn_groups()%>%filter(grepl("Buffalo",network_name))
npn_download_status_data("Testing",
                           years=c('2019'),
                           network_ids=c(891))


ggplot(d,aes(x=as.Date(start_date),
             y=proportion_individuals_with_yes_record))+
         geom_point()+
         facet_grid(phenophase_description~common_name)


df$date <- as.Date(df$date)




library('ggplot2')
ggplot(df, aes(date, count)) +
  geom_line() +
  theme_grey(base_size = 20) +
  facet_grid(.id ~ .)