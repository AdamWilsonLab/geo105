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


##############
############## Individual Trees

npn_groups()%>%filter(grepl("Buffalo",network_name))

d=npn_download_status_data("Testing",
                           years=c('2019'),
                           network_ids=c(891))
d_filtered=d%>%
  rename("a"="intensity_value") %>% 
  mutate(
    date=as.Date(observation_date),
    intensity=case_when(
        a == "Less than 5%" ~ 2.5,
        a == "Less than 25%" ~ 20,
        a == "5-24%" ~ 14.5,
        a == "25-49%" ~ 37,
        a == "50-74%" ~ 62,
        a == "75-94%" ~ 84.5,
        a == "95% or more"  ~ 97.5,
        a == "Less than 3"  ~ 2,  
        a == "3 to 10" ~ 6.5,
        a == "11 to 100" ~ 50,
        a == "101 to 1,000" ~ 500,
        a == "1,001 to 10,000" ~ 5000,
        a == "Little" ~ 5,
        a == "Some" ~ 3,  
        TRUE ~ as.numeric(NA)
      ))%>%
filter(phenophase_description%in%c("Leaves","Colored leaves"))
    
unique(d$phenophase_description)

d_filtered %>% 
  ggplot(aes(x=date,
             y=intensity,
             group=phenophase_description,
             col=phenophase_description))+
         geom_point()+
         facet_wrap(~common_name)+
  ylim(0,100)+
  geom_smooth(span=3)
