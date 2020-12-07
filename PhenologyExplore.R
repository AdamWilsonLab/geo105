library(rnpn)
library(tidyverse)

npn_species_id(ids = 3)

# get buffalo id
gid=npn_groups()%>%
  filter(grepl("Buffalo",network_name))%>%
  select(network_id)
gid

#UB_station=stations[grep(".*Elicott Complex at UB North Campus*",stations$station_name),]
#stid=UB_station$station_id

#inds=npn_indsatstations(stationid=stid)
#d=npn_allobssp(speciesid=102,stationid=stid,startdate='2019-01-01', enddate='2019-12-31')
#d$data[[match(stid,d$stations$station_id)]]
#d$phenophase
#length(d)

data_ind=npn_download_individual_phenometrics(request_source = "Adam",
                                              years=c('2020'),
                                              network_ids = 1047)#gid)


d=npn_download_magnitude_phenometrics(
  request_source="Adam",
  years=c('2020'),
  network_ids = gid,
  period_frequency = 1,
  additional_fields="Plant_Nickname" )%>%
  mutate_if(is_numeric,function(x)ifelse(x==-9999,NA,x))


##############
############## Individual Trees

npn_groups()%>%filter(grepl("Buffalo",network_name))

d=npn_download_status_data("Testing",
                           years=c('2020'),
                           additional_fields=list("Plant_Nickname","Observer_ID"), 
                           network_ids=gid) %>% 
  as.tbl() %>% 
  mutate(tag=as.numeric(substr(plant_nickname,1,3)))

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


### Site level analysis
npn_download_individual_phenometrics(
  request_source="Adam",
  years=c('2019'),
  network_ids = gid,
  additional_fields="Plant_Nickname" )%>%
  mutate_if(is_numeric,function(x)ifelse(x==-9999,NA,x))


d_site=npn_download_site_phenometrics(
  request_source="Adam",
  years=c('2019'),
  network_ids = gid,
  additional_fields="Plant_Nickname" )%>%
  mutate_if(is_numeric,function(x)ifelse(x==-9999,NA,x))


d_sugarmaple=npn_download_magnitude_phenometrics(
  request_source="Adam",
  species_ids = "61",
  phenophase_ids = "483",
  years=c(2013:2019),
  period_frequency = 7,
  climate_data = T,
  additional_fields="Plant_Nickname" )%>%
  mutate_if(is_numeric,function(x)ifelse(x==-9999,NA,x))

ggplot(d_sugarmaple,aes(x=as.Date(start_date),y=proportion_yes_records))+
  geom_point()+
  geom_smooth(span=.1,n=1000)+
  xlab("Date")+
  ylab("Proportion of 'yes' observations for Leaves")+
  ylim(0,1)+
  ggtitle("Seasonal Leaf Cover Variability in Sugar Maple",subtitle = "Data from the National Phenology Network")
  

