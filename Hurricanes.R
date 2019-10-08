library(HURDAT)
library(tidyverse)
library(lubridate)
library(sf)
library(ggmap)
library(spData)
library(zoo)
data(world)
data(us_states)

library(rnoaa)
library(ncdf4)



al <- get_hurdat(basin = "AL")#%>%
#  st_as_sf(coords = c("Lon","Lat"))


## Get PDI and SST data from EPA
## https://www.epa.gov/climate-indicators/climate-change-indicators-tropical-cyclone-activity
pdi=read_csv("https://www.epa.gov/sites/production/files/2016-08/cyclones_fig-3.csv",skip=6)%>%
  select(year=Year,
         sst=`Smoothed sea surface temperature`,
         pdi=`Smoothed Power Dissipation Index`)%>%
  na.omit()


## Filter only 'strong' storms
strong<- al%>%
  filter(Wind>113)%>%  # Category 4 hurricane https://www.nhc.noaa.gov/aboutsshws.php
  group_by(year=year(DateTime))%>%
  summarize(strong_hurricane_count=length(unique(Key)))

## Generate combined annual dataset
annual=al%>%
  group_by(year=year(DateTime),
           key=Key)%>%
  summarize(Wind=max(Wind,na.rm=T),  #get storm-level summaries
            Pressure=min(Pressure,na.rm=T))%>%
  group_by(year)%>%
  summarize(wind_max=max(Wind,na.rm=T),  #combine to annual summaries
            pressure_min=min(Pressure,na.rm=T),
            hurricane_count=length(unique(key)))%>%
  left_join(strong,by="year")%>%
  mutate(strong_hurricane_count=ifelse(is.na(strong_hurricane_count),0,strong_hurricane_count),
         hurricane_count=ifelse(is.na(hurricane_count),0,hurricane_count))%>%
  left_join(pdi,by="year")%>%
    filter(year>1950)%>%
  mutate( #5 year rolling averages (to match sst and pdi)
    pressure_min=rollmean(pressure_min,5,align="center",fill = NA,na.rm=T),
    wind_max=rollmean(wind_max,5,align="center",fill = NA,na.rm=T),
    hurricane_count=rollmean(hurricane_count,5,align="center",fill = NA),
    strong_hurricane_count=rollmean(strong_hurricane_count,5,align="center",fill = NA)
  )

annual%>%
  write_csv("output/hurricanes.csv")


# wind_max: 5 year rolling average of maximum recorded wind speed (in knots) in that year.  Source: https://www.nhc.noaa.gov/data/#hurdat
# pressure_min: 5 year rolling average of minimum recorded air pressure (in millibars) in that year.  Source: https://www.nhc.noaa.gov/data/#hurdat
# hurricane_count: 5 year rolling average of the number of hurricanes recorded in that year. Source: https://www.nhc.noaa.gov/data/#hurdat
# strong_hurricane_count: 5 year rolling average of the number of category 4+ hurricanes recorded in that year. Source: https://www.nhc.noaa.gov/data/#hurdat
#  sst - 5 year rolling average of Aug-Oct SST (Degrees )
#  PDI - 5 year rolling average of the Power Dissipation Index (m^3/s^2) from Emanuel, K.A. 2016 update to data originally published in: Emanuel, K.A. 2007. Environmental factors affecting tropical cyclone power dissipation. J. Climate 20(22):5497–5509. https://www.epa.gov/climate-indicators/climate-change-indicators-tropical-cyclone-activity

annuall=gather(annual,var,value,-year)
annual2=gather(annual,var,value,-year,-sst)

ggplot(annuall,aes(x=year,y=value))+
  facet_wrap(~var,scales="free_y",ncol=1)+
  geom_line()+
  geom_smooth(method="lm")



ggplot(annual2,aes(x=sst,y=value))+
  facet_wrap(~var,scales="free_y",ncol=1)+
  geom_point()+
  geom_smooth(method="lm")


summary(lm(pdi~year,data=filter(annual,year>1952)))

t.test(annual$sst,annual$year)
# region=st_bbox(al)
# 
# map1=ggplot() +
#   #  geom_sf(data=world,
#   #          inherit.aes = F,size=.1,
#   #          fill="grey",colour="black")+
#   stat_bin2d(data=al,
#              aes(y=st_coordinates(al)[,2],
#                  x=st_coordinates(al)[,1]),bins=100)+
#   scale_fill_distiller(palette="YlOrRd",
#                        trans="log",
#                        direction=-1,
#                        breaks = c(1,10,100,1000))+
#   coord_sf(ylim=region[c(2,4)],
#            xlim=region[c(1,3)])+
#   labs(x="",y="")
# 
# map1
