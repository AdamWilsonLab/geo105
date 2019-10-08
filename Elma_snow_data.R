# Elma Snow Data

library(rgdal)
library(sf)

# Anomalies
library(rnoaa)
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(zoo)
library(lubridate)

stations=ghcnd_stations()

d=as.Date("2018-07-01")

stations%>%filter(
  grepl("ELMA CENTER",name),
  grepl("NY",state),
  element=="SNOW",
  last_year>=2017,
)

data=meteo_tidy_ghcnd("US1NYER0075",var=c("SNOW","SNWD"),date_min = as.Date("1940-01-01"))

data_2=data%>%mutate(
  snow=ifelse(is.na(snow),0,snow*.039),
  snwd=ifelse(is.na(snwd),0,snwd*.039),
  day=as.numeric(format(date,"%d")),
  month=as.numeric(format(date,"%m")),
  year=as.numeric(format(date,"%Y")),
  doy=as.numeric(format(date,"%j")),
  dyear=year+(month/12),
  winter=ifelse(month<6,year-1,year),
  fyear=ymd(paste(ifelse(month<6,2018,2017),month,day)),
  season=between(fyear,ymd("20180105"),ymd("20180305")))%>%
  group_by(winter)

firstday<-
  data_2%>%
  filter(snwd>2)%>%
  summarize(first=first(date,order_by = date))

data_winter<- data_2 %>%
  group_by(winter)%>%
  filter(season)%>%
  summarize(
    tdays=n(),
    ndays=sum(snwd>0,na.rm=T),
    pdays=ndays/tdays
)
data_winter 



  
data_winter  

ggplot(data_2,aes(x=fyear))+
  facet_wrap(~winter,ncol=1)+
  xlim(ymd(c("20171101","20180501")))+
  geom_rect(aes(xmin=ymd("20170105"),xmax=ymd("20180305"),ymin=0,ymax=32))+
  geom_area(aes(y=snwd),fill=grey(0.6))+
  geom_line(aes(y=snow),col="red")

    
