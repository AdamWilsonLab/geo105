# Elma Snow Data

library(rgdal)
library(sf)
library(scales)

# Anomalies
library(rnoaa)
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(zoo)
library(lubridate)

d=as.Date("2018-07-01")

if(F){
stations=ghcnd_stations()

stations%>%filter(
  grepl("ELMA CENTER",name),
  grepl("NY",state),
  element=="SNOW",
  last_year>=2017,
)

stations%>%filter(
  grepl("BUFFALO",name),
  grepl("NY",state),
  element=="SNOW",
  last_year>=2017,
)

}


data=meteo_tidy_ghcnd(#"US1NYER0075", #elma
                      "USW00014733", #Buffalo
                      var=c("SNOW","SNWD"),
                      date_min = as.Date("1900-01-01")
                      )

data_2=data%>%
  mutate(
  snow=ifelse(is.na(snow),0,snow*.039),
  snwd=ifelse(is.na(snwd),0,snwd*.039),
  day=as.numeric(format(date,"%d")),
  month=as.numeric(format(date,"%m")),
  year=as.numeric(format(date,"%Y")),
  doy=as.numeric(format(date,"%j")),
  dyear=year+(month/12),
  winter=ifelse(month<6,year-1,year),
  fyear=ymd(paste(ifelse(month<6,2018,2017),month,day)),
  season=between(fyear,ymd("20180105"),ymd("20180305")),
  decade=floor(winter/10)*10,
  era=cut(winter,breaks = c(1900,2000,2020)))%>%
#  filter(winter>2009) %>% 
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
data_2 %>% 
  filter(winter>2009) %>% 
  ggplot(aes(x=fyear))+
  facet_wrap(~winter,ncol=1)+
  xlim(ymd(c("20171101","20180501")))+
  geom_rect(aes(xmin=ymd("20170105"),
                xmax=ymd("20180305"),ymin=0,ymax=32))+
  geom_area(aes(y=snwd),fill=grey(0.6))+
  geom_line(aes(y=snow),col="red")

### Cumulative Snowfall
sum_snow <- data_2 %>% 
  group_by(winter) %>% 
  arrange(dyear) %>% 
  mutate(snowfall=cumsum(snow),
         maxsnow=max(snowfall))

sum_snow %>% 
  ggplot(aes(x=fyear,y=snowfall,color=winter,
             group=as.factor(winter)))+
  geom_line()+
  geom_line(data=filter(sum_snow,winter==2019),col="black",size=2)+
  geom_text(aes(label=winter,x=ymd("20170401"),y=maxsnow))+
  scale_color_viridis_c(name="Year")+
    scale_x_date(name = "Date",
               limits = ymd(c("20171015","20180501")),
               breaks='1 month',
               labels = date_format("%b"))+
    geom_smooth(aes(group=1),col="red")+
  ylab("Cumulative Snowfall (cm)")


### Mean Snowdepth
mean_snowd <- data_2 %>% 
  group_by(winter) %>% 
  arrange(dyear) %>% 
  mutate(snowfall=rollmean(snwd,k = 5,fill = NA))

mean_snowd %>% 
  ggplot(aes(x=fyear,y=snowfall,color=winter,
             group=as.factor(winter)))+
  geom_line()+
  geom_line(data=filter(mean_snowd,winter==2019),
            col="black",size=2)+
  scale_color_viridis_c(name="Year")+
  xlim(ymd(c("20171015","20180501")))+
  geom_smooth(aes(color=decade,group=decade),span=.1,col="red")+
  ylab("Mean Snow Depth (cm)")


### Skiable days
### 
ski_depth=4

data_2 %>% 
  group_by(winter, era) %>% 
  arrange(dyear) %>% 
  mutate(ski_day=snwd>ski_depth,
         ski_day_smooth=rollmean(ski_day,k = 3,fill = NA)) %>% 
  group_by(fyear, era) %>% 
  summarize(ski_day=mean(ski_day_smooth)) %>% 
  ggplot(aes(x=fyear,y=ski_day*100,color=era, group=era))+
  geom_point()+
  geom_smooth(span=.5)+
  ylab("Proportion Skiable Days (%)")+
  scale_x_date(name = "Date",
               limits = ymd(c("20171015","20180501")),
               breaks='1 month',
               labels = date_format("%b"))+
  ggtitle("Proportion Skiable Days in Elma, NY",
          subtitle = paste("% Days with Snowdepth >",
                           ski_depth,"cm over ",
                           paste(range(data_2$year),
                                 collapse="-")))
  

