# Anomalies
library(rnoaa)
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(zoo)

if(F){
stations=ghcnd_stations()
d=as.Date("2018-01-01")
stations%>%filter(
  grepl("BUFFALO",name),
  grepl("NY",state),
  last_year>=2018,
  first_year<2000,
  element=="TMIN")
}

data=meteo_tidy_ghcnd("USW00014733",var=c("TMIN","TMAX"),date_min = as.Date("1940-01-01"))

data=data%>%mutate(
  tmin=as.numeric(tmin)/10,
  tmax=as.numeric(tmax)/10,
  tave=(tmin+tmax)/2,
  month=as.numeric(format(date,"%m")),
  year=as.numeric(format(date,"%Y")),
  doy=as.numeric(format(date,"%j")),
  dyear=year+(month/12))%>%
  arrange(date)


## Daily anomolies
data_day=data%>%
  filter(between(date,left=as.Date("1950-01-01"),right=as.Date("1985-12-31")))%>%
  group_by(doy)%>%
  summarize(tmin_mean=mean(tmin,na.rm=T))%>%
  mutate(tmin_rollmean=rollmean(tmin_mean,k=21,fill=NA))%>%
  unique()

data_daya=left_join(data,data_day,by="doy")%>%
  mutate(anom=tmin-tmin_mean,
         col=anom>0)

data_daya%>%filter(year>=2017)%>%
  ggplot(aes(y=tmin,x=date))+
  geom_linerange(aes(ymax=tmin,ymin=tmin_mean,col=col))+
  geom_line(aes(y=tmin_mean))

#####################
## Monthly anomolies
data_month=data%>%
  group_by(month,year)%>%
  summarize(tmin_ymonmean=mean(tmin,na.rm=T),
            tmax_ymonmean=mean(tmax,na.rm=T))

data_montha=data_month%>%
  group_by(month)%>%
  summarize(tmin_monmean=mean(tmin_ymonmean,na.rm=T),
            tmax_monmean=mean(tmax_ymonmean,na.rm=T))%>%
  left_join(y=data_month,by="month")%>%
  mutate(tmin_anom=tmin_ymonmean-tmin_monmean,
         tmax_anom=tmax_ymonmean-tmax_monmean,
         date=as.Date(paste(year,month,15,sep="-")))%>%
  arrange(year,month)

####################
## Annual Anomalies
data_year=data%>%
  group_by(year)%>%
  summarize(tmin=mean(tmin,na.rm=T))%>%
  ungroup()%>%
  mutate(tmin_year=mean(tmin,na.rm=T),
         anom=tmin-tmin_year)
  

## Write csv for import to google sheets
data_daya%>%
  filter(year==2017&month==12)%>%
  dplyr::transmute(date,tmin,tmin_mean=round(tmin_mean,1))%>%
  write.csv(file="output/anom_daily.csv",row.names=F)

data_montha%>%
  filter(year==2017)%>%
  dplyr::transmute(year=year,month=month,date=date,tmin=round(tmin_ymonmean,1),tmin_mean=round(tmin_monmean,1))%>%
  write.csv(file="output/anom_monthly.csv",row.names=F)

data_year%>%
  dplyr::transmute(year=year,tmin=round(tmin,1),tmin_mean=round(tmin_year,1))%>%
  write.csv(file="output/anom_annual.csv",row.names=F)


data_montha%>%
  filter(month==12)%>%
  ggplot(aes(x=date,y=tmin_ymonmean))+
  geom_line(col="red")+
  geom_smooth(method = "lm", se=FALSE, color="black", formula = "y~x") +
  guides(color="legend",position="top")+
  stat_poly_eq(formula = "y~x", 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
               parse = TRUE,size=5)+
  stat_fit_glance(method = "lm",
                  method.args = list(formula = "y~x"),
                  geom = 'text',
                  aes(label = paste0("P-value = ", sprintf("%5f",signif(..p.value.., digits = 6)))),
                  label.x.npc = 'left', label.y.npc = 0.87, size = 5)+
  labs(x="Temperature (F)",
       y="Year",
       title="Mean January Minimum Temperature 1940-2017\n Buffalo Niagara Airport, NY",
       caption="Data from http://www.nws.noaa.gov/climate/index.php?wfo=buf")


ptrend=data_year%>%
  filter(year<2017)%>%
ggplot(aes(x=year,y=tmin))+
  geom_line(col="red")+
  geom_smooth(method = "lm", se=FALSE, color="black", formula = "y~x") +
  guides(color="legend",position="top")+
  stat_poly_eq(formula = "y~x", 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
               parse = TRUE,size=5)+
  stat_fit_glance(method = "lm",
                method.args = list(formula = "y~x"),
                geom = 'text',
                aes(label = paste0("P-value = ", sprintf("%5f",signif(..p.value.., digits = 6)))),
                label.x.npc = 'left', label.y.npc = 0.87, size = 5)+
  labs(x="Temperature (F)",
       y="Year",
      title="Mean Annual Minimum Temperature 1940-2017\n Buffalo Niagara Airport, NY",
      caption="Data from http://www.nws.noaa.gov/climate/index.php?wfo=buf")
  
png(filename = "output/trend.png",width=800)
print(ptrend)
dev.off()

####

# Anomaly compare


