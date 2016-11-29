library(dygraphs)
library(xts)
library(rnoaa)
library(ggiraph)
library(dplyr)
library(htmlwidgets)


## Get CO2 data
co2=read.table("ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_mm_mlo.txt",skip=70)
colnames(co2)=c("year","month","decimaldate","average","interpolated","trend","days")
head(co2)
co2=co2%>%
  mutate(date=as.Date(paste(year,month,15,sep="-")))%>%
  filter(average>0)

lm_co2=lm(average~decimaldate,data=co2)

## Get Temperature data 
#stations=ghcnd_stations()

#stations%>%filter(
#  grepl("BUFFALO",name),
#  grepl("NY",state),
#  last_year==2016,
#  first_year<1958,
#  element=="TMAX")

data=meteo_tidy_ghcnd("USW00014733",var=c("TMAX","TMIN"),date_min = as.Date("1900-01-01"))

temp_ts=data%>%
  arrange(date)%>%
  mutate(
  tmin=tmin/10,
  tmax=tmax/10,
  tave=(tmin+tmax)/2,
  tmin_60=rollapply(tmin,FUN="mean",width = 60,fill = c(NA,NA,NA)),
  tmax_60=rollapply(tmax,FUN="mean",width = 60,fill = c(NA,NA,NA)),
  month=as.numeric(format(date,"%m")),
  year=as.numeric(format(date,"%Y")),
  dyear=year+(month/12))%>%
  select(-id,-date,-month,-year,-dyear)

co2_ts=select(co2,interpolated,trend)%>%xts(order.by=co2$date)

d=dygraph(data=co2_ts, main = "Carbon Dioxide Concentrations (1958-2016)",group="trends")%>% 
  dyRangeSelector(dateWindow = c("2013-01-01", "2016-12-31"))

d2=dygraph(data=  xts(temp_ts,order.by=data$date),
           main = "Maximum Daily Temperature in Buffalo, NY",group="trends")%>% 
  dyRangeSelector(dateWindow = c("2013-01-01", "2016-12-31"))

d
d2

saveWidget(d, file="CO2.html")
saveWidget(d2, file="temp.html")


ggplot(temp_ts,aes(tmax))

