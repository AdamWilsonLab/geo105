# Anomalies


library(rnoaa)
library(dplyr)
library(ggplot2)
library(ggpmisc)

stations=ghcnd_stations()

d=as.Date("2016-08-01")

stations%>%filter(
  grepl("BUFFALO",name),
  grepl("NY",state),
  last_year==2016,
  first_year<2000,
  element=="TMIN")

data=ghcnd_search("USW00014733",var=c("TMIN"),date_min = as.Date("1900-01-01"))[[1]]

data=data%>%mutate(
  tmin=tmin/10,
  month=as.numeric(format(date,"%m")),
  year=as.numeric(format(date,"%Y")),
  dyear=year+(month/12))

data_year=data%>%
  group_by(year)%>%
  summarize(tmin=mean(tmin,na.rm=T))


formula <- y ~ x
lm1=lm(tmin~year,data=data_year)
str(summary(lm1))

ggplot(data_year,aes(x=year,y=tmin))+
  geom_line(col="grey")+
  geom_smooth(method = "lm", se=FALSE, color="black", formula = formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
               parse = TRUE)+
  stat_fit_glance(method = "lm",
                method.args = list(formula = formula),
                geom = 'text',
                aes(label = paste0("P-value = ", signif(..p.value.., digits = 4))),
                label.x.npc = 'left', label.y.npc = 0.87, size = 4)
  
  

