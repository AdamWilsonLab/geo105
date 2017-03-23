
# GISS DATA MAP
library(raster)
library(ggplot2)
library(rasterVis)

d=raster("data/amaps.nc")
d2=asFactor(reclassify(d,c(-Inf,-.2,1,-.2,.2,2,.2,Inf,3)))
levels(d2)=data.frame(ID=1:3,Anomaly=c("Below Average","Average","Above Average"))

gplot(d2)+
  geom_tile(aes(fill=c("< -0.2 ","-0.2 - 0.2",">0.2")[value]))+
  borders(database = "world", regions = ".", fill = NA, colour = "grey50")+
  scale_fill_manual(guide = "legend",name="Anomaly",values=grey(c(0.2,0.5,0.8)),na.value="white")+
  coord_fixed()


r=data.frame(coordinates(d),anom=values(d))
r$bin=cut(r$anom,c(min(r$anom,na.rm=T),-.2,.2,max(r$anom,na.rm=T)))


  ggplot(r,aes(x=x,y=y,fill=bin))+
    geom_tile()+
    borders(database = "world", regions = ".", fill = NA, colour = "white")+
    scale_fill_manual(guide = "legend",name="Anomaly (C)",values=grey(c(0.8,0.5,0.2)),na.value="white")+
    ylab("Latitude")+xlab("Longitude")
    coord_fixed()+
    theme_gray(base_size = 75)
