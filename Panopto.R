# Panopto reports
# 
# 
library(tidyverse)
classpath="~/Documents/Work/courses/202009/202009_GEO105/"

p = read_csv(file.path(classpath,"grades/panopto/folder-Completion_2020-08-01--2020-12-07_4eabcd71-1ffb-4920-b03c-abfe0103ebfd.csv"))%>% 
  mutate(ubit=sub("unified\\\\","",UserName))

lab_titles=c(
  "Love Canal",
  "Phenology Presentation Fall 2020",
  "Introduction to GEO105",
  "Anomalies tutorial",
  "Climate Wedges Presentation Fall 2020",
  "Coin Flip Activity Tutorial",
  "GEO 105 Anomalies Lab Introduction",
  "GEO105 Food Systems Lecture Fall 2020",
  "Hurricane Presentation Week 5",
  "Introduction to GEO105",
  "Love Canal",
  "Nature of Science Presentation Fall 2020",
  "Phenology 2 Fall 2020",
  "Phenology Presentation Fall 2020",
  "Pika & Natural Selection Presentation Fall 2020",
  "Tutorial on NPN Data Visualization Tool - Phenology2 Fall 2020",
  "Viruses and Public Health Intro",
  "GEO105 NPN Tutorial",
  "Final Reflection"
  )


pgrades <- p %>% 
  filter(`Session Name`%in%lab_titles) %>% 
  group_by(ubit) %>% 
  summarize(participation=mean(`Percent Completed`))

# Join with grade center data
gfiles=list.files(file.path(classpath,"grades/"),full=T,pattern="[0-9].csv")
gfile=gfiles[length(gfiles)]
grades=read_csv(gfile)

colnames(grades)
pcol="Participation [Total Pts: 100 Percentage] |1380461" 

# remove sections A&B from video accounting
absec = c('noheladi','pearlbey','djbrown9','rdcaputi','ndiagaci','hecoffed','mtcreary','kassimda','jbdietz','emilygiz','karanjag','clai9','jhlynch','tjpitruz','annikara','rickett2','rodrigu2','ajrosenl','bmsalomo','dvalenci','mcwaters','sjwhyte','fxie2','rayjonyo','npzorn','rhackerm','umeratiq','ethanbee','ekchalme','brdykes2','emgarret','johniemm','jackjura','lpkelly','dilynnke','sukishui','marenama','johnmccr','abmercha','paytonmi','mpack2','bmsamol','apsemko','ujjwalas','jessesta','mpvia','smwalsh3','jgyorko','yuchenzo')

grades2=
  bind_rows( #don't update section A&B!
    filter(grades,
         !Username%in%absec)%>% 
      left_join(
        select(pgrades,Username=ubit,participation),
        by="Username") %>%
      mutate(participation=ifelse(is.na(participation),0,participation)) %>% 
      mutate(!!quo_name(pcol) := participation) %>% 
      select(!starts_with("X"),-participation),
    filter(grades,
           Username%in%absec)) 
      
# Write grades file
write_csv(grades2,sub(".csv","_panopolyupdated.csv",gfile),na = "")


if(F){
# look up a single student
  p %>% 
  filter(ubit=="abkissoo") %>% 
  select(ubit, `Session Name`,`Minutes Delivered`,`Average Minutes Delivered`,`Percent Completed`)

  ggplot(pgrades,aes(x=participation))+
    geom_histogram()

  ggplot(grades2,aes(x=`Participation [Total Pts: 100 Percentage] |1380461`,
                     y=`Final Grade [Total Pts: up to 42.68187 Percentage] |1380458`))+
    geom_point()
  
    
  }