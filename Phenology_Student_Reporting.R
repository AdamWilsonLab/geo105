library(tidyverse)
library(rnpn)
library(lubridate)


classpath="~/Documents/Work/courses/201909/GEO105_Fall2019/Labs/03_Phenology"

groups=read_csv("~/Documents/Work/courses/201909/GEO105_Fall2019/Labs/03_Phenology/201909_105_groups/20191125120520_2199_19957_COMB_groupmembers.csv") %>% 
  mutate(ubit=`User Name`,group=sub("Section_gc_","",`Group Code`),
         first_name=`First Name`,last_name=`Last Name`,student_id=`Student Id`) %>% 
  select(student_id,ubit,first_name,last_name,group) %>% 
  filter(!is.na(student_id))

users=read_csv("~/Documents/Work/courses/201909/GEO105_Fall2019/Labs/03_Phenology/groupData_University_at_Buffalo.csv") %>% 
  select(Observer_ID,User_Email) %>% 
  mutate(ubit=sub("@buffalo.edu","",User_Email)) %>% 
  distinct() %>% 
  left_join(groups)


  

raw_data=npn_download_status_data("Adam Wilson",
                           years=c('2019'),
                           additional_fields=list("Plant_Nickname",
                                                  "ObservedBy_Person_ID"), 
                           network_ids=c(891)) %>% 
  as.tbl()

d=raw_data%>% 
  mutate(tag=as.numeric(substr(plant_nickname,1,3)),
         date=as.Date(observation_date),
         week=week(date)) %>% 
  left_join(users,by=c("observedby_person_id"="Observer_ID")) %>% 
  filter(!ubit%in%c("adammichaelwilson@gmail.com","davidnkarp@gmail.com","hstokes"))


## tree by tree records
d_trees <- d %>%
  filter(phenophase_description=="Leaves") %>% 
  group_by(ubit, plant_nickname)%>%
  summarize(total_obs=n()) %>% 
  mutate(tree_id_n = paste0(plant_nickname," (n=",total_obs,")"), 
         tree_num = paste0("tree_",row_number()))%>% 
  select(ubit,tree_id_n,tree_num)%>% 
  spread(tree_num,tree_id_n)

d_obs <- d %>% 
  filter(phenophase_description=="Leaves") %>% 
  group_by(group, student_id,ubit, week)%>%
  summarize(obs_week=n()) %>% 
  group_by(group, ubit)%>%
  summarize(obs_weeks=sum(obs_week>0),
            total_obs=sum(obs_week)) %>% 
  mutate(percent=100*total_obs/40,
         capped_percent=ifelse(percent>100,100,percent)) %>% 
  arrange(group,ubit)


d_summary=
  d_obs %>% 
  left_join(d_trees,by="ubit")

# write the student summary
write_csv(d_summary,file.path(classpath,"Phenology_Student_Summary.csv"))

## observation plot
d%>%
  filter(phenophase_description=="Leaves") %>% 
  group_by(group,ubit, week)%>%
  summarize(total_obs=n()) %>% 
  left_join(select(ungroup(d_obs),ubit,percent),by="ubit") %>% 
  ggplot(aes(x=week,y=reorder(ubit,percent),fill=total_obs)) +
  facet_wrap(~group,scales="free_y")+
  geom_tile()+
  scale_fill_viridis_c()+
  xlim(35,47)+
  ylab("UBit Name")


# Total observations
d_count <- d%>%
  filter(phenophase_description=="Leaves") %>% 
  group_by(group,ubit) %>% 
  summarize(total_obs=n()) %>% 
  mutate(percent=100*total_obs/40,
         capped_percent=ifelse(percent>100,100,percent))


d_count <- d%>%
  filter(phenophase_description=="Leaves") %>% 
  group_by(ubit) %>% 
  summarize(total_obs=n()) %>% 
  mutate(percent=100*total_obs/40)


d_count %>% 
  ggplot(aes(x=percent))+
  geom_histogram()

  
group_by(ubit)%>%
  summarize(total_obs=n()) %>% 
  mutate(tree_id_n = paste0(plant_nickname," (n=",total_obs,")"), 
         tree_num = paste0("tree_",row_number()))%>% 
  select(ubit,tree_id_n,tree_num)%>% 
  spread(tree_num,tree_id_n)


write_csv(d_report,"~/Documents/Work/courses/201909/GEO105_Fall2019/Labs/03_Phenology/Group_data_Summary.csv")

d%>%
  group_by(Plant_Nickname)%>%
  summarize(n=n()) %>% 
  arrange(desc(n)) 

unique(d$Abundance_Category_Value)
unique(d$Phenophase_Name)

d %>% 
  filter(Phenophase_Name=="Leaves") %>% 
#         Common_Name=="sugar maple") %>% 
  ggplot(aes(x=Observation_Date,y=abundance))+
#           group=Plant_Nickname,
#           color=Plant_Nickname))+
  geom_point()+
  geom_smooth(method="loess",span=2,se=F)+
  facet_wrap(~Common_Name)+
  ylab("Percent Green Leaves")
