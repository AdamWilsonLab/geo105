library(tidyverse)
library(rnpn)
library(lubridate)
library(zoo)


classpath="~/Documents/Work/courses/201909/GEO105_Fall2019/Labs/03_Phenology"

semester_start=as.Date("2019-8-26")
semester_stop=as.Date("2019-12-31")

groups=read_csv("~/Documents/Work/courses/201909/GEO105_Fall2019/Labs/03_Phenology/201909_105_groups/20191125120520_2199_19957_COMB_groupmembers.csv") %>% 
  mutate(ubit=tolower(`User Name`),group=sub("Section_gc_","",`Group Code`),
         first_name=`First Name`,last_name=`Last Name`,student_id=`Student Id`) %>% 
  select(student_id,ubit,first_name,last_name,group) %>% 
  filter(!is.na(student_id))

users=read_csv("~/Documents/Work/courses/201909/GEO105_Fall2019/Labs/03_Phenology/groupData_University_at_Buffalo.csv") %>% 
  select(Observer_ID,User_Email) %>% 
  mutate(ubit=tolower(sub("@buffalo.edu","",User_Email))) %>% 
  distinct() %>% 
  left_join(groups)

# d1=npn_download_individual_phenometrics("Adam Wilson",
#                                      years=c('2019'),
#                                      additional_fields=list("Plant_Nickname",
#                                                             "ObservedBy_Person_ID"), 
#                                      network_ids=c(891)) %>% 
#   as.tbl()


raw_data=npn_download_status_data("Adam Wilson",
                           years=c('2019'),
                           additional_fields=list("Plant_Nickname",
                                                  "ObservedBy_Person_ID",
                                                  "Submission_Datetime"), 
                           network_ids=c(891)) %>% 
  as.tbl()

d=raw_data%>% 
  filter(between(as.Date(observation_date),
                 as.Date(semester_start),
                 as.Date(semester_stop))) %>% 
  mutate(tag=as.numeric(substr(plant_nickname,1,3)),
         date=as.Date(observation_date),
         submission_date=as.Date(submission_datetime),
         week=week(date)-week(semester_start),
         week_factor=as.factor(week)) %>% 
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


library(zoo)
d_obs_weekly <- d %>%
  filter(phenophase_description=="Leaves",
         between(week,1,15)) %>%
  mutate(datedif=submission_date-date) %>%
  group_by(group, student_id,ubit, week_factor)%>%
  summarize(obs_week=n(),
            median_date_dif=as.numeric(median(datedif))) %>%
  complete(ubit,week_factor, fill = list(obs_week = 0)) %>%
  group_by(group, ubit)%>%
  arrange(group, ubit, week_factor) %>%
  mutate(obs7=rollapply(obs_week,3,mean,
                        align='center',na.rm=T,partial=T),
         obs_week_filled=ifelse(obs_week<4&obs7>3,obs7,obs_week))

d_obs<- d_obs_weekly %>% 
  summarize(
    obs_week_count=sum(obs_week>0),#) %>% #,
    obs_week_filled_count=sum(obs_week_filled>2.6),
    median_date_dif=median(median_date_dif,na.rm=T)) %>%
  mutate(
    obs_weeks_mean=mean(obs_week_count),
#    obs_weeks_4mean=mean(obs_week_4count),
#    mean_weekly_obs=sum(obs_week_count),
    percent_raw=100*pmax(obs_week_count,obs_week_filled_count)/10,
 #   percent4=100*obs_week_4count/10,
    percent=ifelse(percent_raw>100,100,percent_raw)) %>%
  arrange(group,ubit)

d_summary=
  d_obs %>% 
  left_join(d_trees,by="ubit") %>% 
  select(-percent_raw,-obs_weeks_mean)

d_summary %>% 
  filter(obs_week_count<9,percent>80)

# write the student summary
write_csv(d_summary,file.path(classpath,"Phenology_Student_Summary.csv"))

## observation plot
d%>%
  filter(phenophase_description=="Leaves",
         between(week,1,15)) %>% 
  group_by(group,ubit, week)%>%
  summarize(total_obs=n()) %>% 
  left_join(select(ungroup(d_obs),ubit,percent),by="ubit") %>% 
#  ggplot(aes(x=week,y=reorder(ubit,percent),fill=total_obs)) +
  ggplot(aes(x=week,y=ubit,fill=total_obs)) +
  facet_wrap(~group,scales="free_y")+
  geom_tile()+
  geom_text(data=d_summary, 
            mapping=aes(x=16,y=ubit,label=percent),
            size=3,hjust="left",
            inherit.aes=F)+
  scale_fill_viridis_c()+
  xlim(0,18)+
  ylab("UBit Name")+
  xlab("Week of the semester")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


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



# compare with daves data 20191218
dk=read_csv("~/Downloads/all_lab_trees_final2.csv") %>% 
  left_join(d_summary)

dk_update <- dk %>% mutate(dif=percent-aw.score)

ggplot(dk_update,aes(y=dif,x= median_date_dif))+geom_point()+
  ylab("Difference between posted scores and 12/18 udpate")+
  xlab("Median difference between date 'observed' and date submitted to NPN")

dk_update %>% 
  filter(dif>1) %>% 
  arrange(group,ubit)

hist(dk_update$dif)

d %>% filter(ubit=="sprinsto",phenophase_description=="Leaves") %>% 
  ggplot(aes(x=observation_date,y=submission_datetime))+geom_point()

d %>% filter(phenophase_description=="Leaves") %>%
  group_by(submission_date,date) %>% 
  summarize(total_observations=n()) %>%
  ggplot(aes(x=date,y=submission_date,fill=total_observations))+
  geom_tile()+
  geom_hline(aes(yintercept=as.Date("2019-12-06")),col="red")+
  scale_x_date()+
  xlab("Observation Date")

