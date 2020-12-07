library(tidyverse)
library(rnpn)
library(lubridate)
library(zoo)


classpath="~/Documents/Work/courses/202009/202009_GEO105/phenology observations/"

semester_start=as.Date("2020-8-26")
semester_stop=as.Date("2020-12-31")


ta_emails=c("adammichaelwilson@gmail.com",
            "davidnkarp@gmail.com","hstokes@buffalo.edu",
            "dnkarp@buffalo.edu",
            "moiraone@buffalo.edu",
            "szou2@buffalo.edu")

#import group data from UBLearns
groups=read_csv(file.path(classpath,"20201207105401_2209_19321_COMB_groupmembers.csv")) %>% 
  mutate(ubit=tolower(`User Name`),group=sub("Section_gc_","",`Group Code`),
         first_name=`First Name`,last_name=`Last Name`,student_id=`Student Id`) %>% 
  select(student_id,ubit,first_name,last_name,section=group) %>% 
  filter(!is.na(student_id))


# users in campus group
users_campus=read_csv("~/Documents/Work/courses/202009/202009_GEO105/phenology observations/groupData_University_at_Buffalo.csv") %>% 
  filter(Observation_Date>semester_start) %>% 
  select(NNid=Observer_ID,email=User_Email) %>% 
  mutate(ubit=tolower(sub("@buffalo.edu","",email)),site="Campus") %>% 
  distinct() #%>% 
#  left_join(groups,by="ubit")

# users in off-campus group
users_backyard=read_csv("~/Documents/Work/courses/202009/202009_GEO105/phenology observations/University at Buffalo Backyard Observers_11.19.2020.csv") %>% 
  select(NNid=Person_ID,email) %>% 
  mutate(ubit=tolower(sub("@buffalo.edu","",email)),site="Backyard") %>% 
  distinct() #%>% 
#  left_join(groups)

#create combined user group
nn_users=bind_rows(users_campus,users_backyard)


# find users with out matching email in ublearns
missing_emails=anti_join(nn_users,groups) %>% 
  filter(!email%in%ta_emails) %>% 
  select(email)
missing_emails


# Update incorrect emails
if(F){
anti_join(groups,nn_users) %>% 
  arrange(section,last_name)

groups %>% filter(first_name=="tyler")
groups %>% filter(first_name=="guido")
groups %>% filter(ubit=="tmarchi")
}

nn_users_updated <- nn_users %>% 
    mutate(
    ubit=case_when(
      email=="tyler.marchincin@gmail.com" ~ "tdmarchi",
      email=="rubinstein.rosa@gmail.com" ~ "rosarubi",
      TRUE ~ ubit)
  )


users=left_join(nn_users_updated,groups,by="ubit")  


# Get individual observations
raw_data=npn_download_status_data("Adam Wilson",
                           years=c('2020'),
                           additional_fields=list("Plant_Nickname",
                                                  "ObservedBy_Person_ID",
                                                  "Submission_Datetime"), 
                           network_ids=c(1047,891)) %>% 
  as_tibble()


# process raw data
d=raw_data%>% 
  filter(between(as.Date(observation_date),
                 as.Date(semester_start),
                 as.Date(semester_stop))) %>% 
  mutate(date=as.Date(observation_date), #tag=as.numeric(substr(plant_nickname,1,3))
         submission_date=as.Date(submission_datetime),
         observation_date=as.Date(observation_date),
         week=week(date)-week(semester_start),
         week_factor=as.factor(week),
         abundance_value=ifelse(abundance_value==-9999,NA,abundance_value),
         intensity_value=ifelse(intensity_value=="-9999",NA,intensity_value),
         ) %>% 
  left_join(users,by=c("observedby_person_id"="NNid")) %>% 
  filter(!ubit%in%ta_emails)


# diagnose problems by looking for students
if(F){
  # check individual students
  temail="thparent@buffalo.edu"
  temail="aemammos@buffalo.edu"
  temail="jamajama@buffalo.edu"
  
  users %>% filter(email==temail)
  groups %>% filter(ubit==sub("@buffalo.edu","",temail))
  d %>% filter(ubit==sub("@buffalo.edu","",temail)) %>% View()
}


## tree by tree records
d_trees <- d %>%
  filter(phenophase_description=="Leaves") %>% 
  group_by(ubit, email, plant_nickname,site, section)%>%
  summarize(total_obs=n(),obs_date_first=min(date),obs_date_last=max(date)) %>% 
  #mutate(tree_id_n = paste0(plant_nickname," (n=",total_obs,")"), 
  #       tree_num = paste0("tree_",row_number()))%>% 
  #select(ubit,tree_id_n,tree_num)%>% 
  select(section, ubit,email,site,total_obs,plant_nickname,obs_date_first,obs_date_last)%>%
  arrange(section,email,site)
  

library(zoo)
d_obs_weekly <- d %>%
  filter(phenophase_description=="Leaves",
         between(week,1,15)) %>%
  mutate(datedif=submission_date-date) %>%
  group_by(section, student_id,ubit,email, week_factor)%>%
  summarize(obs_week=n(),
            median_date_dif=as.numeric(median(datedif)),
            site=paste(unique(site),collapse="-")) %>%
  complete(ubit,email,week_factor, fill = list(obs_week = 0)) %>%
  group_by(section, ubit, email)%>%
  arrange(section, ubit, email, week_factor) %>%
  mutate(obs7=rollapply(obs_week,3,mean,
                        align='center',na.rm=T,partial=T),
         obs_week_filled=ifelse(obs_week<4&obs7>3,obs7,obs_week))

d_obs<- d_obs_weekly %>% 
  summarize(
    site=paste(unique(site),collapse="-"),
    obs_week_count=sum(obs_week>0),#) %>% #,
    obs_week_filled_count=sum(obs_week_filled>2.6),
    median_date_dif=median(median_date_dif,na.rm=T)) %>%
  mutate(
#    obs_weeks_mean=mean(obs_week_count),
#    obs_weeks_4mean=mean(obs_week_4count),
#    mean_weekly_obs=sum(obs_week_count),
    percent_raw=100*pmax(obs_week_count,obs_week_filled_count)/10,
 #   percent4=100*obs_week_4count/10,
    percent=ifelse(percent_raw>100,100,percent_raw),
    percent=ifelse(is.na(percent),0,percent)) %>%
  arrange(section,email) %>% 
  filter(!is.na(ubit))

 d_summary=
   d_obs %>% 
   left_join(d_trees,by="ubit") 

 # Join with grade center data
gfiles=list.files(file.path(classpath,"../grades/"),full=T,pattern="[0-9].csv")
grades=read_csv(gfiles[length(gfiles)])
phen_col="Phenology Observations [Total Pts: 100 Percentage] |1380462" 

grades2=grades %>% 
    left_join(
    select(d_obs,Username=ubit,phen=percent,site=site),
    by="Username") %>% 
  mutate(phen=ifelse(is.na(phen),0,phen)) %>% 
  mutate(!!quo_name(phen_col) := phen) %>% 
  select(!starts_with("X"),-phen)


if(F){
  grades2 %>% 
  select(Username,`Final Grade [Total Pts: up to 40.41369 Percentage] |1380458`,phen_col,section) %>% 
  arrange(section,Username) %>% 
View()

  ggplot(grades2,aes(
    x=`Final Grade [Total Pts: up to 40.41369 Percentage] |1380458`,
    y=`Phenology Observations [Total Pts: 100 Percentage] |1380462`,
    color=site))+
    geom_point()
  
  }

# Write grades file
write_csv(grades2,sub(".csv","_phenologyupdated.csv",gfiles[1]),na = "")

# write the student summary
write_csv(d_trees,file.path(classpath,"Phenology_Student_Summary_by_tree.csv"))
write_csv(d_obs,file.path(classpath,"Phenology_Student_Summary.csv"))


## observation plot
d%>%
  filter(phenophase_description=="Leaves",
         between(week,1,15),
         !is.na(ubit)) %>% 
  group_by(section,ubit, week)%>%
  summarize(total_obs=n()) %>% 
  left_join(select(ungroup(d_obs),ubit,percent),by="ubit") %>% 
#  ggplot(aes(x=week,y=reorder(ubit,percent),fill=total_obs)) +
  ggplot(aes(x=week,y=ubit,fill=total_obs)) +
  facet_wrap(~section,scales="free_y")+
  geom_tile()+
  geom_text(data=d_obs, 
            mapping=aes(x=16,y=ubit,label=percent),
            size=2.5,hjust="left",
            inherit.aes=F)+
  scale_fill_viridis_c()+
  xlim(0,18)+
  ylab("UBit Name")+
  xlab("Week of the semester")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Total observations
d_count <- d%>%
  filter(phenophase_description=="Leaves") %>% 
  group_by(section,ubit) %>% 
#  group_by(ubit) %>% 
  summarize(total_obs=n()) %>% 
  mutate(percent1=100*total_obs/40,
         percent=ifelse(percent1>100,100,percent1)) %>% 
  select(-percent1)

## Find missing students
groups %>% 
  select(ubit,first_name,last_name,section) %>% 
    group_by(section) %>% 
  outer_join(d_count)
d_count <- d%>%
  filter(phenophase_description=="Leaves") %>% 
  group_by(ubit) %>% 
  summarize(total_obs=n()) %>% 
  mutate(percent=100*total_obs/40)


d_count %>% 
  ggplot(aes(x=capped_percent))+
  geom_histogram(binwidth=10)+
  xlab("Phenology Grade")

# d_trees %>% 
# group_by(ubit)%>%
#   summarize(total_obs=n()) %>% 
#   mutate(tree_id_n = paste0(plant_nickname," (n=",total_obs,")"), 
#          tree_num = paste0("tree_",row_number()))%>% 
#   select(ubit,tree_id_n,tree_num)%>% 
#   spread(tree_num,tree_id_n)


write_csv(d_report,"~/Documents/Work/courses/201909/GEO105_Fall2019/Labs/03_Phenology/Group_data_Summary.csv")

d%>%
  group_by(Plant_Nickname)%>%
  summarize(n=n()) %>% 
  arrange(desc(n)) 

unique(d$Abundance_Category_Value)
unique(d$Phenophase_Name)

d %>% 
  filter(phenophase_description=="Leaves") %>% 
#         Common_Name=="sugar maple") %>% 
  ggplot(aes(x=observation_date,y=intensity_value))+
#           group=Plant_Nickname,
#           color=Plant_Nickname))+
  geom_point()+
  geom_smooth(method="loess",span=2,se=F)+
  facet_wrap(~common_name)+
  ylab("Percent Green Leaves")



# compare with daves data 20191218
#dk=read_csv("~/Downloads/all_lab_trees_final2.csv") %>% 
#  left_join(d_summary)
#dk_update <- dk %>% mutate(dif=percent-aw.score)

#ggplot(dk_update,aes(y=dif,x= median_date_dif))+geom_point()+
#  ylab("Difference between posted scores and 12/18 udpate")+
#  xlab("Median difference between date 'observed' and date submitted to NPN")

#dk_update %>% 
#  filter(dif>1) %>% 
#  arrange(group,ubit)

#hist(dk_update$dif)

#d %>% filter(ubit=="sprinsto",phenophase_description=="Leaves") %>% 
#  ggplot(aes(x=observation_date,y=submission_datetime))+geom_point()
#
#d %>% filter(phenophase_description=="Leaves") %>%
#  group_by(submission_date,date) %>% 
#  summarize(total_observations=n()) %>%
#  ggplot(aes(x=date,y=submission_date,fill=total_observations))+
#  geom_tile()+
#  geom_hline(aes(yintercept=as.Date("2019-12-06")),col="red")+
#  scale_x_date()+
#  xlab("Observation Date")

# map of sites
library(sf)
library(leaflet)

locations <- raw_data %>% 
  distinct(site_id,latitude,longitude) #%>% 
#  st_as_sf(coords = c("longitude", "latitude"), crs = 4396)

leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng = locations$longitude,lat = locations$latitude)

