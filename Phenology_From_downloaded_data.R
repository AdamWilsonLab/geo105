library(tidyverse)

d=read_csv("~/Documents/Work/courses/201909/GEO105_Fall2019/Labs/03_Phenology/groupData_University_at_Buffalo (3).csv") %>% 
  rename("a"="Abundance_Category_Value") %>% 
  mutate(abundance=case_when(
    a == "Less than 5%" ~ 2.5,
    a == "Less than 25%" ~ 20,
    a == "5-24%" ~ 14.5,
    a == "25-49%" ~ 37,
    a == "50-74%" ~ 62,
    a == "75-94%" ~ 84.5,
    a == "95% or more"  ~ 97.5,
    a == "Less than 3"  ~ 2,  
    a == "3 to 10" ~ 6.5,
    a == "11 to 100" ~ 50,
    a == "101 to 1,000" ~ 500,
    a == "1,001 to 10,000" ~ 5000,
    a == "Little" ~ 5,
    a == "Some" ~ 3,    
    TRUE ~ as.numeric(NA)
  ))


d_report <- d%>%
  group_by(User_Email, Plant_Nickname)%>%
  summarize(total_obs=n()) %>% 
  mutate(tree_id_n = paste0(Plant_Nickname," (n=",total_obs,")"))%>% 
  mutate(id = paste0("tree_",row_number()))%>% 
  select(User_Email,tree_id_n,id)%>% 
  spread(id,tree_id_n)


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
