
library(tidyverse)

file="~/Documents/Work/courses/201909/GEO105_Fall2019/grades/gc_2199_19957_COMB_fullgc_2019-12-20-08-55-11.csv"

groups=read_csv("~/Documents/Work/courses/201909/GEO105_Fall2019/Labs/03_Phenology/201909_105_groups/20191125120520_2199_19957_COMB_groupmembers.csv") %>% 
  mutate(ubit=tolower(`User Name`),group=sub("Section_gc_","",`Group Code`),
         first_name=`First Name`,last_name=`Last Name`,student_id=`Student Id`) %>% 
  select(student_id,ubit,first_name,last_name,group) %>% 
  filter(!is.na(student_id))

ta_groups<-data.frame(
  TA=c("Shengyuan","Shengyuan","Yue","Yue","David","David","Hannah","Hannah"),
  group=c("A","B","C","D","E","F","G","H"))


g=read.csv(file) %>% 
  left_join(groups,by=c("Username"="ubit")) %>% 
  left_join(ta_groups)


gb=g
colnames(gb)=gsub("[.]","_",sub("[.][.].*","",colnames(g)))

ta_grades <- gb %>% 
  filter(!is.na(TA)) %>% 
  select(-Last_Name,-First_Name,-Final_Grade_Letter,
         -Seminar_1,-Seminar_2,-Seminar_3,-Seminar_4,
         -Syllabus_Quiz,-contains("X"),-student_id,-first_name) %>% 
  gather(item,grade,-Username,-TA) %>% 
  mutate(grade=as.numeric(grade))


  ggplot(ta_grades,aes(x=item,y=grade,color=TA))+
  geom_boxplot()+
  coord_flip()

  ta_grades %>% 
    filter(item%in%c("Final_Grade","Comprehension","Analytic")) %>% 
  ggplot(aes(x=item,y=grade,color=TA))+
    geom_boxplot()
  
ta_grades %>% 
  filter(item%in%c("Final_Grade","Analytic")) %>% 
  group_by(TA,item) %>% 
  summarize(mean_sd=paste0(round(mean(grade),1),
                           " (sd=",sd=round(sd(grade),1),")")) %>% 
  spread(item,mean_sd)

ta_grades %>% 
  filter(item%in%c("Final_Grade")) %>% 
  ggplot(aes(x=grade,color=TA))+
  geom_density()