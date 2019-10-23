
library(tidyverse)


g=read_csv("~/Documents/Work/courses/201809/GEO105/gc_2189_22749_COMB_fullgc_2019-01-21-21-06-06.csv")%>%
  mutate(
    final=`Final Grade with extra credit [Total Pts: up to 48.05 Percentage] |1141581`,
    attend=`Participation [Total Pts: 50 Percentage] |1141577`,
    reflect=g$`Final Reflection for ePortfolio [Total Pts: 20 Score] |1141552`,
    ave=mean(`Nature of Science [Total Pts: 66 Percentage] |1141572`:`Climate Change Mitigation [Total Pts: 18 Percentage] |1141559`),
  final_letter=cut(final,
                          breaks = c(0,60,70,80,90,Inf),
                          labels=c("F","D","C","B","A")),
  attend_count=round(13*attend/100))

g_pass=g%>%
  select(`Nature of Science [Total Pts: 66 Percentage] |1141572`:`Climate Change Mitigation [Total Pts: 18 Percentage] |1141559`)%>%
  mutate_all(function(x) x>=80)%>%
  mutate(pass = rowSums(.))%>%
  bind_cols(g)%>%
  mutate(final_spec=
           case_when(
    pass >=  11 & attend_count >= 12  & reflect > 16 ~ "A",
    pass >=  9 & attend_count >= 11   & reflect > 16  ~ "B",
    pass >=  7 & attend_count >= 9   & reflect > 16  ~ "C",
    pass >=  7 & attend_count >= 7   ~ "D",
    TRUE ~ "F"))
    
g_pass%>%
  select(final_letter,final_spec,attend_count,pass,reflect)%>%
  slice(1:10)

table(spec=g_pass$final_spec,trad=g_pass$final_letter)


ggplot(g_pass,aes(x=final,y=pass))+
  geom_point()+
  ylab("Number of labs passed with >80")+
  xlab("Final Grade (without extra credit)")

ggplot(g_pass,aes(x=final_letter,y=pass))+
  geom_boxplot()+
  ylab("Number of labs passed with >80")+
  xlab("Final Grade (without extra credit)")

ggplot(g_pass,aes(x=final_letter,y=attend))+
  geom_boxplot()+
  ylab("Attendance (%)")+
  xlab("Final Grade (without extra credit)")

ggplot(g_pass,aes(x=final_letter,y=final_spec))+
  geom_tile()+
  ylab("Number of labs passed with >80")+
  xlab("Final Grade (without extra credit)")
