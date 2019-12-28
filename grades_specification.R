
library(tidyverse)

# define specs
grade_letters= c("F","D","D+","C-","C","C+","B-","B","B+","A-","A")

specs=data.frame(
  grade=factor(grade_letters,levels = grade_letters,ordered=T),
  percentage=c(50,66,68,72,76,78,82,86,88,92,96),
  reflect=c(0,0,0,0,0,0,1,1,1,1,1),
  analysis=c(0,0,0,4:11),
  comprehension=c(0,6,7,8,8,9,9,10,10,11,11),
  attend=c(0,7,7,8,8,9,9,10,10,11,11)
)


file="~/Documents/Work/courses/201901/GEO105/grades/gc_2191_20526_COMB_fullgc_2019-05-21-21-15-31.csv"


g=read.csv(file)
gb=g
colnames(gb)=gsub("[.]","_",sub("[.][.].*","",colnames(g)))

#save original colnames
g_originalnames=read.csv(file,check.names = F)


# Identify comprehension and analysis columns
p1=c(which(grepl("Part_1",colnames(gb))),which(grepl("Debate",colnames(g))))
p2=c(which(grepl("Part_2",colnames(gb))),which(grepl("Debate",colnames(g))))

colnames(gb)[p1]
colnames(gb)[p2]

## Attempt statistics
g2=gb%>%
  mutate_at(c(p1,p2),function(x)as.numeric(as.character(x)))%>%
  mutate(mean = rowMeans(.[,c(p1,p2)], na.rm = TRUE))%>%
#  mutate_at(c(p1,p2),function(x) 
#    ifelse(is.na(x),rbinom(n=1,size=1,prob = .$mean),x))%>%
#  mutate_at(c(p1,p2),function(x) 
#    ifelse(is.na(x),1,x))%>%
  mutate(comprehension = rowSums(.[p1], na.rm = TRUE),
         analysis = rowSums(.[p2], na.rm = TRUE))%>%
  select(Last_Name,First_Name,Username,comprehension,analysis,
         attend=Attendance,reflect=Final_Reflection_for_ePortfolio)%>%
  mutate(reflect=ifelse(reflect==1,1,0),
         attend=ifelse(is.na(attend),0,attend))


fgrade=function(comprehension,analysis,attend,reflect,
                specs=specs,
                col=c("grade","percentage")){
  gid=which(
    comprehension>=specs$comprehension &
      analysis>=specs$analysis &
      attend>=specs$attend &
      reflect>=specs$reflect)
  if(length(gid)<1) return(NA)
  mgid=max(gid,na.rm=T)
return(specs[mgid,col])
}


flimit=function(comprehension,analysis,attend,reflect,specs=specs){
  c("comprehension","analysis","attend","reflect")[
  which.min(c(
    max(which(comprehension>=specs$comprehension)),
    max(which(analysis>=specs$analysis)),
    max(which(attend>=specs$attend)),
    max(which(reflect>=specs$reflect))
  ))]
}

g3 <- g2%>%
  rowwise()%>%
  mutate(grade=fgrade(comprehension,analysis,attend,reflect,
                      specs,col="grade"),
         percentage=fgrade(comprehension,analysis,attend,reflect,
                      specs,col="percentage"),
         limit=flimit(comprehension,analysis,attend,reflect,specs))

table(spec=g3$grade,limit=g3$limit)

ggplot(g3,aes(x=grade))+
  geom_histogram(stat="count")+
  ylab("Number of Students")+
  xlab("Final Grade")+
  coord_flip()

  ggplot(g3,aes(x=comprehension,y=analysis,col=grade))+
  geom_hline(data=specs,aes(yintercept=analysis),size=.2)+
  geom_text(data=specs,aes(x=0,y=analysis+.2,label=grade))+
  geom_vline(data=specs,aes(xintercept=comprehension),size=.2)+
  geom_text(data=specs,aes(x=comprehension+.2,y=0.2,label=grade))+
  geom_point(size=3)

specs
# write csv for upload
gfinal=g_originalnames
gfinal[,"grade [Total Pts: 0 Text] |1233691"]=g3$grade
gfinal[,"comprehension [Total Pts: 0 Text] |1233692"]=g3$comprehension
gfinal[,"analysis [Total Pts: 0 Text] |1233697"]=g3$analysis

write_csv(gfinal,"~/Documents/Work/courses/201901/GEO105/grades/final.csv",na = "")
# View(gfinal)
