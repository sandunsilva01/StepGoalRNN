library(readxl)
library(dplyr)

setwd("C:\\Users\\sandu\\OneDrive - Swinburne University\\PhD Documents\\personalisation\\step goals")

#Reading the step trajectory and demographic data sets
Traj_DF<-read_excel("trajectory_data_set.xlsx")
gcc_raw<-read.csv("gcc_memberid.csv")

#Looking for number of MemberId's
Memberid_vec<-unique(Traj_DF$MemberId)

#Extracting only the step entries which are higher than 999
Traj_DF<- Traj_DF %>% dplyr::filter(WalkSteps>=1000)

#Looking for the number of step entries (eventdays)
Traj_Groupdf<- Traj_DF %>% dplyr::group_by(MemberId) %>% dplyr::summarise(NoEntries=length(EventDay))

#Filter the MemberId's who have entered more than 7 step entries 
Traj_Groupdf_G7<-Traj_Groupdf %>% filter(NoEntries>=7)

Traj_DF_G7<-Traj_DF %>% filter(MemberId %in% Traj_Groupdf_G7$MemberId)
Traj_DF_G7$TeamAverage<-(Traj_DF_G7$t_walksteps+Traj_DF_G7$t_bikesteps+
                           Traj_DF_G7$t_swimsteps+Traj_DF_G7$t_othersteps)/Traj_DF_G7$t_stentries

Traj_DF_G7Select<-Traj_DF_G7 %>% dplyr::select(MemberId,EventDay,CreatedDate,WalkSteps,TeamAverage,t_stentries)

#Finding whether the participants synced the entries without a gap

actual_dateDF<-data.frame(MemberId = character(0),EventDay=numeric(0))

for (i in 1:nrow(Traj_Groupdf_G7)){
actual_dateDF2<-data.frame(NULL)

actual_dateDF2<-data.frame(MemberId=rep(as.character(Traj_Groupdf_G7[i,1]),as.numeric(Traj_Groupdf_G7[i,2])),
                           EventDay=seq(1:as.numeric(Traj_Groupdf_G7[i,2])))

actual_dateDF<-rbind.data.frame(actual_dateDF,actual_dateDF2)

}

Traj_DF_G7Select$MemberId<-as.factor(Traj_DF_G7Select$MemberId)
Traj_cleanDF<-left_join(actual_dateDF,Traj_DF_G7Select,by=c("MemberId","EventDay"))
