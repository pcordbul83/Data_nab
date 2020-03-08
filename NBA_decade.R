#clear everything#
rm(list = ls())

#Installing packages#
# packages<-c("dplyr","tidyverse", "ggplot2","lubridate","rdd","readxl","Stack","xlsx")
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(readxl)
library(tidyr)
library(broom)
library(dotwhisker)


#Create an exclusion clause
'%ni%' <- Negate('%in%')

#Load Regular season stats per player#
#LBJ#
#Regular Season
filenames_LBJ <- list.files(path="LBJ/",pattern="*.xls")
fullpath_LBJ<-file.path("LBJ/",filenames_LBJ)
dataset_LBJ<-do.call("rbind",lapply(fullpath_LBJ,FUN=function(files){read_excel(files,col_types = c("numeric", "numeric", "date",
                                                                                                    "text", "text", "text", "text", "text",
                                                                                                    "text", "numeric", "numeric", "numeric", 
                                                                                                    "numeric", "numeric", "numeric", 
                                                                                                    "numeric", "numeric", "numeric", 
                                                                                                    "numeric", "numeric", "numeric", 
                                                                                                    "numeric", "numeric", "numeric", 
                                                                                                    "numeric", "numeric", "numeric", 
                                                                                                    "numeric", "numeric", "numeric"))}))
dataset_LBJ<-dataset_LBJ%>%mutate(playoff=0,name="LBJ")

#Playoffs
dataset_LBJ_po<-read_excel("LBJ_poff.xlsx",col_types = c("numeric", "numeric", "date", 
                                                                  "text", "text", "text", "text", "numeric", 
                                                                  "text", "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric"))
dataset_LBJ_po<-dataset_LBJ_po%>%mutate(playoff=1,name="LBJ")

#SC#
#Regular Season
filenames_SC <- list.files(path="SC/",pattern="*.xls")
fullpath_SC<-file.path("SC/",filenames_SC)
dataset_SC<-do.call("rbind",lapply(fullpath_SC,FUN=function(files){read_excel(files,col_types = c("numeric", "numeric", "date",
                                                                                                  "text", "text", "text", "text", "text",
                                                                                                  "text", "numeric", "numeric", "numeric", 
                                                                                                  "numeric", "numeric", "numeric", 
                                                                                                  "numeric", "numeric", "numeric", 
                                                                                                  "numeric", "numeric", "numeric", 
                                                                                                  "numeric", "numeric", "numeric", 
                                                                                                  "numeric", "numeric", "numeric", 
                                                                                                  "numeric", "numeric", "numeric"))}))
dataset_SC<-dataset_SC%>%mutate(playoff=0,name="SC")

#Playoof
dataset_SC_po<-read_excel("SC_poff.xlsx",col_types = c("numeric", "numeric", "date", 
                                                                "text", "text", "text", "text", "numeric", 
                                                                "text", "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric", 
                                                                "numeric"))
dataset_SC_po<-dataset_SC_po%>%mutate(playoff=1,name="SC")

#JH#
#filenames_JH <- list.files(path="~/Decade/JH",pattern="*.txt")
#fullpath_JH=file.path("~/Decade/JH",filenames_JH)
#dataset_JH<-do.call("rbind",lapply(fullpath_JH,FUN=function(files){ read.csv(files,quote="'")}))

#combine everything into one dataset
dataset_full_regular<-rbind(dataset_LBJ,dataset_SC)
dataset_full_po<-rbind(dataset_LBJ_po,dataset_SC_po)

#Cleaning all the did not play, did not dress, Inactive
#Regular Season
#First we need to convert the GS variable to character
dataset_full_regular$GS<-as.character(dataset_full$GS)
not_play<-c("Did Not Play","Inactive","Did Not Dress","Not With Team")
dataset_full_regular<-dataset_full_regular%>% filter( GS %ni% not_play)

#Playoff
dataset_full_po$GS<-as.character(dataset_full_po$GS)
not_play<-c("Did Not Play","Inactive","Did Not Dress","Not With Team")
dataset_full_po<-dataset_full_po%>% filter( GS %ni% not_play)

#combine playoff and regular
var_list<-intersect(names(dataset_full_po),names(dataset_full_regular))
dataset_full_po<-dataset_full_po%>%select(var_list)
dataset_full_regular<-dataset_full_regular%>%select(var_list)
data_set_full<-rbind(dataset_full_po,dataset_full_regular)


#wide_dataset#
data_set_full<-data_set_full%>%mutate(year=format(as.Date(Date, format="%Y-%m-%d"),"%Y"))%>%
group_by(name,year,year,Rk)%>%arrange(name,year,playoff,Rk)%>%select(name,playoff,year,Rk,PTS,AST,TRB,STL,BLK,TOV)
data_set_full$game <- ave(data_set_full$Rk, data_set_full$year,data_set_full$name, FUN = seq_along)
wide_data<-gather(data_set_full, stat,value, PTS:TOV, factor_key=TRUE)


######Analysis########################
#GRAPHIC analysis#
#trends per player between playoff an regular#
Plot1_off<-wide_data%>%filter(stat==c("PTS","AST","TRB"))%>%ggplot(aes(game,value))+
geom_point(aes(color=playoff))+geom_smooth(method=lm)+facet_grid(name~stat)

Plot1_def<-wide_data%>%filter(stat==c("STL","BLK","TOV"))%>%ggplot(aes(game,value))+
  geom_point(aes(color=playoff))+geom_smooth(method=lm)+facet_grid(name~stat)

####Basic Regression to see if effect#####
reg_1<-data_set_full%>%group_by(name)%>%do(model = lm(playoff ~ PTS+AST+TRB+STL+BLK+TOV, data = .))
coeff_1<-reg_1%>%tidy(model)

#####t-test#####
####LBJ####
LBJ_data<-data_set_full%>%filter(name=="LBJ")
LBJ_PTS<-tidy(lm(PTS~playoff,data=LBJ_data))
LBJ_AST<-tidy(lm(AST~playoff,data=LBJ_data))
LBJ_TRB<-tidy(lm(TRB~playoff,data=LBJ_data))
LBJ_BLK<-tidy(lm(BLK~playoff,data=LBJ_data))
LBJ_STL<-tidy(lm(STL~playoff,data=LBJ_data))
LBJ_TOV<-tidy(lm(TOV~playoff,data=LBJ_data))

LBJ_ttest<-rbind(LBJ_PTS,LBJ_AST,LBJ_TRB,LBJ_BLK,LBJ_STL,LBJ_TOV)
row_names<-c("PTS","AST","TRB","BLK","STL","TOV")
LBJ_ttest<-LBJ_ttest%>%filter(term=="playoff")%>%mutate(player="LBJ",STAT=row_names)

####SC#####
SC_data<-data_set_full%>%filter(name=="SC")
SC_PTS<-tidy(lm(PTS~playoff,data=SC_data))
SC_AST<-tidy(lm(AST~playoff,data=SC_data))
SC_TRB<-tidy(lm(TRB~playoff,data=SC_data))
SC_BLK<-tidy(lm(BLK~playoff,data=SC_data))
SC_STL<-tidy(lm(STL~playoff,data=SC_data))
SC_TOV<-tidy(lm(TOV~playoff,data=SC_data))

SC_ttest<-rbind(SC_PTS,SC_AST,SC_TRB,SC_BLK,SC_STL,SC_TOV)
SC_ttest<-SC_ttest%>%filter(term=="playoff")%>%mutate(player="SC",STAT=row_names)

####combining the data sets#####
TTEST_all<-rbind(LBJ_ttest,SC_ttest)%>%select(player,STAT,estimate,std.error,statistic,p.value)%>%
  mutate(upper=estimate+std.error,lower=estimate-std.error)

Ttest_plot<-TTEST_all%>%ggplot()+geom_pointrange(data=TTEST_all,x=TTEST_all$STAT,y=TTEST_all$estimate,ymin=TTEST_all$lower,ymax=TTEST_all$upper)

#geom_hline(yintercept=0, linetype="dashed", color = "red")

#exporting the data
write.csv(TTEST_all, "ttest.txt",row.names = FALSE)