#clear everything#
rm(list = ls())

#Installing packages#
# packages<-c("dplyr","tidyverse", "ggplot2","lubridate","rdd","readxl","Stack","xlsx")
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(readxl)


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


#The GRAPH
plot1<-data_set_full%>%ggplot(aes(AST,PTS,color=playoff))+geom_point()+facet_grid(name~playoff)

#t test
#selecting only variables that are interesting
#var_interest<-c("PTS","AST","TRB","BLK","STL","TOV","playoff")

#filterning by players
LBJ_ready<-data_set_full%>%filter(name=="LBJ")
SC_ready<-data_set_full%>%filter(name=="SC")

#JH_ready<-data_set_full%>%filter(name=="JH")
#KD_ready<-data_set_full%>%filter(name=="KD")

#Ttest
#LBJ
LBJ_p_AST<-t.test(LBJ_ready$AST~LBJ_ready$playoff,mu=0)$p.value
LBJ_p_PTS<-t.test(LBJ_ready$PTS~LBJ_ready$playoff,mu=0)$p.value
LBJ_p_TRB<-t.test(LBJ_ready$TRB~LBJ_ready$playoff,mu=0)$p.value
LBJ_p_BLK<-t.test(LBJ_ready$BLK~LBJ_ready$playoff,mu=0)$p.value
LBJ_p_STL<-t.test(LBJ_ready$STL~LBJ_ready$playoff,mu=0)$p.value
LBJ_p_TOV<-t.test(LBJ_ready$TOV~LBJ_ready$playoff,mu=0)$p.value
#SC
SC_p_AST<-t.test(SC_ready$AST~SC_ready$playoff,mu=0)$p.value
SC_p_PTS<-t.test(SC_ready$PTS~SC_ready$playoff,mu=0)$p.value
SC_p_TRB<-t.test(SC_ready$TRB~SC_ready$playoff,mu=0)$p.value
SC_p_BLK<-t.test(SC_ready$BLK~SC_ready$playoff,mu=0)$p.value
SC_p_STL<-t.test(SC_ready$STL~SC_ready$playoff,mu=0)$p.value
SC_p_TOV<-t.test(SC_ready$TOV~SC_ready$playoff,mu=0)$p.value
#KD
#KD_p_AST<-t.test(KD_ready$AST~KD_ready$playoff,mu=0)$p.value
#KD_p_PTS<-t.test(KD_ready$PTS~KD_ready$playoff,mu=0)$p.value
#KD_p_TRB<-t.test(KD_ready$TRB~KD_ready$playoff,mu=0)$p.value
#KD_p_BLK<-t.test(KD_ready$BLK~KD_ready$playoff,mu=0)$p.value
#KD_p_STL<-t.test(KD_ready$STL~KD_ready$playoff,mu=0)$p.value
#KD_p_TOV<-t.test(KD_ready$TOV~KD_ready$playoff,mu=0)$p.value

#JH
#JH_p_AST<-t.test(JH_ready$AST~JH_ready$playoff,mu=0)$p.value
#JH_p_PTS<-t.test(JH_ready$PTS~JH_ready$playoff,mu=0)$p.value
#JH_p_TRB<-t.test(JH_ready$TRB~JH_ready$playoff,mu=0)$p.value
#JH_p_BLK<-t.test(JH_ready$BLK~JH_ready$playoff,mu=0)$p.value
#JH_p_STL<-t.test(JH_ready$STL~JH_ready$playoff,mu=0)$p.value
#JH_p_TOV<-t.test(JH_ready$TOV~JH_ready$playoff,mu=0)$p.value



#creationg the table
LBJ_p<-c("LBJ",LBJ_p_AST,LBJ_p_PTS,LBJ_p_TRB,LBJ_p_BLK,LBJ_p_STL,LBJ_p_TOV)
SC_p<-c("SC",SC_p_AST,SC_p_PTS,SC_p_TRB,SC_p_BLK,SC_p_STL,SC_p_TOV)
headers<-c("name","AST","PTS","TRB","BLK","STL","TOV")
p_value_set<-rbind(LBJ_p,SC_p)
colnames(p_value_set)<-headers

#exporting the data
write.xlsx(p_value_set,"pvalues.xlsx")
write.table(data_set_full, "~/Decade/mydata.txt", sep="\t")