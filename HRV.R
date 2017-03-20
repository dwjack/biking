#upload hrv files; HRV files are csv files created from inital cleaning in vivosense
# Author: Cara

library(plyr)
setwd("/Applications/Rdata")

HRV_prog <- list.files("HRV/HRVfiles", recursive = T, pattern = "^(BIKE)", full.names = T)
length(HRV_prog)


hrv.import <- function(HRV){
  hrvhex <- read.csv(HRV, stringsAsFactors = F, header = T,)  [,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25)] 
  names(hrvhex) <- c('subjectID', 'sessionID', 'stage', 'Annotation', 'Timestamp', 'Duration', 'ANN', 'SDNN', 'RMSSD', 'SDSD', 'SDNNi', 'SDANN', 'NN50', 'pNN50', 'VLF', 'LF',
                     'HF', 'LF(NORM)', 'HF(NORM)', 'LF/HF', 'RSA', 'SD1', 'SD2', 'SamplEn', 'Correlation Dimension')
  
  hrvhex}

names(HRV_prog) <- HRV_prog

HRVprog <- ldply(HRV_prog, hrv.import, .progress = "text")
View(HRVprog)

# change time format 
HRVprog$time <- as.POSIXct(HRVprog$Timestamp, format = "%H:%M:%S" , tz = "America/New_York")
# set staged in order 
HRVprog$stage <- factor(HRVprog$stage, levels =c("baseline","immed","post1","post2", "post3", "post4", "eve_base", "eve_immed", "eve_post1", "eve_post2", "eve_post3", "eve_post4" ))


summary(HRVprog$stage=="baseline")
summary(HRVprog$stage=="immed")
library(dplyr)
#2015 data 
HRV_2015 <- filter(HRVprog, subjectID!= "BIKE1016" & subjectID!="BIKE1017"
                   & subjectID!="BIKE1018"& subjectID!="BIKE1019"& subjectID!="BIKE1020"& subjectID!="BIKE1021" 
                   & subjectID!="BIKE1022" & subjectID!="BIKE1025" & subjectID!="BIKE1026" & subjectID!="BIKE2008"
                   & subjectID!="BIKE1033" & subjectID!="BIKE1031" & subjectID!="BIKE1027")
summary(HRV_2015$stage=="baseline")
summary(HRV_2015$stage=="eve_base")

#2016 data
HRV_2016 <- filter(HRVprog, subjectID== "BIKE1016" | subjectID=="BIKE1017" | subjectID=="BIKE1018" | subjectID=="BIKE1019" 
                   | subjectID=="BIKE1020" | subjectID=="BIKE1021" | subjectID=="BIKE1022" 
                   | subjectID=="BIKE1025" | subjectID=="BIKE1026" | subjectID=="BIKE2008"
                   | subjectID=="BIKE1033"| subjectID=="BIKE1031"| subjectID=="BIKE1027")
summary(HRV_2016$stage=="baseline")
summary(HRV_2016$stage=="eve_base")

# Wide data file
#basline 
HRV_base <- filter(HRVprog, stage == "baseline")
names(HRV_base)[names(HRV_base)=="Annotation"] <- "Annotation_base"
names(HRV_base)[names(HRV_base)=="ANN"] <- "ANN_base"
names(HRV_base)[names(HRV_base)=="SDNN"] <- "SDNN_base"
names(HRV_base)[names(HRV_base)=="RMSSD"] <- "RMSSD_base"
names(HRV_base)[names(HRV_base)=="SDSD"] <- "SDSD_base"
names(HRV_base)[names(HRV_base)=="SDANN"] <- "SDANN_base"
names(HRV_base)[names(HRV_base)=="NN50"] <- "NN50_base"
names(HRV_base)[names(HRV_base)=="SDNNi"] <- "SDNNi_base"
names(HRV_base)[names(HRV_base)=="pNN50"] <- "pNN50_base"
names(HRV_base)[names(HRV_base)=="VLF"] <- "VLF_base"
names(HRV_base)[names(HRV_base)=="VLF"] <- "VLF_base"
names(HRV_base)[names(HRV_base)=="LF"] <- "LF_base"
names(HRV_base)[names(HRV_base)=="HF"] <- "HF_base"
names(HRV_base)[names(HRV_base)=="LF (NORM)"] <- "LF (NORM)_base"
names(HRV_base)[names(HRV_base)=="HF (NORM)"] <- "HF (NORM)_base"
names(HRV_base)[names(HRV_base)=="LF/HF"] <- "LF/HF_base"
names(HRV_base)[names(HRV_base)=="RSA"] <- "RSA_base"
names(HRV_base)[names(HRV_base)=="SD1"] <- "SD1_base"
names(HRV_base)[names(HRV_base)=="SD2"] <- "SD2_base"
names(HRV_base)[names(HRV_base)=="SamplEn"] <- "SamplEn_base"
names(HRV_base)[names(HRV_base)=="Correlation Dimension"] <- "Correlation Dimension_base"
#drop columns for stage, timestamp, duration, time
HRV_base <- HRV_base[,-1]
HRV_base <- HRV_base[,-3]
HRV_base <- HRV_base[,-4]
HRV_base <- HRV_base[,-4]
HRV_base <- HRV_base[,-23]


#immed 
HRV_immed <- filter(HRVprog, stage == "immed")
names(HRV_immed)[names(HRV_immed)=="Annotation"] <- "Annotation_immed"
names(HRV_immed)[names(HRV_immed)=="ANN"] <- "ANN_immed"
names(HRV_immed)[names(HRV_immed)=="SDNN"] <- "SDNN_immed"
names(HRV_immed)[names(HRV_immed)=="RMSSD"] <- "RMSSD_immed"
names(HRV_immed)[names(HRV_immed)=="SDSD"] <- "SDSD_immed"
names(HRV_immed)[names(HRV_immed)=="SDANN"] <- "SDANN_immed"
names(HRV_immed)[names(HRV_immed)=="NN50"] <- "NN50_immed"
names(HRV_immed)[names(HRV_immed)=="SDNNi"] <- "SDNNi_immed"
names(HRV_immed)[names(HRV_immed)=="pNN50"] <- "pNN50_immed"
names(HRV_immed)[names(HRV_immed)=="VLF"] <- "VLF_immed"
names(HRV_immed)[names(HRV_immed)=="VLF"] <- "VLF_immed"
names(HRV_immed)[names(HRV_immed)=="LF"] <- "LF_immed"
names(HRV_immed)[names(HRV_immed)=="HF"] <- "HF_immed"
names(HRV_immed)[names(HRV_immed)=="LF (NORM)"] <- "LF (NORM)_immed"
names(HRV_immed)[names(HRV_immed)=="HF (NORM)"] <- "HF (NORM)_immed"
names(HRV_immed)[names(HRV_immed)=="LF/HF"] <- "LF/HF_immed"
names(HRV_immed)[names(HRV_immed)=="RSA"] <- "RSA_immed"
names(HRV_immed)[names(HRV_immed)=="SD1"] <- "SD1_immed"
names(HRV_immed)[names(HRV_immed)=="SD2"] <- "SD2_immed"
names(HRV_immed)[names(HRV_immed)=="SamplEn"] <- "SamplEn_immed"
names(HRV_immed)[names(HRV_immed)=="Correlation Dimension"] <- "Correlation Dimension_immed"
#drop columns for stage, timestamp, duration, time
HRV_immed <- HRV_immed[,-1]
HRV_immed <- HRV_immed[,-3]
HRV_immed <- HRV_immed[,-4]
HRV_immed <- HRV_immed[,-4]
HRV_immed <- HRV_immed[,-23]

#post1
HRV_post1 <- filter(HRVprog, stage == "post1")
names(HRV_post1)[names(HRV_post1)=="Annotation"] <- "Annotation_post1"
names(HRV_post1)[names(HRV_post1)=="ANN"] <- "ANN_post1"
names(HRV_post1)[names(HRV_post1)=="SDNN"] <- "SDNN_post1"
names(HRV_post1)[names(HRV_post1)=="RMSSD"] <- "RMSSD_post1"
names(HRV_post1)[names(HRV_post1)=="SDSD"] <- "SDSD_post1"
names(HRV_post1)[names(HRV_post1)=="SDANN"] <- "SDANN_post1"
names(HRV_post1)[names(HRV_post1)=="NN50"] <- "NN50_post1"
names(HRV_post1)[names(HRV_post1)=="SDNNi"] <- "SDNNi_post1"
names(HRV_post1)[names(HRV_post1)=="pNN50"] <- "pNN50_post1"
names(HRV_post1)[names(HRV_post1)=="VLF"] <- "VLF_post1"
names(HRV_post1)[names(HRV_post1)=="VLF"] <- "VLF_post1"
names(HRV_post1)[names(HRV_post1)=="LF"] <- "LF_post1"
names(HRV_post1)[names(HRV_post1)=="HF"] <- "HF_post1"
names(HRV_post1)[names(HRV_post1)=="LF (NORM)"] <- "LF (NORM)_post1"
names(HRV_post1)[names(HRV_post1)=="HF (NORM)"] <- "HF (NORM)_post1"
names(HRV_post1)[names(HRV_post1)=="LF/HF"] <- "LF/HF_post1"
names(HRV_post1)[names(HRV_post1)=="RSA"] <- "RSA_post1"
names(HRV_post1)[names(HRV_post1)=="SD1"] <- "SD1_post1"
names(HRV_post1)[names(HRV_post1)=="SD2"] <- "SD2_post1"
names(HRV_post1)[names(HRV_post1)=="SamplEn"] <- "SamplEn_post1"
names(HRV_post1)[names(HRV_post1)=="Correlation Dimension"] <- "Correlation Dimension_post1"
#drop columns for stage, timestamp, duration, time
HRV_post1 <- HRV_post1[,-1]
HRV_post1 <- HRV_post1[,-3]
HRV_post1 <- HRV_post1[,-4]
HRV_post1 <- HRV_post1[,-4]
HRV_post1 <- HRV_post1[,-23]

#post2 
HRV_post2 <- filter(HRVprog, stage == "post2")
names(HRV_post2)[names(HRV_post2)=="Annotation"] <- "Annotation_post2"
names(HRV_post2)[names(HRV_post2)=="ANN"] <- "ANN_post2"
names(HRV_post2)[names(HRV_post2)=="SDNN"] <- "SDNN_post2"
names(HRV_post2)[names(HRV_post2)=="RMSSD"] <- "RMSSD_post2"
names(HRV_post2)[names(HRV_post2)=="SDSD"] <- "SDSD_post2"
names(HRV_post2)[names(HRV_post2)=="SDANN"] <- "SDANN_post2"
names(HRV_post2)[names(HRV_post2)=="NN50"] <- "NN50_post2"
names(HRV_post2)[names(HRV_post2)=="SDNNi"] <- "SDNNi_post2"
names(HRV_post2)[names(HRV_post2)=="pNN50"] <- "pNN50_post2"
names(HRV_post2)[names(HRV_post2)=="VLF"] <- "VLF_post2"
names(HRV_post2)[names(HRV_post2)=="VLF"] <- "VLF_post2"
names(HRV_post2)[names(HRV_post2)=="LF"] <- "LF_post2"
names(HRV_post2)[names(HRV_post2)=="HF"] <- "HF_post2"
names(HRV_post2)[names(HRV_post2)=="LF (NORM)"] <- "LF (NORM)_post2"
names(HRV_post2)[names(HRV_post2)=="HF (NORM)"] <- "HF (NORM)_post2"
names(HRV_post2)[names(HRV_post2)=="LF/HF"] <- "LF/HF_post2"
names(HRV_post2)[names(HRV_post2)=="RSA"] <- "RSA_post2"
names(HRV_post2)[names(HRV_post2)=="SD1"] <- "SD1_post2"
names(HRV_post2)[names(HRV_post2)=="SD2"] <- "SD2_post2"
names(HRV_post2)[names(HRV_post2)=="SamplEn"] <- "SamplEn_post2"
names(HRV_post2)[names(HRV_post2)=="Correlation Dimension"] <- "Correlation Dimension_post2"
#drop columns for stage, timestamp, duration, time
HRV_post2 <- HRV_post2[,-1]
HRV_post2 <- HRV_post2[,-3]
HRV_post2 <- HRV_post2[,-4]
HRV_post2 <- HRV_post2[,-4]
HRV_post2 <- HRV_post2[,-23]

#post3 
HRV_post3 <- filter(HRVprog, stage == "post3")
names(HRV_post3)[names(HRV_post3)=="Annotation"] <- "Annotation_post3"
names(HRV_post3)[names(HRV_post3)=="ANN"] <- "ANN_post3"
names(HRV_post3)[names(HRV_post3)=="SDNN"] <- "SDNN_post3"
names(HRV_post3)[names(HRV_post3)=="RMSSD"] <- "RMSSD_post3"
names(HRV_post3)[names(HRV_post3)=="SDSD"] <- "SDSD_post3"
names(HRV_post3)[names(HRV_post3)=="SDANN"] <- "SDANN_post3"
names(HRV_post3)[names(HRV_post3)=="NN50"] <- "NN50_post3"
names(HRV_post3)[names(HRV_post3)=="SDNNi"] <- "SDNNi_post3"
names(HRV_post3)[names(HRV_post3)=="pNN50"] <- "pNN50_post3"
names(HRV_post3)[names(HRV_post3)=="VLF"] <- "VLF_post3"
names(HRV_post3)[names(HRV_post3)=="VLF"] <- "VLF_post3"
names(HRV_post3)[names(HRV_post3)=="LF"] <- "LF_post3"
names(HRV_post3)[names(HRV_post3)=="HF"] <- "HF_post3"
names(HRV_post3)[names(HRV_post3)=="LF (NORM)"] <- "LF (NORM)_post3"
names(HRV_post3)[names(HRV_post3)=="HF (NORM)"] <- "HF (NORM)_post3"
names(HRV_post3)[names(HRV_post3)=="LF/HF"] <- "LF/HF_post3"
names(HRV_post3)[names(HRV_post3)=="RSA"] <- "RSA_post3"
names(HRV_post3)[names(HRV_post3)=="SD1"] <- "SD1_post3"
names(HRV_post3)[names(HRV_post3)=="SD2"] <- "SD2_post3"
names(HRV_post3)[names(HRV_post3)=="SamplEn"] <- "SamplEn_post3"
names(HRV_post3)[names(HRV_post3)=="Correlation Dimension"] <- "Correlation Dimension_post3"
#drop columns for stage, timestamp, duration, time
HRV_post3 <- HRV_post3[,-1]
HRV_post3 <- HRV_post3[,-3]
HRV_post3 <- HRV_post3[,-4]
HRV_post3 <- HRV_post3[,-4]
HRV_post3 <- HRV_post3[,-23]

#post4 
HRV_post4 <- filter(HRVprog, stage == "post4")
names(HRV_post4)[names(HRV_post4)=="Annotation"] <- "Annotation_post4"
names(HRV_post4)[names(HRV_post4)=="ANN"] <- "ANN_post4"
names(HRV_post4)[names(HRV_post4)=="SDNN"] <- "SDNN_post4"
names(HRV_post4)[names(HRV_post4)=="RMSSD"] <- "RMSSD_post4"
names(HRV_post4)[names(HRV_post4)=="SDSD"] <- "SDSD_post4"
names(HRV_post4)[names(HRV_post4)=="SDANN"] <- "SDANN_post4"
names(HRV_post4)[names(HRV_post4)=="NN50"] <- "NN50_post4"
names(HRV_post4)[names(HRV_post4)=="SDNNi"] <- "SDNNi_post4"
names(HRV_post4)[names(HRV_post4)=="pNN50"] <- "pNN50_post4"
names(HRV_post4)[names(HRV_post4)=="VLF"] <- "VLF_post4"
names(HRV_post4)[names(HRV_post4)=="VLF"] <- "VLF_post4"
names(HRV_post4)[names(HRV_post4)=="LF"] <- "LF_post4"
names(HRV_post4)[names(HRV_post4)=="HF"] <- "HF_post4"
names(HRV_post4)[names(HRV_post4)=="LF (NORM)"] <- "LF (NORM)_post4"
names(HRV_post4)[names(HRV_post4)=="HF (NORM)"] <- "HF (NORM)_post4"
names(HRV_post4)[names(HRV_post4)=="LF/HF"] <- "LF/HF_post4"
names(HRV_post4)[names(HRV_post4)=="RSA"] <- "RSA_post4"
names(HRV_post4)[names(HRV_post4)=="SD1"] <- "SD1_post4"
names(HRV_post4)[names(HRV_post4)=="SD2"] <- "SD2_post4"
names(HRV_post4)[names(HRV_post4)=="SamplEn"] <- "SamplEn_post4"
names(HRV_post4)[names(HRV_post4)=="Correlation Dimension"] <- "Correlation Dimension_post4"
#drop columns for stage, timestamp, duration, time
HRV_post4 <- HRV_post4[,-1]
HRV_post4 <- HRV_post4[,-3]
HRV_post4 <- HRV_post4[,-4]
HRV_post4 <- HRV_post4[,-4]
HRV_post4 <- HRV_post4[,-23]

#Eve_base 
HRV_Ebase <- filter(HRVprog, stage == "eve_base")
names(HRV_Ebase)[names(HRV_Ebase)=="Annotation"] <- "Annotation_Ebase"
names(HRV_Ebase)[names(HRV_Ebase)=="ANN"] <- "ANN_Ebase"
names(HRV_Ebase)[names(HRV_Ebase)=="SDNN"] <- "SDNN_Ebase"
names(HRV_Ebase)[names(HRV_Ebase)=="RMSSD"] <- "RMSSD_Ebase"
names(HRV_Ebase)[names(HRV_Ebase)=="SDSD"] <- "SDSD_Ebase"
names(HRV_Ebase)[names(HRV_Ebase)=="SDANN"] <- "SDANN_Ebase"
names(HRV_Ebase)[names(HRV_Ebase)=="NN50"] <- "NN50_Ebase"
names(HRV_Ebase)[names(HRV_Ebase)=="SDNNi"] <- "SDNNi_Ebase"
names(HRV_Ebase)[names(HRV_Ebase)=="pNN50"] <- "pNN50_Ebase"
names(HRV_Ebase)[names(HRV_Ebase)=="VLF"] <- "VLF_Ebase"
names(HRV_Ebase)[names(HRV_Ebase)=="VLF"] <- "VLF_Ebase"
names(HRV_Ebase)[names(HRV_Ebase)=="LF"] <- "LF_Ebase"
names(HRV_Ebase)[names(HRV_Ebase)=="HF"] <- "HF_Ebase"
names(HRV_Ebase)[names(HRV_Ebase)=="LF (NORM)"] <- "LF (NORM)_Ebase"
names(HRV_Ebase)[names(HRV_Ebase)=="HF (NORM)"] <- "HF (NORM)_Ebase"
names(HRV_Ebase)[names(HRV_Ebase)=="LF/HF"] <- "LF/HF_Ebase"
names(HRV_Ebase)[names(HRV_Ebase)=="RSA"] <- "RSA_Ebase"
names(HRV_Ebase)[names(HRV_Ebase)=="SD1"] <- "SD1_Ebase"
names(HRV_Ebase)[names(HRV_Ebase)=="SD2"] <- "SD2_Ebase"
names(HRV_Ebase)[names(HRV_Ebase)=="SamplEn"] <- "SamplEn_Ebase"
names(HRV_Ebase)[names(HRV_Ebase)=="Correlation Dimension"] <- "Correlation Dimension_Ebase"
#drop columns for stage, timestamp, duration, time
HRV_Ebase <- HRV_Ebase[,-1]
HRV_Ebase <- HRV_Ebase[,-3]
HRV_Ebase <- HRV_Ebase[,-4]
HRV_Ebase <- HRV_Ebase[,-4]
HRV_Ebase <- HRV_Ebase[,-23]

#Eve immed
HRV_Eimmed <- filter(HRVprog, stage == "eve_immed")
names(HRV_Eimmed)[names(HRV_Eimmed)=="Annotation"] <- "Annotation_Eimmed"
names(HRV_Eimmed)[names(HRV_Eimmed)=="ANN"] <- "ANN_Eimmed"
names(HRV_Eimmed)[names(HRV_Eimmed)=="SDNN"] <- "SDNN_Eimmed"
names(HRV_Eimmed)[names(HRV_Eimmed)=="RMSSD"] <- "RMSSD_Eimmed"
names(HRV_Eimmed)[names(HRV_Eimmed)=="SDSD"] <- "SDSD_Eimmed"
names(HRV_Eimmed)[names(HRV_Eimmed)=="SDANN"] <- "SDANN_Eimmed"
names(HRV_Eimmed)[names(HRV_Eimmed)=="NN50"] <- "NN50_Eimmed"
names(HRV_Eimmed)[names(HRV_Eimmed)=="SDNNi"] <- "SDNNi_Eimmed"
names(HRV_Eimmed)[names(HRV_Eimmed)=="pNN50"] <- "pNN50_Eimmed"
names(HRV_Eimmed)[names(HRV_Eimmed)=="VLF"] <- "VLF_Eimmed"
names(HRV_Eimmed)[names(HRV_Eimmed)=="VLF"] <- "VLF_Eimmed"
names(HRV_Eimmed)[names(HRV_Eimmed)=="LF"] <- "LF_Eimmed"
names(HRV_Eimmed)[names(HRV_Eimmed)=="HF"] <- "HF_Eimmed"
names(HRV_Eimmed)[names(HRV_Eimmed)=="LF (NORM)"] <- "LF (NORM)_Eimmed"
names(HRV_Eimmed)[names(HRV_Eimmed)=="HF (NORM)"] <- "HF (NORM)_Eimmed"
names(HRV_Eimmed)[names(HRV_Eimmed)=="LF/HF"] <- "LF/HF_Eimmed"
names(HRV_Eimmed)[names(HRV_Eimmed)=="RSA"] <- "RSA_Eimmed"
names(HRV_Eimmed)[names(HRV_Eimmed)=="SD1"] <- "SD1_Eimmed"
names(HRV_Eimmed)[names(HRV_Eimmed)=="SD2"] <- "SD2_Eimmed"
names(HRV_Eimmed)[names(HRV_Eimmed)=="SamplEn"] <- "SamplEn_Eimmed"
names(HRV_Eimmed)[names(HRV_Eimmed)=="Correlation Dimension"] <- "Correlation Dimension_Eimmed"
#drop columns for stage, timestamp, duration, time
HRV_Eimmed <- HRV_Eimmed[,-1]
HRV_Eimmed <- HRV_Eimmed[,-3]
HRV_Eimmed <- HRV_Eimmed[,-4]
HRV_Eimmed <- HRV_Eimmed[,-4]
HRV_Eimmed <- HRV_Eimmed[,-23]


#Epost1 
HRV_Epost1 <- filter(HRVprog, stage == "eve_post1")
names(HRV_Epost1)[names(HRV_Epost1)=="Annotation"] <- "Annotation_Epost1"
names(HRV_Epost1)[names(HRV_Epost1)=="ANN"] <- "ANN_Epost1"
names(HRV_Epost1)[names(HRV_Epost1)=="SDNN"] <- "SDNN_Epost1"
names(HRV_Epost1)[names(HRV_Epost1)=="RMSSD"] <- "RMSSD_Epost1"
names(HRV_Epost1)[names(HRV_Epost1)=="SDSD"] <- "SDSD_Epost1"
names(HRV_Epost1)[names(HRV_Epost1)=="SDANN"] <- "SDANN_Epost1"
names(HRV_Epost1)[names(HRV_Epost1)=="NN50"] <- "NN50_Epost1"
names(HRV_Epost1)[names(HRV_Epost1)=="SDNNi"] <- "SDNNi_Epost1"
names(HRV_Epost1)[names(HRV_Epost1)=="pNN50"] <- "pNN50_Epost1"
names(HRV_Epost1)[names(HRV_Epost1)=="VLF"] <- "VLF_Epost1"
names(HRV_Epost1)[names(HRV_Epost1)=="VLF"] <- "VLF_Epost1"
names(HRV_Epost1)[names(HRV_Epost1)=="LF"] <- "LF_Epost1"
names(HRV_Epost1)[names(HRV_Epost1)=="HF"] <- "HF_Epost1"
names(HRV_Epost1)[names(HRV_Epost1)=="LF (NORM)"] <- "LF (NORM)_Epost1"
names(HRV_Epost1)[names(HRV_Epost1)=="HF (NORM)"] <- "HF (NORM)_Epost1"
names(HRV_Epost1)[names(HRV_Epost1)=="LF/HF"] <- "LF/HF_Epost1"
names(HRV_Epost1)[names(HRV_Epost1)=="RSA"] <- "RSA_Epost1"
names(HRV_Epost1)[names(HRV_Epost1)=="SD1"] <- "SD1_Epost1"
names(HRV_Epost1)[names(HRV_Epost1)=="SD2"] <- "SD2_Epost1"
names(HRV_Epost1)[names(HRV_Epost1)=="SamplEn"] <- "SamplEn_Epost1"
names(HRV_Epost1)[names(HRV_Epost1)=="Correlation Dimension"] <- "Correlation Dimension_Epost1"
#drop columns for stage, timestamp, duration, time
HRV_Epost1 <- HRV_Epost1[,-1]
HRV_Epost1 <- HRV_Epost1[,-3]
HRV_Epost1 <- HRV_Epost1[,-4]
HRV_Epost1 <- HRV_Epost1[,-4]
HRV_Epost1 <- HRV_Epost1[,-23]


#eve post 2
HRV_Epost2 <- filter(HRVprog, stage == "eve_post2")
names(HRV_Epost2)[names(HRV_Epost2)=="Annotation"] <- "Annotation_Epost2"
names(HRV_Epost2)[names(HRV_Epost2)=="ANN"] <- "ANN_Epost2"
names(HRV_Epost2)[names(HRV_Epost2)=="SDNN"] <- "SDNN_Epost2"
names(HRV_Epost2)[names(HRV_Epost2)=="RMSSD"] <- "RMSSD_Epost2"
names(HRV_Epost2)[names(HRV_Epost2)=="SDSD"] <- "SDSD_Epost2"
names(HRV_Epost2)[names(HRV_Epost2)=="SDANN"] <- "SDANN_Epost2"
names(HRV_Epost2)[names(HRV_Epost2)=="NN50"] <- "NN50_Epost2"
names(HRV_Epost2)[names(HRV_Epost2)=="SDNNi"] <- "SDNNi_Epost2"
names(HRV_Epost2)[names(HRV_Epost2)=="pNN50"] <- "pNN50_Epost2"
names(HRV_Epost2)[names(HRV_Epost2)=="VLF"] <- "VLF_Epost2"
names(HRV_Epost2)[names(HRV_Epost2)=="VLF"] <- "VLF_Epost2"
names(HRV_Epost2)[names(HRV_Epost2)=="LF"] <- "LF_Epost2"
names(HRV_Epost2)[names(HRV_Epost2)=="HF"] <- "HF_Epost2"
names(HRV_Epost2)[names(HRV_Epost2)=="LF (NORM)"] <- "LF (NORM)_Epost2"
names(HRV_Epost2)[names(HRV_Epost2)=="HF (NORM)"] <- "HF (NORM)_Epost2"
names(HRV_Epost2)[names(HRV_Epost2)=="LF/HF"] <- "LF/HF_Epost2"
names(HRV_Epost2)[names(HRV_Epost2)=="RSA"] <- "RSA_Epost2"
names(HRV_Epost2)[names(HRV_Epost2)=="SD1"] <- "SD1_Epost2"
names(HRV_Epost2)[names(HRV_Epost2)=="SD2"] <- "SD2_Epost2"
names(HRV_Epost2)[names(HRV_Epost2)=="SamplEn"] <- "SamplEn_Epost2"
names(HRV_Epost2)[names(HRV_Epost2)=="Correlation Dimension"] <- "Correlation Dimension_Epost2"
#drop columns for stage, timestamp, duration, time
HRV_Epost2 <- HRV_Epost2[,-1]
HRV_Epost2 <- HRV_Epost2[,-3]
HRV_Epost2 <- HRV_Epost2[,-4]
HRV_Epost2 <- HRV_Epost2[,-4]
HRV_Epost2 <- HRV_Epost2[,-23]

#eve post 3 
HRV_Epost3 <- filter(HRVprog, stage == "eve_post3")
names(HRV_Epost3)[names(HRV_Epost3)=="Annotation"] <- "Annotation_Epost3"
names(HRV_Epost3)[names(HRV_Epost3)=="ANN"] <- "ANN_Epost3"
names(HRV_Epost3)[names(HRV_Epost3)=="SDNN"] <- "SDNN_Epost3"
names(HRV_Epost3)[names(HRV_Epost3)=="RMSSD"] <- "RMSSD_Epost3"
names(HRV_Epost3)[names(HRV_Epost3)=="SDSD"] <- "SDSD_Epost3"
names(HRV_Epost3)[names(HRV_Epost3)=="SDANN"] <- "SDANN_Epost3"
names(HRV_Epost3)[names(HRV_Epost3)=="NN50"] <- "NN50_Epost3"
names(HRV_Epost3)[names(HRV_Epost3)=="SDNNi"] <- "SDNNi_Epost3"
names(HRV_Epost3)[names(HRV_Epost3)=="pNN50"] <- "pNN50_Epost3"
names(HRV_Epost3)[names(HRV_Epost3)=="VLF"] <- "VLF_Epost3"
names(HRV_Epost3)[names(HRV_Epost3)=="VLF"] <- "VLF_Epost3"
names(HRV_Epost3)[names(HRV_Epost3)=="LF"] <- "LF_Epost3"
names(HRV_Epost3)[names(HRV_Epost3)=="HF"] <- "HF_Epost3"
names(HRV_Epost3)[names(HRV_Epost3)=="LF (NORM)"] <- "LF (NORM)_Epost3"
names(HRV_Epost3)[names(HRV_Epost3)=="HF (NORM)"] <- "HF (NORM)_Epost3"
names(HRV_Epost3)[names(HRV_Epost3)=="LF/HF"] <- "LF/HF_Epost3"
names(HRV_Epost3)[names(HRV_Epost3)=="RSA"] <- "RSA_Epost3"
names(HRV_Epost3)[names(HRV_Epost3)=="SD1"] <- "SD1_Epost3"
names(HRV_Epost3)[names(HRV_Epost3)=="SD2"] <- "SD2_Epost3"
names(HRV_Epost3)[names(HRV_Epost3)=="SamplEn"] <- "SamplEn_Epost3"
names(HRV_Epost3)[names(HRV_Epost3)=="Correlation Dimension"] <- "Correlation Dimension_Epost3"
#drop columns for stage, timestamp, duration, time
HRV_Epost3 <- HRV_Epost3[,-1]
HRV_Epost3 <- HRV_Epost3[,-3]
HRV_Epost3 <- HRV_Epost3[,-4]
HRV_Epost3 <- HRV_Epost3[,-4]
HRV_Epost3 <- HRV_Epost3[,-23]

#eve post 4
HRV_Epost4 <- filter(HRVprog, stage == "eve_post4")
names(HRV_Epost4)[names(HRV_Epost4)=="Annotation"] <- "Annotation_Epost4"
names(HRV_Epost4)[names(HRV_Epost4)=="ANN"] <- "ANN_Epost4"
names(HRV_Epost4)[names(HRV_Epost4)=="SDNN"] <- "SDNN_Epost4"
names(HRV_Epost4)[names(HRV_Epost4)=="RMSSD"] <- "RMSSD_Epost4"
names(HRV_Epost4)[names(HRV_Epost4)=="SDSD"] <- "SDSD_Epost4"
names(HRV_Epost4)[names(HRV_Epost4)=="SDANN"] <- "SDANN_Epost4"
names(HRV_Epost4)[names(HRV_Epost4)=="NN50"] <- "NN50_Epost4"
names(HRV_Epost4)[names(HRV_Epost4)=="SDNNi"] <- "SDNNi_Epost4"
names(HRV_Epost4)[names(HRV_Epost4)=="pNN50"] <- "pNN50_Epost4"
names(HRV_Epost4)[names(HRV_Epost4)=="VLF"] <- "VLF_Epost4"
names(HRV_Epost4)[names(HRV_Epost4)=="VLF"] <- "VLF_Epost4"
names(HRV_Epost4)[names(HRV_Epost4)=="LF"] <- "LF_Epost4"
names(HRV_Epost4)[names(HRV_Epost4)=="HF"] <- "HF_Epost4"
names(HRV_Epost4)[names(HRV_Epost4)=="LF (NORM)"] <- "LF (NORM)_Epost4"
names(HRV_Epost4)[names(HRV_Epost4)=="HF (NORM)"] <- "HF (NORM)_Epost4"
names(HRV_Epost4)[names(HRV_Epost4)=="LF/HF"] <- "LF/HF_Epost4"
names(HRV_Epost4)[names(HRV_Epost4)=="RSA"] <- "RSA_Epost4"
names(HRV_Epost4)[names(HRV_Epost4)=="SD1"] <- "SD1_Epost4"
names(HRV_Epost4)[names(HRV_Epost4)=="SD2"] <- "SD2_Epost4"
names(HRV_Epost4)[names(HRV_Epost4)=="SamplEn"] <- "SamplEn_Epost4"
names(HRV_Epost4)[names(HRV_Epost4)=="Correlation Dimension"] <- "Correlation Dimension_Epost4"
#drop columns for stage, timestamp, duration, time
HRV_Epost4 <- HRV_Epost4[,-1]
HRV_Epost4 <- HRV_Epost4[,-3]
HRV_Epost4 <- HRV_Epost4[,-4]
HRV_Epost4 <- HRV_Epost4[,-4]
HRV_Epost4 <- HRV_Epost4[,-23]

### No eve post 4 yet so did not join it

HRV_wide <- full_join(HRV_base, HRV_immed, by= c("subjectID", "sessionID"))
HRV_wide <- full_join(HRV_wide, HRV_post1, by= c("subjectID", "sessionID"))
HRV_wide <- full_join(HRV_wide, HRV_post2, by= c("subjectID", "sessionID"))
HRV_wide <- full_join(HRV_wide, HRV_post3, by= c("subjectID", "sessionID"))
HRV_wide <- full_join(HRV_wide, HRV_post4, by= c("subjectID", "sessionID"))
HRV_wide <- full_join(HRV_wide, HRV_Ebase, by= c("subjectID", "sessionID"))
HRV_wide <- full_join(HRV_wide, HRV_Eimmed, by= c("subjectID", "sessionID"))
HRV_wide <- full_join(HRV_wide, HRV_Epost1, by= c("subjectID", "sessionID"))
HRV_wide <- full_join(HRV_wide, HRV_Epost2, by= c("subjectID", "sessionID"))
HRV_wide <- full_join(HRV_wide, HRV_Epost3, by= c("subjectID", "sessionID"))
HRV_wide <- full_join(HRV_wide, HRV_Epost4, by= c("subjectID", "sessionID"))

#Add difference from basline column 

HRV_wide$RMSSD_immed_diff <- HRV_wide$RMSSD_immed - HRV_wide$RMSSD_base
HRV_wide$RMSSD_p1_diff <- HRV_wide$RMSSD_post1 - HRV_wide$RMSSD_base
HRV_wide$RMSSD_p2_diff <- HRV_wide$RMSSD_post2 - HRV_wide$RMSSD_base
HRV_wide$RMSSD_p3_diff <- HRV_wide$RMSSD_post3 - HRV_wide$RMSSD_base
HRV_wide$RMSSD_p4_diff <- HRV_wide$RMSSD_post4 - HRV_wide$RMSSD_base

HRV_wide$RMSSD_Eimmed_diff <- HRV_wide$RMSSD_immed - HRV_wide$RMSSD_Ebase
HRV_wide$RMSSD_Ep1_diff <- HRV_wide$RMSSD_Epost1 - HRV_wide$RMSSD_Ebase
HRV_wide$RMSSD_Ep2_diff <- HRV_wide$RMSSD_Epost2 - HRV_wide$RMSSD_Ebase
HRV_wide$RMSSD_Ep3_diff <- HRV_wide$RMSSD_Epost3 - HRV_wide$RMSSD_Ebase
HRV_wide$RMSSD_Ep4_diff <- HRV_wide$RMSSD_Epost4 - HRV_wide$RMSSD_Ebase


HRV_wide$SDNN_immed_diff <- HRV_wide$SDNN_immed - HRV_wide$SDNN_base
HRV_wide$SDNN_p1_diff <- HRV_wide$SDNN_post1 - HRV_wide$SDNN_base
HRV_wide$SDNN_p2_diff <- HRV_wide$SDNN_post2 - HRV_wide$SDNN_base
HRV_wide$SDNN_p3_diff <- HRV_wide$SDNN_post3 - HRV_wide$SDNN_base
HRV_wide$SDNN_p4_diff <- HRV_wide$SDNN_post4 - HRV_wide$SDNN_base

HRV_wide$SDNN_Eimmed_diff <- HRV_wide$SDNN_Eimmed - HRV_wide$SDNN_Ebase
HRV_wide$SDNN_Ep1_diff <- HRV_wide$SDNN_Epost1 - HRV_wide$SDNN_Ebase
HRV_wide$SDNN_Ep2_diff <- HRV_wide$SDNN_Epost2 - HRV_wide$SDNN_Ebase
HRV_wide$SDNN_Ep3_diff <- HRV_wide$SDNN_Epost3 - HRV_wide$SDNN_Ebase
HRV_wide$SDNN_Ep4_diff <- HRV_wide$SDNN_Epost4 - HRV_wide$SDNN_Ebase


#sample plots 
library(dplyr)
library(ggplot2)
library(lubridate)
b10_hrv <- filter(HRVprog, subjectID == "BIKE0010" & sessionID == 1)
ggplot(b10_hrv, aes(x= time, y=RMSSD, color = stage, group = subjectID))+ geom_line() + geom_point() 
ggplot(b10_hrv, aes(x= stage, y=RMSSD, fill = stage))+ geom_bar(stat = "identity")

b10_S2_hrv <- filter(HRVprog, subjectID == "BIKE0010" & sessionID == 2)

ggplot(b10_S2_hrv, aes(x= stage, y=RMSSD, fill = stage))+ geom_bar(stat = "identity")


b07_S1_hrv <- filter(HRVprog, subjectID == "BIKE0007" & sessionID == 1)
ggplot(b07_S1_hrv, aes(x= stage, y=RMSSD, fill = stage))+ geom_bar(stat = "identity")

b1017_S1_hrv <- filter(HRVprog, subjectID == "BIKE1017" & sessionID == 1)
ggplot(b1017_S1_hrv, aes(x= stage, y=RMSSD, fill= stage))+ geom_bar(stat = "identity")

twoCpalette <- c("#FF6600", "#00CCFF", "#00CCFF", "#00CCFF", "#00CCFF", "#00CCFF","#FF6600", "#00CCFF","#00CCFF","#00CCFF","#00CCFF")

ggplot(b1017_S2_hrv, aes(x= stage, y=RMSSD, fill = stage))+ geom_bar(stat = "identity") + scale_fill_manual(values = twoCpalette) + ylab("RMSSD (ms)") + xlab("Stage") + ggtitle("BIKE1017 Session 2: HRV") + theme(legend.position = "none", axis.text.x=element_text(angle = 90 , size=18), axis.text.y= element_text(size=18), axis.title.x= element_text(size=18), axis.title.y= element_text(size=18), title= element_text(size = 18))
