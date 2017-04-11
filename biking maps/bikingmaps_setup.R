#Set up subject files to create maps and plot. These are the maps sent to the participants
# Author: Cara
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggmap)
#load file with cleaned hexoskin , bc, and PM2.5 data. 
#Files resides on labarchieves Biking Potential Inhaled Dose/Cleaned Files/pm_bc_hex
pmbc_all <- read.csv("all_2015_2016_data_as20170406.csv", stringsAsFactors = FALSE)
#load gps file. GPS file obtained directly from database and at 1 min intervals.
# File resides on labarchieves Biking Potential Inhaled Dose/GPS/ GPS files
gps_y1_2 <- read.delim("gps_y1_2_min", header=TRUE, sep="\t")
#rename columns
names(pmbc_all)[names(pmbc_all)=="timestamp"] <- "datetime1"

#match time variables
gps_y1_2$datetime1 <- as.character(gps_y1_2$begin)

#join data
pmbc_gps <- full_join(pmbc_all, gps_y1_2, by=c("subjectid", "datetime1"))

# Create cateorgies for BC data
# bc in ug/m3 

pmbc_gps$BC_cat [pmbc_gps$bc.clean2 <1 ] <- "<1"
pmbc_gps$BC_cat [pmbc_gps$bc.clean2 >=1 & pmbc_gps$bc.clean2 <=2] <- "1-2"
pmbc_gps$BC_cat [pmbc_gps$bc.clean2 >2 & pmbc_gps$bc.clean2 <=4] <- "2-4"
pmbc_gps$BC_cat [pmbc_gps$bc.clean2 >=4] <- ">4"

# bc dose in ng/min

pmbc_gps$BCdose_cat [ pmbc_gps$bcdose < 40] <- "0-40"
pmbc_gps$BCdose_cat [pmbc_gps$bcdose >= 40 & pmbc_gps$bcdose < 100] <- "40-100"
pmbc_gps$BCdose_cat [pmbc_gps$bcdose >= 100 & pmbc_gps$bcdose < 200] <- "100-200"
pmbc_gps$BCdose_cat [pmbc_gps$bcdose > 200] <- ">200"

#Create catorgories for minute ventilation data
pmbc_gps$MV_cat [pmbc_gps$adj.mv < 35] <- "3-35"
pmbc_gps$MV_cat [pmbc_gps$adj.mv >= 35 & pmbc_gps$adj.mv < 50] <- "35-50"
pmbc_gps$MV_cat [pmbc_gps$adj.mv >= 50 & pmbc_gps$adj.mv < 70] <- "50-70"
pmbc_gps$MV_cat [pmbc_gps$adj.mv >= 70 & pmbc_gps$adj.mv < 170 ] <- "70-170"


# cats for PM data
#PM in ug/m3 

pmbc_gps$PM_cat [pmbc_gps$pm.clean2 < 10] <- "<10"
pmbc_gps$PM_cat [pmbc_gps$pm.clean2 >= 10 & pmbc_gps$pm.clean2 <= 25] <- "10-25"
pmbc_gps$PM_cat [pmbc_gps$pm.clean2 > 25 & pmbc_gps$pm.clean2 <= 50] <- "25-50"
pmbc_gps$PM_cat [pmbc_gps$pm.clean2 > 50] <- ">50"

# pm dose ng/min cat 

pmbc_gps$PMdose_cat [pmbc_gps$pmdose < 200] <- "<200"
pmbc_gps$PMdose_cat [pmbc_gps$pmdose >= 200 & pmbc_gps$pmdose < 350] <- "200-350"
pmbc_gps$PMdose_cat [pmbc_gps$pmdose >= 350 & pmbc_gps$pmdose < 500] <- "350-500"
pmbc_gps$PMdose_cat [pmbc_gps$pmdose > 400] <- ">500"

# morning and evening rides. For mapping simpliciy, using 13:00 as the divide
pmbc_gps$time_short <- hour(pmbc_gps$datetime1)
pmbc_gps$tod [pmbc_gps$time_short < 13] <-"Morning"
pmbc_gps$tod [pmbc_gps$time_short >= 13] <-"Evening"
pmbc_gps$tod <- factor(pmbc_gps$tod, levels = c("Morning", "Evening"))

# create subset of data for a single participant pmbc_gps

bike1032 <- subset(pmbc_gps, subjectid== "BIKE1032")

#create sessions if subject did not already have their sessions marked (only pertains to a few subjects)
bike1032$session [bike1032$datetime1 >= "2017-01-06 05:12:00" & bike1032$datetime1 <= "2017-01-06 15:12:00" ] <- 1
bike1032$session [bike1032$datetime1 >= "2017-01-10 05:12:00" & bike1032$datetime1 <= "2017-01-11 15:12:00" ] <- 2
bike1032$session [bike1032$datetime1 >= "2017-01-16 05:12:00" & bike1032$datetime1 <= "2017-01-17 15:12:00" ] <- 3
bike1032$session [bike1032$datetime1 >= "2017-01-18 05:12:00" & bike1032$datetime1 <= "2017-01-19 15:12:00" ] <- 4
bike1032$session [bike1032$datetime1 >= "2017-01-25 05:12:00" & bike1032$datetime1 <= "2017-01-26 15:12:00" ] <- 5

#create a day variable for labeling during facet_wrap 
bike1032$day [bike1032$session == "1"] <-"1/06/17"
bike1032$day [bike1032$session == "2"] <-"1/10/17"
bike1032$day [bike1032$session == "3"] <-"1/16/17"
bike1032$day [bike1032$session == "4"] <-"1/18/17"
bike1032$day [bike1032$session == "5"] <-"1/25/17"

#subset a session to test map
bike1032_s1 <- subset(bike1032, session== "1")

#test map to adjust zoom and location
qmap("New Museum, NYC", zoom = 13, color= "color", legend = "right") + 
  geom_point(data = bike1032_s1, aes(longitude_avg, latitude_avg, color=PM_cat), size =2) + facet_wrap(tod ~day) +
  scale_color_manual(name = "PM2.5 (ug/m^3)" , values = c("#00FF00","#FFFF00", "#FF6600","#CC0000"), breaks= c("<10", "10-25", "25-50", ">50")) +
  theme(legend.title = element_text(size = 18), legend.text = element_text(size = 18), strip.text.x = element_text(size=16), strip.text.y = element_text(size = 16)) 

#write individual file for easier read into the R markdown file.
write.csv(bike1032, "bike1032.csv")


