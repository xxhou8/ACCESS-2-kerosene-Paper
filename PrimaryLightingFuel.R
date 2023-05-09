rm(list = ls())

library(foreign)
library(plm)
library(lmtest)
library(multiwayvcov)
library(xtable)
library(pastecs)
library(Hmisc)
library(readstata13)
library(plyr)
library(stargazer)
library(ifultools)
library(ordinal)
library(doBy)
library("dplyr")
library(car)
library(rgdal)
library(tmap)
library(tmaptools)
library("vcd")
library(formattable)
library(sp)
library(tidyverse)
library(ifultools)
library(Weighted.Desc.Stat)
library(gridExtra)
library(car)
library(GGally)
library(margins)
library(reshape2)


setwd("~/Dropbox/ACCESS-2 kerosene (Xiaoxue, Johannes)/Data")

access<-read.dta13("access2018_2018Dec31.dta")



#### HH Identifiers

#generating a new ID for household
access$ID<-group_indices(access,m1_q3_hhid)
access$id<-1
#rename region-code variables
access<-dplyr::rename(access, "state"=m1_q8_state_code, "district"=m1_q9_district_code, "village"=m1_q11_village_code)
#create a new column as round
access$round<-ifelse(access$year=="2018",1,0)


## Subset observations that report to have grid access
data_1<-subset(access, access$m2_q55_grid=="Yes")

####dependent variable (eliminate off-grid choices)
#data$'primary lighting'[data$m3_q83_light_main=="Kerosene Lamp/Lantern"]<-0
data_1$lighting<-ifelse(data_1$m3_q83_light_main=="Grid Electricity", 1,
                       ifelse(data_1$m3_q83_light_main=="Kerosene Lamp/Lantern", 2 , 3
                       ))
data_2<-subset(data_1,data_1$lighting==1|data_1$lighting==2)
data_2$prim_lighting<-ifelse(data_2$lighting==1,1,0)

## Subset households appear in both rounds
ID_rounds<-aggregate(data_2[c("round","id")],by=list(data_2$ID), sum, na.rm=TRUE)
ID_rounds<-dplyr::rename(ID_rounds, "ID"=Group.1,"sum_round"=round, "observations"=id)
data_2<-merge(data_2,ID_rounds, by="ID")

data<-subset(data_2, data_2$sum_round=="1" & data_2$observations=="2")


#### explanatory variables

#set a new column of electricity hours with missing values replaced by group mean 
data$m2_q70_elec_night_hrs[is.na(data$m2_q70_elec_night_hrs)] <- 
  ave(data$m2_q70_elec_night_hrs,
      data$village, 
      FUN=function(x)mean(x,na.rm = T))[is.na(data$m2_q70_elec_night_hrs)] 

data$'elec_hours_night'<-data$m2_q70_elec_night_hrs 

data$m2_q69_elec_hrs[is.na(data$m2_q69_elec_hrs)] <- 
  ave(data$m2_q69_elec_hrs,
      data$village, 
      FUN=function(x)mean(x,na.rm = T))[is.na(data$m2_q69_elec_hrs)] 

data$'elec_hours'<-data$m2_q69_elec_hrs

#set a new column as electricity hours(day)
data<-mutate(data,'elec_hours_day'=elec_hours-elec_hours_night)
data$elec_hours_day[data$elec_hours_day<0]<-0

#set a new column of electricity outage with missing values replaced by group mean 

# impute <- function(x, fun) { 
#   missing <- is.na(x) 
#   replace(x, missing, fun(x[!missing])) 
# } 
# ddply(data, ~ village, transform, m2_q71_elec_out_days = impute(m2_q71_elec_out_days,mean))
# ddply(data, .(village), impute(m2_q71_elec_out_days,mean))

data$m2_q71_elec_out_days[is.na(data$m2_q71_elec_out_days)] <- ave(data$m2_q71_elec_out_days, 
                                 data$village, 
                                 FUN=function(x)mean(x, 
                                                     na.rm = T))[is.na(data$m2_q71_elec_out_days)] 

data$'elec_outage'<-data$m2_q71_elec_out_days 


#### control variables

#create indicator variables for CASTE
data["Caste_SC_ST"] <- ifelse(data$m1_q25_caste == 1 |data$m1_q25_caste == 2, 1, 0)                                           
data["Caste_OBC"] <- ifelse(data$m1_q25_caste == 3, 1, 0)
data["Caste_General"] <- ifelse(data$m1_q25_caste == 4|data$m1_q25_caste == 5, 1, 0)
data<-dplyr::rename(data, "caste"=m1_q25_caste)

#create a new column as family size
data$'family_size'<-as.numeric(data$m1_q27_no_adults)+as.numeric(data$m1_q29_no_children)

#create a new column as decision maker's education level
data["Edu_NoFormalSchooling"] <- ifelse(data$m1_q23_edu == "No Formal Schooling", 1, 0)
data["Edu_UpTo5thStandard"] <- ifelse(data$m1_q23_edu == "Up to 5th Standard", 1, 0)
data["Edu_MoreThan5thStandard"] <- ifelse(data$m1_q23_edu == "Up to 10th Standard" | 
                                                 data$m1_q23_edu == "12th Standard or Diploma" | 
                                                 data$m1_q23_edu == "Graduate and Above", 1,0)
data<-dplyr::rename(data, "education"=m1_q23_edu)

# data$education<-0
# data$education[data$m1_q23_edu=="Up to 10th standard"|data$m1_q23_edu=="12th Standard or Diploma"]<-1
# data$education[data$m1_q23_edu=="Graduate orabove"]<-2

#create a column of gender 
data["decision_maker"]<-ifelse(data$m1_q38_decision_maker=="Male Household Head", 1, 0)
data["Decision_MaleHouseholdHead"] <- ifelse(data$m1_q38_decision_maker=="Male Household Head", 1, 0)

#create a column as satisfation
# data$satisfaction<-ifelse(data$m3_q85_light_main_satisfy=="Satisfied", 1,
#                        ifelse(data$m3_q85_light_main_satisfy=="Neutral", 2,
#                               ifelse(data$m3_q85_light_main_satisfy=="Unsatisfied", 3
#                               )))

data$satisfaction<-data$m3_q85_light_main_satisfy
data["Satisfied"]<-ifelse(data$satisfaction=="Satisfied",1,0)
data["Neutral"]<-ifelse(data$satisfaction=="Neutral",1,0)
data["Unsatisfied"]<-ifelse(data$satisfaction=="Unsatisfied",1,0)

#create a new column as logarithm of monthly expenditure and use village mean to replace NAs
# only 16 obs, won't cause much difference
#data$m1_q32_month_expenditure[(is.na(data$m1_q32_month_expenditure))]<- 0
data$m1_q32_month_expenditure[is.na(data$m1_q32_month_expenditure)]<-ave(data$m1_q32_month_expenditure, 
                                           data$village, 
                                           FUN=function(x)mean(x, 
                                                               na.rm = T))[is.na(data$m1_q32_month_expenditure)] 


# One observation(ID=796) has value 0 in second round, replace with first round expenditure value
data$m1_q32_month_expenditure[data$ID=="796"]<- rep(data$m1_q32_month_expenditure[data$ID=="796"][1],2)

data$'month_expenditure_log'<-log(data$m1_q32_month_expenditure+1)
#create a column as kerosene expense

data<-dplyr::rename(data,'PDS_kerosene'=m2_q61_4_kero_liter_PDS,
                    'PDS_price'=m2_q61_5_kero_price_PDS,'mkt_kerosene'=m2_q61_6_kero_liter_mkt,
                    'mkt_price'=m2_q61_7_kero_price_mkt,satisfaction=m3_q85_light_main_satisfy)


#replace price of 0 to village average mean
data$PDS_price[data$PDS_price==0]<-NA
data$PDS_price[is.na(data$PDS_price)]<-ave(data$PDS_price, 
                                             data$m1_q8_state, 
                                             FUN=function(x)mean(x, 
                                                                 na.rm = T))[is.na(data$PDS_price)] 

data$mkt_price[data$mkt_price==0]<-NA
data$mkt_price[is.na(data$mkt_price)]<-ave(data$mkt_price, 
                                                 data$m1_q8_state, 
                                                 FUN=function(x)mean(x, 
                                                                     na.rm = T))[is.na(data$mkt_price)] 

data$PDS_kerosene[is.na(data$PDS_kerosene)]<-0
data$mkt_kerosene[is.na(data$mkt_kerosene)]<-0


data["exp_kerosene"]<-data$PDS_kerosene*data$PDS_price+data$mkt_kerosene*data$mkt_price


#### subset a data frame with only useful columns
# data<-dplyr::select(data, ID,state,district,village,prim_lighting,
#                    elec_hours_day,elec_hours_night,elec_outage, caste,decision_maker,family_size,
#                    education,month_expenditure_log,PDS_kerosene,PDS_price,mkt_kerosene,
#                    mkt_price,round,satisfaction)
# dt<-data[,c("caste","decision_maker","family_size","prim_lighting",
#          "elec_hours_day","elec_hours_night","decision_maker","family_size",
#          "education","month_expenditure_log")]

data1<-subset(data,data$round=="0")
data2<-subset(data,data$round=="1")

#graphy distribution

#  create 2015 state-level st## Figure 1: household geoatistics
states_2015 <- data.frame(unique(data1$m1_q8_state))
states_2015["n"] <- 0
states_2015["kerosene"] <- NA
for (i in 1:6) {
   states_2015[i,2] <- sum(data1$m1_q8_state == states_2015[i,1], na.rm = TRUE) #n
   states_2015[i,3] <- sum(data1$prim_lighting=="0" & data1$m1_q8_state == states_2015[i,1], na.rm = TRUE) #gas uptake
 }

states_2015["% kerosene"] <- states_2015$kerosene*100/ states_2015$n

states_2015["ST_NAME"] <- properCase(as.character(unique(states_2015$unique.data1.m1_q8_state.)))

states_2015[states_2015=="West bengal"] <- "West Bengal"
states_2015[states_2015=="Uttar pradesh"] <- "Uttar Pradesh"
states_2015[states_2015=="Madhya pradesh"] <- "Madhya Pradesh"
states_2015[states_2015=="Odisha"] <- "Orissa"

colnames(states_2015) <- c("State", "n", "kerosene", "Kerosene (%)", "ST_NAME")

states_2015$text[states_2015$State=="WEST BENGAL"] <-  "WB"
states_2015$text[states_2015$State=="UTTAR PRADESH"] <- "UP"
states_2015$text[states_2015$State=="MADHYA PRADESH"] <- "MP"
states_2015$text[states_2015$State=="ODISHA"] <- "OD"
states_2015$text[states_2015$State=="BIHAR"] <- "BH"
states_2015$text[states_2015$State=="JHARKHAND"] <- "JK"

##### shape file data

# state shapefile
india_shape_2015 <- readOGR(dsn = "C:/Users/59634/Dropbox/ACCESS-2 kerosene (Xiaoxue, Johannes)/Data/map_data", layer = "INDIA") 
india_shape_2015@data <- left_join(india_shape_2015@data, states_2015, by = "ST_NAME", all = TRUE)
india_shape_partial_2015 <- india_shape_2015[!is.na(india_shape_2015@data$'Kerosene (%)'),]
#india_shape_partial_2018 <- india_shape[!is.na(india_shape_2015@data$'2018 Share of Kerosene'),]

map_2015 <- tm_shape(india_shape_2015) + 
  tm_polygons(col = "white") +
  tm_borders(col = "black") +
  tm_shape(india_shape_partial_2015) +
  tm_polygons("Kerosene (%)", palette = "Blues") + 
  tm_text("text",size=0.5)+
  tm_borders(col = "gray50", lwd = 0.5) + 
  tm_legend(scale = 1.4, aes.color = c(borders = "black"), legend.position = c("right", "top"))

##2018
states_2018 <- data.frame(unique(data2$m1_q8_state))
states_2018["n"] <- 0
states_2018["kerosene"] <- NA
for (i in 1:6) {
  states_2018[i,2] <- sum(data2$m1_q8_state == states_2018[i,1], na.rm = TRUE) #n
  states_2018[i,3] <- sum(data2$prim_lighting=="0" & data2$m1_q8_state == states_2018[i,1], na.rm = TRUE) #gas uptake
}

states_2018["% kerosene"] <- states_2018$kerosene*100/ states_2018$n

states_2018["ST_NAME"] <- properCase(as.character(unique(states_2018$unique.data2.m1_q8_state.)))

states_2018[states_2018=="West bengal"] <- "West Bengal"

states_2018[states_2018=="Uttar pradesh"] <- "Uttar Pradesh"
states_2018[states_2018=="Madhya pradesh"] <- "Madhya Pradesh"
states_2018[states_2018=="Odisha"] <- "Orissa"

colnames(states_2018) <- c("State", "n", "kerosene", "Kerosene (%)", "ST_NAME")


states_2018$text[states_2018$State=="WEST BENGAL"] <-  "WB"
states_2018$text[states_2018$State=="UTTAR PRADESH"] <- "UP"
states_2018$text[states_2018$State=="MADHYA PRADESH"] <- "MP"
states_2018$text[states_2018$State=="ODISHA"] <- "OD"
states_2018$text[states_2018$State=="BIHAR"] <- "BH"
states_2018$text[states_2018$State=="JHARKHAND"] <- "JK"

##### shape file data

# state shapefile
india_shape_2018 <- readOGR(dsn = "C:/Users/59634/Dropbox/ACCESS-2 kerosene (Xiaoxue, Johannes)/Data/map_data", layer = "INDIA") 
india_shape_2018@data <- left_join(india_shape_2018@data, states_2018, by = "ST_NAME", all = TRUE)
india_shape_partial_2018 <- india_shape_2018[!is.na(india_shape_2018@data$'Kerosene (%)'),]
#india_shape_partial_2018 <- india_shape[!is.na(india_shape_2015@data$'2018 Share of Kerosene'),]


map_2018 <- tm_shape(india_shape_2018) + 
  tm_polygons(col = "white") +
  tm_borders(col = "black") +
  tm_shape(india_shape_partial_2018) +
  tm_polygons("Kerosene (%)", palette = "Greens") + 
  tm_text("text",size=0.5)+
  tm_borders(col = "gray50", lwd = 0.5) + 
  tm_legend(scale = 1.4, aes.color = c(borders = "black"), legend.position = c("right", "top"))


pdf("C:/Users/59634/Dropbox/ACCESS-2 kerosene (Xiaoxue, Johannes)/Manuscript/Figures/Map_2015.pdf")
map_2015
dev.off()

pdf("C:/Users/59634/Dropbox/ACCESS-2 kerosene (Xiaoxue, Johannes)/Manuscript/Figures/Map_2018.pdf")
map_2018
dev.off()


## Generate States Summary table
states<-merge(states_2015[c("ST_NAME","n","Kerosene (%)")],
              states_2018[c("ST_NAME","Kerosene (%)")], by="ST_NAME")
states<-dplyr::rename(states, `2015 Share of Kerosene (%)`=`Kerosene (%).x`,
                      `N`=`n`, `2018 Share of Kerosene (%)`=`Kerosene (%).y`,
                      `State`=`ST_NAME`)
#states$`Improvements (%)`<-100*(states$`2015 Share of Kerosene (%)`-states$`2018 Share of Kerosene (%)`)/states$`2015 Share of Kerosene (%)`

states[,3]<-round(states[,3],0)
states[,4]<-round(states[,4],0)

states
stargazer(states, summary=FALSE, float=FALSE,
          out = "~/Dropbox/ACCESS-2 kerosene (Xiaoxue, Johannes)/Manuscript/Tables/states_improve.tex")



### Figure 2: Mosaic Plot of the proportion change in households primary lighting fuel choices

struct <- structable(~ prim_lighting + round, data = data)
struct<-as.table(struct)
struct <-struct/nrow(data)
struct<-percent(struct)
struct<-as.table(struct)
struct<-round(struct,2)

pdf("C:/Users/59634/Dropbox/ACCESS-2 kerosene (Xiaoxue, Johannes)/Manuscript/Figures/Mosaic.pdf")

mosaic(struct, data=data, pop=F, set_labels=list(round = c("2015", "2018"), prim_lighting=c("kerosene","electricity")),
       labeling_args = list(set_varnames = list(prim_lighting = "Primary Lighting Fuel", round = "Round")))
labeling_cells(text = struct, margin = 0)(struct)

dev.off()


### Figure 3: Satisfaction with lighting fuel

#2015/2018 Satisfaction table

light_satisf <- c("Satisfied","Neutral","Unsatisfied")
satisf_2015 = data.frame(matrix(NA, length(light_satisf),2))
colnames(satisf_2015) = c(paste("Electricity (n=", sum(data$prim_lighting==1 & data$round==0),")", sep=""),
                                paste("Kerosene (n=", sum(data$prim_lighting==0 & data$round==0),")", sep=""))
rownames(satisf_2015) = light_satisf

satisf_2015[1,1] =  mean(data$Satisfied[data$prim_lighting==1 & data$round==0], na.rm=TRUE)
satisf_2015[2,1] =  mean(data$Neutral[data$prim_lighting==1 & data$round==0], na.rm=TRUE)
satisf_2015[3,1] =  mean(data$Unsatisfied[data$prim_lighting==1 & data$round==0], na.rm=TRUE)
satisf_2015[1,2] =  mean(data$Satisfied[data$prim_lighting==0 & data$round==0], na.rm=TRUE)
satisf_2015[2,2] =  mean(data$Neutral[data$prim_lighting==0 & data$round==0], na.rm=TRUE)
satisf_2015[3,2] =  mean(data$Unsatisfied[data$prim_lighting==0 & data$round==0], na.rm=TRUE)

satisf_2018 = data.frame(matrix(NA, length(light_satisf),2))
colnames(satisf_2018) = c(paste("Electricity (n=", sum(data2$prim_lighting==1),")", sep=""),
                          paste( "Kerosene (n=", sum(data2$prim_lighting==0),")", sep=""))
rownames(satisf_2018) = light_satisf

satisf_2018[1,1] =  mean(data$Satisfied[data$prim_lighting==1& data$round==1], na.rm=TRUE)
satisf_2018[2,1] =  mean(data$Neutral[data$prim_lighting==1& data$round==1], na.rm=TRUE)
satisf_2018[3,1] =  mean(data$Unsatisfied[data$prim_lighting==1& data$round==1], na.rm=TRUE)
satisf_2018[1,2] =  mean(data$Satisfied[data$prim_lighting==0& data$round==1], na.rm=TRUE)
satisf_2018[2,2] =  mean(data$Neutral[data$prim_lighting==0& data$round==1], na.rm=TRUE)
satisf_2018[3,2] =  mean(data$Unsatisfied[data$prim_lighting==0& data$round==1], na.rm=TRUE)

satisf_2015
satisf_2018

### 0714
satisf_per = data.frame(matrix(NA, length(light_satisf),4))
colnames(satisf_per) = c("2015k","2018k","2015e","2018e")
rownames(satisf_per) = light_satisf

satisf_per[,1]<-satisf_2015[,2]
satisf_per[,2]<-satisf_2018[,2]
satisf_per[,3]<-satisf_2015[,1]
satisf_per[,4]<-satisf_2018[,1]

a1 <- cbind(satisf_per[, 1], 1, c(1:3))
b1 <- cbind(satisf_per[, 2], 2, c(1:3))
c1 <- cbind(satisf_per[, 3], 3, c(1:3))
d1 <- cbind(satisf_per[, 4], 4, c(1:3))

satisf_per <- as.data.frame(rbind(a1, b1, c1, d1))
satisf_per$Fuel<-c("Kerosene","Kerosene","Kerosene","Kerosene","Kerosene","Kerosene","Electricity","Electricity","Electricity","Electricity","Electricity","Electricity")
colnames(satisf_per) <-c("Proportion", "YearFuel", "Satisfaction","Fuel")
satisf_per$Satisfaction<-factor(satisf_per$Satisfaction,labels=c("Satisfied","Neutral","Unsatisfied"))
#satisf_per$YearFuel<-factor(satisf_per$YearFuel,labels=c("2015 ", "2018 ","2015","2018"))
satisf_per$YearFuel<-c("2015","2015","2015","2018","2018","2018","2015","2015","2015","2018","2018","2018")


satis1<-ggplot(satisf_per, aes(x=YearFuel, y=Proportion, fill=Satisfaction)) +
  geom_bar(stat="identity", position="stack")+
  facet_grid(~Fuel)+
  geom_text(aes(label=satisf_per$Satisfaction),position=position_stack(0.5), size=2.5)+
  xlab("")+
  #annotate("text", x=1, y=-0.04, label= "2015",size=3) +
  #annotate("text", x=2, y=-0.04, label= "2018",size=3) +
  #annotate("text", x=3, y=-0.04, label= "2015",size=3) +
  #annotate("text", x=4, y=-0.04, label= "2018",size=3) +
  #annotate("text", x=1.5, y=-0.08, label= "Kerosene",size=3) +
  #annotate("text", x=3.5, y=-0.08, label= "Electricity",size=3) +
  #coord_cartesian(ylim = c(0, 1), expand = FALSE, clip = "off") +
  ylab("Proportion")+
  scale_fill_manual(values=c("#edf8fb", "#b2e2e2", "#66c2a4", "#238b45"))+
  theme_bw()+
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        axis.title.x = element_blank(),
        #axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none")

satisf_pop = data.frame(matrix(NA, length(light_satisf),4))
colnames(satisf_pop) = c("2015k","2018k","2015e","2018e")
rownames(satisf_pop) = light_satisf

satisf_pop[,1]<-satisf_2015[,2]*sum(data1$prim_lighting==0)
satisf_pop[,2]<-satisf_2018[,2]*sum(data2$prim_lighting==0)
satisf_pop[,3]<-satisf_2015[,1]*sum(data1$prim_lighting==1)
satisf_pop[,4]<-satisf_2018[,1]*sum(data2$prim_lighting==1)

satisf_pop

a2 <- cbind(satisf_pop[, 1], 1, c(1:3))
b2 <- cbind(satisf_pop[, 2], 2, c(1:3))
c2 <- cbind(satisf_pop[, 3], 3, c(1:3))
d2 <- cbind(satisf_pop[, 4], 4, c(1:3))

satisf_pop <- as.data.frame(rbind(a2, b2, c2, d2))
satisf_pop$Fuel<-c("Kerosene","Kerosene","Kerosene","Kerosene","Kerosene","Kerosene","Electricity","Electricity","Electricity","Electricity","Electricity","Electricity")
colnames(satisf_pop) <-c("Population", "YearFuel", "Satisfaction","Fuel")
satisf_pop$Satisfaction<-factor(satisf_pop$Satisfaction,labels=c("Satisfied","Neutral","Unsatisfied"))
#satisf_pop[satisf_pop$Fuel=="Kerosene",]$YearFuel<-factor(satisf_pop[satisf_pop$Fuel=="Kerosene",]$YearFuel,labels=c("2015","2018"))
satisf_pop$YearFuel<-c("2015","2015","2015","2018","2018","2018","2015","2015","2015","2018","2018","2018")

satis2<-ggplot(satisf_pop, aes(x=YearFuel, y=Population, fill=Satisfaction)) +
  geom_bar(stat="identity", position = 'stack')+
  facet_grid(~Fuel)+
  geom_text(aes(label=satisf_per$Satisfaction),position=position_stack(0.5), size=2.5)+
  xlab("YearFuel")+
  #annotate("text", x=1, y=-180, label= "2015",size=3) +
  #annotate("text", x=2, y=-180, label= "2018",size=3) +
  #annotate("text", x=3, y=-180, label= "2015",size=3) +
  #annotate("text", x=4, y=-180, label= "2018",size=3) +
  #annotate("text", x=1.5, y=-370, label= "Kerosene",size=3) +
  #annotate("text", x=3.5, y=-370, label= "Electricity",size=3) +
  #coord_cartesian(ylim = c(0, 1), expand = FALSE, clip = "off") +
  ylab("Population")+
  scale_fill_manual(values=c("#edf8fb", "#b2e2e2", "#66c2a4", "#238b45"))+
  theme_bw()+
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        axis.title.x = element_blank(),
        #axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none")


pdf("C:/Users/59634/Dropbox/ACCESS-2 kerosene (Xiaoxue, Johannes)/Manuscript/Figures/Satisfaction2.pdf")
grid.arrange(satis2,satis1, nrow=1)
dev.off()

# Swinging population 

data_swing<-merge(data1[c("ID","village","m1_q8_state","round","prim_lighting","satisfaction",
                     "m3_q84_1_light_main_adequate","m3_q84_2_light_main_reliable",
                     "m3_q84_3_light_main_expensive","m3_q84_4_light_main_safe",
                     "elec_hours_night","elec_hours_day","elec_outage",
                     "month_expenditure_log","m1_q32_month_expenditure",
                     "Caste_SC_ST","Caste_OBC","Caste_General",
                     "Decision_MaleHouseholdHead","family_size",
                     "Edu_NoFormalSchooling","Edu_UpTo5thStandard","Edu_MoreThan5thStandard")],
             data2[c("ID","village","m1_q8_state","round","prim_lighting","satisfaction",
                     "m3_q84_1_light_main_adequate","m3_q84_2_light_main_reliable",
                     "m3_q84_3_light_main_expensive","m3_q84_4_light_main_safe",
                     "elec_hours_night","elec_hours_day","elec_outage",
                     "month_expenditure_log","m1_q32_month_expenditure",
                     "Caste_SC_ST","Caste_OBC","Caste_General",
                     "Decision_MaleHouseholdHead","family_size",
                     "Edu_NoFormalSchooling","Edu_UpTo5thStandard","Edu_MoreThan5thStandard")], by="ID")

ktoe<-subset(data_swing, data_swing$prim_lighting.x==0 & data_swing$prim_lighting.y==1)
ktoe$pop<-"swinging population: kerosene to electricity"
# ktoe$satified<-length(ktoe$satisfaction.y[ktoe$satisfaction.y=="Satisfied"])
# ktoe$nuetral<-length(ktoe$satisfaction.y[ktoe$satisfaction.y=="Neutral"])
# ktoe$unsatified<-length(ktoe$satisfaction.y[ktoe$satisfaction.y=="Unsatisfied"])
# ktoe$reliable<-length(ktoe$m3_q84_2_light_main_reliable.y[ktoe$m3_q84_2_light_main_reliable.y=="Yes"])
# ktoe$unreliable<-length(ktoe$m3_q84_2_light_main_reliable.y[ktoe$m3_q84_2_light_main_reliable.y=="No"])
# ktoe$adequate<-length(ktoe$m3_q84_1_light_main_adequate.y[ktoe$m3_q84_1_light_main_adequate.y=="Yes"])
# ktoe$inadequate<-length(ktoe$m3_q84_1_light_main_adequate.y[ktoe$m3_q84_1_light_main_adequate.y=="No"])

# ktoe_unsatisfied<-subset(ktoe, ktoe$satisfaction.y=="Unsatisfied")
# ktoe_unsatisfied$unreliable<-length(ktoe_unsatisfied$m3_q84_2_light_main_reliable.y[ktoe_unsatisfied$m3_q84_2_light_main_reliable.y=="No"])/nrow(ktoe_unsatisfied)
# ktoe_unsatisfied$inadequate<-length(ktoe_unsatisfied$m3_q84_1_light_main_adequate.y[ktoe_unsatisfied$m3_q84_1_light_main_adequate.y=="No"])/nrow(ktoe_unsatisfied)


etok<-subset(data_swing, data_swing$prim_lighting.x==1 & data_swing$prim_lighting.y==0)
etok$pop<-"swinging population: electricity to kerosene"
# etok$satified<-length(etok$satisfaction.x[etok$satisfaction.x=="Satisfied"])
# etok$nuetral<-length(etok$satisfaction.x[etok$satisfaction.x=="Neutral"])
# etok$unsatified<-length(etok$satisfaction.x[etok$satisfaction.x=="Unsatisfied"])
# etok$reliable<-length(etok$m3_q84_2_light_main_reliable.x[etok$m3_q84_2_light_main_reliable.x=="Yes"])
# etok$unreliable<-length(etok$m3_q84_2_light_main_reliable.x[etok$m3_q84_2_light_main_reliable.x=="No"])
# etok$adequate<-length(etok$m3_q84_1_light_main_adequate.x[etok$m3_q84_1_light_main_adequate.x=="Yes"])
# etok$inadequate<-length(etok$m3_q84_1_light_main_adequate.x[etok$m3_q84_1_light_main_adequate.x=="No"])

# etok_unsatisfied<-subset(etok, etok$satisfaction.x=="Unsatisfied")
# etok_unsatisfied$unreliable<-length(etok_unsatisfied$m3_q84_2_light_main_reliable.x[etok_unsatisfied$m3_q84_2_light_main_reliable.x=="No"])/nrow(etok_unsatisfied)
# etok_unsatisfied$inadequate<-length(etok_unsatisfied$m3_q84_1_light_main_adequate.x[etok_unsatisfied$m3_q84_1_light_main_adequate.x=="No"])/nrow(etok_unsatisfied)


etoe<-subset(data_swing, data_swing$prim_lighting.x==1 & data_swing$prim_lighting.y==1)
etoe$pop<-"electricity user in both rounds"
ktok<-subset(data_swing, data_swing$prim_lighting.x==0 & data_swing$prim_lighting.y==0)
ktok$pop<-"kerosene user in both rounds"

swing<-rbind(ktoe,etok)
swing1<-swing[,1:23]
swing1<-dplyr::rename(swing1, "ID"=ID, "m1_q8_state"=m1_q8_state.x,"round"=round.x,"village"=village.x,"prim_lighting" =prim_lighting.x,  "satisfaction"= satisfaction.x,                
                      "m3_q84_1_light_main_adequate"=m3_q84_1_light_main_adequate.x, "m3_q84_2_light_main_reliable"=m3_q84_2_light_main_reliable.x, 
                      "m3_q84_3_light_main_expensive"=m3_q84_3_light_main_expensive.x,"m3_q84_4_light_main_safe" =m3_q84_4_light_main_safe.x,    
                      "elec_hours_night"=elec_hours_night.x, "elec_hours_day"=elec_hours_day.x, "elec_outage" =elec_outage.x, 
                      "month_expenditure_log"=month_expenditure_log.x, "m1_q32_month_expenditure"=m1_q32_month_expenditure.x,     
                      "Caste_SC_ST"=Caste_SC_ST.x, "Caste_OBC" =Caste_OBC.x, "Caste_General"=Caste_General.x,               
                      "Decision_MaleHouseholdHead"=Decision_MaleHouseholdHead.x,"family_size"=family_size.x,
                      "Edu_NoFormalSchooling"=Edu_NoFormalSchooling.x,"Edu_UpTo5thStandard"=Edu_UpTo5thStandard.x,"Edu_MoreThan5thStandard"=Edu_MoreThan5thStandard.x)
swing2<-swing[,c(1, 24:45)]
swing2<-dplyr::rename(swing2, "ID"=ID,"village"=village.y,"m1_q8_state"=m1_q8_state.y,"round"=round.y,"prim_lighting" =prim_lighting.y,  "satisfaction"= satisfaction.y,                
                      "m3_q84_1_light_main_adequate"=m3_q84_1_light_main_adequate.y, "m3_q84_2_light_main_reliable"=m3_q84_2_light_main_reliable.y, 
                      "m3_q84_3_light_main_expensive"=m3_q84_3_light_main_expensive.y,"m3_q84_4_light_main_safe" =m3_q84_4_light_main_safe.y,    
                      "elec_hours_night"=elec_hours_night.y, "elec_hours_day"=elec_hours_day.y, "elec_outage" =elec_outage.y, 
                      "month_expenditure_log"=month_expenditure_log.y, "m1_q32_month_expenditure"=m1_q32_month_expenditure.y,     
                      "Caste_SC_ST"=Caste_SC_ST.y, "Caste_OBC" =Caste_OBC.y, "Caste_General"=Caste_General.y,               
                      "Decision_MaleHouseholdHead"=Decision_MaleHouseholdHead.y,"family_size"=family_size.y,
                      "Edu_NoFormalSchooling"=Edu_NoFormalSchooling.y,"Edu_UpTo5thStandard"=Edu_UpTo5thStandard.y,"Edu_MoreThan5thStandard"=Edu_MoreThan5thStandard.y)


swing<-rbind(swing1,swing2)

# swingtable<-data.frame(matrix(NA, 2,3))
# colnames(swingtable)<-c("satisfaction of electricity","adequacy of electricity","reliability of electricity")
# rownames(swingtable)<-c("swinging population: kerosene to electricity","swinging population: electricity to kerosene")
   
# swingtable[1,1]<-length(ktoe$satisfaction.y[ktoe$satisfaction.y=="Satisfied"])/nrow(ktoe)
# swingtable[2,1]<-length(etok$satisfaction.x[etok$satisfaction.x=="Satisfied"])/nrow(etok)
# swingtable[1,2]<-length(ktoe$m3_q84_1_light_main_adequate.y[ktoe$m3_q84_1_light_main_adequate.y=="Yes"])/nrow(ktoe)
# swingtable[2,2]<-length(etok$m3_q84_1_light_main_adequate.x[etok$m3_q84_1_light_main_adequate.x=="Yes"])/nrow(etok)
# swingtable[1,3]<-length(ktoe$m3_q84_2_light_main_reliable.y[ktoe$m3_q84_2_light_main_reliable.y=="Yes"])/nrow(ktoe)
# swingtable[2,3]<-length(etok$m3_q84_2_light_main_reliable.x[etok$m3_q84_2_light_main_reliable.x=="Yes"])/nrow(etok)

####### Satisfaction Figure 0722
population<-data.frame(matrix(NA, 4,6))
colnames(population)<-c("2015Satisfied","2015Neutral","2015Unsatisfied","2018Satisfied","2018Neutral","2018Unsatisfied")
rownames(population)<-c("Electricity","Kerosene","Kerosene to Electricity","Electricity to Kerosene")

population[1,1]<-length(etoe$satisfaction.x[etoe$satisfaction.x=="Satisfied"])
population[1,2]<-length(etoe$satisfaction.x[etoe$satisfaction.x=="Neutral"])
population[1,3]<-length(etoe$satisfaction.x[etoe$satisfaction.x=="Unsatisfied"])
population[1,4]<-length(etoe$satisfaction.y[etoe$satisfaction.y=="Satisfied"])
population[1,5]<-length(etoe$satisfaction.y[etoe$satisfaction.y=="Neutral"])
population[1,6]<-length(etoe$satisfaction.y[etoe$satisfaction.y=="Unsatisfied"])
population[2,1]<-length(ktok$satisfaction.x[ktok$satisfaction.x=="Satisfied"])
population[2,2]<-length(ktok$satisfaction.x[ktok$satisfaction.x=="Neutral"])
population[2,3]<-length(ktok$satisfaction.x[ktok$satisfaction.x=="Unsatisfied"])
population[2,4]<-length(ktok$satisfaction.y[ktok$satisfaction.y=="Satisfied"])
population[2,5]<-length(ktok$satisfaction.y[ktok$satisfaction.y=="Neutral"])
population[2,6]<-length(ktok$satisfaction.y[ktok$satisfaction.y=="Unsatisfied"])
population[3,1]<-length(ktoe$satisfaction.x[ktoe$satisfaction.x=="Satisfied"])
population[3,2]<-length(ktoe$satisfaction.x[ktoe$satisfaction.x=="Neutral"])
population[3,3]<-length(ktoe$satisfaction.x[ktoe$satisfaction.x=="Unsatisfied"])
population[3,4]<-length(ktoe$satisfaction.y[ktoe$satisfaction.y=="Satisfied"])
population[3,5]<-length(ktoe$satisfaction.y[ktoe$satisfaction.y=="Neutral"])
population[3,6]<-length(ktoe$satisfaction.y[ktoe$satisfaction.y=="Unsatisfied"])
population[4,1]<-length(etok$satisfaction.x[etok$satisfaction.x=="Satisfied"])
population[4,2]<-length(etok$satisfaction.x[etok$satisfaction.x=="Neutral"])
population[4,3]<-length(etok$satisfaction.x[etok$satisfaction.x=="Unsatisfied"])
population[4,4]<-length(etok$satisfaction.y[etok$satisfaction.y=="Satisfied"])
population[4,5]<-length(etok$satisfaction.y[etok$satisfaction.y=="Neutral"])
population[4,6]<-length(etok$satisfaction.y[etok$satisfaction.y=="Unsatisfied"])

a3 <- cbind(population[, 1], 1, c(1:4))
b3 <- cbind(population[, 2], 2, c(1:4))
c3 <- cbind(population[, 3], 3, c(1:4))
d3 <- cbind(population[, 4], 4, c(1:4))
e3 <- cbind(population[, 5], 5, c(1:4))
f3 <- cbind(population[, 6], 6, c(1:4))

population<- as.data.frame(rbind(a3, b3, c3, d3, e3, f3))
colnames(population) <-c("Population", "Satisfaction","group")
population$Satisfaction<-c("Satisfied","Satisfied","Satisfied","Satisfied","Neutral","Neutral","Neutral","Neutral","Unsatisfied","Unsatisfied","Unsatisfied","Unsatisfied",
                           "Satisfied","Satisfied","Satisfied","Satisfied","Neutral","Neutral","Neutral","Neutral","Unsatisfied","Unsatisfied","Unsatisfied","Unsatisfied")
population$year<-c("2015","2015","2015","2015","2015","2015","2015","2015","2015","2015","2015","2015",
                   "2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018")
population$group<-factor(population$group,labels=c("Electricity","Kerosene","Kerosene to Electricity","Electricity to Kerosene"))
population$Satisfaction<-factor(population$Satisfaction, levels=c("Satisfied","Neutral","Unsatisfied"))

satis3<-ggplot(population, aes(x=year, y=Population, fill=Satisfaction)) +
  geom_bar(stat="identity", position = 'stack')+
  facet_grid(~group)+
  #geom_text(aes(label=population$Satisfaction),position=position_stack(0.5), size=1.5)+
  xlab("Year")+
  ylab("Population")+
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values=c( "#66c2a4", "#b2e2e2","#edf8fb"))+
  theme_bw()+
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        axis.title.x = element_blank(),
        #axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())



percentage<-data.frame(matrix(NA, 4,8))
colnames(percentage)<-c("2015Inadequacy","2015Unreliability","2015Unaffordability","2015Unsafety","2018Inadequacy","2018Unreliability","2018Unaffordability","2018Unsafety")
rownames(percentage)<-c("Electricity","Kerosene","Kerosene to Electricity","Electricity to Kerosene")

percentage[1,2]<-length(etoe$m3_q84_2_light_main_reliable.x[etoe$m3_q84_2_light_main_reliable.x=="No"])/nrow(etoe)
percentage[1,1]<-length(etoe$m3_q84_1_light_main_adequate.x[etoe$m3_q84_1_light_main_adequate.x=="No"])/nrow(etoe)
percentage[1,3]<-length(etoe$m3_q84_3_light_main_expensive.x[etoe$m3_q84_3_light_main_expensive.x=="No"])/nrow(etoe)
percentage[1,4]<-length(etoe$m3_q84_4_light_main_safe.x[etoe$m3_q84_4_light_main_safe.x=="No"])/nrow(etoe)
percentage[1,6]<-length(etoe$m3_q84_2_light_main_reliable.y[etoe$m3_q84_2_light_main_reliable.y=="No"])/nrow(etoe)
percentage[1,5]<-length(etoe$m3_q84_1_light_main_adequate.y[etoe$m3_q84_1_light_main_adequate.y=="No"])/nrow(etoe)
percentage[1,7]<-length(etoe$m3_q84_3_light_main_expensive.y[etoe$m3_q84_3_light_main_expensive.y=="No"])/nrow(etoe)
percentage[1,8]<-length(etoe$m3_q84_4_light_main_safe.y[etoe$m3_q84_4_light_main_safe.y=="No"])/nrow(etoe)

percentage[2,2]<-length(ktok$m3_q84_2_light_main_reliable.x[ktok$m3_q84_2_light_main_reliable.x=="No"])/nrow(ktok)
percentage[2,1]<-length(ktok$m3_q84_1_light_main_adequate.x[ktok$m3_q84_1_light_main_adequate.x=="No"])/nrow(ktok)
percentage[2,3]<-length(ktok$m3_q84_3_light_main_expensive.x[ktok$m3_q84_3_light_main_expensive.x=="No"])/nrow(ktok)
percentage[2,4]<-length(ktok$m3_q84_4_light_main_safe.x[ktok$m3_q84_4_light_main_safe.x=="No"])/nrow(ktok)
percentage[2,6]<-length(ktok$m3_q84_2_light_main_reliable.y[ktok$m3_q84_2_light_main_reliable.y=="No"])/nrow(ktok)
percentage[2,5]<-length(ktok$m3_q84_1_light_main_adequate.y[ktok$m3_q84_1_light_main_adequate.y=="No"])/nrow(ktok)
percentage[2,7]<-length(ktok$m3_q84_3_light_main_expensive.y[ktok$m3_q84_3_light_main_expensive.y=="No"])/nrow(ktok)
percentage[2,8]<-length(ktok$m3_q84_4_light_main_safe.y[ktok$m3_q84_4_light_main_safe.y=="No"])/nrow(ktok)

percentage[3,2]<-length(ktoe$m3_q84_2_light_main_reliable.x[ktoe$m3_q84_2_light_main_reliable.x=="No"])/nrow(ktoe)
percentage[3,1]<-length(ktoe$m3_q84_1_light_main_adequate.x[ktoe$m3_q84_1_light_main_adequate.x=="No"])/nrow(ktoe)
percentage[3,3]<-length(ktoe$m3_q84_3_light_main_expensive.x[ktoe$m3_q84_3_light_main_expensive.x=="No"])/nrow(ktoe)
percentage[3,4]<-length(ktoe$m3_q84_4_light_main_safe.x[ktoe$m3_q84_4_light_main_safe.x=="No"])/nrow(ktoe)
percentage[3,6]<-length(ktoe$m3_q84_2_light_main_reliable.y[ktoe$m3_q84_2_light_main_reliable.y=="No"])/nrow(ktoe)
percentage[3,5]<-length(ktoe$m3_q84_1_light_main_adequate.y[ktoe$m3_q84_1_light_main_adequate.y=="No"])/nrow(ktoe)
percentage[3,7]<-length(ktoe$m3_q84_3_light_main_expensive.y[ktoe$m3_q84_3_light_main_expensive.y=="No"])/nrow(ktoe)
percentage[3,8]<-length(ktoe$m3_q84_4_light_main_safe.y[ktoe$m3_q84_4_light_main_safe.y=="No"])/nrow(ktoe)


percentage[4,2]<-length(etok$m3_q84_2_light_main_reliable.x[etok$m3_q84_2_light_main_reliable.x=="No"])/nrow(etok)
percentage[4,1]<-length(etok$m3_q84_1_light_main_adequate.x[etok$m3_q84_1_light_main_adequate.x=="No"])/nrow(etok)
percentage[4,3]<-length(etok$m3_q84_3_light_main_expensive.x[etok$m3_q84_3_light_main_expensive.x=="No"])/nrow(etok)
percentage[4,4]<-length(etok$m3_q84_4_light_main_safe.x[etok$m3_q84_4_light_main_safe.x=="No"])/nrow(etok)
percentage[4,6]<-length(etok$m3_q84_2_light_main_reliable.y[etok$m3_q84_2_light_main_reliable.y=="No"])/nrow(etok)
percentage[4,5]<-length(etok$m3_q84_1_light_main_adequate.y[etok$m3_q84_1_light_main_adequate.y=="No"])/nrow(etok)
percentage[4,7]<-length(etok$m3_q84_3_light_main_expensive.y[etok$m3_q84_3_light_main_expensive.y=="No"])/nrow(etok)
percentage[4,8]<-length(etok$m3_q84_4_light_main_safe.y[etok$m3_q84_4_light_main_safe.y=="No"])/nrow(etok)

a4 <- cbind(percentage[, 1], 1, c(1:4))
b4 <- cbind(percentage[, 2], 2, c(1:4))
c4 <- cbind(percentage[, 3], 3, c(1:4))
d4 <- cbind(percentage[, 4], 4, c(1:4))
e4 <- cbind(percentage[, 5], 5, c(1:4))
f4 <- cbind(percentage[, 6], 6, c(1:4))
g4 <- cbind(percentage[, 7], 7, c(1:4))
h4 <- cbind(percentage[, 8], 8, c(1:4))

percentage<- as.data.frame(rbind(a4, b4, c4, d4, e4, f4, g4, h4))
colnames(percentage) <-c("Percentage", "Factors","Group")
percentage$Factors<-c("Inadequate","Inadequate","Inadequate","Inadequate","Unreliable","Unreliable","Unreliable","Unreliable",
                      "Unaffordable","Unaffordable","Unaffordable","Unaffordable","Unsafe","Unsafe","Unsafe","Unsafe",
                      "Inadequate","Inadequate","Inadequate","Inadequate","Unreliable","Unreliable","Unreliable","Unreliable",
                      "Unaffordable","Unaffordable","Unaffordable","Unaffordable","Unsafe","Unsafe","Unsafe","Unsafe")
percentage$year<-c("2015","2015","2015","2015","2015","2015","2015","2015","2015","2015","2015","2015","2015","2015","2015","2015",
                   "2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018","2018")
percentage$Group<-factor(percentage$Group,labels=c("Electricity","Kerosene","Kerosene to Electricity","Electricity to Kerosene"))


#population$Satisfaction<-factor(population$Satisfaction, levels=c("Satisfied","Neutral","Unsatisfied"))

satis4<-ggplot(percentage, aes(x=Factors, y=Percentage)) +
  geom_bar(stat="identity", width=0.5, fill=c("#66c2a5","#fc8d62","#8da0cb","#e78ac3","#66c2a5","#fc8d62","#8da0cb","#e78ac3",
                                              "#66c2a5","#fc8d62","#8da0cb","#e78ac3","#66c2a5","#fc8d62","#8da0cb","#e78ac3",
                                              "#66c2a5","#fc8d62","#8da0cb","#e78ac3","#66c2a5","#fc8d62","#8da0cb","#e78ac3",
                                              "#66c2a5","#fc8d62","#8da0cb","#e78ac3","#66c2a5","#fc8d62","#8da0cb","#e78ac3"))+
  facet_grid(year ~ Group)+
  ylab("Proportion")+
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0,0.7))+
  scale_x_discrete(expand = c(0, 0)) +
  theme_bw()+
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=45, hjust=1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.spacing = unit(1, "lines"),
        legend.position = "none")

pdf("C:/Users/59634/Dropbox/ACCESS-2 kerosene (Xiaoxue, Johannes)/Manuscript/Figures/Satisfaction1.pdf")
satis3
dev.off()

pdf("C:/Users/59634/Dropbox/ACCESS-2 kerosene (Xiaoxue, Johannes)/Manuscript/Figures/Satisfaction2.pdf")
satis4
dev.off()

# swingtable_ele_unsat<-data.frame(matrix(NA, 2,2))
# colnames(swingtable_ele_unsat)<-c("ratio of inadequacy","ratio of unreliablity")
# rownames(swingtable_ele_unsat)<-c("kerosene to electricity","electricity to kerosene")
# 
# 
# swingtable_ele_unsat[1,2]<-length(ktoe_unsatisfied$m3_q84_2_light_main_reliable.y[ktoe_unsatisfied$m3_q84_2_light_main_reliable.y=="No"])/nrow(ktoe_unsatisfied)
# swingtable_ele_unsat[1,1]<-length(ktoe_unsatisfied$m3_q84_1_light_main_adequate.y[ktoe_unsatisfied$m3_q84_1_light_main_adequate.y=="No"])/nrow(ktoe_unsatisfied)
# swingtable_ele_unsat[2,1]<-length(etok_unsatisfied$m3_q84_1_light_main_adequate.x[etok_unsatisfied$m3_q84_1_light_main_adequate.x=="No"])/nrow(etok_unsatisfied)
# swingtable_ele_unsat[2,2]<-length(etok_unsatisfied$m3_q84_2_light_main_reliable.x[etok_unsatisfied$m3_q84_2_light_main_reliable.x=="No"])/nrow(etok_unsatisfied)


#2015/2018 Satisfaction plot

satisf_2015_plot <- satisf_2015
satisf_2015_plot$satisfaction <- light_satisf
satisf_2015_melt <- melt(satisf_2015_plot)

light_satisf_2015_plot <- ggplot(satisf_2015_melt, aes(x = satisfaction, y = value, fill = variable)) +   # Fill column
  geom_bar(stat = "identity", position = "dodge", width = .6) +   # draw the bars
  ylim(0,0.6) + 
  scale_x_discrete(limits=light_satisf) + 
  theme_gray() +  # Tufte theme from ggfortify
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank()) +   # Centre plot title
  ggtitle("2015")+
  scale_fill_manual(values=c("darkolivegreen2", "darkolivegreen4")) + # Color palette
  xlab("") + ylab("Proportion") + guides(fill=guide_legend(title="Primary Lighting Fuel (2015)")) + 
  theme(plot.title = element_text(size=22),
        axis.text = element_text(size=18),
        axis.text.x = element_text(size=16, angle=45, hjust=1),
        axis.title = element_text(size=20), 
        legend.text = element_text(size=18),
        legend.title = element_text(size=18))

light_satisf_2015_plot

satisf_2018_plot <- satisf_2018
satisf_2018_plot$satisfaction <- light_satisf
satisf_2018_melt <- melt(satisf_2018_plot)

light_satisf_2018_plot <- ggplot(satisf_2018_melt, aes(x = satisfaction, y = value, fill = variable)) +   # Fill column
  geom_bar(stat = "identity", position = "dodge", width = .6) +   # draw the bars
  scale_y_continuous() + 
  scale_x_discrete(limits=light_satisf) + 
  theme_gray() +  # Tufte theme from ggfortify
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank()) +   # Centre plot title
  ggtitle("2018")+
  scale_fill_manual(values=c("darkolivegreen2", "darkolivegreen4")) + # Color palette
  xlab("") + ylab("Proportion") + guides(fill=guide_legend(title="Primary Lighting Fuel (2018)")) + 
  theme(plot.title = element_text(size=22),
        axis.text = element_text(size=18),
        axis.text.x = element_text(size=16, angle=45, hjust=1),
        axis.title = element_text(size=20), 
        legend.text = element_text(size=18),
        legend.title = element_text(size=18))

light_satisf_2018_plot

pdf("C:/Users/59634/Dropbox/ACCESS-2 kerosene (Xiaoxue, Johannes)/Manuscript/Figures/Satisfaction.pdf")
grid.arrange(light_satisf_2015_plot, light_satisf_2018_plot, nrow=2) # combine plots for final figure
dev.off()

### Figure 5: Kerosene Consumption

prim_ele_PDS <- ggplot(data2, aes(data2$PDS_price)) + 
  geom_density(aes(y=..count..),fill="black") + 
  ggtitle("(A) Kerosene Price from PDS with\n Electricity as Primary Lighting Fuel") + 
  xlim(c(0,100)) + ylim(c(0,400)) + 
  ylab("Number of households") + xlab("Kerosene purchased per month [liter]") + theme_gray() +
  geom_vline(aes(xintercept = mean(data2$PDS_price)), linetype="dashed", colour="grey20") + 
  theme(plot.title = element_text(size=12),
        axis.text = element_text(size=12),
        axis.title = element_text(size=12))
        
        
prim_ele_mkt <- ggplot(data2, aes(data2$mkt_price)) + 
  geom_density(aes(y=..count..), fill="black") + 
  ggtitle("(B) Kerosene Price from market\n with Electricity as Primary Fuel") + 
  xlim(c(0,100)) + ylim(c(0,400)) + 
  ylab("Number of households") + xlab("Kerosene purchased per month [liter]") + theme_gray() + 
  geom_vline(aes(xintercept = mean(data2$mkt_price)), linetype="dashed", colour="grey20") + 
  theme(plot.title = element_text(size=12),
        axis.text = element_text(size=12),
        axis.title = element_text(size=12))


prim_kero_PDS <- ggplot(data1, aes(data1$PDS_price)) + 
  geom_density(aes(y=..count..),fill="black") + 
  ggtitle("(C) Kerosene Price from PDS with\n Kerosene as Primary Lighting Fuel") + 
  xlim(c(0,100)) + ylim(c(0,400)) + 
  ylab("Number of households") + xlab("Kerosene price per liter") + theme_gray() +
  geom_vline(aes(xintercept = median(data1$PDS_price)), linetype="dashed", colour="grey20") + 
  theme(plot.title = element_text(size=12),
        axis.text = element_text(size=12),
        axis.title = element_text(size=12))


prim_kero_mkt <- ggplot(data1, aes(data1$mkt_price)) + 
  geom_density(aes(y=..count..), fill="black") + 
  ggtitle("(D) Kerosene Price from market with\n Kerosene as Primary Fuel") + 
  xlim(c(0,100)) + ylim(c(0,400)) + 
  ylab("Number of households") + xlab("Kerosene price per liter") + theme_gray() + 
  geom_vline(aes(xintercept = median(data1$mkt_price)), linetype="dashed", colour="grey20") + 
  theme(plot.title = element_text(size=12),
        axis.text = element_text(size=12),
        axis.title = element_text(size=12))

pdf("C:/Users/59634/Dropbox/ACCESS-2 kerosene (Xiaoxue, Johannes)/Manuscript/Figures/kerosene.pdf")

grid.arrange(prim_ele_PDS, prim_ele_mkt, prim_kero_PDS, prim_kero_mkt) # print figure
dev.off()


#### Descriptive Statistics

# 2015 explanary variables
sum_stats_2015 <- cbind(data1$prim_lighting,
                   data1$elec_hours_night,
                   data1$elec_hours_day,
                   data1$elec_outage)

sum_stats_output_2015 <- data.frame(matrix(data = NA,
                                      nrow = 4,
                                      ncol = 5))
for(i in 1:4) {
  #n
  sum_stats_output_2015[,5] = rep(nrow(sum_stats_2015), 4)
  #mean; sd; min and max
  sum_stats_output_2015[i,1] = signif(mean(sum_stats_2015[,i]), 3)
  sum_stats_output_2015[i,2] = signif(sd(sum_stats_2015[,i]), 3)
  sum_stats_output_2015[i,3] = signif(min(sum_stats_2015[,i]), 3)
  sum_stats_output_2015[i,4] = signif(max(sum_stats_2015[,i]), 3)
}
sum_stats_output_2015[,5] = format(sum_stats_output_2015[,5], big.mark=",")
colnames(sum_stats_output_2015) = c("Mean", "SD", "Min", "Max", "Observations")
rownames(sum_stats_output_2015) = c("Primary Lighting Electricity",
                               "Electricity Hours (Night)",
                               "Electricity Hours (Day)",
                               "Electricity Outage"
)
sum_stats_output_2015
stargazer(sum_stats_output_2015, summary=FALSE, float=FALSE,
          out = "C:/Users/59634/Dropbox/ACCESS-2 kerosene (Xiaoxue, Johannes)/Manuscript/Tables/sum_stats_2015.tex")



# 2015 control variables
sum_stats_controls_2015 <- cbind(data1$month_expenditure_log,
                            data1$m1_q32_month_expenditure,
                            data1$Caste_SC_ST,
                            data1$Caste_OBC,
                            data1$Caste_General,
                            data1$Decision_MaleHouseholdHead,
                            data1$family_size,
                            data1$Edu_NoFormalSchooling,
                            data1$Edu_UpTo5thStandard,
                            data1$Edu_MoreThan5thStandard
)
sum_stats_controls_output_2015 <- data.frame(matrix(data = NA,
                                               nrow = 10,
                                               ncol = 5))
for(i in 1:10) {
  #n
  sum_stats_controls_output_2015[,5] = rep(nrow(sum_stats_controls_2015), 10)
  #mean; sd; min and max
  sum_stats_controls_output_2015[i,1] = signif(mean(sum_stats_controls_2015[,i]), 3)
  sum_stats_controls_output_2015[1,2] = signif(sd(sum_stats_controls_2015[,1]),3)
  sum_stats_controls_output_2015[2,2] = signif(sd(sum_stats_controls_2015[,2]),3)
  sum_stats_controls_output_2015[7,2] = signif(sd(sum_stats_controls_2015[,7]),3)
  sum_stats_controls_output_2015[i,3] = signif(min(sum_stats_controls_2015[,i]), 3)
  sum_stats_controls_output_2015[i,4] = signif(max(sum_stats_controls_2015[,i]), 3)
}


sum_stats_controls_output_2015[,5] = format(sum_stats_controls_output_2015[,5], big.mark=",")
colnames(sum_stats_controls_output_2015) = c("Mean", "SD", "Min", "Max", "Observations")
rownames(sum_stats_controls_output_2015) = c("log(Monthly Expenditure)",
                                        "Monthly Expenditure",
                                        "Caste: SC/ST",
                                        "Caste: OBC",
                                        "Caste: General",
                                        "Male HouseholdHead",
                                        "Family Size",
                                        "Edu: NoFormalSchooling",
                                        "Edu: UpTo5thStandard",
                                        "Edu: MoreThan5thStandard"
)
sum_stats_controls_output_2015
stargazer(sum_stats_controls_output_2015, summary=FALSE, float=FALSE,
          out = "C:/Users/59634/Dropbox/ACCESS-2 kerosene (Xiaoxue, Johannes)/Manuscript/Tables/sum_stats_controls_2015.tex")


# 2018 explanary variables
sum_stats_2018 <- cbind(data2$prim_lighting,
                        data2$elec_hours_night,
                        data2$elec_hours_day,
                        data2$elec_outage)

sum_stats_output_2018 <- data.frame(matrix(data = NA,
                                           nrow = 4,
                                           ncol = 5))
for(i in 1:4) {
  #n
  sum_stats_output_2018[,5] = rep(nrow(sum_stats_2018), 4)
  #mean; sd; min and max
  sum_stats_output_2018[i,1] = signif(mean(sum_stats_2018[,i]), 3)
  sum_stats_output_2018[i,2] = signif(sd(sum_stats_2018[,i]), 3)
  sum_stats_output_2018[i,3] = signif(min(sum_stats_2018[,i]), 3)
  sum_stats_output_2018[i,4] = signif(max(sum_stats_2018[,i]), 3)
}
sum_stats_output_2018[,5] = format(sum_stats_output_2018[,5], big.mark=",")
colnames(sum_stats_output_2018) = c("Mean", "SD", "Min", "Max", "Observations")
rownames(sum_stats_output_2018) = c("Primary Lighting Electricity",
                                    "Electricity Hours (Night)",
                                    "Electricity Hours (Day)",
                                    "Electricity Outage"
)
sum_stats_output_2018
stargazer(sum_stats_output_2018, summary=FALSE, float=FALSE,
          out = "C:/Users/59634/Dropbox/ACCESS-2 kerosene (Xiaoxue, Johannes)/Manuscript/Tables/sum_stats_2018.tex")



#control variables
sum_stats_controls_2018 <- cbind(data2$month_expenditure_log,
                                 data2$m1_q32_month_expenditure,
                                 data2$Caste_SC_ST,
                                 data2$Caste_OBC,
                                 data2$Caste_General,
                                 data2$Decision_MaleHouseholdHead,
                                 data2$family_size,
                                 data2$Edu_NoFormalSchooling,
                                 data2$Edu_UpTo5thStandard,
                                 data2$Edu_MoreThan5thStandard
)
sum_stats_controls_output_2018 <- data.frame(matrix(data = NA,
                                                    nrow = 10,
                                                    ncol = 5))
for(i in 1:10) {
  #n
  sum_stats_controls_output_2018[,5] = rep(nrow(sum_stats_controls_2018), 10)
  #mean; sd; min and max
  sum_stats_controls_output_2018[i,1] = signif(mean(sum_stats_controls_2018[,i]), 3)
  sum_stats_controls_output_2018[1,2] = signif(sd(sum_stats_controls_2018[,1]), 3)
  sum_stats_controls_output_2018[2,2] = signif(sd(sum_stats_controls_2018[,2]), 3)
  sum_stats_controls_output_2018[7,2] = signif(sd(sum_stats_controls_2018[,7]), 3)
  sum_stats_controls_output_2018[i,3] = signif(min(sum_stats_controls_2018[,i]), 3)
  sum_stats_controls_output_2018[i,4] = signif(max(sum_stats_controls_2018[,i]), 3)
}


sum_stats_controls_output_2018[,5] = format(sum_stats_controls_output_2018[,5], big.mark=",")
colnames(sum_stats_controls_output_2018) = c("Mean", "SD", "Min", "Max", "Observations")
rownames(sum_stats_controls_output_2018) = c("log(Monthly Expenditure)",
                                              "Monthly Expenditure",
                                              "Caste: SC/ST",
                                              "Caste: OBC",
                                              "Caste: General",
                                              "Male HouseholdHead",
                                              "family size",
                                              "Edu: NoFormalSchooling",
                                              "Edu: UpTo5thStandard",
                                              "Edu: MoreThan5thStandard"
)
sum_stats_controls_output_2018
stargazer(sum_stats_controls_output_2018, summary=FALSE, float=FALSE,
          out = "C:/Users/59634/Dropbox/ACCESS-2 kerosene (Xiaoxue, Johannes)/Manuscript/Tables/sum_stats_controls_2018.tex")


#### Regression Analysis

vcovCL <- function(x, cluster.by, type="sss", dfcw=1){
  require(sandwich)
  cluster <- cluster.by
  M <- length(unique(cluster))   
  N <- length(cluster)
  stopifnot(N == length(x$residuals))
  K <- x$rank
  stopifnot(type=="sss")  
  if(type=="sss"){
    dfc <- (M/(M-1))*((N-1)/(N-K))
  }
  uj  <- apply(estfun(x), 2, function(y) tapply(y, cluster, sum))
  mycov <- dfc * sandwich(x, meat=crossprod(uj)/N) * dfcw
  return(mycov)
}

## Calculate standard error for odds ratios
get.or.se <- function(model) {
  broom::tidy(model) %>% 
    mutate(or = exp(estimate),
           var.diag = diag(vcov(model)),
           or.se = sqrt(or^2 * var.diag)) %>%
    select(or.se) %>% unlist %>% unname
}

model.1 <- glm(prim_lighting ~ elec_hours_night+elec_hours_day, family="binomial", data)
data.x1 <-data[,c("ID","village","prim_lighting")]
coef.1 <- coeftest(model.1, vcov=function(x) vcovCL(x, data.x1$village))
coef.1[,2]<-get.or.se(model.1)

model.2<-glm(prim_lighting ~ elec_hours_night+elec_hours_day+as.factor(m1_q8_state)+as.factor(round), family="binomial", data)
data.x2 <-data[,c("ID","village","prim_lighting")]
coef.2 <- coeftest(model.2, vcov=function(x) vcovCL(x, data.x2$village))
coef.2[,2]<-get.or.se(model.2)

model.3<- glm(prim_lighting ~ elec_outage, family="binomial", data)
data.x3 <-na.omit(data[,c("ID", "village",all.vars(formula(model.3)))])
coef.3 <- coeftest(model.3, vcov=function(x) vcovCL(x, data.x3$village))
coef.3[,2]<-get.or.se(model.3)

model.4<-glm(prim_lighting ~ elec_outage+as.factor(m1_q8_state)+as.factor(round), family="binomial", data)

data.x4 <-na.omit(data[,c("ID", "village",all.vars(formula(model.4)))])
coef.4 <- coeftest(model.4, vcov=function(x) vcovCL(x, data.x4$village))
coef.4[,2]<-get.or.se(model.4)

model.5<-glm(prim_lighting ~ elec_hours_night+elec_hours_day+elec_outage, family="binomial", data)
data.x5 <-na.omit(data[,c("ID", "village",all.vars(formula(model.5)))])
coef.5 <- coeftest(model.5, vcov=function(x) vcovCL(x, data.x5$village))
coef.5[,2]<-get.or.se(model.5)

model.6 <- glm(prim_lighting ~ elec_hours_night+elec_hours_day+elec_outage+as.factor(m1_q8_state)+as.factor(round),family="binomial",data)
data.x6 <- na.omit(data[ , c("ID","village", all.vars(formula(model.6)))])
coef.6 <- coeftest(model.6, vcov=function(x) vcovCL(x, data.x6$village))
coef.6[,2]<-get.or.se(model.6)

### Table 2

fit.list1 <- list(model.1,model.2,model.3,model.4,model.5,model.6)
#coef1 <- list(exp(coef.1[-1,1]),exp(coef.2[-1,1]),exp(coef.3[-1,1]),exp(coef.4[-1,1]),exp(coef.5[-1,1]),exp(coef.6[-1,1]))
coef1<-list(exp(coef.1[c(2,3),1]),exp(coef.2[c(2,3),1]),exp(coef.3[c(2),1]),exp(coef.4[-1,1]),exp(coef.5[c(2,3,4),1]),exp(coef.6[c(2,3,4),1]))
se1 <- list(coef.1[c(2,3),2],coef.2[c(2,3),2],coef.3[c(2),2],coef.4[-1,2],coef.5[c(2,3,4),2],coef.6[c(2,3,4),2])
pvals1<-list(coef.1[c(2,3),4],coef.2[c(2,3),4],coef.3[c(2),4],coef.4[-1,4],coef.5[c(2,3,4),4],coef.6[c(2,3,4),4])
cov.labs <- c("Electricity Hours (night)","Electricity Hours (day)","Electricity Outage")
omit.list <-c("ID", "JHARKHAND","MADHYA PRADESH","ODISHA","UTTAR PRADESH","WEST BENGA","round","Constant")
omit.stats <- c("rsq","ser","f")

sink("C:/Users/59634/Dropbox/ACCESS-2 kerosene (Xiaoxue, Johannes)/Manuscript/Tables/reg_results_explanatory.tex")
stargazer(title = "Primary Lighting Fuel Choice",
          coef=coef1,
          se = se1,
          header = F,
          fit.list1,
          p=pvals1,
          omit.stat = omit.stats,
          omit = omit.list,
          covariate.labels=cov.labs,
          no.space = T,
          suppress.errors = F,float = F,
          dep.var.labels = c("Primary Lighting Fuel (1= Electricity, 0=Kerosene)"),
          add.lines = list(c("Region FE?","No","Yes","No","Yes","No","Yes"),
                           c("Round FE?","No","Yes","No","Yes","No","Yes")))
sink()

#### regressions with controls
model.1b <- glm(prim_lighting ~ elec_hours_night+elec_hours_day
                +month_expenditure_log
                +Caste_SC_ST
                +Caste_OBC
                +Caste_General
                +decision_maker
                +family_size
                +Edu_MoreThan5thStandard
                +Edu_UpTo5thStandard
                +Edu_NoFormalSchooling,
                family=binomial,
                data = data)
data.x1b <- na.omit(data[ , c("ID", "village",
                              all.vars(formula(model.1b)))])
coef.1b <- coeftest(model.1b, vcov=function(x) vcovCL(x, data.x1b$village))
coef.1b[,2]<-get.or.se(model.1b)

model.2b <- glm(prim_lighting ~ elec_hours_night+elec_hours_day
                +as.factor(m1_q8_state)
                +as.factor(round)
                +month_expenditure_log
                +Caste_SC_ST
                +Caste_OBC
                +Caste_General
                +decision_maker
                +family_size
                +Edu_MoreThan5thStandard
                +Edu_UpTo5thStandard
                +Edu_NoFormalSchooling,
                family="binomial",
                data = data)
data.x2b <- na.omit(data[ , c("ID", "village",
                              all.vars(formula(model.2b)))])
coef.2b <- coeftest(model.2b, vcov=function(x) vcovCL(x, data.x2b$village))
coef.2b[,2]<-get.or.se(model.2b)

model.3b <- glm(prim_lighting ~ elec_outage
                +month_expenditure_log
                +Caste_SC_ST
                +Caste_OBC
                +Caste_General
                +decision_maker
                +family_size
                +Edu_MoreThan5thStandard
                +Edu_UpTo5thStandard
                +Edu_NoFormalSchooling,
                family="binomial",
                data = data)
data.x3b <- na.omit(data[ , c("ID", "village",
                              all.vars(formula(model.3b)))])
coef.3b <- coeftest(model.3b, vcov=function(x) vcovCL(x, data.x3b$village))
coef.3b[,2]<-get.or.se(model.3b)

model.4b <- glm(prim_lighting ~ elec_outage 
                +as.factor(m1_q8_state)
                +as.factor(round)
                +month_expenditure_log
                +Caste_SC_ST
                +Caste_OBC
                +Caste_General
                +decision_maker
                +family_size
                +Edu_MoreThan5thStandard
                +Edu_UpTo5thStandard
                +Edu_NoFormalSchooling,
                family="binomial",
                data = data)
data.x4b <- na.omit(data[ , c("ID", "village",
                              all.vars(formula(model.4b)))])
coef.4b <- coeftest(model.4b, vcov=function(x) vcovCL(x, data.x4b$village))
coef.4b[,2]<-get.or.se(model.4b)

model.5b <- glm(prim_lighting ~ elec_hours_night+elec_hours_day+elec_outage
                +month_expenditure_log
                +Caste_SC_ST
                +Caste_OBC
                #+Caste_General
                +decision_maker
                +family_size
                +Edu_MoreThan5thStandard
                +Edu_UpTo5thStandard
                #+Edu_NoFormalSchooling
                ,
                family="binomial",
                data = data)
data.x5b <- na.omit(data[ , c("ID", "village",
                              all.vars(formula(model.5b)))])
coef.5b <- coeftest(model.5b, vcov=function(x) vcovCL(x, data.x5b$village))
coef.5b[,2]<-get.or.se(model.5b)

model.6b <- glm(prim_lighting ~ elec_hours_night+elec_hours_day+elec_outage
                +as.factor(m1_q8_state)
                +as.factor(round)
                +month_expenditure_log
                +Caste_SC_ST
                +Caste_OBC
                #+Caste_General
                +decision_maker
                +family_size
                +Edu_MoreThan5thStandard
                +Edu_UpTo5thStandard
                #+Edu_NoFormalSchooling
                ,
                family="binomial",
                data = data)
data.x6b <- na.omit(data[ , c("ID", "village",
                              all.vars(formula(model.6b)))])
coef.6b <- coeftest(model.6b, vcov=function(x) vcovCL(x, data.x6b$village))

coef.6b[,2]<-get.or.se(model.6b)


### Table 3: table of models with controls
fit.list2 <- list(model.1b, model.2b,model.3b,model.4b,model.5b,model.6b)
#coef2<- list(exp(coef.1b[-1,1]),exp(coef.2b[-1,1]),exp(coef.3b[-1,1]),exp(coef.4b[-1,1]),exp(coef.5b[-1,1]),exp(coef.6b[-1,1]))
coef2<- list(exp(coef.1b[-1,1]),exp(coef.2b[-c(1,4,5,6,7,8,9),1]),exp(coef.3b[-1,1]),exp(coef.4b[-c(1,3,4,5,6,7,8),1]),exp(coef.5b[-1,1]),exp(coef.6b[-c(1,5,6,7,8,9,10),1]))
se2 <- list(coef.1b[-1,2],coef.2b[-c(1,4,5,6,7,8,9),2],coef.3b[-1,2],coef.4b[-c(1,3,4,5,6,7,8),2],coef.5b[-1,2],coef.6b[-c(1,5,6,7,8,9,10),2])
pvals2 <- list(coef.1b[-1,4],coef.2b[-c(1,4,5,6,7,8,9),4],coef.3b[-1,4],coef.4b[-c(1,3,4,5,6,7,8),4],coef.5b[-1,4],coef.6b[-c(1,5,6,7,8,9,10),4])

cov.labs2 <- c("Electricity Hours (night)","Electricity Hours (day)","Electricity Outage",
              "Monthly Expenditure (log)","Caste (SCST)","Caste (OBC)","Caste (General)",
              "Male Household Head","Family Size","Education (More than 5th Standard)",
              "Education (Up to 5th Standard)","Education(NoFormalSchooling)")

keep.list.controls <-c("Electricity Hours (night)","Electricity Hours (day)","Electricity Outage",
"Monthly Expenditure (log)","Caste (SCST)","Caste (OBC)",
"Male Household Head","Family Size","Education (More than 5th Standard)",
"Education (Up to 5th Standard)")

omit.list.controls <-c("ID","JHARKHAND","MADHYA PRADESH","ODISHA","UTTAR PRADESH","WEST BENGA","round","Constant")
omit.stats <- c("rsq","ser","f")

sink("C:/Users/59634/Dropbox/ACCESS-2 kerosene (Xiaoxue, Johannes)/Manuscript/Tables/reg_results_controls.tex")
stargazer(title = "Primary Lighting Fuel Choice",
          coef=coef2,
          se = se2,
          fit.list2,
          header = F,
          covariate.labels=cov.labs2,
          omit.stat = omit.stats,
          omit = omit.list.controls,
          p=pvals2,
          suppress.errors = F,float = F,no.space=T,
          dep.var.labels = c("Primary Lighting Fuel (1= Electricity, 0=Kerosene)"),
          add.lines = list(c("Region FE?","No","Yes","No","Yes","No","Yes"),
                           c("Round FE?","No","Yes","No","Yes","No","Yes")))
sink()




##### Appendix analysis:include off-grid lighting
data_3<-subset(access, access$m2_q68_elec=="Yes")

ID_rounds3<-aggregate(data_3[c("round","id")],by=list(data_3$ID), sum, na.rm=TRUE)
#ID_rounds3<-dplyr::rename(ID_rounds, "ID"=Group.1,"sum_round"=round, "observations"=id)
data_3<-merge(data_3,ID_rounds, by="ID")

data_3<-subset(data_3, data_3$sum_round=="1" & data_3$observations=="2")

data_3$prim_lighting<-ifelse(data_3$m3_q83_light_main=="Grid Electricity", 1,0)

data_3$'elec_hours_night'<-data_3$m2_q70_elec_night_hrs 
data_3$'elec_hours'<-data_3$m2_q69_elec_hrs
data_3<-mutate(data_3,'elec_hours_day'=elec_hours-elec_hours_night)
data_3$elec_hours_day[data_3$elec_hours_day<0]<-0
data_3$m2_q71_elec_out_days[is.na(data_3$m2_q71_elec_out_days)] <- ave(data_3$m2_q71_elec_out_days, 
                                                                   data_3$village, 
                                                                   FUN=function(x)mean(x, 
                                                                                       na.rm = T))[is.na(data_3$m2_q71_elec_out_days)] 
data_3$'elec_outage'<-data_3$m2_q71_elec_out_days 

# data3<-subset(data_3,data_3$round=="0")
# data4<-subset(data_3,data_3$round=="1")

model1 <- glm(prim_lighting ~ elec_hours_night+elec_hours_day, family="binomial", data_3)
data3.x1 <-data_3[,c("ID","village","prim_lighting")]
coef1 <- coeftest(model1, vcov=function(x) vcovCL(x, data3.x1$village))
coef1[,2]<-get.or.se(model1)

model2<-glm(prim_lighting ~ elec_hours_night+elec_hours_day+as.factor(m1_q8_state)+as.factor(round), family="binomial", data_3)
data3.x2 <-data_3[,c("ID","village","prim_lighting")]
coef2 <- coeftest(model2, vcov=function(x) vcovCL(x, data3.x2$village))
coef2[,2]<-get.or.se(model2)

model3<- glm(prim_lighting ~ elec_outage, family="binomial", data_3)
data3.x3 <-na.omit(data_3[,c("ID", "village",all.vars(formula(model3)))])
coef3 <- coeftest(model3, vcov=function(x) vcovCL(x, data3.x3$village))
coef3[,2]<-get.or.se(model3)

model4<-glm(prim_lighting ~ elec_outage+as.factor(m1_q8_state)+as.factor(round), family="binomial", data_3)
data3.x4 <-na.omit(data_3[,c("ID", "village",all.vars(formula(model4)))])
coef4 <- coeftest(model4, vcov=function(x) vcovCL(x, data3.x4$village))
coef4[,2]<-get.or.se(model4)

model5<-glm(prim_lighting ~ elec_hours_night+elec_hours_day+elec_outage, family="binomial", data_3)
data3.x5 <-na.omit(data_3[,c("ID", "village",all.vars(formula(model5)))])
coef5 <- coeftest(model5, vcov=function(x) vcovCL(x, data3.x5$village))
coef5[,2]<-get.or.se(model5)

model6 <- glm(prim_lighting ~ elec_hours_night+elec_hours_day+elec_outage+as.factor(m1_q8_state)+as.factor(round),family="binomial",data_3)
data3.x6 <- na.omit(data_3[ , c("ID","village", all.vars(formula(model6)))])
coef6 <- coeftest(model6, vcov=function(x) vcovCL(x, data3.x6$village))
coef6[,2]<-get.or.se(model6)

fitlist <- list(model1,model2,model3,model4,model5,model6)
#coef1 <- list(exp(coef.1[-1,1]),exp(coef.2[-1,1]),exp(coef.3[-1,1]),exp(coef.4[-1,1]),exp(coef.5[-1,1]),exp(coef.6[-1,1]))
coef<-list(exp(coef1[c(2,3),1]),exp(coef2[c(2,3),1]),exp(coef3[c(2),1]),exp(coef4[-1,1]),exp(coef5[c(2,3,4),1]),exp(coef6[c(2,3,4),1]))
se<- list(coef1[c(2,3),2],coef2[c(2,3),2],coef3[c(2),2],coef4[-1,2],coef5[c(2,3,4),2],coef6[c(2,3,4),2])
pvals<-list(coef1[c(2,3),4],coef2[c(2,3),4],coef3[c(2),4],coef4[-1,4],coef5[c(2,3,4),4],coef6[c(2,3,4),4])
covlabs <- c("Electricity Hours (night)","Electricity Hours (day)","Electricity Outage")
omitlist <-c("ID", "JHARKHAND","MADHYA PRADESH","ODISHA","UTTAR PRADESH","WEST BENGA","round","Constant")
omitstats <- c()

sink("C:/Users/59634/Dropbox/ACCESS-2 kerosene (Xiaoxue, Sean, Johannes)/Manuscript/Tables/reg_robust_explanatory.tex")
stargazer(title = "Primary Lighting Fuel Choice",
          coef=coef,
          se = se,
          header = F,
          fitlist,
          p=pvals,
          omit.stat = omitstats,
          omit = omitlist,
          covariate.labels=covlabs,
          no.space = T,
          suppress.errors = F,float = F,
          dep.var.labels = c("Primary Lighting Fuel (1= Electricity, 0=Kerosene)"),
          add.lines = list(c("Region FE?","No","Yes","No","Yes","No","Yes"),
                           c("Round FE?","No","Yes","No","Yes","No","Yes")))
sink()

## Marginal Effect 
x<-summary(margins(model.6b))
y<-x[c(7,6,8,15,2,1,3,4,5,9),]
y$factor<-c("Hours (night)", "Hours (day)", "Outage", "Monthly Expenditure (log)", "Caste (SC/ST)", "Caste (OBC)", "Male Household Head", 
            "Education (More than 5th Standard)", "Education (Up to 5th Standard)","Family Size")
y$factor<-factor(y$factor, levels=unique(y$factor))
colortable<-tibble(
  factor = c("Hours (night)", "Hours (day)", "Outage", "Monthly Expenditure (log)", "Caste (SC/ST)", "Caste (OBC)", "Male Household Head", 
             "Education (More than 5th Standard)", "Education (Up to 5th Standard)","Family Size"),
  color = c("red", "red","red", "grey", "grey", "grey", "grey","grey","grey","grey")
)
pdf("C:/Users/59634/Dropbox/ACCESS-2 kerosene (Xiaoxue, Johannes)/Manuscript/Figures/AME.pdf")
ggplot(data = y, aes(factor, AME)) +
  geom_point(aes(factor, AME), size=3, col="red") +
  #geom_bar(position=position_dodge(), stat="identity")+
  #geom_col() +
  #scale_fill_manual(values="red")+
  geom_errorbar(aes(x = factor, ymin = lower, ymax = upper), width=0.1, size=1) +
  geom_hline(yintercept = 0) +
  ylab("Average Marginal Effect")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,hjust=1))

dev.off()


#### SWING: regressions with controls
model.1c <- glm(prim_lighting ~ elec_hours_night+elec_hours_day
                +month_expenditure_log
                +Caste_SC_ST
                +Caste_OBC
                +Caste_General
                +Decision_MaleHouseholdHead
                +family_size
                +Edu_MoreThan5thStandard
                +Edu_UpTo5thStandard
                +Edu_NoFormalSchooling,
                family=binomial,
                data = swing)
data.x1c <- na.omit(swing[ , c("ID", "village",
                              all.vars(formula(model.1c)))])
coef.1c <- coeftest(model.1c, vcov=function(x) vcovCL(x, data.x1c$village))
coef.1c[,2]<-get.or.se(model.1c)

model.2c <- glm(prim_lighting ~ elec_hours_night+elec_hours_day
                +as.factor(m1_q8_state)
                +as.factor(round)
                +month_expenditure_log
                +Caste_SC_ST
                +Caste_OBC
                +Caste_General
                +Decision_MaleHouseholdHead
                +family_size
                +Edu_MoreThan5thStandard
                +Edu_UpTo5thStandard
                +Edu_NoFormalSchooling,
                family="binomial",
                data = swing)
data.x2c <- na.omit(swing[ , c("ID", "village",
                              all.vars(formula(model.2c)))])
coef.2c <- coeftest(model.2c, vcov=function(x) vcovCL(x, data.x2c$village))
coef.2c[,2]<-get.or.se(model.2c)

model.3c <- glm(prim_lighting ~ elec_outage
                +month_expenditure_log
                +Caste_SC_ST
                +Caste_OBC
                +Caste_General
                +Decision_MaleHouseholdHead
                +family_size
                +Edu_MoreThan5thStandard
                +Edu_UpTo5thStandard
                +Edu_NoFormalSchooling,
                family="binomial",
                data = swing)
data.x3c <- na.omit(swing[ , c("ID", "village",
                              all.vars(formula(model.3c)))])
coef.3c <- coeftest(model.3c, vcov=function(x) vcovCL(x, data.x3c$village))
coef.3c[,2]<-get.or.se(model.3c)

model.4c <- glm(prim_lighting ~ elec_outage 
                +as.factor(m1_q8_state)
                +as.factor(round)
                +month_expenditure_log
                +Caste_SC_ST
                +Caste_OBC
                +Caste_General
                +Decision_MaleHouseholdHead
                +family_size
                +Edu_MoreThan5thStandard
                +Edu_UpTo5thStandard
                +Edu_NoFormalSchooling,
                family="binomial",
                data = swing)
data.x4c <- na.omit(swing[ , c("ID", "village",
                              all.vars(formula(model.4c)))])
coef.4c<- coeftest(model.4c, vcov=function(x) vcovCL(x, data.x4c$village))
coef.4c[,2]<-get.or.se(model.4c)

model.5c <- glm(prim_lighting ~ elec_hours_night+elec_hours_day+elec_outage
                +month_expenditure_log
                +Caste_SC_ST
                +Caste_OBC
                +Caste_General
                +Decision_MaleHouseholdHead
                +family_size
                +Edu_MoreThan5thStandard
                +Edu_UpTo5thStandard
                +Edu_NoFormalSchooling,
                family="binomial",
                data = swing)
data.x5c <- na.omit(swing[ , c("ID", "village",
                              all.vars(formula(model.5c)))])
coef.5c <- coeftest(model.5c, vcov=function(x) vcovCL(x, data.x5c$village))
coef.5c[,2]<-get.or.se(model.5c)

model.6c <- glm(prim_lighting ~ elec_hours_night+elec_hours_day+elec_outage
                +as.factor(m1_q8_state)
                +as.factor(round)
                +month_expenditure_log
                +Caste_SC_ST
                +Caste_OBC
                +Caste_General
                +Decision_MaleHouseholdHead
                +family_size
                +Edu_MoreThan5thStandard
                +Edu_UpTo5thStandard
                +Edu_NoFormalSchooling,
                family="binomial",
                data = swing)
data.x6c <- na.omit(swing[ , c("ID", "village",
                              all.vars(formula(model.6c)))])
coef.6c <- coeftest(model.6c, vcov=function(x) vcovCL(x, data.x6c$village))

coef.6c[,2]<-get.or.se(model.6c)


### Table 3: table of models with controls
fit.list3 <- list(model.1c, model.2c,model.3c,model.4c,model.5c,model.6c)
coef3<- list(exp(coef.1c[-1,1]),exp(coef.2c[-c(1,4,5,6,7,8,9),1]),exp(coef.3c[-1,1]),exp(coef.4c[-c(1,3,4,5,6,7,8),1]),exp(coef.5c[-1,1]),exp(coef.6c[-c(1,5,6,7,8,9,10),1]))
se3 <- list(coef.1c[-1,2],coef.2c[-c(1,4,5,6,7,8,9),2],coef.3c[-1,2],coef.4c[-c(1,3,4,5,6,7,8),2],coef.5c[-1,2],coef.6c[-c(1,5,6,7,8,9,10),2])
pvals3 <- list(coef.1c[-1,4],coef.2c[-c(1,4,5,6,7,8,9),4],coef.3c[-1,4],coef.4c[-c(1,3,4,5,6,7,8),4],coef.5c[-1,4],coef.6c[-c(1,5,6,7,8,9,10),4])

cov.labs3 <- c("Electricity Hours (night)","Electricity Hours (day)","Electricity Outage",
               "Monthly Expenditure (log)","Caste (SCST)","Caste (OBC)","Caste (General)",
               "Male Household Head","Family Size","Education (More than 5th Standard)",
               "Education (Up to 5th Standard)","Education(NoFormalSchooling)")

keep.list.controls <-c("Electricity Hours (night)","Electricity Hours (day)","Electricity Outage",
                       "Monthly Expenditure (log)","Caste (SCST)","Caste (OBC)",
                       "Male Household Head","Family Size","Education (More than 5th Standard)",
                       "Education (Up to 5th Standard)")

omit.list.controls <-c("ID","JHARKHAND","MADHYA PRADESH","ODISHA","UTTAR PRADESH","WEST BENGA","round","Constant")
omit.stats <- c("rsq","ser","f")

sink("C:/Users/59634/Dropbox/ACCESS-2 kerosene (Xiaoxue, Johannes)/Manuscript/Tables/reg_sw_results_controls.tex")
stargazer(title = "Primary Lighting Fuel Choice",
          coef=coef3,
          se = se3,
          fit.list3,
          header = F,
          covariate.labels=cov.labs3,
          omit.stat = omit.stats,
          omit = omit.list.controls,
          p=pvals3,
          suppress.errors = F,float = F,no.space=T,
          dep.var.labels = c("Primary Lighting Fuel (1= Electricity, 0=Kerosene)"),
          add.lines = list(c("Region FE?","No","Yes","No","Yes","No","Yes"),
                           c("Round FE?","No","Yes","No","Yes","No","Yes")))
sink()

### Table A2 linear robustness
model.1d <- lm(prim_lighting ~ elec_hours_night+elec_hours_day, data)
data.x1d <-data[,c("ID","village","prim_lighting")]
coef.1d <- coeftest(model.1d, vcov=function(x) vcovCL(x, data.x1d$village))
coef.1d[,2]<-get.or.se(model.1d)

model.2d<-lm(prim_lighting ~ elec_hours_night+elec_hours_day+as.factor(m1_q8_state)+as.factor(round),data)
data.x2d <-data[,c("ID","village","prim_lighting")]
coef.2d <- coeftest(model.2d, vcov=function(x) vcovCL(x, data.x2d$village))
coef.2d[,2]<-get.or.se(model.2d)

model.3d<- lm(prim_lighting ~ elec_outage, data)
data.x3d <-na.omit(data[,c("ID", "village",all.vars(formula(model.3d)))])
coef.3d <- coeftest(model.3d, vcov=function(x) vcovCL(x, data.x3d$village))
coef.3d[,2]<-get.or.se(model.3d)

model.4d<-lm(prim_lighting ~ elec_outage+as.factor(m1_q8_state)+as.factor(round), data)
data.x4d <-na.omit(data[,c("ID", "village",all.vars(formula(model.4d)))])
coef.4d <- coeftest(model.4d, vcov=function(x) vcovCL(x, data.x4d$village))
coef.4d[,2]<-get.or.se(model.4d)

model.5d<-lm(prim_lighting ~ elec_hours_night+elec_hours_day+elec_outage, data)
data.x5d <-na.omit(data[,c("ID", "village",all.vars(formula(model.5d)))])
coef.5d <- coeftest(model.5d, vcov=function(x) vcovCL(x, data.x5d$village))
coef.5d[,2]<-get.or.se(model.5d)

model.6d <- lm(prim_lighting ~ elec_hours_night+elec_hours_day+elec_outage+as.factor(m1_q8_state)+as.factor(round),data)
data.x6d <- na.omit(data[ , c("ID","village", all.vars(formula(model.6d)))])
coef.6d <- coeftest(model.6d, vcov=function(x) vcovCL(x, data.x6d$village))
coef.6d[,2]<-get.or.se(model.6d)


fit.list1d <- list(model.1d,model.2d,model.3d,model.4d,model.5d,model.6d)
#coef1 <- list(exp(coef.1[-1,1]),exp(coef.2[-1,1]),exp(coef.3[-1,1]),exp(coef.4[-1,1]),exp(coef.5[-1,1]),exp(coef.6[-1,1]))
coef1d<-list(exp(coef.1d[c(2,3),1]),exp(coef.2d[c(2,3),1]),exp(coef.3d[c(2),1]),exp(coef.4d[-1,1]),exp(coef.5d[c(2,3,4),1]),exp(coef.6d[c(2,3,4),1]))
se1d <- list(coef.1d[c(2,3),2],coef.2d[c(2,3),2],coef.3d[c(2),2],coef.4d[-1,2],coef.5d[c(2,3,4),2],coef.6d[c(2,3,4),2])
pvals1d<-list(coef.1d[c(2,3),4],coef.2d[c(2,3),4],coef.3d[c(2),4],coef.4d[-1,4],coef.5d[c(2,3,4),4],coef.6d[c(2,3,4),4])
cov.labs <- c("Electricity Hours (night)","Electricity Hours (day)","Electricity Outage")
omit.list <-c("ID", "JHARKHAND","MADHYA PRADESH","ODISHA","UTTAR PRADESH","WEST BENGA","round","Constant")
omit.stats <- c("rsq","ser","f")

sink("C:/Users/59634/Dropbox/ACCESS-2 kerosene (Xiaoxue, Johannes)/Manuscript/Tables/reg_results_linear.tex")
stargazer(title = "Primary Lighting Fuel Choice",
          coef=coef1d,
          se = se1d,
          header = F,
          fit.list1d,
          p=pvals1d,
          omit.stat = omit.stats,
          omit = omit.list,
          covariate.labels=cov.labs,
          no.space = T,
          suppress.errors = F,float = F,
          dep.var.labels = c("Primary Lighting Fuel (1= Electricity, 0=Kerosene)"),
          add.lines = list(c("Region FE?","No","Yes","No","Yes","No","Yes"),
                           c("Round FE?","No","Yes","No","Yes","No","Yes")))
sink()
