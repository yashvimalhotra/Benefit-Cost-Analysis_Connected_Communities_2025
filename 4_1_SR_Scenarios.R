library(ggplot2)
library(lubridate)
library(timeDate)
library(MASS)
library(reshape2)
library(reshape)
library(dplyr)
library(stringr)
library(tidyverse)
library(readr)


# Input Data and Functions ------------------------------------------------
sync_reserve_events <- read.csv("C:/Users/malhotra.164/OneDrive - The Ohio State University/Yashvi Malhotra-KWEST/BCA/R_BCA/R_BCA/Raw_data/RT_LMP_2019_2021/Scenarios_2022_2023/Sync_Reserve_Prices/sync_reserve_events.csv")


file_list <- list.files(path = "C:/Users/malhotra.164/OneDrive - The Ohio State University/Yashvi Malhotra-KWEST/BCA/R_BCA/R_BCA/Raw_data/SR_market", pattern = "\\.csv$", full.names = TRUE)
file_data <- lapply(file_list, read_csv)
combined_data <- bind_rows(file_data, .id = "file_id")

# Function to calculate the previous business day
prev_business_day <- function(date) {
  # Loop until the date is not Saturday (7) or Sunday (1)
  while(any(wday(date) %in% c(1, 7))) {
    date[wday(date) %in% c(1, 7)] <- date[wday(date) %in% c(1, 7)] - days(1)}
  return(date)}

# SR Prices ---------------------------------------------------------------
rt_hrl_lmps<-combined_data|> filter(locale=="PJM_RTO" & service=="REG") |> 
  dplyr::select(-file_id,-locale,-service)|>
  mutate(Timestamp=mdy_hms(datetime_beginning_ept, tz="EST"),Date=date(Timestamp), DR.Hour=hour(Timestamp))

rt_hrl_lmps <- rt_hrl_lmps[order(rt_hrl_lmps$Timestamp),]
rt_hrl_lmps <- distinct(rt_hrl_lmps)
rt_hrl_lmps<-rt_hrl_lmps|>
  dplyr::select("Timestamp","Date","DR.Hour","mcp_capped")|>
  distinct()

# SR Events ---------------------------------------------------------------
SR_events<-sync_reserve_events|>
  mutate(event_start_ept=mdy_hms(event_start_ept, tz="EST"), 
         event_end_ept=mdy_hms(event_end_ept,tz="EST"),
         event_days = date(event_end_ept),
         tentative_end_date = event_days - days(1),
         end_date = prev_business_day(tentative_end_date),
         starthour = hour(event_start_ept),
         endhour = if_else(hour(event_end_ept) == starthour,hour(event_end_ept) + 1,hour(event_end_ept)),
         DR_preevent = if_else(starthour - 1 <= 0,starthour,starthour - 1),
         Date = event_days,
         DR.Hour =starthour) |>
  dplyr::select(event_days,end_date,starthour,endhour,DR_preevent,Date, DR.Hour)|>
  dplyr::filter(year(event_days)<=2022 & year(event_days)>=2019)|>
  distinct()

SR_events_join<- SR_events|>dplyr::select(Date,DR.Hour)
 SR_events_join<-left_join(SR_events_join,rt_hrl_lmps)|> 
   dplyr::select(Date,DR.Hour,mcp_capped)

# Data Input and wrangling ------------------------------------------------

setwd("C:/Users/malhotra.164/OneDrive - The Ohio State University/Yashvi Malhotra-KWEST/BCA/R_BCA/R_BCA/Raw_data")
Building_OSU <- read.csv("Building_CC_utilities_outliers")
source("C:/Users/malhotra.164/OneDrive - The Ohio State University/Research Work_Clark/Quantifying Load Shed-Model Evaluation/Model_Eval/Loading data/Quantifying_Flexibility/gbm_baseline.R")

# Demand Flexibility for SR ----------------------------------------------------

Building_data<- Building_OSU |>
  mutate(Weekday = lubridate::wday(Date, week_start=1),
         Date=ymd(Date))|>
  filter(Weekday!=6 & Weekday!=7 & Holidays==0)|> 
  dplyr::select(-DR.Day, -DR.Event)


Building_data<- left_join(Building_data,SR_events_join,relationship = "many-to-many")

Building_data<-Building_data|> mutate(DR.Day=ifelse(is.na(DR.Hour),"No","Yes"),
                            DR.Event=ifelse(Hour==DR.Hour,"Yes","No")) #check the logic
Building<- Building_data |>
  filter (DR.Day=="No")|>
  dplyr::select(Timestamp, id, Date, Hour,Type,
                "Electricity_Consumed_kWh","Electricity.outlier",
                "CW_elec","CW_elec.outlier",
                Building_Occupancy_DEV,
                Ambient_Temperature_F)

# DR Schedule
DR_dates_times <- SR_events

# prediction data
baseline_energy<- Building_data |> 
  filter (DR.Day=="Yes")|>
  dplyr::select(Timestamp, Date, Hour,DR.Event,Type,
                id, Name,Gross.Square.Feet,Type,mcp_capped,
                "Electricity_Consumed_kWh","Electricity.outlier",
                "CW_elec","CW_elec.outlier",
                Building_Occupancy_DEV,
                Ambient_Temperature_F) 

building_id<-sort(unique(Building$id))


# Models to estimate flexibility ------------------------------------------

# 1. PJM Same Day (Work best)
# filter_baseline_energy<-filter(baseline_energy, DR.Event!="Yes")
# split_id<-split(filter_baseline_energy,filter_baseline_energy$id) 
# 
# for (k in 1:length(DR_dates_times$event_days)){
#   split_id_date<-lapply(split_id, function(x) filter(x, Date == DR_dates_times$event_days[k] &
#                                                        Hour >= DR_dates_times$DR_preevent[k]-3 &
#                                                        Hour != DR_dates_times$endhour[k]+1 & # New addition
#                                                        Hour <= DR_dates_times$endhour[k]+3))
#   for (j in 1:length(building_id)){
#     model_average<-split_id_date[[j]]|>
#       filter(Electricity.outlier!="TRUE" | CW_elec.outlier!="TRUE")
#     
#     if (nrow(model_average) == 0) next# **Skip iteration if model_average is empty**
#     
#     model_average<-model_average |> group_by(Date) |>
#       summarise(mean_elec= mean(Electricity_Consumed_kWh, na.rm = TRUE),
#                 mean_cw= mean(CW_elec, na.rm = TRUE))
#     
#     # **Check if model_average has valid values before assignment**
#     if (!is.na(model_average$mean_elec) & !is.na(model_average$mean_cw)) {
#       baseline_energy$Elec_Same_Day[baseline_energy$Date == DR_dates_times$event_days[k] & 
#                                       baseline_energy$id == building_id[j]] <- model_average$mean_elec
#       baseline_energy$CW_Same_Day[baseline_energy$Date == DR_dates_times$event_days[k] & 
#                                     baseline_energy$id == building_id[j]] <- model_average$mean_cw
#     }
#   }
# }


# GBM Baseline model ------------------------------------------------------
t_elec<-Building[,c("Timestamp","id","Electricity_Consumed_kWh","Ambient_Temperature_F","Electricity.outlier")]|>
  filter(Electricity.outlier!="TRUE")|>dplyr::select(-Electricity.outlier)|>
  dplyr::rename("time"="Timestamp","eload"="Electricity_Consumed_kWh","temp"="Ambient_Temperature_F")|>
  mutate(time=as.POSIXct(time) , time=format(time,"%m/%d/%y %H:%M"))

p_elec<-baseline_energy|>dplyr::select("Timestamp","id","Electricity_Consumed_kWh","Ambient_Temperature_F","Electricity.outlier")|>
  filter(Electricity.outlier!="TRUE")|>dplyr::select(-Electricity.outlier)|>
  dplyr::rename("time"="Timestamp","eload"="Electricity_Consumed_kWh","temp"="Ambient_Temperature_F")|>
  mutate(time=as.POSIXct(time) , time=format(time,"%m/%d/%y %H:%M"))

building_id<-sort(unique(t_elec$id)) #21 buildings

for (j in 1: length(building_id)){
  split_id<-filter(t_elec, id==building_id[j]) 
  split_id$id<-NULL
  
  split_id_DR<-filter(p_elec, id==building_id[j]) 
  split_id_DR$id<-NULL
  model.gbm<-gbm_baseline(train_Data = split_id,
                          pred_Data = split_id_DR, k_folds = 5,
                          variables = c("temp", "tow"), ncores = parallel::detectCores(logical = F),
                          cv_blocks = "weeks", iter = seq(from = 50, to = 300, by = 25),
                          depth = c(3:7), lr = c(0.05, 0.1), subsample = c(0.5),
                          verbose = FALSE)
  baseline_energy$gbm_elec[baseline_energy$id == building_id[j]]<-model.gbm$prediction}


t_cw<-Building[,c("Timestamp","id","CW_elec","Ambient_Temperature_F","CW_elec.outlier")]|>
  filter(CW_elec.outlier!="TRUE")|>dplyr::select(-CW_elec.outlier)|>
  dplyr::rename("time"="Timestamp","eload"="CW_elec","temp"="Ambient_Temperature_F")|>
  mutate(time=as.POSIXct(time) , time=format(time,"%m/%d/%y %H:%M"))

p_cw<-baseline_energy[,c("Timestamp","id","CW_elec","Ambient_Temperature_F","CW_elec.outlier")]|>
  filter(CW_elec.outlier!="TRUE")|>dplyr::select(-CW_elec.outlier)|>
  dplyr::rename("time"="Timestamp","eload"="CW_elec","temp"="Ambient_Temperature_F")|>
  mutate(time=as.POSIXct(time) , time=format(time,"%m/%d/%y %H:%M"))

building_id<-sort(unique(t_cw$id)) # 16 buildings

for (j in 1: length(building_id)){
  split_id<-filter(t_cw, id==building_id[j]) 
  split_id$id<-NULL
  
  split_id_DR<-filter(p_cw, id==building_id[j]) 
  split_id_DR$id<-NULL
  
  model.gbm_cw<-gbm_baseline(train_Data = split_id,
                             pred_Data = split_id_DR, k_folds = 5,
                             variables = c("temp", "tow"), ncores = parallel::detectCores(logical = F),
                             cv_blocks = "weeks", iter = seq(from = 50, to = 300, by = 25),
                             depth = c(3:7), lr = c(0.05, 0.1), subsample = c(0.5),
                             verbose = FALSE)
  
  baseline_energy$gbm_cw[baseline_energy$id == building_id[j]]<-model.gbm_cw$prediction
}

write.table(baseline_energy,file="loads_shed_SR.csv", sep=",")
save.image("C:/Users/malhotra.164/OneDrive - The Ohio State University/Yashvi Malhotra-KWEST/BCA/R_BCA/R_BCA/Raw_data/SR_market_baseline_image.RData")
baseline_energy1<-baseline_energy

# Reduced Flexibility for SR ----------------------------------------------------

DR_dates_times <- SR_events
rf_Building_data <- read.csv("rf_Building_data.csv")

rf_Building_data<- rf_Building_data|>
  dplyr::select(-DR.Day, -DR.Event)|>
  dplyr::mutate(Date=ymd(Date,tz="EST"),Timestamp=ymd_hms(Timestamp,tz="EST"))

rf_Building_data<- left_join(rf_Building_data,SR_events_join,relationship = "many-to-many")

rf_Building_data<-rf_Building_data|> mutate(DR.Day=ifelse(is.na(DR.Hour),"No","Yes"),
                                      DR.Event=ifelse(Hour==DR.Hour,"Yes","No")) #check the logic


#Training data for reduced flexibility
Building<-rf_Building_data |>
  filter(DR.Day!="Yes")|>
  dplyr::select(Timestamp, id, Date, Hour, 
                Elec_ECM,"Electricity.outlier",
                CW_ECM,"CW_elec.outlier",
                Building_Occupancy_DEV,
                Ambient_Temperature_F ,
                DR.Event)|>
  dplyr::rename(CW_elec=CW_ECM, Electricity_Consumed_kWh=Elec_ECM)

# prediction data
baseline_energy<- rf_Building_data |> 
  filter(DR.Day=="Yes") |> 
  dplyr::select(Timestamp, Date, Hour,DR.Event,Type,mcp_capped,
                id, Name,Gross.Square.Feet,
                Elec_ECM,"Electricity.outlier",
                CW_ECM,"CW_elec.outlier",
                Building_Occupancy_DEV,
                Ambient_Temperature_F)|>
  dplyr::rename("CW_elec"=CW_ECM,"Electricity_Consumed_kWh"=Elec_ECM)

# Elec
t_elec<-Building[,c("Timestamp","id","Electricity_Consumed_kWh","Ambient_Temperature_F","Electricity.outlier")]|>
  filter(Electricity.outlier!="TRUE")|>dplyr::select(-Electricity.outlier)|>
  dplyr::rename("time"="Timestamp","eload"="Electricity_Consumed_kWh","temp"="Ambient_Temperature_F")|>
  mutate(time=as.POSIXct(time) , time=format(time,"%m/%d/%y %H:%M"))

p_elec<-baseline_energy|>dplyr::select("Timestamp","id","Electricity_Consumed_kWh","Ambient_Temperature_F","Electricity.outlier")|>
  filter(Electricity.outlier!="TRUE")|>dplyr::select(-Electricity.outlier)|>
  dplyr::rename("time"="Timestamp","eload"="Electricity_Consumed_kWh","temp"="Ambient_Temperature_F")|>
  mutate(time=as.POSIXct(time) , time=format(time,"%m/%d/%y %H:%M"))

building_id<-sort(unique(t_elec$id)) #21 buildings

for (j in 1: length(building_id)){
  split_id<-filter(t_elec, id==building_id[j]) 
  split_id$id<-NULL
  
  split_id_DR<-filter(p_elec, id==building_id[j]) 
  split_id_DR$id<-NULL
  model.gbm<-gbm_baseline(train_Data = split_id,
                          pred_Data = split_id_DR, k_folds = 5,
                          variables = c("temp", "tow"), ncores = parallel::detectCores(logical = F),
                          cv_blocks = "weeks", iter = seq(from = 50, to = 300, by = 25),
                          depth = c(3:7), lr = c(0.05, 0.1), subsample = c(0.5),
                          verbose = FALSE)
  
  baseline_energy$gbm_elec[baseline_energy$id == building_id[j]]<-model.gbm$prediction}

# Chilled Water
t_cw<-Building[,c("Timestamp","id","CW_elec","Ambient_Temperature_F","CW_elec.outlier")]|>
  filter(CW_elec.outlier!="TRUE")|>dplyr::select(-CW_elec.outlier)|>
  dplyr::rename("time"="Timestamp","eload"="CW_elec","temp"="Ambient_Temperature_F")|>
  mutate(time=as.POSIXct(time) , time=format(time,"%m/%d/%y %H:%M"))

p_cw<-baseline_energy[,c("Timestamp","id","CW_elec","Ambient_Temperature_F","CW_elec.outlier")]|>
  filter(CW_elec.outlier!="TRUE")|>dplyr::select(-CW_elec.outlier)|>
  dplyr::rename("time"="Timestamp","eload"="CW_elec","temp"="Ambient_Temperature_F")|>
  mutate(time=as.POSIXct(time) , time=format(time,"%m/%d/%y %H:%M"))

building_id<-sort(unique(t_cw$id)) # 16 buildings
building_id<-sort(unique(t_cw$id)) # 16 buildings
for (j in 1: length(building_id)){
  split_id<-filter(t_cw, id==building_id[j]) 
  split_id$id<-NULL
  
  split_id_DR<-filter(p_cw, id==building_id[j]) 
  split_id_DR$id<-NULL
  
  model.gbm_cw<-gbm_baseline(train_Data = split_id,
                             pred_Data = split_id_DR, k_folds = 5,
                             variables = c("temp", "tow"), ncores = parallel::detectCores(logical = F),
                             cv_blocks = "weeks", iter = seq(from = 50, to = 300, by = 25),
                             depth = c(3:7), lr = c(0.05, 0.1), subsample = c(0.5),
                             verbose = FALSE)
  
  baseline_energy$gbm_cw[baseline_energy$id == building_id[j]]<-model.gbm_cw$prediction
}

write.table(baseline_energy,file="rf_CC_SR.csv", sep=",")
save.image("C:/Users/malhotra.164/OneDrive - The Ohio State University/Yashvi Malhotra-KWEST/BCA/R_BCA/R_BCA/Raw_data/Images/sr_reduced_flexibility.RData")