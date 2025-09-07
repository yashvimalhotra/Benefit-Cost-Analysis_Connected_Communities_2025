library(ggplot2)
library(lubridate)
library(timeDate)
library(MASS)
library(reshape2)
library(reshape)
library(dplyr)
library(stringr)
library(segmented)
library(tidyverse)
library(fpp3)

# Quant data -- for other markets --DR dates for energy and SR markets from 2019 to 2022

# Data Input and wrangling ------------------------------------------------

# Quantifying load flexibility in CC buildings
setwd("C:/Users/malhotra.164/OneDrive - The Ohio State University/Yashvi Malhotra-KWEST/BCA/R_BCA/R_BCA/Raw_data")
Building_OSU <- read.csv("Building_CC_utilities_outliers") #read.csv("CC_21_Building.csv")
DR_dates_times <- read.csv("C:/Users/malhotra.164/OneDrive - The Ohio State University/Research Work_Clark/Meter placement/SavedData/Rawdata/DR_TMY_CBL.csv")
source("C:/Users/malhotra.164/OneDrive - The Ohio State University/Research Work_Clark/Quantifying Load Shed-Model Evaluation/Model_Eval/Loading data/Quantifying_Flexibility/towt_baseline.R")
source("C:/Users/malhotra.164/OneDrive - The Ohio State University/Research Work_Clark/Quantifying Load Shed-Model Evaluation/Model_Eval/Loading data/Quantifying_Flexibility/gbm_baseline.R")


# Building data
Building_OSU$DR.Day<-as.character(Building_OSU$DR.Day)
Building_OSU$DR.Day[is.na(Building_OSU$DR.Day)]<-FALSE
Building_data<- Building_OSU |>
  mutate(Weekday = lubridate::wday(Date, week_start=1))|>
  filter(Weekday!=6 & Weekday!=7 & Holidays==0)


Building<- Building_data |>
  filter(DR.Day!="Yes")|>
  dplyr::select(Timestamp, id, Date, Hour, 
                "Electricity_Consumed_kWh","Electricity.outlier",
                "CW_elec","CW_elec.outlier",
                Building_Occupancy_DEV,
                Ambient_Temperature_F ,
                DR.Event)

# DR Schedule
DR_dates_times <- DR_dates_times |>
  mutate(event_days = mdy(event_days),
         enddate = mdy(enddate),
         Weekday = lubridate::wday(event_days, week_start=1),
         Year= year(event_days))|>
  filter(Weekday!=6 & Weekday!=7)|> 
  dplyr::select(event_days, enddate, 
                starthour, endhour, DR_preevent)

# prediction data
baseline_energy<- Building_data |> 
  filter(DR.Day=="Yes") |> 
  dplyr::select(Timestamp, Date, Hour,DR.Event,
                id, Name,Gross.Square.Feet,
                "Electricity_Consumed_kWh","Electricity.outlier",
                "CW_elec","CW_elec.outlier",
                Building_Occupancy_DEV,
                Ambient_Temperature_F) 

building_id<-sort(unique(Building$id))


# Models to estimate flexibility ------------------------------------------

########################################
# 1. PJM Same Day (Work best)
filter_baseline_energy<-filter(baseline_energy, DR.Event!="Yes")
split_id<-split(filter_baseline_energy,filter_baseline_energy$id) 
for (k in 1:length(DR_dates_times$event_days)){
  split_id_date<-lapply(split_id, function(x) filter(x, Date == DR_dates_times$event_days[k] &
                                                       Hour >= DR_dates_times$DR_preevent[k]-3 & 
                                                       Hour != DR_dates_times$endhour[k]+1 & # New addition
                                                       Hour <= DR_dates_times$endhour[k]+3))  
  for (j in 1:length(building_id)){
    model_average<-split_id_date[[j]]|>
      filter(Electricity.outlier!="TRUE" | CW_elec.outlier!="TRUE")
    
    if (nrow(model_average) == 0) next# **Skip iteration if model_average is empty**
    
    model_average<-model_average |> group_by(Date) |>
      summarise(mean_elec= mean(Electricity_Consumed_kWh, na.rm = TRUE),
                mean_cw= mean(CW_elec, na.rm = TRUE))
    
    # **Check if model_average has valid values before assignment**
    if (!is.na(model_average$mean_elec) & !is.na(model_average$mean_cw)) {
      baseline_energy$Elec_Same_Day[baseline_energy$Date == DR_dates_times$event_days[k] & 
                                      baseline_energy$id == building_id[j]] <- model_average$mean_elec
      baseline_energy$CW_Same_Day[baseline_energy$Date == DR_dates_times$event_days[k] & 
                                    baseline_energy$id == building_id[j]] <- model_average$mean_cw
    }
  }
}

##### TOWT #######
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
  model.towt<-towt_baseline(train_Data = split_id,
                            pred_Data = split_id_DR, timescaleDays = 14, verbosity = 1,
                            intervalMinutes = 60, fahrenheit = T)
  
  baseline_energy$towt_elec[baseline_energy$id == building_id[j]]<-model.towt$prediction
}


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
  model.towt<-towt_baseline(train_Data = split_id,
                            pred_Data = split_id_DR, timescaleDays = 14, verbosity = 1,
                            intervalMinutes = 60, fahrenheit = T)
  
  baseline_energy$towt_cw[baseline_energy$id == building_id[j]]<-model.towt$prediction
}

# GBM Baseline model ------------------------------------------------------

# Elec
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
  
  baseline_energy$gbm_elec[baseline_energy$id == building_id[j]]<-model.gbm$prediction
}

# Chilled Water
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

write.table(baseline_energy,file="loads_shed_estimates_CC_2019_22.csv", sep=",")
