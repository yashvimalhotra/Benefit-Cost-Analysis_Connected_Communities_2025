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
# Load the package
library(fuzzyjoin)

# Quantifying load flexibility in CC buildings after ECM implementation

# Data Input and wrangling ------------------------------------------------
setwd("C:/Users/malhotra.164/OneDrive - The Ohio State University/Yashvi Malhotra-KWEST/BCA/R_BCA/R_BCA/Raw_data")
Building_OSU <- read.csv("Building_CC_utilities_outliers")

DR_dates_times <- read.csv("C:/Users/malhotra.164/OneDrive - The Ohio State University/Research Work_Clark/Meter placement/SavedData/Rawdata/DR_TMY_CBL.csv")

#update ecm auidt reports
EE_input_R <- read.csv("C:/Users/malhotra.164/OneDrive - The Ohio State University/Yashvi Malhotra-KWEST/BCA/R_BCA/R_BCA/Raw_data/EE_input_R.csv")

source("C:/Users/malhotra.164/OneDrive - The Ohio State University/Research Work_Clark/Quantifying Load Shed-Model Evaluation/Model_Eval/Loading data/Quantifying_Flexibility/Raw_Data/towt_baseline.R")
source("C:/Users/malhotra.164/OneDrive - The Ohio State University/Research Work_Clark/Quantifying Load Shed-Model Evaluation/Model_Eval/Loading data/Quantifying_Flexibility/Raw_Data/gbm_baseline.R")

# Building data ----------------------------------------------------------- This has to be grouped 
Building_data<- Building_OSU|>
  dplyr::mutate(Date=ymd(Date,tz="EST"),Timestamp=ymd_hms(Timestamp,tz="EST"))|>
  dplyr::select(-"Chilled_Water_Consumed_kBTU", - "Steam_Consumed_kBTU",-"Natural_Gas_Consumed_kBTU", 
                -"CW.Source", -"CW.data",-"Decision",
                -"Electricity_Consumed_kWh1",-"Electricity.imputed",
                -"CW_elec1",-"CW_elec.imputed",
                -"Steam_Consumed_kBTU1",- "Steam.imputed",- "Steam.outlier", -"Natural_Gas_Consumed_kBTU1", - "NG.imputed",
                -"NG.outlier",-"tow")

Building_data$DR.Day[is.na(Building_data$DR.Day)]<-FALSE

Building_data<- Building_data|>
  mutate(Weekday = lubridate::wday(Date, week_start=1))|>
  filter(Weekday!=6 & Weekday!=7 & Holidays==0)
  
# Yearly ECM ---------------------------------------------------------------------
ecm_savings<- EE_input_R|>
  mutate(End.Date=dmy(End.Date.of.ECM.implementation_DD_MM_YYY),
         Year=year(End.Date),
         Building.Electric.Savings_MMbtu=as.numeric(Electric.Savings.at.Plant.Level..MMBtu...linked.to.building.electricity.and.chilled.water.utilities.),
         Building.Electric.Savings_kWh2=Building.Electric.Savings_MMbtu*10^6/3412,
         Building.Electric.Savings_kWh=as.numeric(Building.Electric.Savings_kWh),
         Building.CW.Savings_MMbtu=as.numeric(Building.Chilled.Water.Savings..mmBtu),
         Building.CW.Savings_kWh=Building.CW.Savings_MMbtu*10^6/3412)|>
  dplyr::select("id","Building","ECM",Year,Building.Electric.Savings_kWh2,End.Date,
         "Building.Electric.Savings_kWh",Building.CW.Savings_kWh)|>
  mutate(Building.Electric.Savings_kWh = ifelse(is.na(Building.Electric.Savings_kWh), Building.Electric.Savings_kWh2,
                                                Building.Electric.Savings_kWh))|>dplyr::select(-Building.Electric.Savings_kWh2)

ecm_savings_profile<- ecm_savings |>
  group_by(End.Date,id) |>
  summarise(across(Building.Electric.Savings_kWh:Building.CW.Savings_kWh, ~sum(.x, na.rm = TRUE)))|>
  mutate(Electric_savings_kWh_hourly = (Building.Electric.Savings_kWh )/8760,
         CW_savings_kWh_hourly= Building.CW.Savings_kWh/8760)|>
  group_by(id) |>
  mutate(Cumulative_Elec_savings_kWh_hourly = cumsum(Electric_savings_kWh_hourly),
         Cumulative_CW_savings_kWh_hourly = cumsum(CW_savings_kWh_hourly))

# Building data with ECM ---------------------------------------------------------------------
Building_data_ecm <- fuzzy_left_join(Building_data,ecm_savings_profile,by = c("id", "Date" = "End.Date"),match_fun = list(`==`, `>`))

rf_Building_data <- Building_data_ecm |>
  dplyr::group_by(id.x, Date) |>
  filter(if (all(is.na(End.Date))) TRUE else End.Date == max(End.Date, na.rm = TRUE)) |>
  ungroup()|>
  mutate(Elec_ECM = if_else(is.na(End.Date),
                                  Electricity_Consumed_kWh,
                                  if_else(Date > End.Date,
                                          Electricity_Consumed_kWh - Cumulative_Elec_savings_kWh_hourly,
                                          Electricity_Consumed_kWh)),
         CW_ECM = if_else(is.na(End.Date),
                                    CW_elec,
                                    if_else(Date > End.Date,
                                            CW_elec - Cumulative_CW_savings_kWh_hourly,
                                            CW_elec)))|>
  dplyr::rename(id=id.x)|>
  dplyr::select(id, Name,Gross.Square.Feet,Type,
           Timestamp,Year,Date,Hour,DR.Event,End.Date,DR.Day,
           Electricity_Consumed_kWh, Elec_ECM, 
           CW_elec, CW_ECM,Electricity.outlier,CW_elec.outlier,
           Building_Occupancy_DEV,
           Ambient_Temperature_F) |>
  group_by(id, Name,Gross.Square.Feet,Type,
           Timestamp,Year,Date,Hour,DR.Event,End.Date,DR.Day,
           Electricity_Consumed_kWh, Elec_ECM, Electricity.outlier,CW_elec.outlier,
           CW_elec, CW_ECM,
           Building_Occupancy_DEV,
           Ambient_Temperature_F) |>
  filter(if (all(is.na(End.Date))) TRUE else End.Date == max(End.Date, na.rm = TRUE))|>
  ungroup()

write.table(rf_Building_data,file="rf_Building_data.csv", sep=",")

#Training data for reduced flexibility
Building<- rf_Building_data|>
  filter(DR.Day!="Yes")|>
  dplyr::select(Timestamp, id, Date, Hour, Type,
                Elec_ECM,
                CW_ECM,Electricity.outlier,CW_elec.outlier,
                Building_Occupancy_DEV,
                Ambient_Temperature_F ,
                DR.Event)|>
  dplyr::rename(CW_elec=CW_ECM,
        Electricity_Consumed_kWh=Elec_ECM)

summary(Building)

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
baseline_energy<- rf_Building_data|> 
  filter(DR.Day=="Yes") |> 
  dplyr::select(Timestamp, Date, Hour,DR.Event,
                id, Name,Gross.Square.Feet,Type,
                Elec_ECM,"Electricity.outlier",
                CW_ECM,"CW_elec.outlier",
                Building_Occupancy_DEV,
                Ambient_Temperature_F)|>
  dplyr::rename("CW_elec"=CW_ECM,"Electricity_Consumed_kWh"=Elec_ECM)

# Models to estimate flexibility ------------------------------------------

# 1. PJM Same Day (Work best)
building_id<-sort(unique(Building$id))
filter_baseline_energy<-filter(baseline_energy, DR.Event!="Yes")
split_id<-split(filter_baseline_energy,filter_baseline_energy$id) 

for (k in 1:length(DR_dates_times$event_days)){
  split_id_date<-lapply(split_id, function(x) filter(x, Date == DR_dates_times$event_days[k] &
                                                       Hour >= DR_dates_times$DR_preevent[k]-3 & 
                                                       Hour != DR_dates_times$endhour[k]+1 & # New addition
                                                       Hour <= DR_dates_times$endhour[k]+3))  
  for (j in 1:length(building_id)){
    # Electricity
    model_average<-split_id_date[[j]]|>filter(Electricity.outlier!="TRUE")
    if (nrow(model_average) == 0) next
    
    model_average<-model_average |> group_by(Date) |>
      summarise(mean_elec= mean(Electricity_Consumed_kWh, na.rm = TRUE))
    
    if (!is.na(model_average$mean_elec)) {
      baseline_energy$Elec_Same_Day[baseline_energy$Date == DR_dates_times$event_days[k] & 
                                      baseline_energy$id == building_id[j]] <- model_average$mean_elec}
    
    model_average<-split_id_date[[j]]|>filter(CW_elec.outlier!="TRUE")
    if (nrow(model_average) == 0) next
    
    model_average<-model_average |> group_by(Date) |>
      summarise(mean_cw= mean(CW_elec, na.rm = TRUE))
    
    if (!is.na(model_average$mean_cw)) {
      baseline_energy$CW_Same_Day[baseline_energy$Date == DR_dates_times$event_days[k] & 
                                    baseline_energy$id == building_id[j]] <- model_average$mean_cw}
  }
}

##### TOWT #######
t_elec<-Building[,c("Timestamp","id","Electricity_Consumed_kWh","Ambient_Temperature_F","Electricity.outlier")]|>
  filter(Electricity.outlier!="TRUE")|>dplyr::select(-Electricity.outlier)|>
  filter(!is.na(Electricity_Consumed_kWh))|>filter(Electricity_Consumed_kWh>0)|> # remove NA values and negative values
  dplyr::rename("time"="Timestamp","eload"="Electricity_Consumed_kWh","temp"="Ambient_Temperature_F")|>
  mutate(time=as.POSIXct(time) , time=format(time,"%m/%d/%y %H:%M"))

p_elec<-baseline_energy|>dplyr::select("Timestamp","id","Electricity_Consumed_kWh","Ambient_Temperature_F","Electricity.outlier")|>
  filter(Electricity.outlier!="TRUE")|>dplyr::select(-Electricity.outlier)|>
  dplyr::rename("time"="Timestamp","eload"="Electricity_Consumed_kWh","temp"="Ambient_Temperature_F")|>
  mutate(time=as.POSIXct(time) , time=format(time,"%m/%d/%y %H:%M"))

# building_id<-sort(unique(t_elec$id)) #21 buildings

# for (j in 1: length(building_id)){
#   split_id<-filter(t_elec, id==building_id[j]) 
#   split_id$id<-NULL
#   split_id_DR<-filter(p_elec, id==building_id[j]) 
#   split_id_DR$id<-NULL
#   model.towt<-towt_baseline(train_Data = split_id,
#                             pred_Data = split_id_DR, timescaleDays = 14, verbosity = 1,
#                             intervalMinutes = 60, fahrenheit = T)
#   
#   baseline_energy$towt_elec[baseline_energy$id == building_id[j]]<-model.towt$prediction
# }


t_cw<-Building[,c("Timestamp","id","CW_elec","Ambient_Temperature_F","CW_elec.outlier")]|>
  filter(CW_elec.outlier!="TRUE")|>dplyr::select(-CW_elec.outlier)|>
  filter(!is.na(CW_elec))|>filter(CW_elec>0)|> # remove NA values and negative values
  dplyr::rename("time"="Timestamp","eload"="CW_elec","temp"="Ambient_Temperature_F")|>
  mutate(time=as.POSIXct(time) , time=format(time,"%m/%d/%y %H:%M"))

p_cw<-baseline_energy[,c("Timestamp","id","CW_elec","Ambient_Temperature_F","CW_elec.outlier")]|>
  filter(CW_elec.outlier!="TRUE")|>dplyr::select(-CW_elec.outlier)|>
  dplyr::rename("time"="Timestamp","eload"="CW_elec","temp"="Ambient_Temperature_F")|>
  mutate(time=as.POSIXct(time) , time=format(time,"%m/%d/%y %H:%M"))

# building_id<-sort(unique(t_cw$id)) # 16 buildings

# for (j in 1: length(building_id)){
#   split_id<-filter(t_cw, id==building_id[j]) 
#   split_id$id<-NULL
#   split_id_DR<-filter(p_cw, id==building_id[j]) 
#   split_id_DR$id<-NULL
#   model.towt<-towt_baseline(train_Data = split_id,
#                             pred_Data = split_id_DR, timescaleDays = 14, verbosity = 1,
#                             intervalMinutes = 60, fahrenheit = T)
#   
#   baseline_energy$towt_cw[baseline_energy$id == building_id[j]]<-model.towt$prediction
# }

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

write.table(baseline_energy,file="rf_CC_2019_22_audit.csv", sep=",")
save.image("C:/Users/malhotra.164/OneDrive - The Ohio State University/Yashvi Malhotra-KWEST/BCA/R_BCA/R_BCA/Raw_data/Images/quant_reduced_flexibility.RData")
