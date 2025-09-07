library(lubridate)
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
library(ggpattern)
library(gridExtra)

gc()
dr_building_id<-c(25,49,50,67,160,249)
elec_price=40.8/1000 #$/MW


# Input data for Avoided Energy --------------------------------------------------------------
setwd("C:/Users/malhotra.164/OneDrive - The Ohio State University/Yashvi Malhotra-KWEST/BCA/R_BCA/R_BCA/Raw_data")
wind_yearly <- read.csv("wind_cc.csv")
solar_yearly <- read.csv("solar_yearly.csv")

data_2019_22 <- read.csv("Building_CC_utilities_outliers")
baseline_energy <- read.csv("baseline_energy_CC_2019_20.csv")
EE_input_R <- read.csv("C:/Users/malhotra.164/OneDrive - The Ohio State University/Yashvi Malhotra-KWEST/BCA/R_BCA/R_BCA/Raw_data/EE_input_R.csv")

#update gbm
forecast_elec <- read.csv("combined_elec_forecasts_gbm.csv")
forecast_cw <- read.csv("combined_cw_forecasts_gbm.csv")
forecast_steam <- read.csv("combined_steam_forecasts_gbm.csv")
forecast_ng <- read.csv("combined_ng_forecasts_gbm.csv")

# Demand Side Management #Capacity
#Capacity
capacity_shed1 <- read.csv("loads_shed_estimates_CC_2019_22.csv")
yearly_tariff <- read.csv("yearly_tariff.csv")

yearly_tariff <- yearly_tariff|>
  dplyr::select(Year, PJM_Capacity.clearing.price)|>
  dplyr::rename(Price=PJM_Capacity.clearing.price) 
Building_type<-read.csv("Building_type.csv")

capacity_shed<-left_join(capacity_shed1,Building_type, by="id")
capacity_shed<-capacity_shed|>
  mutate(Year=year(Timestamp), Market="Capacity")|>
  left_join(yearly_tariff, by="Year")|>
  dplyr::select(-Year,-towt_elec, -towt_cw)

# RE: Solar and Wind ------------------------------------------------------
# Electricity purchased in 1 hour assuming uniform distribution
solar_yearly<-solar_yearly|>
  mutate(Date=dmy(Time.stamp), 
         Hour=as.numeric(Hour),
         Year=year(Date))|> group_by(Year)|>
  summarise(Solar_kwh_year=sum(System_power_generated_kW))

re<-wind_yearly|>
  mutate(Year=as.numeric(Year),
         Wind_ppa_MWh=Contract_amount*CUF*Portion_CC*8760)|>
  dplyr::select(Year,Wind_ppa_MWh)|>filter(Year<2023)|>mutate(Solar_MWh=solar_yearly$Solar_kwh_year/10^3)

# Yearly ECM ---------------------------------------------------------------------
ecm_savings<- EE_input_R|>
  mutate(End.Date=dmy(End.Date.of.ECM.implementation_DD_MM_YYY),
         Year=year(End.Date),
         Capital_Costs=as.numeric(Total.Measure.Cost_dollar..),
         OM_Costs=as.numeric(Annual.Operational...Maintenance.Savings_..Year),
         Building.Electric.Savings_MMbtu=as.numeric(Electric.Savings.at.Plant.Level..MMBtu...linked.to.building.electricity.and.chilled.water.utilities.),
         Building.Electric.Savings_kWh2=Building.Electric.Savings_MMbtu*10^6/3412,
         Building.Electric.Savings_kWh=as.numeric(Building.Electric.Savings_kWh),
         Building.CW.Savings_MMbtu=as.numeric(Building.Chilled.Water.Savings..mmBtu),
         Building.CW.Savings_kWh=Building.CW.Savings_MMbtu*10^6/3412,
         Building.Steam.Savings_mmBTU=as.numeric(Building.Steam.Savings_mmBTU_Year))|>
  dplyr::select("id","Building","ECM",Year,Capital_Costs,OM_Costs,Building.Electric.Savings_kWh2,                                                    
                "Building.Electric.Savings_kWh",Building.CW.Savings_kWh,Building.Steam.Savings_mmBTU)|>
  mutate(Building.Electric.Savings_kWh = ifelse(is.na(Building.Electric.Savings_kWh), Building.Electric.Savings_kWh2,
                                                Building.Electric.Savings_kWh))|>dplyr::select(-Building.Electric.Savings_kWh2)

ecm_savings <-left_join(ecm_savings ,Building_type)

ecm_savings_yearly <- ecm_savings |>
  group_by(Year,Type) |> 
  summarise(across(Capital_Costs:Building.Steam.Savings_mmBTU, ~sum(.x, na.rm = TRUE))) |>
  arrange(Year) |> 
  mutate(Cooling_savings_MWh = (Building.Electric.Savings_kWh + Building.CW.Savings_kWh)/1000)

write.table(ecm_savings_yearly, file="ecm_savings_type.csv", sep=",", row.names = FALSE)

# Load Shed and Rebound---------------------------------------------------------------
Scenarios_energy<-capacity_shed|>
  mutate(DR.Building = if_else(id %in% dr_building_id, "Yes", "No"),
         Year=year(Date),
         CW_elec=if_else(is.na(CW_elec), 0, CW_elec),
         gbm_cw=if_else(is.na(gbm_cw) | gbm_cw<0, 0,gbm_cw),
         gbm_elec=if_else(gbm_elec<0, 0, gbm_elec),
         Actual = Electricity_Consumed_kWh + CW_elec,
         baseline_GBM = gbm_elec + gbm_cw,
         Shed_GBM=if_else((baseline_GBM-Actual)<0, 0, baseline_GBM-Actual))|>
  filter(DR.Event=="Yes" & DR.Building=="Yes" & Market=="Capacity")|>
  group_by(Year,Market,Type)|>
  summarise(Net_Load_Shed_kWh=sum(Shed_GBM))|>
  mutate(Market="Capacity-DR")

# Baseline ----------------------------------------------------------------
actual_energy_2019_22 <- data_2019_22 |>
  mutate(Actual_Cooling_kWh = (Electricity_Consumed_kWh + CW_elec),
    Steam_Consumed_kBTU = if_else(Decision == "Steam" | Decision == "Steam+HW", Steam_Consumed_kBTU, 0)) |>
  dplyr::select(Timestamp,id,Electricity_Consumed_kWh,CW_elec,Actual_Cooling_kWh, Steam_Consumed_kBTU,Type)

baseline_energy_2020 <- baseline_energy |>
  mutate(Year = year(Timestamp),
         Baseline_Cooling_kWh = (gbm_elec + gbm_cw),
         Baseline_Heating_kBTU = gbm_steam) |> 
  rename(model_elec_kWh=gbm_elec,
         model_cw_kWh=gbm_cw)|>
  dplyr::select(Timestamp,id,model_elec_kWh,model_cw_kWh,Baseline_Cooling_kWh, Baseline_Heating_kBTU)

baseline_energy_2020<-left_join(baseline_energy_2020,Building_type)

# Forecast for 2021 and 2022 (CW is negative, steam and ng is NA)
forecast_elec1 <- forecast_elec |> mutate(model_elec_kWh = ifelse(.mean < 0, 0, .mean))|>
  dplyr::select(id, Timestamp, model_elec_kWh)

forecast_cw1 <- forecast_cw |> mutate(model_cw_kWh = ifelse(.mean < 0, 0, .mean)) |>
  dplyr::select(id, Timestamp,model_cw_kWh)

forecast_steam1 <- forecast_steam |> mutate(Steam_Consumed_kBTU = ifelse(.mean < 0, 0, .mean)) |>
  dplyr::select(id, Timestamp,Steam_Consumed_kBTU)

forecast_util<-left_join(forecast_elec1,forecast_cw1)
forecast_util<-left_join(forecast_util,forecast_steam1)

forecast_util<-forecast_util|>
  mutate(Year=year(Timestamp),
         Baseline_Cooling_kWh = (model_elec_kWh + model_cw_kWh),
         Baseline_Heating_kBTU = Steam_Consumed_kBTU)|>
  dplyr::select(Timestamp,id,model_elec_kWh,model_cw_kWh,Baseline_Cooling_kWh, Baseline_Heating_kBTU)

forecast_util<-left_join(forecast_util,Building_type)

avoided_energy<-actual_energy_2019_22|>filter(year(Timestamp)==2019)|>
  rename(Baseline_Cooling_kWh=Actual_Cooling_kWh,
         Baseline_Heating_kBTU=Steam_Consumed_kBTU,
         model_elec_kWh=Electricity_Consumed_kWh,
         model_cw_kWh=CW_elec)
avoided_energy<-rbind(avoided_energy,baseline_energy_2020,forecast_util)
avoided_energy<-left_join(actual_energy_2019_22,avoided_energy)

avoided_energy<-avoided_energy|> mutate(Date=date(Timestamp), Hour=hour(Timestamp), Year=year(Timestamp))

# Avoided Energy ----------------------------------------------------------

avoided_energy_error<-avoided_energy|>
  group_by(Year,Type)|>
  summarise(Baseline_Cooling_MWh = sum(Baseline_Cooling_kWh, na.rm = TRUE)/10^3,    
            Baseline_Heating_mMBTU = sum(Baseline_Heating_kBTU, na.rm = TRUE)/10^3)

avoided_energy_error<-left_join(avoided_energy_error,ecm_savings_yearly)
avoided_energy_error<-avoided_energy_error|>dplyr::select(-Capital_Costs,-OM_Costs,-Building.Electric.Savings_kWh,-Building.CW.Savings_kWh)
avoided_energy_error<-left_join(avoided_energy_error,Scenarios_energy)
avoided_energy_error<-left_join(avoided_energy_error,re)
# see if i need to divide it with 10^3

write.table(avoided_energy_error, file="avoided_energy_type.csv", sep=",", row.names = FALSE)

