library(lubridate)
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
library(ggpattern)
library(fuzzyjoin)

gc()
# Input data for Avoided Peak --------------------------------------------------------------
setwd("C:/Users/malhotra.164/OneDrive - The Ohio State University/Yashvi Malhotra-KWEST/BCA/R_BCA/R_BCA/Raw_data")
wind_yearly <- read.csv("wind_cc.csv") # The price starts from 46.5*(1+2%)^(2019-2013)
solar_yearly <- read.csv("solar_yearly.csv")

data_2019_22 <- read.csv("Building_CC_utilities_outliers")
baseline_energy <- read.csv("baseline_energy_CC_2019_20.csv")

#ECM audit reports
EE_input_R <- read.csv("C:/Users/malhotra.164/OneDrive - The Ohio State University/Yashvi Malhotra-KWEST/BCA/R_BCA/R_BCA/Raw_data/EE_input_R.csv")

forecast_elec <- read.csv("combined_elec_forecasts_gbm.csv")
forecast_cw <- read.csv("combined_cw_forecasts_gbm.csv")
forecast_steam <- read.csv("combined_steam_forecasts_gbm.csv")
forecast_ng <- read.csv("combined_ng_forecasts_gbm.csv")

yearly_tariff <- read.csv("yearly_tariff.csv")

yearly_tariff <- yearly_tariff|>  
  mutate(Price=PJM_Capacity.clearing.price, # Price is $/MW
         Elec_Fixed_Price_MW=as.numeric(Transmission.Costs)*10^3) |>  # $/kW 
  dplyr::select(Year, Price,Elec_Fixed_Price_MW) 

# RE: Solar and Wind ------------------------------------------------------
# Electricity purchased in 1 hour assuming uniform distribution
wind_yearly<-wind_yearly|>
  mutate(Electricity_Purchased_MW=Contract_amount*CUF*Portion_CC)

# We make a years profile
avoided_peak<-solar_yearly|>
  mutate(Date=dmy(Time.stamp,tz="EST"), Hour=as.numeric(Hour),
         Solar_kW= as.numeric(System_power_generated_kW))|>
  dplyr::select(-Time.stamp,-System_power_generated_kW)|>
  mutate(Wind_kW=unique(wind_yearly$Electricity_Purchased_MW)*1000)

# * (1-0.01)^(0:14)/1000 Solar degradation
avoided_peak<- bind_rows(avoided_peak,
  avoided_peak |> mutate(Date = update(Date, year = 2020,tz="EST")),
  avoided_peak |> mutate(Date = update(Date, year = 2021,tz="EST")),
  avoided_peak |> mutate(Date = update(Date, year = 2022,tz="EST")))

wind_yearly<-NULL; solar_yearly<-NULL

# For a calender year
# Building data ----------------------------------------------------------- This has to be grouped 
Building_data<- data_2019_22|>
  dplyr::mutate(Date=ymd(Date,tz="EST"),Timestamp=ymd_hms(Timestamp,tz="EST"),
                Actual_Cooling_Load_kWh=Electricity_Consumed_kWh+CW_elec)|>
  dplyr::select(id, Timestamp, Year, Date, Hour, Actual_Cooling_Load_kWh,Steam_Consumed_kBTU)

# ECM ---------------------------------------------------------------------
ecm_savings<- EE_input_R|>
  mutate(End.Date=dmy(End.Date.of.ECM.implementation_DD_MM_YYY),
         Year=year(End.Date),
         Building.Electric.Savings_MMbtu=as.numeric(Electric.Savings.at.Plant.Level..MMBtu...linked.to.building.electricity.and.chilled.water.utilities.),
         Building.Electric.Savings_kWh2=Building.Electric.Savings_MMbtu*10^6/3412,
         Building.Electric.Savings_kWh=as.numeric(Building.Electric.Savings_kWh),
         Building.CW.Savings_MMbtu=as.numeric(Building.Chilled.Water.Savings..mmBtu),
         Building.CW.Savings_kWh=Building.CW.Savings_MMbtu*10^6/3412,
         Building.Steam.Savings_mmBTU=as.numeric(Building.Steam.Savings_mmBTU_Year))|>
  dplyr::select("id","Building","ECM",Year,Building.Electric.Savings_kWh2,End.Date,                                           
         "Building.Electric.Savings_kWh",Building.CW.Savings_kWh,Building.Steam.Savings_mmBTU)|>
  mutate(Building.Electric.Savings_kWh = ifelse(is.na(Building.Electric.Savings_kWh), Building.Electric.Savings_kWh2,
                                                Building.Electric.Savings_kWh))|>dplyr::select(-Building.Electric.Savings_kWh2)

ecm_savings_profile<- ecm_savings |>
  group_by(End.Date,id) |> 
  summarise(across(Building.Electric.Savings_kWh:Building.Steam.Savings_mmBTU, ~sum(.x, na.rm = TRUE)))|>
  mutate(Cooling_savings_kWh_hourly = (Building.Electric.Savings_kWh + Building.CW.Savings_kWh)/8760,
         Heating_savings_mmBTU_hourly=Building.Steam.Savings_mmBTU/8760)|>
  # dplyr::select(-Building.Electric.Savings_kWh,-Building.CW.Savings_kWh,-Building.Steam.Savings_mmBTU)|>
  group_by(id) |>
  mutate(Cumulative_Cooling_savings_kWh_hourly = cumsum(Cooling_savings_kWh_hourly), 
         Cumulative_Heating_savings_mmBTU_hourly = cumsum(Heating_savings_mmBTU_hourly))

# Building data with ECM ---------------------------------------------------------------------
Building_data_ecm <- fuzzy_left_join(Building_data,ecm_savings_profile,by = c("id", "Date" = "End.Date"),match_fun = list(`==`, `>`))

Building_data_filtered <- Building_data_ecm |>
  dplyr::group_by(id.x, Date) |> 
  filter(if (all(is.na(End.Date))) TRUE else End.Date == max(End.Date, na.rm = TRUE)) |>
  ungroup()|>
  mutate(Steam_Consumed_mmBTU = Steam_Consumed_kBTU / 1000,
         EE_Cooling_kWh = if_else(is.na(End.Date),
                                  Actual_Cooling_Load_kWh,
                                  if_else(Date > End.Date,
                                          Actual_Cooling_Load_kWh - Cumulative_Cooling_savings_kWh_hourly,
                                          Actual_Cooling_Load_kWh)),
         EE_Heating_mmBTU = if_else(is.na(End.Date),
                                    Steam_Consumed_mmBTU,
                                    if_else(Date > End.Date,
                                            Steam_Consumed_mmBTU - Cumulative_Heating_savings_mmBTU_hourly,
                                            Steam_Consumed_mmBTU)),
         ECM_Cooling_kWh = if_else(is.na(End.Date), 0,
                                   if_else(Date > End.Date,
                                           Cumulative_Cooling_savings_kWh_hourly,0)), 
         ECM_Heating_mmBTU = if_else(is.na(End.Date), 0,
                                     if_else(Date > End.Date,
                                             Cumulative_Heating_savings_mmBTU_hourly, 0)))|>
  dplyr::rename(id=id.x)|>
  dplyr::select(id, Timestamp,Year,Date,Hour,End.Date, 
                Actual_Cooling_Load_kWh, EE_Cooling_kWh, ECM_Cooling_kWh,
                Steam_Consumed_mmBTU, EE_Heating_mmBTU, ECM_Heating_mmBTU)|>
  group_by(id, Timestamp,Year,Date,Hour,
           Actual_Cooling_Load_kWh, EE_Cooling_kWh, ECM_Cooling_kWh,
           Steam_Consumed_mmBTU, EE_Heating_mmBTU, ECM_Heating_mmBTU) |>
  dplyr::filter(if (all(is.na(End.Date))) TRUE else End.Date == max(End.Date, na.rm = TRUE))|>
  ungroup()


Building_data_ecm_corrected<- Building_data_filtered |>
  group_by(Timestamp,Year, Date, Hour)|>
  summarise(ECM_Cooling_kWh=sum(EE_Cooling_kWh, na.rm=T),
            ECM_Heating_mmBTU=sum(EE_Heating_mmBTU, na.rm=T))

avoided_peak_building<-left_join(Building_data_ecm_corrected,avoided_peak) 

# Input data # Reduced Flexibility Demand Side Management--------------------------------------------------------------
setwd("C:/Users/malhotra.164/OneDrive - The Ohio State University/Yashvi Malhotra-KWEST/BCA/R_BCA/R_BCA/Raw_data")
dr_building_id<-c(25,49,50,67,160,249)

# Reduced Flexibility Capacity
capacity_shed<- read.csv("rf_CC_2019_22_audit.csv")
Building_type<-read.csv("Building_type.csv")

# capacity_shed<-left_join(capacity_shed1,Building_type, by="id")
capacity_shed<-capacity_shed|>
  mutate(Year=year(Timestamp), Market="Capacity")|>
  left_join(yearly_tariff, by="Year")|> # We have all 3 models
  dplyr::select(-Year,- Elec_Fixed_Price_MW,-"Elec_Same_Day",-"CW_Same_Day")

# Reduced Flexibility Energy
energy_shed <- read.csv("rf_CC_Energy.csv")

energy_shed<-energy_shed|> mutate(Market="Energy")|> 
  dplyr::rename(Price=total_lmp_rt) 

# Reduced Flexibility SR
SR_shed <- read.csv("rf_CC_SR.csv")
SR_shed<-SR_shed|> mutate(Market="SR")|>
  dplyr::rename(Price=mcp_capped)

colnames(capacity_shed)
colnames(energy_shed)
colnames(SR_shed)

shed<-rbind(capacity_shed, energy_shed, SR_shed)
load_shed<-shed|>
  mutate(Date=ymd(Date,tz="EST"),
         DR.Building = if_else(id %in% dr_building_id, "Yes", "No"),
         Year=year(Date),
         CW_elec=if_else(is.na(CW_elec), 0, CW_elec),
         Actual = Electricity_Consumed_kWh + CW_elec,
         gbm_cw=if_else(is.na(gbm_cw) | gbm_cw<0, 0,gbm_cw),
         gbm_elec=if_else(gbm_elec<0, 0, gbm_elec),
         baseline_GBM = gbm_elec + gbm_cw,
         Shed_GBM=if_else((baseline_GBM-Actual)<0, 0, baseline_GBM-Actual))|>
  dplyr::select(Year, Date,Hour,id, Name,DR.Building,
                Price,
                Market,DR.Event,Shed_GBM) 

# Scenario 1 : Benefit -----------------------------------------------------------------
# To quantify the peak reduction due to CC interventions: DSM

S1_peak<-load_shed|>
  filter(DR.Event=="Yes" & DR.Building=="Yes" & Market=="Capacity")|>
  group_by(Year, Date, Hour)|> # group all buildings
  summarise(Shed_GBM=sum(Shed_GBM, na.rm=T))

avoided_peak_S1<-left_join(avoided_peak_building,S1_peak) #bau
# Wind is too much!
avoided_peak_S1<- avoided_peak_S1|>
  mutate(delta_P = rowSums(across(c(Solar_kW, ECM_Cooling_kWh, Wind_kW, Shed_GBM)), na.rm = TRUE))

#breakdown
breakdown_peak <- avoided_peak_S1 |>
  group_by(Year) |>
  slice_min(order_by = delta_P, with_ties = FALSE) |>
  ungroup() |>
  mutate(
    delta_P_MW = delta_P / 10^3,
    solar_percent = round(Solar_kW*100/ delta_P),
    ecm_percent = round(ECM_Cooling_kWh*100 / delta_P),
    wind_percent = round(Wind_kW*100 / delta_P),
    shed_percent = round(Shed_GBM*100 / delta_P)
  )

# Calculating the benefit from avoided power
avoided_peak_S1<- avoided_peak_S1|>
  group_by(Year)|>
  summarise(delta_P=min(delta_P))|> # this should be minimum reduction => Max overall peak is what is calculated 
  mutate(delta_P_MW=(delta_P)/10^3)

avoided_peak_S1<-left_join(avoided_peak_S1, yearly_tariff)
avoided_peak_S1<-avoided_peak_S1|>
  mutate(B_cap1=delta_P/1000*Price*365 ,
         B_cap=B_cap1*0.8, 
         B_peak=delta_P/1000*Elec_Fixed_Price_MW, 
         Market="BAU:Capacity-DR")

avoided_peak_summary<-avoided_peak_S1
# Scenario 2 -----------------------------------------------------------------
# 2	All 21 buildings participate in the capacity market
S2_peak<-load_shed|>
  filter(DR.Event=="Yes"  & Market=="Capacity")|>
  group_by(Year, Date, Hour)|> # group all buildings
  summarise(Shed_GBM=sum(Shed_GBM, na.rm=T))

avoided_peak_S2<-left_join(avoided_peak_building,S2_peak) 
avoided_peak_S2<- avoided_peak_S2|>
  mutate(delta_P = rowSums(across(c(Solar_kW, ECM_Cooling_kWh, Wind_kW, Shed_GBM)), na.rm = TRUE))

# Calculating the benefit from avoided power
avoided_peak_S2<- avoided_peak_S2|>
  group_by(Year)|>
  summarise(delta_P=min(delta_P))|> # this should be minimum reduction => Max overall peak is what is calculated 
  mutate(delta_P_MW=(delta_P)/10^3)

avoided_peak_S2<-left_join(avoided_peak_S2, yearly_tariff)
avoided_peak_S2<-avoided_peak_S2|>
  mutate(B_cap1=delta_P/1000*Price*365 ,
         B_cap=B_cap1*0.8, 
         B_peak=delta_P/1000*Elec_Fixed_Price_MW,
         Market="S1:Capacity")

avoided_peak_summary<-rbind(avoided_peak_summary,avoided_peak_S2)

# Scenario 3 -----------------------------------------------------------------
# 3	All 21 Buildings participate in the SR market 10 minutes
S3_peak<-load_shed|>
  filter(DR.Event=="Yes" & Market=="SR")|>
  group_by(Year, Date, Hour)|> # group all buildings
  summarise(Shed_GBM=sum(Shed_GBM, na.rm=T))

avoided_peak_S3<-left_join(avoided_peak_building,S3_peak) 
avoided_peak_S3<- avoided_peak_S3|>
  mutate(delta_P = rowSums(across(c(Solar_kW, ECM_Cooling_kWh, Wind_kW, Shed_GBM)), na.rm = TRUE))

# Calculating the benefit from avoided power
avoided_peak_S3<- avoided_peak_S3|>
  group_by(Year)|>
  summarise(delta_P=min(delta_P))|> # this should be minimum reduction => Max overall peak is what is calculated 
  mutate(delta_P_MW=(delta_P)/10^3)

avoided_peak_S3<-left_join(avoided_peak_S3, yearly_tariff)
avoided_peak_S3<-avoided_peak_S3|>
  mutate(B_cap1=0 ,
         B_cap=0, 
         B_peak=delta_P/1000*Elec_Fixed_Price_MW,
         Market="S2:Energy")

avoided_peak_summary<-rbind(avoided_peak_summary,avoided_peak_S3)

# Scenario 4 -----------------------------------------------------------------
# 4	All 21 buildings participate in the energy markets 
S4_peak<-load_shed|>
  filter(DR.Event=="Yes" & Market=="Energy")|>
  group_by(Year, Date, Hour)|> # group all buildings
  summarise(Shed_GBM=sum(Shed_GBM, na.rm=T))

avoided_peak_S4<-left_join(avoided_peak_building,S4_peak)
avoided_peak_S4<- avoided_peak_S4|>
  mutate(delta_P = rowSums(across(c(Solar_kW, ECM_Cooling_kWh, Wind_kW, Shed_GBM)), na.rm = TRUE))

# Calculating the benefit from avoided power
avoided_peak_S4<- avoided_peak_S4|>
  group_by(Year)|>
  summarise(delta_P=min(delta_P))|> # this should be minimum reduction => Max overall peak is what is calculated 
  mutate(delta_P_MW=(delta_P)/10^3)

avoided_peak_S4<-left_join(avoided_peak_S4, yearly_tariff)
avoided_peak_S4<-avoided_peak_S4|>
  mutate(B_cap1=0 ,
         B_cap=0, 
         B_peak=delta_P/1000*Elec_Fixed_Price_MW,
         Market="S3:SR")

avoided_peak_summary<-rbind(avoided_peak_summary,avoided_peak_S4)

# Scenario 5 -----------------------------------------------------------------
S5_peak <- load_shed |> 
  filter(DR.Event == "Yes") |>
  group_by(Year, Date, Hour, Market)|> 
  summarise(Shed_GBM=sum(Shed_GBM, na.rm=T))|>
  group_by(Year, Date, Hour)|> 
  summarise(Shed_GBM=mean(Shed_GBM, na.rm=T))

avoided_peak_S5<-left_join(avoided_peak_building,S5_peak)
avoided_peak_S5<- avoided_peak_S5|>
  mutate(delta_P = rowSums(across(c(Solar_kW, ECM_Cooling_kWh, Wind_kW, Shed_GBM)), na.rm = TRUE))

# Calculating the benefit from avoided power
avoided_peak_S5<- avoided_peak_S5|>
  group_by(Year)|>
  summarise(delta_P=min(delta_P))|> # this should be minimum reduction => Max overall peak is what is calculated 
  mutate(delta_P_MW=(delta_P)/10^3)

avoided_peak_S5<-left_join(avoided_peak_S5, yearly_tariff)
avoided_peak_S5<-avoided_peak_S5|>
  mutate(B_cap1=delta_P/1000*Price*365 ,
         B_cap=B_cap1*0.8, 
         B_peak=delta_P/1000*Elec_Fixed_Price_MW,
         Market="S4:Combined")

avoided_peak_summary<-rbind(avoided_peak_summary,avoided_peak_S5)

# AVOIDED PEAK CALCULATIONS FOR THE ENTIRE CC-----------------------------------------------------------------

write.table(avoided_peak_summary, file="avoided_rfpeak_summary.csv", sep=",", row.names = FALSE)
save.image("C:/Users/malhotra.164/OneDrive - The Ohio State University/Yashvi Malhotra-KWEST/BCA/R_BCA/R_BCA/Raw_data/Images/Avoided_rfPeak.RData")
