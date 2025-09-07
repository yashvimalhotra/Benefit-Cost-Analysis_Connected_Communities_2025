library(lubridate)
library(tidyr)
library(tidyverse)
library(dplyr)

gc()
# Input data # Demand Side Management--------------------------------------------------------------
setwd("C:/Users/malhotra.164/OneDrive - The Ohio State University/Yashvi Malhotra-KWEST/BCA/R_BCA/R_BCA/Raw_data")
dr_building_id<-c(25,49,50,67,160,249)
elec_price=40.8/1000 #$/MW


# Energy-SR Markets----------------------------------------------------------------
# Energy LMP is $/MWh
energy_shed <- read.csv("loads_shed_Energy_Market.csv")
energy_shed<-energy_shed|> mutate(Market="Energy")|> dplyr::rename(Price=total_lmp_rt) 
rf_energy<- read.csv("rf_CC_Energy.csv")
rf_energy<-rf_energy|>mutate(Market="Energy")|>dplyr::rename(Price=total_lmp_rt) 

# Energy LMP 2
energy_shed2 <- read.csv("loads_shed_Energy_200.csv")
energy_shed2<-energy_shed2|> mutate(Market="Energy2")|> dplyr::rename(Price=total_lmp_rt) 
rf_energy2<- read.csv("rf_200_Energy.csv")
rf_energy2<-rf_energy2|>mutate(Market="Energy2")|>dplyr::rename(Price=total_lmp_rt) 

# Load Shed and Rebound---------------------------------------------------------------
shed<-rbind(energy_shed, energy_shed2)
load_shed<-shed|>
  mutate(DR.Building = if_else(id %in% dr_building_id, "Yes", "No"),
         Elec_Price=elec_price, 
         Year=year(Date),
         CW_elec=if_else(is.na(CW_elec), 0, CW_elec),
         Actual = Electricity_Consumed_kWh + CW_elec,
         gbm_cw=if_else(is.na(gbm_cw) | gbm_cw<0, 0,gbm_cw),
         gbm_elec=if_else(gbm_elec<0, 0, gbm_elec),
         baseline_GBM = gbm_elec + gbm_cw,
         Shed_GBM=if_else((baseline_GBM-Actual)<0, 0, baseline_GBM-Actual), #Actual<Baseline
         Rebound_GBM=if_else(Shed_GBM==0 | (Actual-baseline_GBM)<0 , 0, Actual-baseline_GBM))|>
    dplyr::select(Year, Date,Hour,id, Name,DR.Building,
           Price,Elec_Price,Gross.Square.Feet,
           Market,DR.Event,Shed_GBM,Rebound_GBM) |>  #Shed_Same_Day,Shed_GBM1, 
  mutate(Rebound = if_else(lag(DR.Event, 1, order_by = Date) == "Yes" & Hour <= 23, "Yes", "No"),
         Rebound = if_else(lag(DR.Event, 2, order_by = Date) == "Yes" & Hour <= 23, "Yes", "No"),
         Rebound = if_else(lag(DR.Event, 3, order_by = Date) == "Yes" & Hour <= 23, "Yes", "No"))

# Reduced Flexibility -----------------------------------------------------------------

reduced_flex<-rbind(rf_energy,rf_energy2)
reduced_flex<-reduced_flex|>
  mutate(DR.Building = if_else(id %in% dr_building_id, "Yes", "No"),
         Year=year(Date),CW_elec=if_else(is.na(CW_elec), 0, CW_elec),
         gbm_cw=if_else(is.na(gbm_cw) | gbm_cw<0, 0,gbm_cw),
         gbm_elec=if_else(gbm_elec<0, 0, gbm_elec),
         Actual = Electricity_Consumed_kWh + CW_elec,
         baseline_GBM = gbm_elec + gbm_cw,
         Shed_GBM=if_else((baseline_GBM-Actual)<0, 0, baseline_GBM-Actual) #Actual<Baseline
         )


# Scenario 4 -----------------------------------------------------------------
# 4	All 21 buildings participate in the energy markets 
S4_energy<-load_shed|>
  filter(DR.Event=="Yes" & Market=="Energy")|>
  dplyr::select(Year, Date, Market,Price,Shed_GBM,id,DR.Event,Elec_Price) |>
  mutate(Compensation_dollar=(Shed_GBM/1000)*Price)|> # Calculated for every hour # unit of price is $/MWh
  group_by(Year,Market)|> 
  summarise(GBM_Shed_kWh=sum(Shed_GBM),
            Compensation_dollar=sum(Compensation_dollar))|>
  mutate(GBM_Rebound_kWh=0)

rf_S4_energy<-reduced_flex|>
  filter(DR.Event=="Yes" & Market=="Energy")|>
  mutate(Compensation_dollar=(Shed_GBM/1000)*Price)|> 
  group_by(Year,Market)|> 
  summarise(Reduced_Shed_kWh=sum(Shed_GBM),
            Reduced_Compensation_dollar=sum(Compensation_dollar))

S4_energy<-left_join(S4_energy,rf_S4_energy)

report<-S4_energy|>
  mutate(GBM_Shed_kWh=round(GBM_Shed_kWh,0),
         Reduced_Shed_kWh=round(Reduced_Shed_kWh,0),
         Compensation_dollar=round(Compensation_dollar,0),
         Reduced_Compensation_dollar=round(Reduced_Compensation_dollar,0),
         rf_reduction=round((Reduced_Shed_kWh-GBM_Shed_kWh)*100/GBM_Shed_kWh,0))|>
  select(-GBM_Rebound_kWh)

# Scenario 4.2 -----------------------------------------------------------------
S4_energy2<-load_shed|>
  filter(DR.Event=="Yes" & Market=="Energy2")|>
  dplyr::select(Year, Date, Market,Price,Shed_GBM,id,DR.Event,Elec_Price) |>
  mutate(Compensation_dollar=(Shed_GBM/1000)*Price)|> # Calculated for every hour # unit of price is $/MWh
  group_by(Year,Market,Elec_Price)|> 
  summarise(GBM_Shed_kWh=sum(Shed_GBM),
            Compensation_dollar=sum(Compensation_dollar))|>
  mutate(GBM_Rebound_kWh=0,B_cap2=0,
         Reduced_Shed_kWh=0,Reduced_Compensation_dollar=0)

rf_S4_energy2<-reduced_flex|>
  filter(DR.Event=="Yes" & Market=="Energy2")|>
  mutate(Compensation_dollar=(Shed_GBM/1000)*Price)|> 
  group_by(Year,Market)|> 
  summarise(Reduced_Shed_kWh=sum(Shed_GBM),
            Reduced_Compensation_dollar=sum(Compensation_dollar))

S4_energy2<-left_join(S4_energy2,rf_S4_energy2)

report2<-S4_energy2|>
  mutate(GBM_Shed_kWh=round(GBM_Shed_kWh,0),
         Reduced_Shed_kWh=round(Reduced_Shed_kWh,0),
         Compensation_dollar=round(Compensation_dollar,0),
         Reduced_Compensation_dollar=round(Reduced_Compensation_dollar,0),
         rf_reduction=round((Reduced_Shed_kWh-GBM_Shed_kWh)*100/GBM_Shed_kWh,0))|>
  select(-GBM_Rebound_kWh)

report3<-rbind(report,report2)