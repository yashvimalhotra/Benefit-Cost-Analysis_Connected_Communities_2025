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
# elec_price=40.8/1000 #$/MW
# Elec_Fixed_Price=7.83 # $/kW

# Input data for Avoided Energy --------------------------------------------------------------
setwd("C:/Users/yashv/OneDrive - The Ohio State University/Yashvi Malhotra-KWEST/BCA/R_BCA/R_BCA/Raw_data")
wind_yearly <- read.csv("wind_cc.csv")
solar_yearly <- read.csv("solar_yearly.csv")

data_2019_22 <- read.csv("Building_CC_utilities_outliers")
baseline_energy <- read.csv("baseline_energy_CC_2019_20.csv")
#update ecm audit reports
EE_input_R <- read.csv("C:/Users/yashv/OneDrive - The Ohio State University/Yashvi Malhotra-KWEST/BCA/R_BCA/R_BCA/Raw_data/EE_input_R.csv")

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
Building_type<-data_2019_22|>dplyr::select(id,Type)|>distinct()

write.table(Building_type, file="Building_type.csv", sep=",", row.names = FALSE)

capacity_shed<-left_join(capacity_shed1,Building_type, by="id")
capacity_shed<-capacity_shed|>
  mutate(Year=year(Timestamp), Market="Capacity")|>
  left_join(yearly_tariff, by="Year")|>
  dplyr::select(-Year,-towt_elec, -towt_cw)

rf_capacity<- read.csv("rf_CC_2019_22_audit.csv")
rf_capacity<-left_join(rf_capacity,Building_type, by="id")
rf_capacity<-rf_capacity|>
  mutate(Year=year(Timestamp), Market="Capacity")|>
  left_join(yearly_tariff, by="Year")|>
  dplyr::select(-Year,-Elec_Same_Day,-CW_Same_Day) #-towt_elec, -towt_cw,

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
  
ecm_savings_yearly <- ecm_savings |>
  group_by(Year) |> 
  summarise(across(Capital_Costs:Building.Steam.Savings_mmBTU, ~sum(.x, na.rm = TRUE))) |>
  arrange(Year) |> 
  mutate(Cooling_savings_MWh = (Building.Electric.Savings_kWh + Building.CW.Savings_kWh)/1000)

write.table(ecm_savings_yearly, file="ecm_savings_yearly.csv", sep=",", row.names = FALSE)

# Load Shed and Rebound---------------------------------------------------------------
## All under-estimates have been set to 0
# Rebound would exist for buildings that actually shed load
# rebound is when the actual energy is higher than baseline however we have cases where shed is negative so 
# we should exclude the load shed in those cases

load_shed<-capacity_shed
load_shed<-load_shed|>
  mutate(DR.Building = if_else(id %in% dr_building_id, "Yes", "No"),
         Year=year(Date),
         CW_elec=if_else(is.na(CW_elec), 0, CW_elec),
         gbm_cw=if_else(is.na(gbm_cw) | gbm_cw<0, 0,gbm_cw),
         gbm_elec=if_else(gbm_elec<0, 0, gbm_elec),
         Actual = Electricity_Consumed_kWh + CW_elec,
         baseline_GBM = gbm_elec + gbm_cw,
         Shed_GBM=if_else((baseline_GBM-Actual)<0, 0, baseline_GBM-Actual) #Actual<Baseline
         )|>
  mutate(Rebound_GBM=if_else(Shed_GBM==0 | (Actual-baseline_GBM)<0 , 0, Actual-baseline_GBM),  # Baseline<Actual | if shed is 0
         # Rebound_GBM=if_else(Shed_GBM==0 , 0, Actual-baseline_GBM),  # if shed is 0
         Rebound = if_else(lag(DR.Event, 1, order_by = Date) == "Yes" & Hour <= 23, "Yes", "No"), 
         Rebound = if_else(lag(DR.Event, 2, order_by = Date) == "Yes" & Hour <= 23, "Yes", "No"),
         Rebound = if_else(lag(DR.Event, 3, order_by = Date) == "Yes" & Hour <= 23, "Yes", "No"))

# Reduced Flexibility -----------------------------------------------------------------
reduced_flex<-rf_capacity

## All under-estimates have been set to 0
reduced_flex<-reduced_flex|>
  mutate(DR.Building = if_else(id %in% dr_building_id, "Yes", "No"),
         Year=year(Date),CW_elec=if_else(is.na(CW_elec), 0, CW_elec),
         gbm_cw=if_else(is.na(gbm_cw) | gbm_cw<0, 0,gbm_cw),
         gbm_elec=if_else(gbm_elec<0, 0, gbm_elec),
         Actual = Electricity_Consumed_kWh + CW_elec,
         baseline_GBM = gbm_elec + gbm_cw,
         Shed_GBM=if_else((baseline_GBM-Actual)<0, 0, baseline_GBM-Actual) #Actual<Baseline
         )

# Scenario 1 -----------------------------------------------------------------
# 1	6/21 buildings participate equally in the 3 markets
  
# S1_peak<-load_shed|>
#   filter(DR.Event=="Yes" & DR.Building=="Yes" & Market=="Capacity")|>
#   group_by(Year,Date, id,Name,Market,Price)|>
#   summarise(GBM_Shed_kW=mean(Shed_GBM),
#             Peak_of_GBM_Shed_kW=max(Shed_GBM))|>
#   group_by(Year,Price)|>
#   summarise(Mean_Shed_kW=sum(GBM_Shed_kW),
#             Peak_Shed_kW=sum(Peak_of_GBM_Shed_kW),
#             n_events = n_distinct(Date))|>
#   mutate(Mean_Shed=Mean_Shed_kW/1000*Price*n_events ,
#          Peak_Shed=Peak_Shed_kW/1000*Price*n_events,
#          Mean_COMP_CSP_d=Mean_Shed*0.8, # Cut for CSP
#          Peak_COMP_CSP_d=Peak_Shed*0.8)

S1_energy<-load_shed|>
  filter(DR.Event=="Yes" & DR.Building=="Yes" & Market=="Capacity")|>
  group_by(Year,Market)|>
  summarise(GBM_Shed_kWh=sum(Shed_GBM))
  # left_join(S1_peak |> dplyr::select(Year, Mean_COMP_CSP_d, Peak_COMP_CSP_d), by = "Year") |>
  # rename(Compensation_dollar = Mean_COMP_CSP_d,
  #        max_Compensation_dollar = Peak_COMP_CSP_d)

# rebound_S1_peak<-load_shed|>
#   filter(Rebound=="Yes"& DR.Building=="Yes" & DR.Event!="Yes"& Market=="Capacity")|>
#   group_by(DR.Building,Year,Date, id,Name,Market,Elec_Fixed_Price)|>
#   summarise(GBM_Rebound_kW=mean(Rebound_GBM),
#             Peak_of_GBM_Rebound_kW=max(Rebound_GBM))|>
#   group_by(Year,Elec_Fixed_Price)|>
#   summarise(Mean_rebound_kW=sum(GBM_Rebound_kW),
#             Peak_rebound_kW=sum(Peak_of_GBM_Rebound_kW))|>
#   mutate(Mean_rebound_d=Mean_rebound_kW*Elec_Fixed_Price,
#          Peak_rebound_d=Peak_rebound_kW*Elec_Fixed_Price)

rebound_S1_energy<-load_shed|>
  filter(Rebound=="Yes"& DR.Building=="Yes" & DR.Event!="Yes"& Market=="Capacity")|>
  group_by(Year,Market)|>
  summarise(GBM_Rebound_kWh=sum(Rebound_GBM))
  # left_join(rebound_S1_peak |> dplyr::select(Year, Mean_rebound_d, Peak_rebound_d), by = "Year") |>
  # rename(Rebound_dollar = Mean_rebound_d,
  #        max_Rebound_dollar = Peak_rebound_d)

# rf_S1_peak<-reduced_flex|>
#   filter(DR.Event=="Yes" & DR.Building=="Yes" & Market=="Capacity")|>
#   group_by(DR.Building,Year,Date, id,Name,Market,Price)|>
#   summarise(GBM_Shed_kW=mean(Shed_GBM),
#             Peak_of_GBM_Shed_kW=max(Shed_GBM))|>
#   group_by(Year,Price)|>
#   summarise(Mean_Shed_kW=sum(GBM_Shed_kW),
#             Peak_Shed_kW=sum(Peak_of_GBM_Shed_kW),
#             n_events = n_distinct(Date))|>
#   mutate(Mean_Shed=Mean_Shed_kW/1000*Price*n_events ,
#          Peak_Shed=Peak_Shed_kW/1000*Price*n_events,
#          Mean_RedCOMP_CSP_d=Mean_Shed*0.8, # Cut for CSP
#          Peak_RedCOMP_CSP_d=Peak_Shed*0.8)

rf_S1_energy<-reduced_flex|>
  filter(DR.Event=="Yes" & DR.Building=="Yes" & Market=="Capacity")|>#Reduced Shed can be higher than estimated shed!
  group_by(Year,Market)|>
  summarise(Reduced_Shed_kWh=sum(Shed_GBM))
  # left_join(rf_S1_peak |> dplyr::select(Year, Mean_RedCOMP_CSP_d, Peak_RedCOMP_CSP_d), by = "Year") |>
  # rename(Reduced_Compensation_dollar = Mean_RedCOMP_CSP_d,
  #        max_Reduced_Compensation_dollar = Peak_RedCOMP_CSP_d)

S1_energy<-left_join(S1_energy, rebound_S1_energy)
S1_energy<-left_join(S1_energy, rf_S1_energy)
S1_energy<-S1_energy|>mutate(Market="Capacity-DR")

S1_energy<-S1_energy|>
  mutate(percent=(Reduced_Shed_kWh-GBM_Shed_kWh)*100/GBM_Shed_kWh)
# S1_peak<-left_join(S1_peak, rebound_S1_peak)
# S1_peak<-left_join(S1_peak, rf_S1_peak)
# S1_peak<-S1_peak|>mutate(Market="Capacity-DR")

# Combined ----------------------------------------------------------------
# Rebound affects only Load Shed AND Reduced Flexibility is its own category 

Scenarios_energy<-(S1_energy)
Scenarios_energy<-Scenarios_energy|>arrange(Year)|> 
  dplyr::select(Year,Market,Reduced_Shed_kWh,GBM_Shed_kWh,GBM_Rebound_kWh)|>
  mutate(Net_Load_Shed_kWh=GBM_Shed_kWh+GBM_Rebound_kWh)
# Baseline ----------------------------------------------------------------
actual_energy_2019_22 <- data_2019_22 |>
  mutate(Actual_Cooling_kWh = (Electricity_Consumed_kWh + CW_elec),
    Steam_Consumed_kBTU = if_else(Decision == "Steam" | Decision == "Steam+HW", Steam_Consumed_kBTU, 0)) |>
  dplyr::select(Timestamp,id,Electricity_Consumed_kWh,CW_elec,Actual_Cooling_kWh, Steam_Consumed_kBTU)

baseline_energy_2020 <- baseline_energy |>
  mutate(Year = year(Timestamp),
         Baseline_Cooling_kWh = (gbm_elec + gbm_cw),
         Baseline_Heating_kBTU = gbm_steam) |> 
  dplyr::rename(model_elec_kWh=gbm_elec,
         model_cw_kWh=gbm_cw)|>
  dplyr::select(Timestamp,id,model_elec_kWh,model_cw_kWh,Baseline_Cooling_kWh, Baseline_Heating_kBTU)

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

avoided_energy<-actual_energy_2019_22|>filter(year(Timestamp)==2019)|>
  dplyr::rename(Baseline_Cooling_kWh=Actual_Cooling_kWh,
         Baseline_Heating_kBTU=Steam_Consumed_kBTU,
         model_elec_kWh=Electricity_Consumed_kWh,
         model_cw_kWh=CW_elec)
avoided_energy<-rbind(avoided_energy,baseline_energy_2020,forecast_util)
avoided_energy<-left_join(actual_energy_2019_22,avoided_energy)

avoided_energy<-avoided_energy|> mutate(Date=date(Timestamp), Hour=hour(Timestamp), Year=year(Timestamp))

# Avoided Energy ----------------------------------------------------------

avoided_energy_error<-avoided_energy|>
  group_by(Year)|>
  summarise(Baseline_Cooling_MWh = sum(Baseline_Cooling_kWh, na.rm = TRUE)/10^3,    
            SD_Cooling_MWh = sd(Baseline_Cooling_kWh, na.rm = TRUE), # see if i need to divide it with 10^3
            Baseline_Heating_mMBTU = sum(Baseline_Heating_kBTU, na.rm = TRUE)/10^3,
            SD_Heating_mMBTU = sd(Baseline_Heating_kBTU, na.rm = TRUE))

avoided_energy_error<-left_join(avoided_energy_error,ecm_savings_yearly)
avoided_energy_error<-avoided_energy_error|>dplyr::select(-Capital_Costs,-OM_Costs,-Building.Electric.Savings_kWh,-Building.CW.Savings_kWh)
avoided_energy_error<-left_join(avoided_energy_error,Scenarios_energy)
avoided_energy_error<-left_join(avoided_energy_error,re)
# see if i need to divide it with 10^3

write.table(avoided_energy_error, file="avoided_energy_yearly.csv", sep=",", row.names = FALSE)

avoided_energy_percentage <- avoided_energy_error |>
  mutate(Net_Load_Shed_MWh=Net_Load_Shed_kWh/1000,
         CumCooling_savings_MWh=cumsum(Cooling_savings_MWh),
         CumHeating_savings_mmBTU=cumsum(Building.Steam.Savings_mmBTU),
        total_cooling = Wind_ppa_MWh + CumCooling_savings_MWh + Net_Load_Shed_MWh + Solar_MWh) |>
  transmute(percent_wind = round(100 * Wind_ppa_MWh / total_cooling, 1),
    percent_ecm = round(100 * CumCooling_savings_MWh / total_cooling, 1),
    percent_shed = round(100 * Net_Load_Shed_MWh / total_cooling, 1),
    percent_solar = round(100 * Solar_MWh / total_cooling, 1))

# Avoided Cooling and Heating for Plotting -------

# avoided_steam<- avoided_energy_error|> 
#   mutate("Avoided Heating Energy"=cumsum(Building.Steam.Savings_mmBTU))|>
#   dplyr::select(Year,Baseline_Heating_mMBTU,SD_Heating_mMBTU,"Avoided Heating Energy")|> 
#   dplyr::rename("Baseline Heating Load"="Baseline_Heating_mMBTU")|> 
#   pivot_longer(cols = c("Avoided Heating Energy","Baseline Heating Load"), 
#                names_to = "Measure", values_to = "value")|> 
#   mutate(Measure = factor(Measure, levels = c("Baseline Heating Load", "Avoided Heating Energy")), 
#          SD_heating = ifelse(Measure == "Avoided Heating Energy", 0, SD_Heating_mMBTU))|>
#   dplyr::select(-SD_Heating_mMBTU)

avoided_steam <- avoided_energy_error |>
  mutate(`Avoided Heating Energy` = cumsum(Building.Steam.Savings_mmBTU * 1.05506)) |>  # Convert to GJ
  dplyr::select(Year, Baseline_Heating_mMBTU, SD_Heating_mMBTU, `Avoided Heating Energy`) |>
  dplyr::mutate(`Baseline Heating Load` = Baseline_Heating_mMBTU * 1.05506,  # Convert to GJ
                SD_Heating_GJ = SD_Heating_mMBTU * 1.05506) |>               # Convert to GJ
  dplyr::select(-Baseline_Heating_mMBTU, -SD_Heating_mMBTU) |>
  pivot_longer(cols = c("Avoided Heating Energy", "Baseline Heating Load"),
               names_to = "Measure", values_to = "value") |>
  mutate(
    Measure = factor(Measure, levels = c("Baseline Heating Load", "Avoided Heating Energy")),
    SD_heating = ifelse(Measure == "Avoided Heating Energy", 0, SD_Heating_GJ)
  ) |>
  dplyr::select(-SD_Heating_GJ)



custom_order <- c("Baseline Heating Load","Avoided Heating Energy")
palette_colors <- c('#999999','#e41a1c')

#####
avoided_elec<-avoided_energy_error|> 
  mutate(Net_Load_Shed_MWh=Net_Load_Shed_kWh/1000,
         Reduced_Shed_MWh=Reduced_Shed_kWh/1000,
         "Baseline Cooling Load"=Baseline_Cooling_MWh,
         Cooling_savings_MWh=cumsum(Cooling_savings_MWh), # cumulative savings
         "Avoided Cooling Energy" = Wind_ppa_MWh + Cooling_savings_MWh + Net_Load_Shed_MWh + Solar_MWh) |>
  dplyr::select(Year,"Avoided Cooling Energy","Baseline Cooling Load","SD_Cooling_MWh",
                Reduced_Shed_MWh)|>
  reshape2::melt(id =c("Year","SD_Cooling_MWh","Reduced_Shed_MWh"))|>
  mutate(ymin = case_when(
      variable == "Baseline Cooling Load" ~ value - SD_Cooling_MWh,
      variable == "Avoided Cooling Energy" ~ value - Reduced_Shed_MWh),
    ymax = case_when(variable == "Baseline Cooling Load" ~ value + SD_Cooling_MWh,
      variable == "Avoided Cooling Energy" ~ value ),
    variable = factor(variable, levels = c("Baseline Cooling Load","Avoided Cooling Energy")))

custom_order3 <- c("Baseline Cooling Load","Avoided Cooling Energy")
palette_colors3 <- c('#999999','darkblue')
# Plot --------------------------------------------------------------------
cool_plot <- ggplot(avoided_elec, aes(x = Year, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge(), aes(y = value), color = "black", linewidth = 1) +
  geom_errorbar(aes(x = Year,
                    ymin =ymin,
                    ymax =ymax),
                position = position_dodge(width = 0.9),
                width = 0.3, linewidth = 1.5, color = "black") +
  theme_classic() +
  theme(panel.background = element_rect(fill = NA, color = "black"), 
        text = element_text(size = 25), 
        axis.text = element_text(size = 25, color = "black"),  # Black font for axis text
        axis.title.x = element_blank(),  # Remove x-axis title
        axis.text.x = element_blank(),  # Remove x-axis labels
        axis.ticks.x = element_blank(),  # Remove x-axis ticks
        legend.position = c(0.15, 0.85),  # top-left
        legend.key = element_rect(fill = "white")) +
  labs(y = "\n(MWh-Log scale)", 
       fill = NULL) +
  scale_fill_manual(breaks = custom_order3, values = palette_colors3)+ 
  scale_y_continuous(trans = "log10", breaks = scales::log_breaks(n = 6),
                     labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  coord_cartesian(ylim = c(10, 10^6)) + ggtitle(" A")
 
heat_plot<- ggplot(avoided_steam, aes(x = Year, fill = Measure)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), aes(y = value), color = "black", linewidth = 1) +
  geom_errorbar(aes(x = Year, 
                    ymin =value-SD_heating, 
                    ymax =value+SD_heating),
                position = position_dodge(width = 0.9),
                width = 0.3, linewidth = 1.5, color = "black") +
  #labs(y = "\n(mMBTU - Log scale)", x = "Year", fill = NULL) +
  labs(y = "\n(GJ - Log scale)", x = "Year", fill = NULL)
  theme_classic() +
  theme(panel.background = element_rect(fill = NA, color = "black"), 
        text = element_text(size = 25), 
        axis.text = element_text(size = 25, color = "black"),
        axis.text.x = element_text(hjust = 1, color = "black"),
        legend.position = c(0.15, 0.85),
        legend.key = element_rect(fill = "white")) +
  scale_fill_manual(values = palette_colors) +
  scale_y_continuous(trans = "log10", breaks = scales::log_breaks(n = 7),
                     labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  coord_cartesian(ylim = c(10, 10^7)) +
  ggtitle(" B")

ggarrange(cool_plot, heat_plot, ncol = 1, nrow = 2, align = "v", heights = c(1, 1))

#######
path_to_fig <- "C:/Users/yashv/OneDrive - The Ohio State University/Yashvi Malhotra-KWEST/BCA/BCA_paper/Figures/"

ggsave(file.path(path_to_fig, "avoided_heating_cooling.png"),
       plot = last_plot(), width = 10, height = 8, dpi = 300)

