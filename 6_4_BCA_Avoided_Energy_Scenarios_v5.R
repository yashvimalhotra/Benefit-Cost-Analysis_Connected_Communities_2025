library(lubridate)
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(GGally)

## avoided peak first
gc()
# Input data # Demand Side Management--------------------------------------------------------------
setwd("C:/Users/malhotra.164/OneDrive - The Ohio State University/Yashvi Malhotra-KWEST/BCA/R_BCA/R_BCA/Raw_data")
dr_building_id<-c(25,49,50,67,160,249)
elec_price=40.8/1000 #$/MW

avoided_peak_summary<- read.csv("avoided_Type_peak.csv")
avoided_peak_summary<-avoided_peak_summary|> dplyr::select(Year,Market,B_cap,Type)

# Capacity
capacity_shed1 <- read.csv("loads_shed_estimates_CC_2019_22.csv")
Building_type<-read.csv("Building_type.csv")

capacity_shed<-left_join(capacity_shed1,Building_type, by="id")
capacity_shed<-capacity_shed|>
  mutate(Year=year(Timestamp), Market="Capacity",Price=0)|> # null price
  dplyr::select(-Year,-towt_elec, -towt_cw,-Elec_Same_Day,-CW_Same_Day)

# Energy-SR Markets----------------------------------------------------------------
# Energy
energy_shed <- read.csv("loads_shed_Energy_Market.csv")
energy_shed<-energy_shed|> mutate(Market="Energy")|> 
  dplyr::rename(Price=total_lmp_rt) 

# SR
SR_shed <- read.csv("loads_shed_SR.csv")
SR_shed<-SR_shed|> mutate(Market="SR")|>
  dplyr::rename(Price=mcp_capped)

# Load Shed and Rebound---------------------------------------------------------------
shed<-rbind(capacity_shed, energy_shed, SR_shed)

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
         )|>
    dplyr::select(Year, Date,Hour,id, Name,DR.Building,Type,
           Price,Elec_Price,
           Market,DR.Event,Shed_GBM)
# Scenario 1 -----------------------------------------------------------------
# 1	6/21 buildings participate equally in the 3 markets
  
S1_energy<-load_shed|>
  filter(DR.Event=="Yes" & DR.Building=="Yes" & Market=="Capacity")|>
  group_by(Year,Market,Type)|> 
  summarise(GBM_Shed_kWh=sum(Shed_GBM))|>
  mutate(Market="BAU:Capacity-DR")
  
S1_energy<-left_join(S1_energy,avoided_peak_summary)
S1_energy<-S1_energy|> dplyr::rename(Compensation_dollar=B_cap) 

# Scenario 2 -----------------------------------------------------------------
# 2	All 21 buildings participate in the capacity market
S2_energy<-load_shed|>
  filter(DR.Event=="Yes" & Market=="Capacity")|>
  group_by(Year,Market,Type)|> 
  summarise(GBM_Shed_kWh=sum(Shed_GBM))|>
  mutate(Market="S1:Capacity")


S2_energy<-left_join(S2_energy,avoided_peak_summary)
S2_energy<-S2_energy|> dplyr::rename(Compensation_dollar=B_cap) 

# Scenario 3 -----------------------------------------------------------------
# 3	All 21 Buildings participate in the SR market 10 minutes
S3_energy<-load_shed|>
  filter(DR.Event=="Yes" & Market=="SR")|>
  mutate(Compensation_dollar=(Shed_GBM/1000)*Price)|> # Calculated for 10 min $/MWh
  group_by(Year,Market,Type)|> 
  summarise(GBM_Shed_kWh=sum(Shed_GBM),
            Compensation_dollar=sum(Compensation_dollar))|>
  mutate(GBM_Rebound_kWh=0)

# Scenario 4 -----------------------------------------------------------------
# 4	All 21 buildings participate in the energy markets 
S4_energy<-load_shed|>
  filter(DR.Event=="Yes" & Market=="Energy")|>
  dplyr::select(Year, Date, Market,Price,Shed_GBM,id,DR.Event,Type) |>
  mutate(Compensation_dollar=(Shed_GBM/1000)*Price)|> # Calculated for every hour # unit of price is $/MWh
  group_by(Year,Market,Type)|> 
  summarise(GBM_Shed_kWh=sum(Shed_GBM),
            Compensation_dollar=sum(Compensation_dollar))|>
  mutate(GBM_Rebound_kWh=0)

# Scenario 5 -----------------------------------------------------------------
S5_energy <- load_shed |> 
  filter(DR.Event == "Yes") |>
  dplyr::select(Year, Date, Hour, DR.Event,id, Market,Price,Shed_GBM,Type) |> 
  mutate(Compensation_dollar=case_when(Market=="Energy" ~ (Shed_GBM/1000)*Price,
                                       Market=="SR"~ (Shed_GBM/1000)*Price,
                                       Market=="Capacity"~ if_else(Shed_GBM!=0,1,0))) |> #Calculated for every hour # Check unit of price is $/kW
  group_by(Year, Date, Market,Type) |> 
  summarise(GBM_Shed_kWh=sum(Shed_GBM),
            Compensation_dollar=sum(Compensation_dollar))|>
  pivot_wider(names_from = Market, values_from = c(GBM_Shed_kWh,Compensation_dollar))

S5_cap<-S5_energy|>group_by(Year,Type)|>summarise(sc=sum(Compensation_dollar_Capacity,na.rm=T))

S5<-avoided_peak_summary|>filter(Market== "S4:Combined")
S5<-left_join(S5,S5_cap)
S5<-S5|>mutate(B_cap_day=B_cap/sc)

S5_energy <-left_join(S5_energy,S5)|>
  mutate(Compensation_dollar_Capacity=if_else(is.na(Compensation_dollar_Capacity),NA,B_cap_day))|>
  mutate(Total_Shed_kWh = rowMeans(across(c(GBM_Shed_kWh_SR, GBM_Shed_kWh_Energy, GBM_Shed_kWh_Capacity)), na.rm = TRUE),
         Total_Compensation_dollar = rowMeans(across(c(Compensation_dollar_SR, Compensation_dollar_Energy)), na.rm = TRUE))|>
  group_by(Year,Type)|>
  summarise(GBM_Shed_kWh=sum(Total_Shed_kWh),
            Compensation_dollar=sum(Total_Compensation_dollar, na.rm = TRUE))


S5_energy<-S5_energy|>mutate(Market="Combined")

## Plot #------------------------------------------------------------------
 
Scenarios_energy<-rbind(S1_energy,S2_energy,S3_energy,S4_energy,S5_energy) 
write.table(Scenarios_energy, file="Scenarios_energy_Type.csv", sep=",", row.names = FALSE)


## Plot #------------------------------------------------------------------
Scenarios_energy1<-read.csv("Scenarios_energy.csv")

Scenarios_energy<-Scenarios_energy1|>
  arrange(Year)|> 
  dplyr::select(Year,Market,Reduced_Shed_kWh,GBM_Shed_kWh,GBM_Rebound_kWh)|>
  mutate(Net_Load_Shed_kWh=GBM_Shed_kWh-GBM_Rebound_kWh)


dr_colors <- c("#f0f9e8","#bae4bc","#7bccc4","#43a2ca","#0868ac")
dr_order <- c("S1:BAU","S2:Capacity","S3:Energy","S4:SR","S5:Combined")
Scenarios_energy<- Scenarios_energy|>
  mutate(Market = recode(Market,
                         "BAU:Capacity-DR"= "S1:BAU",
                         "S1:Capacity"= "S2:Capacity",
                         "Energy"= "S3:Energy",
                         "SR"= "S4:SR",
                         "Combined"= "S5:Combined"))|>
  mutate(Market = factor(Market, levels = dr_order))



## Money Plot #------------------------------------------------------------------
money_colors <- c("#feebe2","#fbb4b9","#f768a1","#c51b8a","#7a0177")

exp=0
Scenarios_money<-Scenarios_energy1|>
  arrange(Year)|> 
  dplyr::select("Year", "Market","Compensation_dollar",
                "Reduced_Compensation_dollar")|>
  mutate(min_comp=(Reduced_Compensation_dollar)/10^exp,
         mean_comp=(Compensation_dollar)/10^exp, #-Rebound_dollar
         max_comp=(Compensation_dollar)/10^exp)|> # No rebound
  mutate(Market = recode(Market,
                         "BAU:Capacity-DR"= "S1:BAU",
                         "S1:Capacity"= "S2:Capacity",
                         "Energy"= "S3:Energy",
                         "SR"= "S4:SR",
                         "Combined"= "S5:Combined"))|>
  mutate(Market = factor(Market, levels = dr_order))


# Arranged ----------------------------------------------------------------

# Plot 1: Load Shed
p1 <- Scenarios_energy |>
  ggplot(aes(x = Market, y = Net_Load_Shed_kWh, fill = Market)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", size = 1) +
  geom_errorbar(aes(ymin = Reduced_Shed_kWh, ymax = GBM_Shed_kWh),
                width = 0.2, linewidth = 1) +
  facet_grid(~Year) +
  labs(y = "\nAggregated Load Shed (kWh)\n", x = NULL) +
  theme_classic() +
  theme(panel.background = element_rect(fill = NA, color = "black"), 
        text = element_text(size = 18),
        axis.text = element_text(size = 18, color = "black"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none") +
  scale_fill_manual(values = dr_colors) +
  scale_y_continuous(trans = "log10", breaks = scales::log_breaks(n = 7),
                     labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  ggtitle("A")

# Plot 2: Grid Compensation
p2 <- Scenarios_money |>
  ggplot(aes(x = Market, y = mean_comp, fill = Market)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", size = 1) +
  geom_errorbar(aes(ymin = min_comp, ymax = max_comp),
                width = 0.2, linewidth = 1) +
  facet_grid(~Year) +
  labs(y = "\nGrid Compensation ($)\n", x = NULL) +
  theme_classic() +
  theme(panel.background = element_rect(fill = NA, color = "black"), 
        text = element_text(size = 18),
        axis.text = element_text(size = 18, color = "black"),
        axis.text.x = element_text(angle = 55, hjust = 1),
        legend.position = "none") +
  scale_fill_manual(values = money_colors) +
  scale_y_continuous(trans = "log10", breaks = scales::log_breaks(n = 7),
                     labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  ggtitle("B")
library(ggpubr)
# Combine the plots
ggarrange(p1, p2, ncol = 1, nrow = 2, align = "v", heights = c(1, 1.5))



