library(lubridate)
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(GGally)

gc()
# Input data # Demand Side Management--------------------------------------------------------------
setwd("C:/Users/yashv/OneDrive - The Ohio State University/Yashvi Malhotra-KWEST/BCA/R_BCA/R_BCA/Raw_data")
dr_building_id<-c(25,49,50,67,160,249)
elec_price=40.8/1000 #$/MW

avoided_peak_summary<- read.csv("avoided_peak_summary.csv") # To get capacity market costs
avoided_peak_summary<-avoided_peak_summary|> dplyr::select(Year,Market,B_cap,B_cap2) # strike price 2025

avoided_rfpeak_summary<- read.csv("avoided_rfpeak_summary.csv") # To get capacity market costs
avoided_rfpeak_summary<-avoided_rfpeak_summary|> dplyr::select(Year,Market,B_cap)

# Capacity
capacity_shed1 <- read.csv("loads_shed_estimates_CC_2019_22.csv")
Building_type<-read.csv("Building_type.csv")
capacity_shed<-left_join(capacity_shed1,Building_type)
capacity_shed<-capacity_shed|>
  mutate(Year=year(Timestamp), Market="Capacity",Price=0)|> # null price
  dplyr::select(-Year,-towt_elec,-towt_cw)# -Elec_Same_Day,-CW_Same_Day

rf_capacity<- read.csv("rf_CC_2019_22_audit.csv")
rf_capacity<-rf_capacity|>
  mutate(Year=year(Timestamp), Market="Capacity",Price=0)|> # null price
  dplyr::select(-Year,-Elec_Same_Day,-CW_Same_Day)#
# Energy-SR Markets----------------------------------------------------------------
# Energy LMP is $/MWh
energy_shed <- read.csv("loads_shed_Energy_Market.csv")
energy_shed<-energy_shed|> mutate(Market="Energy")|> dplyr::rename(Price=total_lmp_rt) 
rf_energy<- read.csv("rf_CC_Energy.csv")
rf_energy<-rf_energy|>mutate(Market="Energy")|>dplyr::rename(Price=total_lmp_rt) 

# SR
SR_shed <- read.csv("loads_shed_SR.csv")
SR_shed<-SR_shed|> mutate(Market="SR")|>dplyr::rename(Price=mcp_capped)

rf_SR<- read.csv("rf_CC_SR.csv")
rf_SR<- rf_SR|>mutate(Market="SR")|> dplyr::rename(Price=mcp_capped)
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
         Rebound_GBM=if_else(Shed_GBM==0 | (Actual-baseline_GBM)<0 , 0, Actual-baseline_GBM))|>
    dplyr::select(Year, Date,Hour,id, Name,DR.Building,
           Price,Elec_Price,Gross.Square.Feet,
           Market,DR.Event,Shed_GBM,Rebound_GBM) |>  #Shed_Same_Day,Shed_GBM1, 
  mutate(Rebound = if_else(lag(DR.Event, 1, order_by = Date) == "Yes" & Hour <= 23, "Yes", "No"),
         Rebound = if_else(lag(DR.Event, 2, order_by = Date) == "Yes" & Hour <= 23, "Yes", "No"),
         Rebound = if_else(lag(DR.Event, 3, order_by = Date) == "Yes" & Hour <= 23, "Yes", "No"))

# Reduced Flexibility -----------------------------------------------------------------

reduced_flex<-rbind(rf_capacity,rf_energy,rf_SR)
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


S1_energy<-load_shed|>
  filter(DR.Event=="Yes" & DR.Building=="Yes" & Market=="Capacity")|>
  group_by(Year,Market)|> 
  summarise(GBM_Shed_kWh=sum(Shed_GBM))|>
  mutate(Market="BAU:Capacity-DR")
  
S1_energy<-left_join(S1_energy,avoided_peak_summary)
S1_energy<-S1_energy|> 
  dplyr::rename(Compensation_dollar=B_cap) 

rebound_S1_energy<-load_shed|>
  filter(Rebound=="Yes"& DR.Building=="Yes" & DR.Event!="Yes"& Market=="Capacity")|>
  group_by(Year,Market)|> 
  summarise(GBM_Rebound_kWh=sum(Rebound_GBM))|>
  mutate(Market="BAU:Capacity-DR")

S1_energy<-left_join(S1_energy, rebound_S1_energy)

rf_S1_energy<-reduced_flex|>
  filter(DR.Event=="Yes" & DR.Building=="Yes" & Market=="Capacity")|>#Reduced Shed can be higher than estimated shed!
  group_by(Year,Market)|>  
  summarise(Reduced_Shed_kWh=sum(Shed_GBM))|>
  mutate(Market="BAU:Capacity-DR")

rf_S1_energy<-left_join(rf_S1_energy,avoided_rfpeak_summary) # Reduced Flexibility 
rf_S1_energy<-rf_S1_energy|> dplyr::rename(Reduced_Compensation_dollar=B_cap) 
S1_energy<-left_join(S1_energy, rf_S1_energy)

report1<-S1_energy|>
  mutate(GBM_Shed_MWh=round(GBM_Shed_kWh/10^3,2),
         Reduced_Shed_MWh=round(Reduced_Shed_kWh/10^3,2),
         Compensation_dollar=round(Compensation_dollar/10^3,0),
         Reduced_Compensation_dollar=round(Reduced_Compensation_dollar/10^3,0),
         rf_reduction=round((Reduced_Shed_kWh-GBM_Shed_kWh)*100/GBM_Shed_kWh,0))|>
  select(-GBM_Shed_kWh,-Reduced_Shed_kWh,-GBM_Rebound_kWh)

# Scenario 2 -----------------------------------------------------------------
# 2	All 21 buildings participate in the capacity market
report_SI_shed<-load_shed|>
  filter(DR.Event=="Yes" & Market=="Capacity")|>
  mutate(GBM_Shed_Wft=Shed_GBM*1000/Gross.Square.Feet)|>
  group_by(Name,DR.Building, Year)|> 
  summarise(GBM_Shed_Wft=round(mean(GBM_Shed_Wft),2))|>
  pivot_wider(names_from = Year, values_from = c(GBM_Shed_Wft))



S2_energy<-load_shed|>
  filter(DR.Event=="Yes" & Market=="Capacity")|>
  group_by(Year,Market)|> 
  summarise(GBM_Shed_kWh=sum(Shed_GBM))|>
  mutate(Market="S1:Capacity")

#breakdown
breakdown_id<- load_shed |>
  filter(DR.Event == "Yes", Market == "Capacity") |>
  group_by(Year, id, Name,Date, Hour) |>
  summarise(GBM_Shed_kWh = sum(Shed_GBM, na.rm = TRUE), .groups = "drop") |>
  group_by(Year) |>
  slice_max(order_by = GBM_Shed_kWh, n = 1) # Top 1 per year


S2_energy<-left_join(S2_energy,avoided_peak_summary)
S2_energy<-S2_energy|> dplyr::rename(Compensation_dollar=B_cap) 

rebound_S2_energy<-load_shed|>
  filter(Rebound=="Yes" & DR.Event!="Yes"& Market=="Capacity")|>
  group_by(Year,Market)|> 
  summarise(GBM_Rebound_kWh=sum(Rebound_GBM))|>
  mutate(Market="S1:Capacity")

S2_energy<-left_join(S2_energy, rebound_S2_energy)

rf_S2_energy<-reduced_flex|>
  filter(DR.Event=="Yes" & Market=="Capacity")|>
  group_by(Year,Market)|>  
  summarise(Reduced_Shed_kWh=sum(Shed_GBM))|>
  mutate(Market="S1:Capacity")

rf_S2_energy<-left_join(rf_S2_energy,avoided_rfpeak_summary) # Reduced Flexibility 
rf_S2_energy<-rf_S2_energy|> dplyr::rename(Reduced_Compensation_dollar=B_cap) 
S2_energy<-left_join(S2_energy, rf_S2_energy)

report<-S2_energy|>
  mutate(GBM_Shed_MWh=round(GBM_Shed_kWh/10^3,0),
         Reduced_Shed_MWh=round(Reduced_Shed_kWh/10^3,0),
         Compensation_dollar=round(Compensation_dollar/10^3,0),
         Reduced_Compensation_dollar=round(Reduced_Compensation_dollar/10^3,0),
         rf_reduction=round((Reduced_Shed_kWh-GBM_Shed_kWh)*100/GBM_Shed_kWh,0))|>
  select(-GBM_Shed_kWh,-Reduced_Shed_kWh,-GBM_Rebound_kWh)

# Scenario 3 -----------------------------------------------------------------
# 3	All 21 Buildings participate in the SR market 10 minutes
S3_energy<-load_shed|>
  filter(DR.Event=="Yes" & Market=="SR")|>
  mutate(Compensation_dollar=(Shed_GBM/1000)*Price)|> # Calculated for 10 min $/MWh
  group_by(Year,Market)|> 
  summarise(GBM_Shed_kWh=sum(Shed_GBM),
            Compensation_dollar=sum(Compensation_dollar))|>
  mutate(GBM_Rebound_kWh=0)

rf_S3_energy<-reduced_flex|>
  filter(DR.Event=="Yes" & Market=="SR")|> 
  mutate(Compensation_dollar=(Shed_GBM/1000)*Price)|> 
  group_by(Year,Market)|> 
  summarise(Reduced_Shed_kWh=sum(Shed_GBM),
            Reduced_Compensation_dollar=sum(Compensation_dollar))

S3_energy<-left_join(S3_energy,rf_S3_energy)

report<-S3_energy|>
  mutate(GBM_Shed_kWh=round(GBM_Shed_kWh,0),
         Reduced_Shed_kWh=round(Reduced_Shed_kWh,0),
         Compensation_dollar=round(Compensation_dollar,0),
         Reduced_Compensation_dollar=round(Reduced_Compensation_dollar,0),
         rf_reduction=round((Reduced_Shed_kWh-GBM_Shed_kWh)*100/GBM_Shed_kWh,0))|>
  select(-GBM_Rebound_kWh)

# Scenario 4 -----------------------------------------------------------------
# 4	All 21 buildings participate in the energy markets 
S4_energy<-load_shed|>
  filter(DR.Event=="Yes" & Market=="Energy")|>
  dplyr::select(Year, Date, Market,Price,Shed_GBM,id,DR.Event) |>
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

# Scenario 5 -----------------------------------------------------------------
S5_energy <- load_shed |> 
  filter(DR.Event == "Yes") |>
  dplyr::select(Year, Date, Hour, DR.Event,id, Market,Price,Shed_GBM) |> 
  mutate(Compensation_dollar=case_when(Market=="Energy" ~ (Shed_GBM/1000)*Price,
                                       Market=="SR"~ (Shed_GBM/1000)*Price,
                                       Market=="Capacity"~ if_else(Shed_GBM!=0,1,0))) |> #Calculated for every hour # Check unit of price is $/kW
  group_by(Year, Date, Market) |> 
  summarise(GBM_Shed_kWh=sum(Shed_GBM),
            Compensation_dollar=sum(Compensation_dollar))|>
  pivot_wider(names_from = Market, values_from = c(GBM_Shed_kWh,Compensation_dollar))

S5_cap<-S5_energy|>group_by(Year)|>summarise(sc=sum(Compensation_dollar_Capacity,na.rm=T))

S5<-avoided_peak_summary|>filter(Market== "S4:Combined")
S5<-left_join(S5,S5_cap)
S5<-S5|>mutate(B_cap_day=B_cap/sc)

S5_energy <-left_join(S5_energy,S5)|>
  mutate(Compensation_dollar_Capacity=if_else(is.na(Compensation_dollar_Capacity),NA,B_cap_day))|>
  mutate(Total_Shed_kWh = rowMeans(across(c(GBM_Shed_kWh_SR, GBM_Shed_kWh_Energy, GBM_Shed_kWh_Capacity)), na.rm = TRUE),
         Total_Compensation_dollar = rowMeans(across(c(Compensation_dollar_SR, Compensation_dollar_Energy)), na.rm = TRUE))|>
  group_by(Year)|>
  summarise(GBM_Shed_kWh=sum(Total_Shed_kWh),
            Compensation_dollar=sum(Total_Compensation_dollar, na.rm = TRUE))

rebound_S5_energy<-load_shed|> 
  filter(Rebound=="Yes"& DR.Building=="Yes" & DR.Event!="Yes" & Market=="Capacity")|>
  group_by(Year,Market,Elec_Price)|> 
  summarise(GBM_Rebound_kWh=sum(Rebound_GBM))

S5_energy<-left_join(S5_energy , rebound_S5_energy)

rf_S5_energy<-reduced_flex|>
  filter(DR.Event == "Yes") |>
  dplyr::select(Year, Date, Hour, DR.Event,id, Market,Price,Shed_GBM) |> 
  mutate(Compensation_dollar=case_when(Market=="Energy" ~ (Shed_GBM/1000)*Price,
                                       Market=="SR"~ (Shed_GBM/1000)*Price,
                                       Market=="Capacity"~ if_else(Shed_GBM!=0,1,0))) |> 
  group_by(Year, Date, Market) |> 
  summarise(GBM_Shed_kWh=sum(Shed_GBM),
            Compensation_dollar=sum(Compensation_dollar))|>
  pivot_wider(names_from = Market, values_from = c(GBM_Shed_kWh,Compensation_dollar))

S5_rf<-avoided_rfpeak_summary|>filter(Market== "S4:Combined") # Rf peak
S5_rf<-left_join(S5_rf,S5_cap)
S5_rf<-S5_rf|>mutate(B_cap_day=B_cap/sc)

rf_S5_energy <-left_join(rf_S5_energy,S5_rf)|>
  mutate(Compensation_dollar_Capacity=if_else(is.na(Compensation_dollar_Capacity),NA,B_cap_day))|>
  mutate(Total_Shed_kWh = rowMeans(across(c(GBM_Shed_kWh_SR, GBM_Shed_kWh_Energy, GBM_Shed_kWh_Capacity)), na.rm = TRUE),
         Total_Compensation_dollar = rowMeans(across(c(Compensation_dollar_SR, Compensation_dollar_Energy)), na.rm = TRUE))|>
  group_by(Year)|> 
  summarise(Reduced_Shed_kWh=sum(Total_Shed_kWh),
            Reduced_Compensation_dollar=sum(Total_Compensation_dollar, na.rm = TRUE))

S5_energy<-left_join(S5_energy, rf_S5_energy)
S5_energy<-S5_energy|>mutate(Market="Combined")

report<-S5_energy|>
  mutate(GBM_Shed_MWh=round(GBM_Shed_kWh/10^3,0),
         Reduced_Shed_MWh=round(Reduced_Shed_kWh/10^3,0),
         Compensation_dollar=round(Compensation_dollar/10^3,0),
         Reduced_Compensation_dollar=round(Reduced_Compensation_dollar/10^3,0),
         rf_reduction=round((Reduced_Shed_kWh-GBM_Shed_kWh)*100/GBM_Shed_kWh,0))|>
  select(-GBM_Shed_kWh,-Reduced_Shed_kWh,-GBM_Rebound_kWh)
## Plot #------------------------------------------------------------------
 
Scenarios_energy<-rbind(S1_energy,S2_energy,S3_energy,S4_energy,S5_energy) 
report<-Scenarios_energy|>
  mutate(GBM_Shed_MWh=round(GBM_Shed_kWh/10^3,2),
         Reduced_Shed_MWh=round(Reduced_Shed_kWh/10^3,2),
         Compensation_dollar=round(Compensation_dollar/10^3,0),
         Reduced_Compensation_dollar=round(Reduced_Compensation_dollar/10^3,0),
         rf_reduction=round((Reduced_Shed_kWh-GBM_Shed_kWh)*100/GBM_Shed_kWh,0))|>
  select(-GBM_Shed_kWh,-Reduced_Shed_kWh,-GBM_Rebound_kWh)

write.table(Scenarios_energy, file="Scenarios_energy.csv", sep=",", row.names = FALSE)
write.table(report, file="report_Scenarios_energy.csv", sep=",", row.names = FALSE)

Scenarios_energy<-Scenarios_energy|>
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
Scenarios_money<-rbind(S1_energy,S2_energy, S3_energy, S4_energy, S5_energy)


write.table(Scenarios_money, file="Scenarios_money.csv", sep=",", row.names = FALSE)

report_energy<-Scenarios_money|>
  arrange(Year)|> 
  dplyr::select(Market,Year,GBM_Shed_kWh,Reduced_Shed_kWh,Compensation_dollar,
                Reduced_Compensation_dollar)|>
  dplyr::mutate(across(where(is.numeric), ~ signif(., 3)))

uncertainity_energy<-Scenarios_money|>
  filter(Year==2022 & (Market=="S1:Capacity" |  Market=="BAU:Capacity-DR"))|> 
  dplyr::select(Market,Year,GBM_Shed_kWh,Compensation_dollar,B_cap2)|>
  mutate(percent=(B_cap2-Compensation_dollar)/Compensation_dollar)|>
  dplyr::mutate(across(where(is.numeric), ~ signif(., 3)))


exp=0
Scenarios_money<-Scenarios_money|>
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

path_to_fig <- "C:/Users/yashv/OneDrive - The Ohio State University/Yashvi Malhotra-KWEST/BCA/BCA_paper/Figures/"

ggsave(file.path(path_to_fig, "scenarios.png"),
       plot = last_plot(), width = 10, height = 8, dpi = 300)
