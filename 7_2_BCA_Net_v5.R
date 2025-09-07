library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

#avoided peak
# avoided energy 

# Input data for Business Model --------------------------------------------------------------
setwd("C:/Users/malhotra.164/OneDrive - The Ohio State University/Yashvi Malhotra-KWEST/BCA/R_BCA/R_BCA/Raw_data")


Emissions1 <- read.csv("Emissions1.csv") # Weight
wind_yearly <- read.csv("wind_cc.csv") # Weight

ecm_savings_yearly<-read.csv("ecm_savings_type.csv")
avoided_energy_yearly <- read.csv("avoided_energy_type.csv") #id and type
avoided_peak_summary<- read.csv("avoided_Type_peak.csv") # id and type
Scenarios_energy<- read.csv("Scenarios_energy_Type.csv") #id and type

# Assumptions
elec_price<-40.8 #$/MWh
steam_price<-25.034 #$/MMBTU

O_M_Solar_dollar=22*101 # For solar it is 22$/kW-yr
Capital_Cost_Solar_dollar=177347.92 # Solar PV Capital cost $ 177,347.92 from SAM or 1.76$/Wp

W_t=30*5*52*8
n_ppl=1

# Units is dollar and MWh
# 1. Emissions -> Weight  ----------------------------------------------------------------
Emissions<-Emissions1|>
  mutate(Change_Em=Post.Change-Original, 
         Converted_value_ton=Change_Em*pound_ton, 
         dollar_values=Converted_value_ton*dollar_ton, 
         Year=2019)|> group_by(Year)|> 
  summarise(Benefits.from.avoided.Emissions=abs(sum(dollar_values,na.rm = TRUE)))

# 2. Renewable -> Weight  ----------------------------------------------------------------
re_costs<-wind_yearly|>
  mutate(Year=as.numeric(Year),
         Wind_Contract_Costs_d=Contract_amount*CUF*Portion_CC*Purchase_cost_..MW_46.5_2*8760, ### Multiply with the number of hours
         Capital_Cost_Solar_d=if_else(Year==2019,Capital_Cost_Solar_dollar,0),
         `O&M_Cost_Solar_d` = O_M_Solar_dollar)|> # degrade by X%
  dplyr::select(-Contract_amount,-CUF,-Portion_CC,-Purchase_cost_..MW_46.5_2,-Electricity_Purchased_MWh)

# 3. Peak # get weight here ----------------------------------------------------------------
peak_summary<-avoided_peak_summary|>
  group_by(Year,Market)|>
  summarise(total_P=sum(delta_P))
  
avoided_peak<-avoided_peak_summary|>
  group_by(Year,Market,Type,B_peak)|>
  summarise(delta_P=sum(delta_P))

avoided_peak<-left_join(avoided_peak,peak_summary)
avoided_peak<-avoided_peak|>
  mutate(wt=round(delta_P/total_P,2))|>select(-total_P)

unique(avoided_peak$Market)

# 4. Demand Response : BAU  ----------------------------------------------------------------
unique(Scenarios_energy$Market)
Scenarios_energy<-Scenarios_energy|>
  dplyr::select(-GBM_Rebound_kWh)|>
  mutate(Market = recode(Market,
                         "Energy"= "S2:Energy",
                         "SR"= "S3:SR",
                         "Combined"= "S4:Combined"))
avoided_peak<-left_join(avoided_peak,Scenarios_energy)
scenario<-unique(avoided_peak$Market)

# 4. Energy ----------------------------------------------------------------
avoided_energy<-avoided_energy_yearly|># has data only for BAU scenario
  dplyr::select(-Market,-Net_Load_Shed_kWh)

# BAU ----------------------------------------------------------------
select_one<-avoided_peak|>filter(Market==scenario[1])
select_one<-left_join(avoided_energy,select_one,by = join_by(Year, Type))
select_one<-select_one|>
  mutate(Wind_ppa_MWh=wt*Wind_ppa_MWh,
         Solar_MWh=wt*Solar_MWh,
         Building.Steam.Savings_mmBTU = replace_na(Building.Steam.Savings_mmBTU, 0),
         Cooling_savings_MWh= replace_na(Cooling_savings_MWh, 0),
         GBM_Shed_kWh= replace_na(GBM_Shed_kWh, 0),
         Compensation_dollar= replace_na(Compensation_dollar, 0))
# Costs -------------------------------------------------------------------
wt_buildings<-select_one|>select(Year,wt,Type)

wt_buildings<-wt_buildings|>mutate(B_em=Emissions$Benefits.from.avoided.Emissions)


wt_buildings_extended<- do.call(rbind, lapply((max(wt_buildings$Year) + 1):(2019+24), function(y) {
  row <- tail(wt_buildings, 6)
  row$Year <- y
  return(row)}))
wt_buildings<-rbind(wt_buildings,wt_buildings_extended)

Extended_costs<-left_join(wt_buildings,re_costs)
Extended_costs<-Extended_costs|>
  mutate(Wind_Contract_Costs_d=Wind_Contract_Costs_d*wt,
         Capital_Cost_Solar_d=Capital_Cost_Solar_d*wt,
         `O&M_Cost_Solar_d`=`O&M_Cost_Solar_d`*wt,
         B_em=B_em*wt,
         C_staff_d = W_t * n_ppl*wt)
         
costs<-left_join(Extended_costs,ecm_savings_yearly)|>
  mutate(ECM_OM_Cost_cumulative_d = cumsum(ifelse(is.na(OM_Costs), 0, OM_Costs)),
         ECM_Capital_Cost_d=replace_na(Capital_Costs, 0))|> 
  mutate(C_cap_d=ECM_Capital_Cost_d+Capital_Cost_Solar_d,
         C_O_M_d=`O&M_Cost_Solar_d`+ECM_OM_Cost_cumulative_d)|>
  dplyr::select(-Building.Electric.Savings_kWh,-Building.CW.Savings_kWh,-Building.Steam.Savings_mmBTU,-Cooling_savings_MWh,
                -Capital_Costs,-OM_Costs,
                -ECM_Capital_Cost_d,-Capital_Cost_Solar_d,-`O&M_Cost_Solar_d`,-ECM_OM_Cost_cumulative_d)

# benefits with ecm ----------------------------------------------------------------
benefits<- do.call(rbind, lapply((max(select_one$Year) + 1):(2019 + 24), function(y) {
  row <- tail(select_one, 6)
  row$Year <- y
  return(row)}))
benefits<- rbind(select_one,benefits)

####### CHECK gc for library doesn't get extended beyond 2022

ecm_2023<-ecm_savings_yearly$Cooling_savings_MWh[ecm_savings_yearly$Year == 2023]
sh_2024<-ecm_savings_yearly$Cooling_savings_MWh[ecm_savings_yearly$Year == 2024]

rec_2023<-ecm_2023[2]
sh_2023<-ecm_2023[3]

heat_2023<-ecm_savings_yearly$Building.Steam.Savings_mmBTU[ecm_savings_yearly$Year == 2023]

heatrec_2023<-heat_2023[2]
heatsh_2023<-heat_2023[3]

benefits_updated <- benefits |>
  arrange(Type, Year) |>
  mutate(Cooling_savings_MWh = case_when(
      Type == "Student Housing" & Year == 2023 ~ sh_2023,
      Type == "Library" & Year >= 2023 ~ 0,
      Type == "Recreation" & Year == 2023 ~ rec_2023,
      Type == "Student Housing" & Year == 2024 ~ sh_2024,
      TRUE ~ Cooling_savings_MWh  # Leave other types unchanged
    ),

    Building.Steam.Savings_mmBTU = case_when(
      Type == "Student Housing" & Year == 2023 ~ heatsh_2023,
      Type == "Library" & Year >= 2023 ~ 0,
      Type == "Recreation" & Year == 2023 ~ heatrec_2023,
      TRUE ~ Building.Steam.Savings_mmBTU
    )
  ) |>
  group_by(Type) |>
  mutate(cumCooling_savings_MWh = cumsum(Cooling_savings_MWh),
    cumBuilding.Steam.Savings_mmBTU = cumsum(Building.Steam.Savings_mmBTU)) |>
  ungroup()

benefits_updated1<- benefits_updated|>
  mutate(Net_Load_Shed_MWh=GBM_Shed_kWh/1000,
         B_gc=Compensation_dollar,
    E_Cool=Solar_MWh+cumCooling_savings_MWh+Wind_ppa_MWh+Net_Load_Shed_MWh,
         B_Heat=cumBuilding.Steam.Savings_mmBTU*steam_price,
         B_Cool=(E_Cool)*elec_price,
    B_em=costs$B_em)|>
  dplyr::select(-Baseline_Cooling_MWh,-Baseline_Heating_mMBTU, -Wind_ppa_MWh,-Solar_MWh,
         -Cooling_savings_MWh,-cumCooling_savings_MWh)

costs<-costs|>select(-B_em)
cost_pivot<- costs|> pivot_longer(cols = c ("C_cap_d","C_O_M_d","Wind_Contract_Costs_d","C_staff_d"),
                                  names_to = "bca", values_to = "Dollar")|>
  mutate(Dollar=-1*Dollar)


benefit_pivot <- benefits_updated1|> dplyr::select(Year,Type,B_Cool,B_Heat, B_gc,B_peak,B_em)|>
  pivot_longer(cols = c(B_Cool, B_gc,B_peak, B_Heat,B_em),
               names_to = "bca", values_to = "Dollar")

# Cumulative Discounted Benefits

discount_rate=0.05
cum_bca<-benefits_updated1|> 
  dplyr::select(Year,Type,B_Cool,B_Heat,B_peak,B_gc,B_em)|>
  left_join(costs)|>
  mutate(Benefits_d=B_Cool+B_Heat+B_gc+B_peak+B_em,
         Costs_d=(C_cap_d+C_O_M_d+Wind_Contract_Costs_d+C_staff_d))|>
  group_by(Year,Type)|>
  mutate(D_benefits_d=Benefits_d/(1+discount_rate)^(Year-2019),
         D_costs_d=Costs_d/(1+discount_rate)^(Year-2019))|>
  ungroup()

cum_bca<-cum_bca|>
  select(Year, Type, Benefits_d,Costs_d,D_benefits_d,D_costs_d)|>
  mutate(Net_Benefits=Benefits_d-Costs_d,
         Discounted_Net_Benefits=D_benefits_d-D_costs_d)|>
  group_by(Type)|>
  mutate(cumDiscounted_Net_Benefits=cumsum(Discounted_Net_Benefits))

cum_bca |>
  ggplot(aes(x = as.factor(Year))) +
  geom_bar(stat = "identity", aes(y = cumDiscounted_Net_Benefits/10^6), fill = "#b3cde3", color = "black") +
  facet_wrap(~Type)+
  theme_classic() +
  theme(panel.background = element_rect(fill = NA, color = "black"),
    text = element_text(size = 14),  # Reduced text size for clarity
    axis.text = element_text(size = 14, color = "black"),  # Black font for axis text
    axis.text.x = element_text(angle = 60, hjust = 1, color = "black"),  # Tilted x-axis labels
    legend.key = element_rect(fill = "white")) +
 labs(y = "\n$-Mil", x = "Year") +
    scale_y_continuous(breaks = seq(-3, 11, by =1),
                       limits = c(-3, 11),  # Force axis to include 40
      labels = scales::label_comma())

########## Performance Metrics Public and Private #################
# Create an empty results dataframe
pf <- data.frame(
  Type = character(),
  npv = numeric(),
  b_c_ratio = numeric(),
  payback_year = numeric(),
  irr = numeric(),
  stringsAsFactors = FALSE
)

type<-unique(cum_bca$Type)
# Loop over each of the 6 types
for (i in 1:6) {
  current_type <- type[i]
  years <- cum_bca |> filter(Type == current_type) |> arrange(Year)
  
  # Compute NPV and B/C ratio
  pm_bca <- years |>
    summarise(
      npv = sum(Discounted_Net_Benefits, na.rm = TRUE) / 1e6,
      b_c_ratio = sum(Benefits_d, na.rm = TRUE) / sum(Costs_d, na.rm = TRUE)
    )
  
  # --- Calculate Payback Year ---
  # Add logical column to check when cumulative discounted net benefits are positive
  years$positive_cum <- years$cumDiscounted_Net_Benefits > 0
  payback_year <- NA  # Default NA if never paid back
  
  payback_year_index <- which(years$positive_cum)
  if (length(payback_year_index) > 0) {
    for (j in payback_year_index) {if (all(years$cumDiscounted_Net_Benefits[j:nrow(years)] > 0)) {
        payback_year <- years$Year[j] - 2019  # Offset from base year
        break}}}
  
  # --- Calculate IRR ---
  cash_flows <- years$Net_Benefits
  irr_function <- function(rate) {
    sum(cash_flows / (1 + rate)^(years$Year - 2019))
  }
  
  # Check if IRR root can be found safely
  f_low <- tryCatch(irr_function(-0.99), error = function(e) NA)
  f_high <- tryCatch(irr_function(1), error = function(e) NA)
  
  if (!is.na(f_low) && !is.na(f_high) && f_low * f_high < 0) {
    irr_result <- tryCatch(
      uniroot(irr_function, c(-0.99, 1), tol = 1e-6)$root,
      error = function(e) NA
    )
    irr_percent <- round(irr_result * 100, 2)
  } else {
    irr_percent <- 98
  }
  
  # Append results
  pf <- rbind(pf, data.frame(
    Type = current_type,
    npv = pm_bca$npv,
    b_c_ratio = pm_bca$b_c_ratio,
    payback_year = payback_year,
    irr = irr_percent
  ))
}