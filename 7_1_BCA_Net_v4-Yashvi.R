library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

# Input data for Business Model --------------------------------------------------------------
setwd("C:/Users/yashv/OneDrive - The Ohio State University/Yashvi Malhotra-KWEST/BCA/R_BCA/R_BCA/Raw_data")
discount_rate=0.05

Emissions1 <- read.csv("Emissions1.csv")
wind_yearly <- read.csv("wind_cc.csv")
ecm_savings_yearly<-read.csv("ecm_savings_yearly.csv")
avoided_energy_yearly <- read.csv("avoided_energy_yearly.csv")
avoided_peak_summary<- read.csv("avoided_peak_summary.csv") 
Scenarios_energy<- read.csv("Scenarios_energy.csv")
# Scenarios_energy<-rbind(Scenarios_energy,S4_energy2)
# Assumptions
elec_price<-40.8 #$/MWh
steam_price<-25.034 #$/MMBTU

O_M_Solar_dollar=22*101 # For solar it is 22$/kW-yr
Capital_Cost_Solar_dollar=177347.92 # Solar PV Capital cost $ 177,347.92 from SAM or 1.76$/Wp

W_t=30*5*52*8
n_ppl=1

# Units is dollar and MWh
# 1. Emissions ----------------------------------------------------------------
Emissions<-Emissions1|>
  mutate(Change_Em=Post.Change-Original, 
         Converted_value_ton=Change_Em*pound_ton, 
         dollar_values=Converted_value_ton*dollar_ton, 
         Year=2019)|> group_by(Year)|> 
  summarise(Benefits.from.avoided.Emissions=abs(sum(dollar_values,na.rm = TRUE)))

# 2. Renewable ----------------------------------------------------------------
re_costs<-wind_yearly|>
  mutate(Year=as.numeric(Year),
         Wind_Contract_Costs_d=Contract_amount*CUF*Portion_CC*Purchase_cost_..MW_46.5_2*8760, ### Multiply with the number of hours
         Capital_Cost_Solar_d=if_else(Year==2019,Capital_Cost_Solar_dollar,0),
         `O&M_Cost_Solar_d` = O_M_Solar_dollar)|> # degrade by X%
  dplyr::select(-Contract_amount,-CUF,-Portion_CC,-Purchase_cost_..MW_46.5_2,-Electricity_Purchased_MWh)

# 3. Peak  ----------------------------------------------------------------
avoided_peak_summary<-avoided_peak_summary|>dplyr::select(Year,Market,B_peak)
unique(avoided_peak_summary$Market)
# 4. Demand Response : BAU  ----------------------------------------------------------------
Scenarios_money<-Scenarios_energy|>dplyr::select(-Elec_Price)|>
  mutate(Market = recode(Market,
                         "Energy"= "S2:Energy",
                         "SR"= "S3:SR",
                         "Combined"= "S4:Combined"))

# 4. Energy ----------------------------------------------------------------
avoided_energy<-avoided_energy_yearly|>
  dplyr::select("Year","Baseline_Cooling_MWh","Baseline_Heating_mMBTU","Building.Steam.Savings_mmBTU",
         "Cooling_savings_MWh","Wind_ppa_MWh","Solar_MWh")

avoided_peak_summary<-left_join(avoided_peak_summary,avoided_energy)
Scenarios_money<-left_join(Scenarios_money,avoided_peak_summary)

# Costs -------------------------------------------------------------------
costs<-left_join(re_costs,ecm_savings_yearly)|>
  mutate(ECM_OM_Cost_cumulative_d = cumsum(ifelse(is.na(OM_Costs), 0, OM_Costs)),
         ECM_Capital_Cost_d=replace_na(Capital_Costs, 0),
         C_staff_d = W_t * n_ppl)|>
  mutate(C_cap_d=ECM_Capital_Cost_d+Capital_Cost_Solar_d,
         C_O_M_d=`O&M_Cost_Solar_d`+ECM_OM_Cost_cumulative_d)|>
  dplyr::select(-Building.Electric.Savings_kWh,-Building.CW.Savings_kWh,-Building.Steam.Savings_mmBTU,-Cooling_savings_MWh,
                -Capital_Costs,-OM_Costs,
                -ECM_Capital_Cost_d,-Capital_Cost_Solar_d,-`O&M_Cost_Solar_d`,-ECM_OM_Cost_cumulative_d)

# Select one ----------------------------------------------------------------
scenario<-unique(Scenarios_money$Market)
pf <- data.frame(
  Scenario = character(),
  NPV_Emissions =  numeric(),
  NPV =  numeric(),
  b_c_ratio_Emissions =  numeric(),
  b_c_ratio =  numeric(),
  payback_year_Emissions =  numeric(),
  payback_year =  numeric(),
  irr_Emissions = numeric(),
  irr =  numeric(),
  g_c_pct_Emissions =numeric(),
  g_c_pct =numeric()
  )
for (i in 1:5){
  select_one<-Scenarios_money|>filter(Market==scenario[i])

benefits<- do.call(rbind, lapply((max(select_one$Year) + 1):(2019 + 24), function(y) {
  row <- tail(select_one, 1)
  row$Year <- y
  return(row)}))
benefits<- rbind(select_one,benefits)

benefits_updated<- benefits|>
  mutate(Cooling_savings_MWh = case_when(
      Year <= 2022 ~ Cooling_savings_MWh,
      Year == 2023 ~ ecm_savings_yearly$Cooling_savings_MWh[ecm_savings_yearly$Year == 2023],
      Year == 2024 ~ ecm_savings_yearly$Cooling_savings_MWh[ecm_savings_yearly$Year == 2024],
      Year >= 2025 ~ 0),
    Building.Steam.Savings_mmBTU = case_when(
      Year <= 2022 ~ Building.Steam.Savings_mmBTU,
      Year == 2023 ~ ecm_savings_yearly$Building.Steam.Savings_mmBTU[ecm_savings_yearly$Year == 2023],
      Year >= 2024 ~ 0),
    cumCooling_savings_MWh=cumsum(Cooling_savings_MWh),
    cumBuilding.Steam.Savings_mmBTU=cumsum(Building.Steam.Savings_mmBTU))

benefits_updated1<- benefits_updated|>
  mutate(Net_Load_Shed_MWh=GBM_Shed_kWh/1000,
         B_gc=Compensation_dollar,
         #B_gc=if_else(Year<=2021, Compensation_dollar,Compensation_dollar*0.26), # Uncertainty assessment for Energy 
         #B_gc=if_else(Year<=2021, Compensation_dollar,B_cap2), # Uncertainty assessment for Capacity 
    E_Cool=Solar_MWh+cumCooling_savings_MWh+Wind_ppa_MWh+Net_Load_Shed_MWh,
         B_Heat=cumBuilding.Steam.Savings_mmBTU*steam_price,
         B_Cool=(E_Cool)*elec_price, 
         B_Emiss=Emissions$Benefits.from.avoided.Emissions)|>
  dplyr::select(-Baseline_Cooling_MWh,-Baseline_Heating_mMBTU, -Wind_ppa_MWh,-Solar_MWh,
         -Cooling_savings_MWh,-cumCooling_savings_MWh)

# Cumulative Discounted Benefits
cum_bca<-benefits_updated1|> dplyr::select(Year,B_Cool,B_Heat,B_Emiss, B_peak,B_gc)|>
  mutate(Benefits_d=B_Cool+B_Heat+B_gc+B_peak+B_Emiss,
         D_benefits_d=Benefits_d/(1+discount_rate)^(Year-2019))|>
  left_join(costs)|>
  mutate(Costs_d=(C_cap_d+C_O_M_d+Wind_Contract_Costs_d+C_staff_d),
         D_costs_d=Costs_d/(1+discount_rate)^(Year-2019),
         Net_Benefits=Benefits_d-Costs_d,
         Discounted_Net_Benefits=D_benefits_d-D_costs_d,
         cumDiscounted_Net_Benefits=cumsum(Discounted_Net_Benefits))

# Cumulative Discounted Benefits without Avoided Emissions
cum_bca_pvt<-benefits_updated1|> dplyr::select(Year,B_Cool,B_Heat,B_peak,B_gc)|>
  mutate(Benefits_d=B_Cool+B_Heat+B_gc+B_peak,
         D_benefits_d=Benefits_d/(1+discount_rate)^(Year-2019))|>
  left_join(costs)|>
  mutate(Costs_d=(C_cap_d+C_O_M_d+Wind_Contract_Costs_d+C_staff_d),
         D_costs_d=Costs_d/(1+discount_rate)^(Year-2019),
         
         Net_Benefits=Benefits_d-Costs_d,
         Discounted_Net_Benefits=D_benefits_d-D_costs_d,
         cumDiscounted_Net_Benefits=cumsum(Discounted_Net_Benefits))

########## Performance Metrics Public and Private #################
pm_bca<-cum_bca|>
  summarise(npv=sum(Discounted_Net_Benefits)/10^6, 
            g_c_pct=sum(B_gc)*100/sum(Discounted_Net_Benefits),
            b_c_ratio=sum(Benefits_d)/sum(Costs_d),
            payback_year = min(Year[cumDiscounted_Net_Benefits > 0], na.rm = TRUE),
            payback_year=payback_year-2019)

cash_flows <- cum_bca$Net_Benefits
irr_function <- function(rate) {
  sum(cash_flows / (1 + rate)^(cum_bca$Year - 2019))}
irr_result <- uniroot(irr_function, c(-0.99, 1), tol = 1e-6)$root
irr_percent <- round(irr_result * 100, 2)

# Performance Metrics Private 
pm_bca_pvt<-cum_bca_pvt|>
  summarise(npv=sum(Discounted_Net_Benefits)/10^6, 
            g_c_pct=sum(B_gc)*100/sum(Discounted_Net_Benefits),
            b_c_ratio=sum(Benefits_d)/sum(Costs_d),
            payback_year = min(Year[cumDiscounted_Net_Benefits > 0], na.rm = TRUE),
            payback_year=payback_year-2019)
cash_flows_pvt <- cum_bca_pvt$Net_Benefits

irr_function_pvt <- function(rate) {
  sum(cash_flows_pvt/ (1 + rate)^(cum_bca_pvt$Year - 2019))}
irr_result_pvt <- uniroot(irr_function_pvt, c(-0.99, 1), tol = 1e-6)$root
irr_percent_pvt <- round(irr_result_pvt * 100, 2)

# Append results
pf <- rbind(pf, data.frame(
  Scenario = scenario[i],
  NPV_Emissions = round(pm_bca$npv,0),
  NPV = round(pm_bca_pvt$npv,0),
  
  b_c_ratio_Emissions = round(pm_bca$b_c_ratio,1),
  b_c_ratio = round(pm_bca_pvt$b_c_ratio,1),
  
  payback_year_Emissions = pm_bca$payback_year,
  payback_year = pm_bca_pvt$payback_year,
  irr_Emissions = round(irr_percent,0),
  irr = round(irr_percent_pvt,0),
  
  g_c_pct_Emissions = round(pm_bca$g_c_pct,0),
  g_c_pct = round(pm_bca_pvt$g_c_pct,0)
  
  
))

}


# Graph-------------------------------------------------------------------
# cost_pivot <- costs |> pivot_longer(
#   cols = c ("C_cap_d", "C_O_M_d", "Wind_Contract_Costs_d", "C_staff_d"),
#   names_to = "bca",
#   values_to = "Dollar"
# ) |>
#   mutate(Dollar = -1 * Dollar)
# benefit_pivot <- benefits_updated1 |> dplyr::select(Year, B_Cool, B_Heat, B_Emiss, B_gc, B_peak) |>
#   pivot_longer(
#     cols = c(B_Cool, B_gc, , B_peak, B_Heat, B_Emiss),
#     names_to = "bca",
#     values_to = "Dollar"
#   )
# bca <- rbind(benefit_pivot, cost_pivot)
# bca_order <- c(
#   "Capital Costs",
#   "Contract Costs",
#   "Staffing Costs",
#   "Operation and Maintainance",
#   "Grid compensation",
#   "Avoided Peak",
#   "Avoided Electricity purchase",
#   "Avoided Emissions",
#   "Avoided Steam use"
# )
# 
# 
# bca_colors <- c(
#   "#d7301f",
#   "#fc8d59",
#   "#fdcc8a",
#   "#fef0d9",
#   "#f1eef6",
#   "#bdc9e1",
#   "#74a9cf",
#   "#2b8cbe",
#   "#045a8d"
# )
# bca <- bca |> mutate(
#   bca = recode(
#     bca,
#     "C_O_M_d" = "Operation and Maintainance",
#     "C_staff_d" = "Staffing Costs",
#     "Wind_Contract_Costs_d" = "Contract Costs",
#     "C_cap_d" = "Capital Costs",
#     "B_peak" = "Avoided Peak",
#     "B_Emiss" = "Avoided Emissions",
#     "B_Heat" = "Avoided Steam use",
#     "B_gc" = "Grid compensation",
#     "B_Cool" = "Avoided Electricity purchase"
#   )
# ) |>
#   mutate(bca = factor(bca, levels = bca_order))
# bca |>
#   ggplot(aes(x = as.factor(Year), fill = bca)) +
#   geom_bar(
#     stat = "identity",
#     position = "dodge",
#     aes(y = Dollar / 10^6),
#     color = "black"
#   ) +
#   theme_classic() +
#   theme(
#     panel.background = element_rect(fill = NA, color = "black"),
#     text = element_text(size = 25),
#     axis.text = element_text(size = 25, color = "black"),
#     axis.text.x = element_text(
#       angle = 45,
#       hjust = 1,
#       color = "black"
#     ),
#     legend.position = c(0.85, 0.3),
#     legend.key = element_rect(fill = "white"),
#     legend.spacing.y = unit(1, "cm"),
#     legend.text = element_text(size = 20),
#     legend.key.size = unit(1, "cm")
#   ) +
#   labs(y = "\n$-Mil", x = "Year", fill = NULL) +
#   scale_fill_manual(values = bca_colors) +
#   scale_y_continuous(
#     breaks = seq(-8, 4, by = 1),
#     limits = c(-8, 4),
#     labels = scales::label_comma()
#   )
# 
# path_to_fig <- "C:/Users/yashv/OneDrive - The Ohio State University/Yashvi Malhotra-KWEST/BCA/BCA_paper/Figures/"
# 
# ggsave(file.path(path_to_fig, "bca.png"),
#        plot = last_plot(), width = 10, height = 8, dpi = 300)
# 
# cum_bca |>
#   ggplot(aes(x = as.factor(Year))) +
#   geom_bar(
#     stat = "identity",
#     aes(y = cumDiscounted_Net_Benefits / 10^6),
#     fill = "#b3cde3",
#     color = "black"
#   ) +
#   theme_classic() +
#   theme(
#     panel.background = element_rect(fill = NA, color = "black"),
#     text = element_text(size = 25),
#     # Reduced text size for clarity
#     axis.text = element_text(size = 25, color = "black"),
#     # Black font for axis text
#     axis.text.x = element_text(
#       angle = 45,
#       hjust = 1,
#       color = "black"
#     ),
#     # Tilted x-axis labels
#     legend.key = element_rect(fill = "white")
#   ) +
#   labs(y = "\n$-Mil", x = "Year") +
#   scale_y_continuous(
#     breaks = seq(-10, 35, by = 5),
#     limits = c(-10, 35),
#     # Force axis to include 40
#     labels = scales::label_comma()
#   )
# path_to_fig <- "C:/Users/yashv/OneDrive - The Ohio State University/Yashvi Malhotra-KWEST/BCA/BCA_paper/Figures/"
# 
# ggsave(file.path(path_to_fig, "cum_bca.png"),
#        plot = last_plot(), width = 10, height = 8, dpi = 300)

# Slide -------------------------------------------------------------------
# 
# dplot_order <- c("Costs","Benefits","Net Benefits")
# dplot_colors <- c("#d7301f","#045a8d","#fef0d9")
# dplot<-cum_bca|>dplyr::select(Year, D_costs_d, D_benefits_d, Discounted_Net_Benefits)|>
#   summarise("Costs"=sum(D_costs_d),
#             "Benefits"=sum(D_benefits_d),
#             "Net Benefits"=sum(Discounted_Net_Benefits))|>
#   pivot_longer(cols = c("Costs","Benefits","Net Benefits"),
#                names_to = "dplot", values_to = "Dollar")|>
#   mutate(dplot = factor(dplot, levels = dplot_order), Duration="Project duration is 25 years")
# dplot|>
#   ggplot(aes(x = as.factor(Duration), fill = dplot)) +
#   geom_bar(stat = "identity", aes(y = Dollar / 10^6),
#            position ="dodge", width = 0.4, color = "black") +
#   theme_classic() +
#   theme(panel.background = element_rect(fill = NA, color = "black"),
#         text = element_text(size = 50),
#         axis.text = element_text(size = 50, color = "black"),
#         legend.position = c(0.17, 0.82),
#         legend.key = element_rect(fill = "white"),
#         legend.spacing.y = unit(1, "cm"),
#         legend.text = element_text(size = 50),
#         legend.key.size = unit(1.2, "cm")) +
#   labs(y = "\n$-Mil",    x = NULL, fill = NULL) +
#   scale_fill_manual(values = dplot_colors)+
#   scale_x_discrete(expand = expansion(mult = c(0, 0))) +  # removes padding on x-axis
#   scale_y_continuous(
#     breaks = seq(0, 60, by = 10),
#     limits = c(0, 60),
#     labels = scales::label_comma())



# EXTRA -------------------------------------------------------------------
# reports benefits and costs in thousand
report<-cum_bca|>
  mutate(B_Cool=round(B_Cool/10^3,0),
         B_Heat=round(B_Heat/10^3,0),
         B_Emiss=round(B_Emiss/10^3,0),                   
         B_peak=round(B_peak/10^3,0),
         B_gc=round(B_gc/10^3,0),
         Benefits_d=round(Benefits_d/10^3,0),           
         Wind_Contract_Costs_d=round(Wind_Contract_Costs_d,0)/10^3,
         C_staff_d=round(C_staff_d/10^3,0),
         C_cap_d=round(C_cap_d/10^3,0),
         C_O_M_d=round(C_O_M_d/10^3,0),                  
         Costs_d=round(Costs_d/10^3,0),
         Net_Benefits=round(Net_Benefits/10^3,0))

report_benefits<-report|>
  # filter(Year<=2022)|>
  dplyr::select(Year,B_Cool,B_Heat,B_Emiss, B_peak,B_gc,Benefits_d)

summary_benefits<-report_benefits|>
  # summarise(B_Cool=round(sum(B_Cool),0),
  #        B_Heat=round(sum(B_Heat),0),
  #        B_Emiss=round(sum(B_Emiss),0),                   
  #        B_peak=round(sum(B_peak),0),
  #        B_gc=round(sum(B_gc),0),
  #        Benefits_d=round(sum(Benefits_d),0))|>
  mutate(B_Cool=round((B_Cool*100)/Benefits_d,0),
         B_Heat=round((B_Heat*100)/Benefits_d,0),
         B_Emiss=round((B_Emiss*100)/Benefits_d,0),                   
         B_peak=round((B_peak*100)/Benefits_d,0),
         B_gc=round((B_gc*100)/Benefits_d,0))
# filter(Year==2019|Year==2043)

report_costs<-report|>
  dplyr::select(Year,C_cap_d,C_O_M_d,Wind_Contract_Costs_d,C_staff_d,Costs_d)

#Percentage of the costs
summary_costs<-report_costs|>
  mutate(C_cap_d=round((C_cap_d*100)/Costs_d,0),
         C_staff_d=round((C_staff_d*100)/Costs_d,0),
         Wind_Contract_Costs_d=round((Wind_Contract_Costs_d*100)/Costs_d,0),                   
         C_O_M_d=round((C_O_M_d*100)/Costs_d,0))|> dplyr::select(-Costs_d)
         
