library(ggplot2)
library(lubridate)
library(timeDate)
library(dplyr)
library(stringr)
library(segmented)
library(tidyverse)
library(fpp3)
library(fable.prophet)

setwd("C:/Users/malhotra.164/OneDrive - The Ohio State University/Yashvi Malhotra-KWEST/BCA/R_BCA/R_BCA/Raw_data")
baseline_energy <- read.csv("baseline_energy_CC_2019_20.csv")
data_2019_22 <- read.csv("Building_CC_utilities_outliers") 

# training data 2020 should not have outliers
training_data<-baseline_energy|>
  mutate(Timestamp=ymd_hms(Timestamp,tz="EST"),
         Date=ymd(Date),
         WeekDay=wday(Date,week_start = 1),
         ht=ifelse(Holidays == 1 | WeekDay >= 6, 1, 0),
         ht=as.factor(ht))|>
  dplyr::select(Timestamp,Date, Hour, "Holidays",ht,
                DR.Event,Ambient_Temperature_F,
                id, "Name","DR.Buildings",Gross.Square.Feet,"Building_Occupancy_DEV",
                Decision,
                "gbm_elec","gbm_cw","gbm_steam","gbm_ng")|> #rename()
  as_tsibble(index = Timestamp, key=c(id)) 

# Prediction data 2021-2022
future_df<-data_2019_22|>
  filter(Year>=2021)|>
  mutate(Timestamp=ymd_hms(Timestamp,tz="EST"),
         Date=ymd(Date),
         WeekDay=wday(Date,week_start = 1),
         ht=ifelse(Holidays == 1 | WeekDay >= 6, 1, 0),
         ht=as.factor(ht))|>
  dplyr::select(Timestamp,Date, Hour, "Holidays",ht,
                DR.Event,Ambient_Temperature_F,
                id, "Name","DR.Buildings",Gross.Square.Feet,"Building_Occupancy_DEV",
                "Electricity_Consumed_kWh",
                "Chilled_Water_Consumed_kBTU","CW_elec",
                Decision,
                "Steam_Consumed_kBTU","Natural_Gas_Consumed_kBTU",
                Electricity.outlier,CW_elec.outlier,
                         Steam.outlier,NG.outlier)|>
  as_tsibble(index = Timestamp, key=id) 


# Prophet Model -----------------------------------------------------------
fit_prophet_elec_list<-list()
fit_prophet_cw_list<-list()
fit_prophet_steam_list<-list()
fit_prophet_ng_list<-list()

# Electricity 
t_elec <- training_data|> mutate(Et=.data[["gbm_elec"]])|>
  dplyr::select(Timestamp,Name, id, Ambient_Temperature_F,Building_Occupancy_DEV,ht,Et)

p_elec <- future_df|> mutate(Et=.data[["Electricity_Consumed_kWh"]])|>
  dplyr::select(Timestamp, id, Ambient_Temperature_F,Building_Occupancy_DEV,ht,Et,Electricity.outlier)

building_id<-sort(unique(t_elec$id))# 21 buildings

training <- t_elec|> filter (id== building_id[1]) 
prediction <- p_elec|> filter (id== building_id[1])

fit_prophet_harmonic <- training|>  model(prophet(Et ~ Ambient_Temperature_F +Building_Occupancy_DEV + ht +
                  season(period = "day", order = 10) +
                  season(period = "week", order = 5) +
                  season(period = "year", order = 3)))
glance(fit_prophet_harmonic)
 
forecast_elec<-forecast(fit_prophet_harmonic,prediction)
fit_prophet_elec_list[[paste0(building_id[1])]]<-fit_prophet_harmonic[[2]][[1]]
combined_elec<-forecast_elec

for (i in 2:length(building_id)){
  training <- t_elec|> filter (id== building_id[i]) 
  prediction <- p_elec|> filter (id== building_id[i])
  fit_prophet_harmonic <- training|> 
    model(prophet(Et ~ Ambient_Temperature_F +Building_Occupancy_DEV + ht +
                    season(period = "day", order = 10) +
                    season(period = "week", order = 5) +
                    season(period = "year", order = 3)))
  forecast_elec<-forecast(fit_prophet_harmonic,prediction)
  fit_prophet_elec_list[[paste0(building_id[i])]]<-fit_prophet_harmonic[[2]][[1]]
  combined_elec<-bind_rows(combined_elec,forecast_elec) }
 
write.table(combined_elec, file="combined_elec_forecasts_gbm.csv", sep=",", row.names = FALSE)
# Forecast for 2021 and 2022 (CW is negative, steam and ng is NA)
forecast_elec1<-forecast_elec|>mutate(Year=year(Timestamp))|>group_by(Year)|>summarise(Elec_MWh=sum(.mean,na.rm = TRUE)/1000)


# Chilled Water
t_cw <- training_data|> mutate(Et=.data[["gbm_cw"]])|>
  filter(!(Et == 0 | is.na(Et)))|>  # Remove rows where Et is 0 or NA
  dplyr::select(Timestamp,Name, id, Ambient_Temperature_F,Building_Occupancy_DEV,ht,Et)

p_cw <- future_df|> mutate(Et=.data[["CW_elec"]])|>
  filter(!(Et == 0 | is.na(Et)))|>  # Remove rows where Et is 0 or NA
  dplyr::select(Timestamp, id, Ambient_Temperature_F,Building_Occupancy_DEV,ht,Et, CW_elec.outlier)

building_id<-sort(unique(t_cw$id))

training <- t_cw|> filter (id== building_id[1])
prediction <- p_cw|> filter (id== building_id[1])

fit_prophet_harmonic <- training|>  model(prophet(Et ~ Ambient_Temperature_F +Building_Occupancy_DEV + ht +
                                                    season(period = "day", order = 10) +
                                                    season(period = "week", order = 5) +
                                                    season(period = "year", order = 3)))
glance(fit_prophet_harmonic)
  
forecast_cw<-forecast(fit_prophet_harmonic,prediction)
fit_prophet_cw_list[[paste0(building_id[1])]]<-fit_prophet_harmonic[[2]][[1]]
combined_cw<-forecast_cw

for (i in 2:length(building_id)){
  training <- t_cw|> filter (id== building_id[i])
  prediction <- p_cw|> filter (id== building_id[i])
  fit_prophet_harmonic <- training|> 
    model(prophet(Et ~ Ambient_Temperature_F +Building_Occupancy_DEV + ht +
                    season(period = "day", order = 10) +
                    season(period = "week", order = 5) +
                    season(period = "year", order = 3)))
  forecast_cw<-forecast(fit_prophet_harmonic,prediction)
  fit_prophet_cw_list[[paste0(building_id[i])]]<-fit_prophet_harmonic[[2]][[1]]
  combined_cw<-bind_rows(combined_cw,forecast_cw) }

write.table(combined_cw, file="combined_cw_forecasts_gbm.csv", sep=",", row.names = FALSE)

# Forecast for 2021 and 2022 (CW is negative, steam and ng is NA)
forecast_cw1 <- forecast_cw|>mutate(Year=year(Timestamp))|>group_by(Year)|>summarise(CW_Elec_MWh=sum(.mean,na.rm = TRUE)/1000)

# 3. Steam
t_steam<- training_data|>
  filter(Decision == "Steam+HW" | Decision == "Steam")|>
  mutate(Et=.data[["gbm_steam"]])|>
  dplyr::select(Timestamp,Name, id, Ambient_Temperature_F,Building_Occupancy_DEV,ht,Et)

p_steam <- future_df|> 
  filter(Decision == "Steam+HW" | Decision == "Steam")|>
  mutate(Et=.data[["Steam_Consumed_kBTU"]])|>
  dplyr::select(Timestamp, id, Ambient_Temperature_F,Building_Occupancy_DEV,ht,Et,Steam.outlier)

building_id<-sort(unique(t_steam$id))

training <- t_steam|> filter (id== building_id[1])
prediction <- p_steam|> filter (id== building_id[1])

fit_prophet_harmonic <- training|>  model(prophet(Et ~ Ambient_Temperature_F +Building_Occupancy_DEV + ht +
                                                    season(period = "day", order = 10) +
                                                    season(period = "week", order = 5) +
                                                    season(period = "year", order = 3)))
glance(fit_prophet_harmonic)


forecast_steam<-forecast(fit_prophet_harmonic,prediction)
fit_prophet_steam_list[[paste0(building_id[1])]]<-fit_prophet_harmonic[[2]][[1]]
combined_steam<-forecast_steam

for (i in 2:length(building_id)){
  training <- t_steam|> filter (id== building_id[i])
  prediction <- p_steam|> filter (id== building_id[i])
  
  fit_prophet_harmonic <- training|>  model(prophet(Et ~ Ambient_Temperature_F +Building_Occupancy_DEV + ht +
                                                      season(period = "day", order = 10) +
                                                      season(period = "week", order = 5) +
                                                      season(period = "year", order = 3)))
  forecast_steam<-forecast(fit_prophet_harmonic,prediction)
  fit_prophet_steam_list[[paste0(building_id[i])]]<-fit_prophet_harmonic[[2]][[1]]
  combined_steam<-bind_rows(combined_steam,forecast_steam) }

write.table(combined_steam, file="combined_steam_forecasts_gbm.csv", sep=",", row.names = FALSE)

# Forecast for 2021 and 2022 (CW is negative, steam and ng is NA)
forecast_steam1 <- forecast_steam|>mutate(Year=year(Timestamp))|>group_by(Year)|>summarise(Steam_MmBTU=sum(.mean,na.rm = TRUE)/10^6)

# 4. Natural gas
t_ng<- training_data|> filter(Decision == "NG")|>
  mutate(Et=.data[["gbm_ng"]])|>
  filter(Decision == "NG")|>
  dplyr::select(Timestamp,Name, id, Ambient_Temperature_F,Building_Occupancy_DEV,ht,Et)

p_ng <- future_df|>
  filter(Decision == "NG")|>
  mutate(Et=.data[["Natural_Gas_Consumed_kBTU"]])|>
  dplyr::select(Timestamp, id, Ambient_Temperature_F,Building_Occupancy_DEV,ht,Et,NG.outlier)

building_id<-sort(unique(t_ng$id))

training <- t_ng|> filter (id== building_id[1]) 
prediction <- p_ng|> filter (id== building_id[1])
fit_prophet_harmonic <- training|>  model(prophet(Et ~ Ambient_Temperature_F +Building_Occupancy_DEV + ht +
                                                    season(period = "day", order = 10) +
                                                    season(period = "week", order = 5) +
                                                    season(period = "year", order = 3)))
glance(fit_prophet_harmonic)

forecast_ng<-forecast(fit_prophet_harmonic,prediction)
fit_prophet_ng_list[[paste0(building_id[1])]]<-fit_prophet_harmonic[[2]][[1]]
combined_ng<-forecast_ng

for (i in 2:length(building_id)){
  training <- t_ng|> filter (id== building_id[i]) 
  prediction <- p_ng|> filter (id== building_id[i])
  
  fit_prophet_harmonic <- training|>  model(prophet(Et ~ Ambient_Temperature_F +Building_Occupancy_DEV + ht +
                                                      season(period = "day", order = 10) +
                                                      season(period = "week", order = 5) +
                                                      season(period = "year", order = 3)))
  forecast_ng<-forecast(fit_prophet_harmonic,prediction)
  fit_prophet_ng_list[[paste0(building_id[i])]]<-fit_prophet_harmonic[[2]][[1]]
  combined_ng<-bind_rows(combined_ng,forecast_ng)}

write.table(combined_ng, file="combined_ng_forecasts_gbm.csv", sep=",", row.names = FALSE)

# Forecast for 2021 and 2022 (CW is negative, steam and ng is NA)
forecast_ng1 <- forecast_ng|>group_by(lubridate::year(Timestamp))|>summarise(Ng_MmBTU=sum(.mean,na.rm = TRUE)/10^6)
save.image("C:/Users/malhotra.164/OneDrive - The Ohio State University/Yashvi Malhotra-KWEST/BCA/R_BCA/R_BCA/Raw_data/forecasting_prophet_gbm.RData")

