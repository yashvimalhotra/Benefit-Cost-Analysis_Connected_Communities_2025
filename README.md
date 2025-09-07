# Benefit-Cost-Analysis_Connected_Communities_2025

1.1.	TOWT_baseline_BCA

      Input: 
      i. CC_buildingdata_BCA.csv
      
      Output: 
      i. baseline_BCA_CC_2019.csv

2.1.	Quant_BCA_Scenarios

      Input:
      i. Building_CC_utilities_outliers.csv
      ii.	D_dates_times.csv
      iii. towt_baseline.csv
      iv.	gbm_baseline.csv
      
      Output: 
      i. loads_shed_estimates_CC_2019_22.csv

2.2.	Quant_BCA_rf_audit 

      Input: 
      i. Building_CC_utilities_outliers.csv
      ii.	D_dates_times.csv
      iii. towt_baseline.csv
      iv.	gbm_baseline.csv
      v.	elec_ECM.csv
      vi.	cw_ECM.csv
      
      Output: 
      i. rf_CC_2019_22.csv

3.0	GBMForecast_Prophet

      Input: 
      i. baseline_energy_CC_2019_20.csv
      ii. Building_CC_utilities_outliers.csv
      
      Output:
      i.	combined_elec_forecasts_gbm.csv
      ii.	combined_cw_forecasts_gbm.csv
      iii.combined_steam_forecasts_gbm.csv
      iv.	combined_ng_forecasts_gbm.csv
      
4.1.	SR Scenarios

      Input: 
      
      Output:
      i.	loads_shed_SR.csv
      ii.	rf_CC_SR.csv
      
4.2.	Energy Market Scenarios

      Input: 
      
      Output:
      i.	write.table(baseline_energy,file="loads_shed_Energy_200.csv", sep=",")
      ii.	write.table(baseline_energy,file="rf_200_Energy.csv", sep=",")

5.1.	BCA_Avoided_Peak_v3_rf

      Input: 
      
      Output:
      i.	avoided_rfpeak_summary.csv)


5.2.	BCA_Avoided_Peak_v3_audit [price change]

      Input
      i.	wind_cc.csv
      ii.	solar_yearly.csv
      iii. Building_CC_utilities_outliers.csv
      iv.	baseline_energy_CC_2019_20.csv
      v.	EE_input_R.csv
      vi.	combined_elec_forecasts_gbm.csv
      vii. combined_cw_forecasts_gbm.csv
      viii.	combined_steam_forecasts_gbm.csv
      ix.	combined_ng_forecasts_gbm.csv
      x.	yearly_tariff.csv
      xi.	Building_data_filtered.csv
      xii. loads_shed_estimates_CC_2019_22.csv
      xiii. loads_shed_Energy_Market.csv
      xiv. loads_shed_SR.csv
      
      Output:
      i.	avoided_peak_summary.csv

5.3.	BCA_Avoided_Peak_v5 (Type)

      Input: 
      i.	wind_cc.csv
      ii.	solar_yearly.csv
      iii. Building_CC_utilities_outliers.csv
      iv.	baseline_energy_CC_2019_20.csv
      v.	EE_input_R.csv
      vi.	combined_elec_forecasts_gbm.csv
      vii. combined_cw_forecasts_gbm.csv
      viii.	combined_steam_forecasts_gbm.csv
      ix.	combined_ng_forecasts_gbm.csv
      x.	yearly_tariff.csv
      xi.	loads_shed_estimates_CC_2019_22.csv
      xii.	Building_type.csv
      xiii. loads_shed_Energy_Market.csv
      xiv. loads_shed_SR.csv
      
      Output:
      i.	avoided_Type_peak.csv

6.1	BCA_Avoided_Energy_BAU

      Input: 
      i.	wind_cc.csv
      ii.	solar_yearly.csv
      iii. Building_CC_utilities_outliers.csv
      iv.	baseline_energy_CC_2019_20.csv
      v.	EE_input_R.csv
      vi.	combined_elec_forecasts_gbm.csv
      vii. combined_cw_forecasts_gbm.csv
      viii.	combined_steam_forecasts_gbm.csv
      ix.	combined_ng_forecasts_gbm.csv
      x.	yearly_tariff.csv
      xi.	loads_shed_estimates_CC_2019_22.csv
      xii.	Building_type.csv
      xiii. loads_shed_Energy_Market.csv
      xii.	rf_CC_2019_22_audit.csv
      
      Output:
      i.	Building_type.csv
      ii.	ecm_savings_yearly.csv
      iii.avoided_energy_yearly.csv

6.2	BCA_Avoided_Energy_Scenarios_v4_Yashvi (run again to account for updated capacity charges)

      Input: 
      i.	avoided_peak_summary.csv
      ii. avoided_rfpeak_summary.csv # To get capacity market costs
      iii.	loads_shed_estimates_CC_2019_22.csv
      iv.	rf_CC_2019_22_audit.csv
      v. loads_shed_Energy_Market.csv
      vi. rf_CC_Energy.csv
      vii.	loads_shed_SR.csv
      viii.	rf_CC_SR.csv
      
      Output:
      i.	Scenarios_energy.csv
      ii.	report_Scenarios_energy.csv
      iii. Scenarios_money.csv

6.3	BCA_Avoided_Energy_BAU_Type

      Input: 
      i.	wind_cc.csv
      ii.	solar_yearly.csv
      iii. Building_CC_utilities_outliers.csv
      iv.	baseline_energy_CC_2019_20.csv
      v.	EE_input_R.csv
      vi.	combined_elec_forecasts_gbm.csv
      vii. combined_cw_forecasts_gbm.csv
      viii.	combined_steam_forecasts_gbm.csv
      ix.	combined_ng_forecasts_gbm.csv
      x.	yearly_tariff.csv
      xi.	loads_shed_estimates_CC_2019_22.csv
      
      Output: 
      i.	ecm_savings_type.csv
      ii.	avoided_energy_type.csv

6.4	BCA_Avoided_Energy_Scenarios_v5 [also for TYPE]

      Input:
     
      Output: 
      i. Scenarios_energy_Type.csv

6.5	BCA_Avoided_Energy_Scenarios_v4_energy_uncertainity : To see changes energy market if lmp was 200

      Input: 
      i.	loads_shed_Energy_Market.csv
      ii.	rf_CC_Energy.csv
      iii.loads_shed_Energy_200.csv
      iv.	rf_200_Energy.csv
      
      Output:

7.1	BCA_Net_v4_Yashvi

      Input: 
      i.	Emissions1 <- read.csv("Emissions1.csv")
      ii.	wind_yearly <- read.csv("wind_cc.csv")
      iii.	ecm_savings_yearly<-read.csv("ecm_savings_yearly.csv")
      iv.	avoided_energy_yearly <- read.csv("avoided_energy_yearly.csv")
      v.	avoided_peak_summary<- read.csv("avoided_peak_summary.csv") 
      vi.	Scenarios_energy<- read.csv("Scenarios_energy.csv")
      vii.	Scenarios_energy<-rbind(Scenarios_energy,S4_energy2)
      
      Output:
   
7.2	BCA_Net_v5

      Input: 
      i.	Emissions1.csv # Weight
      ii.	wind_cc.csv # Weight
      iii.ecm_savings_type.csv
      iv.avoided_energy_type.csv
      v.	avoided_Type_peak.csv") # id and type
      vi.	Scenarios_energy_Type.csv
      	
      Output:

7.3	BCA_Net_v5 _TYPE

      Input: 
      i. Emissions1.csv # Weight
      ii.	wind_cc.csv # Weight
      iii. ecm_savings_type.csv
      iv.	avoided_energy_type.csv
      v.	avoided_Type_peak.csv
      vi.	Scenarios_energy_Type.csv
      
      Output:
