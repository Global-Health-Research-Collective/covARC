rm(list=ls())
library(dplyr)
library(curl)
library(tidyverse)

library(aws.s3)
Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIAQKKGYNQC433Z2ZNA",
           "AWS_SECRET_ACCESS_KEY" = "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")

s3_bucket="covid-data-overall"

readFromS3 = function(filename, bucket, sep = ','){
  return(s3read_using(FUN=read.csv, 
                      bucket = bucket, 
                      object=filename,
                      sep = sep, header=T))
}

variants_dataset <- readFromS3('s3://covid-data-overall/processed/risk-calculator-data/all_countries_galphabet_variants.csv',s3_bucket,sep = ',' )

graphical = data.frame()
country_list <- read.csv('C:/Users/shrey/Downloads/covid/country_name_code.csv')
#country_list <- read.csv(curl("https://raw.githubusercontent.com/Volunteer-Collab/risk_calculator/main/Risk_calculator/risk_calculator_simple/custom_dataset/country_name_code.csv?token=ACZDUYBLH4IUWYMBZBM7IKTBBP62I"))
country_list <- country_list[-c(1,2)]
#==============Define the inputs======================================================================================
country_name ="United States"
people_passed_outdoor = 10
people_passed_indoor = 5
#date = "2021-10-10"
region = "Massachusetts"
mask_type = "N95 respirator"
type_vaccine_dose = "No Vaccine"
age_group = 30
gender = "Male"
chronic_illness = "No" #Diabetes, Heart Disease, Cancer, Lung disease, High Blood Pressure, Immunocompromised, Asthma, Kidney Disease, Obesity, Sickle Cell Anemia, HIV, Liver Disease

#Suggestive evidence: Qian et al. study of cases in China, Jimenez's Aerosol Transmission Model, lack of surge from BLM protests, anecdotal CO2 data from protests, zero outdoor outbreaks of any kind, many indoor dining outbreaks, despite both indoor and outdoor dining being open in the US
#=====================================================================================================================
country_code = country_list$CountryCode_2[country_list$CountryName==country_name]
country_code_csv = paste(country_code,"csv",sep = ".")
#=================Filtering the US Dataset from Oxford Dataset========================================================
#Just use the covid policy tracker for everything rather than coronasurveys

if (country_code=="US") {
  #jhu_dataset <- read.csv(curl("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv"))
  jhu_dataset <- readFromS3('s3://covid-data-overall/processed/risk-calculator-data/USStates_confirmed_cases_deaths.csv',s3_bucket,sep = ',')
  #jhu_dataset <- jhu_dataset[jhu_dataset$CountryName=="United States",]
  #jhu_dataset <- jhu_dataset[c(1,3,6,40,41)]
  jhu_dataset <- jhu_dataset %>% mutate_if(is.character,as.factor)
  jhu_dataset <- jhu_dataset[jhu_dataset$RegionName!="",]
  jhu_dataset$Date<-as.Date(as.character(jhu_dataset$Date),format="%Y%m%d")
  summary(jhu_dataset)
  jhu_dataset <- jhu_dataset[-c(2,6,7)]
  colnames(jhu_dataset) <- c("date","region","Confirmed","Deaths")
} else {
  jhu_dataset <- read.csv(curl(paste("https://raw.githubusercontent.com/GCGImdea/coronasurveys/master/data/jhu/region/",country_code_csv, sep="")))
  jhu_dataset<-jhu_dataset[order(jhu_dataset[,1] ),]
  
  jhu_dataset <- jhu_dataset[,-c(1,8,9)]
  jhu_dataset <- jhu_dataset[-c(4,5,6)]
  
}  

if(country_code=="CA"){
  #==============Functions to Remove some common errors in JHU=================
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Unknown",]
  #======Functions to scrape out Canada JHU Dataset============================
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Recovered",]
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Diamond Princess",]
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Grand Princess",]
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Repatriated Travellers",]
  
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Montreal, QC",]
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Calgary, Alberta",]
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Edmonton, Alberta",]
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Toronto, ON",]
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="London, ON",]
  
  #jhu_dataset$region[jhu_dataset$region=="Montreal, QC"] <- "Quebec"
  #jhu_dataset$region[jhu_dataset$region=="Calgary, Alberta"] = "Alberta"
  #hu_dataset$region[jhu_dataset$region=="Edmonton, Alberta"] = "Alberta"
  #jhu_dataset$region[jhu_dataset$region=="Toronto, ON"] = "Ontario"
  #jhu_dataset$region[jhu_dataset$region=="London, ON"] = "Ontario"
  #=============================================================================
}


ratio_dataset <- read.csv("C:/Users/shrey/Downloads/covid/Ratios_Survey.csv")
#ratio_dataset <- read.csv(curl("https://raw.githubusercontent.com/Volunteer-Collab/risk_calculator/main/Risk_calculator/risk_calculator_simple/custom_dataset/Ratios_Survey.csv?token=ACZDUYD4JVF3ZIVRPC3ZV5LBBP7RW"))
ratio_dataset <- ratio_dataset[-c(1:6)]
ratio = ratio_dataset$Survey_Reported_Ratio[ratio_dataset$Country==country_name]
jhu_dataset$Confirmed_u <- jhu_dataset$Confirmed*ratio

population_full <- read.csv("C:/Users/shrey/Downloads/covid/UID_ISO_FIPS_LookUp_Table_population_Countries_Regions.csv")
#population_full <- read.csv(curl("https://raw.githubusercontent.com/Volunteer-Collab/risk_calculator/main/Risk_calculator/risk_calculator_simple/custom_dataset/UID_ISO_FIPS_LookUp_Table_population_Countries_Regions.csv?token=ACZDUYH5CI5OQ6DZWPN44P3BBP6VM"))
population_full <- population_full[-c(1,2,3,4,5,6,9,10,11)]

population_full <- population_full[!is.na(population_full$Population),]
population_full <- population_full %>% mutate_if(is.character,as.factor)
population_full$Country_Region <- gsub("US", "United States", population_full$Country_Region)

if(!is.na(region)){
  pop_var <- population_full$Population[population_full$Province_State==region & population_full$Country_Region==country_name]
  pop_var <- pop_var[1]
}else{
  pop_var<-population_full$Population[population_full$Country_Region==country_name]
}

library(zoo)
#======================================================================================================
#======================================================================================================
if(!is.na(region)){
  jhu_dataset<-jhu_dataset %>%
    group_by(region) %>%
    mutate(NewCases = Confirmed - lag(Confirmed, default = first(Confirmed)))
  jhu_dataset <- jhu_dataset %>%
    dplyr::group_by(region) %>%
    dplyr::mutate(fourteen_day_agg = rollapply(NewCases, 14, sum, align="right", fill= NA))
  
}else{
  jhu_dataset<-jhu_dataset %>%    
    mutate(NewCases = Confirmed - lag(Confirmed, default = first(Confirmed)))
  jhu_dataset <- jhu_dataset %>%
    dplyr::mutate(fourteen_day_agg = rollapply(NewCases, 14, sum, align="right", fill= NA))  
}
#===============================================================================================
if(!is.na(region)){
  jhu_dataset<-jhu_dataset %>%
    group_by(region) %>%
    mutate(NewCases_u = Confirmed_u - lag(Confirmed_u, default = first(Confirmed_u)))
  jhu_dataset <- jhu_dataset %>%
    dplyr::group_by(region) %>%
    dplyr::mutate(fourteen_day_agg_u = rollapply(NewCases_u, 14, sum, align="right", fill= NA))
  
}else{
  jhu_dataset<-jhu_dataset %>%    
    mutate(NewCases_u = Confirmed_u - lag(Confirmed, default = first(Confirmed)))
  jhu_dataset <- jhu_dataset %>%
    dplyr::mutate(fourteen_day_agg_u = rollapply(NewCases_u, 14, sum, align="right", fill= NA))  
}
#======================================================================================================
#======================================================================================================
jhu_dataset_agg <- jhu_dataset[!is.na(jhu_dataset$fourteen_day_agg),]
jhu_dataset_agg <- jhu_dataset_agg %>% mutate_if(is.character,as.factor)
#======================================================================================================
jhu_dataset_agg_u <- jhu_dataset[!is.na(jhu_dataset$fourteen_day_agg_u),]
jhu_dataset_agg_u <- jhu_dataset_agg_u %>% mutate_if(is.character,as.factor)
#======================================================================================================
#======================================================================================================
jhu_dataset_date <- jhu_dataset_agg[which(jhu_dataset_agg$region==region),]

for (date in jhu_dataset_date$date){
  if(!is.na(region)){
    fac_aggregate_cases <- jhu_dataset_agg$fourteen_day_agg[which(jhu_dataset_agg$region==region & as.Date(jhu_dataset_agg$date)==as.Date(date))]
  }else{
    fac_aggregate_cases <- jhu_dataset_agg$fourteen_day_agg[as.Date(jhu_dataset_agg$date)==as.Date(date)]
  }
  #======================================================================================================
  if(!is.na(region)){
    fac_aggregate_cases_u <- jhu_dataset_agg$fourteen_day_agg_u[which(jhu_dataset_agg_u$region==region & as.Date(jhu_dataset_agg_u$date)==as.Date(date))]
  }else{
    fac_aggregate_cases_u <- jhu_dataset_agg$fourteen_day_agg_u[as.Date(jhu_dataset_agg_u$date)==as.Date(date)]
  }
  #======================================================================================================
  #======================================================================================================
  fac_aggregate_cases_per_pop <- fac_aggregate_cases/pop_var 
  fac_aggregate_cases_per_pop_u <- fac_aggregate_cases_u/pop_var 
  #======================================================================================================
  #======================================================================================================
  
  mask <- read.csv("C:/Users/shrey/Downloads/covid/mask.csv") #Ref: https://www.epa.gov/sciencematters/epa-researchers-test-effectiveness-face-masks-disinfection-methods-against-covid-19
  #mask <- read.csv(curl("https://raw.githubusercontent.com/Volunteer-Collab/risk_calculator/main/Risk_calculator/risk_calculator_simple/custom_dataset/mask.csv?token=ACZDUYEEPMIQ2GRZNQXAPYLBBP6K6"))
  mask<- mask %>% mutate_if(is.character,as.factor)
  mask$risk <- 1-mask$FFE
  summary(mask)
  vaccine_eff <- read.csv("C:/Users/shrey/Downloads/covid/vaccine_eff.csv", sep = ",") #Ref: https://yourlocalepidemiologist.substack.com/p/vaccine-table-update-lots-on-delta
  #vaccine_eff <- read.csv(curl("https://raw.githubusercontent.com/Volunteer-Collab/risk_calculator/main/Risk_calculator/risk_calculator_simple/custom_dataset/vaccine_eff.csv?token=ACZDUYEVXTJVJEBGNSWYYV3BBP6RI"))
  vaccine_eff$vaccine <- as.factor(vaccine_eff$vaccine)
  
  #================data pulling script======================== 
  #variants_dataset <- read.csv(curl("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv"))
  
  variants_dataset <- readFromS3('s3://covid-data-overall/processed/risk-calculator-data/all_countries_galphabet_variants.csv',s3_bucket,sep = ',' )
  #variants_dataset <- readFromS3('s3://covid-data-overall/processed/risk-calculator-data/OxCGRT_latest.csv', s3_bucket, sep=',')
  summary(variants_dataset)
  variants_dataset$Date <- as.Date(variants_dataset$Date)
  
  variants_dataset <- variants_dataset[variants_dataset$CountryName == country_name,]
  variants_dataset <- variants_dataset[-c(2,4)]
  variants_date = variants_dataset[variants_dataset$Date==as.Date(date)-31,]
  variants_date[is.na(variants_date)] <- 0
  variants_date <- variants_date[-c(2)]
  
  
  #===============Alpha Variant===========================================
  if (!is.na(variants_date$Alpha)) {
    factor_variant_alpha = variants_date$Alpha
  } else {factor_variant_alpha = 0}
  
  #===============Beta Variant============================================
  #variants_date$prevalence_gaussian5_b.1.351 = variants_date$prevalence_gaussian5_b.1.351+variants_date$prevalence_gaussian5_b.1.351.2+variants_date$prevalence_gaussian5_b.1.351.3
  if(!is.na(variants_date$Beta)){
    factor_variant_beta = variants_date$Beta
  } else {factor_variant_beta = 0}
  
  #===============Gamma Variant===========================================
  #variants_date$prevalence_gaussian5_p.1 = variants_date$prevalence_gaussian5_p.1+variants_date$prevalence_gaussian5_p.1.1+variants_date$prevalence_gaussian5_p.1.2+variants_date$prevalence_gaussian5_p.1.3+variants_date$prevalence_gaussian5_p.1.4+variants_date$prevalence_gaussian5_p.1.6+variants_date$prevalence_gaussian5_p.1.7+variants_date$prevalence_gaussian5_p.1.8
  if (!is.na(variants_date$Gamma)){
    factor_variant_gamma = variants_date$Gamma
  } else {factor_variant_gamma = 0}
  
  #===============Delta Variant===========================================
  #variants_date$prevalence_gaussian5_b.1.617.2 = variants_date$prevalence_gaussian5_b.1.617.2+variants_date$prevalence_gaussian5_ay.2+variants_date$prevalence_gaussian5_ay.3+variants_date$prevalence_gaussian5_ay.3.1+variants_date$prevalence_gaussian5_ay.4+variants_date$prevalence_gaussian5_ay.5+variants_date$prevalence_gaussian5_ay.6+variants_date$prevalence_gaussian5_ay.7+variants_date$prevalence_gaussian5_ay.9+variants_date$prevalence_gaussian5_ay.10+variants_date$prevalence_gaussian5_ay.11+variants_date$prevalence_gaussian5_ay.12
  if (!is.na(variants_date$Delta)) {
    factor_variant_delta = variants_date$Delta
  } else {factor_variant_delta = 0}
  
  fac_mask = mask$risk[which(mask$mask_type==mask_type)]
  if (!is.na(1-(factor_variant_alpha+factor_variant_beta+factor_variant_gamma+factor_variant_delta))) {
    factor_variant_normal = 1-(factor_variant_alpha+factor_variant_beta+factor_variant_gamma+factor_variant_delta)
  } else {factor_variant_normal = 0}
  
  
  #fac_variants = vaccine risk factor reduction
  #Can add more variants data here
  
  fac_vaccine_normal = 1-(vaccine_eff$normal[which(vaccine_eff$vaccine == type_vaccine_dose)]) #1- 80-95% efficacy
  fac_vaccine_alpha = 1-(vaccine_eff$alpha[which(vaccine_eff$vaccine == type_vaccine_dose)])
  fac_vaccine_beta = 1-(vaccine_eff$beta[which(vaccine_eff$vaccine == type_vaccine_dose)])
  fac_vaccine_gamma = 1-(vaccine_eff$gamma[which(vaccine_eff$vaccine == type_vaccine_dose)])
  fac_vaccine_delta = 1-(vaccine_eff$delta[which(vaccine_eff$vaccine == type_vaccine_dose)])
  
  fac_vaccine_normal_u = 1-(vaccine_eff$normal_u[which(vaccine_eff$vaccine == type_vaccine_dose)]) #1- 80-95% efficacy
  fac_vaccine_alpha_u = 1-(vaccine_eff$alpha_u[which(vaccine_eff$vaccine == type_vaccine_dose)])
  fac_vaccine_beta_u = 1-(vaccine_eff$beta_u[which(vaccine_eff$vaccine == type_vaccine_dose)])
  fac_vaccine_gamma_u = 1-(vaccine_eff$gamma_u[which(vaccine_eff$vaccine == type_vaccine_dose)])
  fac_vaccine_delta_u = 1-(vaccine_eff$delta_u[which(vaccine_eff$vaccine == type_vaccine_dose)])
  
  #======================================================================================================
  #======================================================================================================
  fac_variants_u = factor_variant_normal*fac_vaccine_normal + factor_variant_alpha*fac_vaccine_alpha + factor_variant_beta*fac_vaccine_beta+ factor_variant_gamma*fac_vaccine_gamma+ factor_variant_delta*fac_vaccine_delta
  #======================================================================================================
  fac_variants = factor_variant_normal*fac_vaccine_normal_u + factor_variant_alpha*fac_vaccine_alpha_u + factor_variant_beta*fac_vaccine_beta_u+ factor_variant_gamma*fac_vaccine_gamma_u+ factor_variant_delta*fac_vaccine_delta_u
  #======================================================================================================
  #======================================================================================================
  
  if(fac_variants!=0){
    risk_factor_outdoor = 0.05*people_passed_outdoor*(fac_aggregate_cases_per_pop*fac_mask*fac_variants)
    risk_factor_indoor = people_passed_indoor*(fac_aggregate_cases_per_pop*fac_mask*fac_variants)
  }else{
    fac_aggregate_cases_per_pop <- fac_aggregate_cases_per_pop*fac_vaccine_normal
    risk_factor_outdoor = 0.05*people_passed_outdoor*(fac_aggregate_cases_per_pop*fac_mask)
    risk_factor_indoor = people_passed_indoor*(fac_aggregate_cases_per_pop*fac_mask)
  }
  #======================================================================================================
  if(fac_variants_u!=0){
    risk_factor_outdoor_u = 0.05*people_passed_outdoor*(fac_aggregate_cases_per_pop_u*fac_mask*fac_variants_u)
    risk_factor_indoor_u = people_passed_indoor*(fac_aggregate_cases_per_pop_u*fac_mask*fac_variants_u)
  }else{
    fac_aggregate_cases_per_pop_u <- fac_aggregate_cases_per_pop_u*fac_vaccine_normal_u
    risk_factor_outdoor_u = 0.05*people_passed_outdoor*(fac_aggregate_cases_per_pop_u*fac_mask)
    risk_factor_indoor_u = people_passed_indoor*(fac_aggregate_cases_per_pop_u*fac_mask)
  }
  #======================================================================================================
  #======================================================================================================
  
  cum_risk_factor <- risk_factor_outdoor+risk_factor_indoor
  
  cum_risk_factor_u <- risk_factor_outdoor_u+risk_factor_indoor_u
  
  if(age_group>=0 & age_group<=17){
    hosp_risk_lower = cum_risk_factor_u*0.008
    death_risk_lower = cum_risk_factor_u*0.000015
    
    hosp_risk_upper = cum_risk_factor*0.008
    death_risk_upper = cum_risk_factor*0.000015
  }
  if(age_group>=18 & age_group<=49){
    hosp_risk_lower = cum_risk_factor_u*0.025
    death_risk_lower = cum_risk_factor_u*0.0007
    hosp_risk_upper = cum_risk_factor*0.025
    death_risk_upper = cum_risk_factor*0.0007
  }
  if(age_group>=50 & age_group<=64){
    hosp_risk_lower = cum_risk_factor_u*0.079
    death_risk_lower = cum_risk_factor_u*0.007
    hosp_risk_upper = cum_risk_factor*0.079
    death_risk_upper = cum_risk_factor*0.007
  }
  if(age_group>=65){
    hosp_risk_lower = cum_risk_factor_u*0.23
    death_risk_lower = cum_risk_factor_u*0.06
    hosp_risk_upper = cum_risk_factor*0.23
    death_risk_upper = cum_risk_factor*0.06
  }
  
  if (chronic_illness == "Yes"){
    hosp_risk_lower = hosp_risk_lower*2.5
    hosp_risk_upper = hosp_risk_upper*2.5
    
    death_risk_lower = death_risk_lower*1.2
    death_risk_upper = death_risk_upper*6.9
  }
  if(chronic_illness == "No"){
    hosp_risk_lower = hosp_risk_lower
    hosp_risk_upper = hosp_risk_upper
    
    death_risk_lower = death_risk_lower
    death_risk_upper = death_risk_upper
  }
  
  if(gender == "Male"){
    death_risk_lower = death_risk_lower*1.5
    death_risk_upper = death_risk_upper*2.3
  }
  
  if(!is.na(variants_date$Alpha)){
    hosp_risk_lower=1.5*hosp_risk_lower
    hosp_risk_upper=1.6*hosp_risk_upper
    death_risk_lower=1.4*death_risk_lower
    death_risk_upper=1.7*death_risk_upper
  }
  
  #if(!is.na(variants_date$prevalence_gaussian5_b.1.617.2)){
  #  hosp_risk_lower=1.5*hosp_risk_lower
  #  hosp_risk_upper=1.6*hosp_risk_upper
  #  death_risk_lower=1.4*death_risk_lower
  #  death_risk_upper=1.7*death_risk_upper
  #}
  
  if(!is.na(variants_date$Gamma)){
    #  hosp_risk_lower=1.5*hosp_risk_lower
    #  hosp_risk_upper=1.6*hosp_risk_upper
    death_risk_lower=1.2*death_risk_lower
    death_risk_upper=1.9*death_risk_upper
  }
  if(!is.na(variants_date$Delta)){
    hosp_risk_lower=1.9*hosp_risk_lower
    hosp_risk_upper=3*hosp_risk_upper
    death_risk_lower=1.5*death_risk_lower
    death_risk_upper=3.3*death_risk_upper
  }
  
  hosp_risk_lower<-ifelse (type_vaccine_dose=="Moderna (Dose 1)" | type_vaccine_dose=="Pfizer (Dose 1)" | type_vaccine_dose=="Astrazeneca (Dose 1)", 0.1*hosp_risk_lower, 0.7*hosp_risk_lower)
  hosp_risk_upper<-ifelse (type_vaccine_dose=="Moderna (Dose 1)" | type_vaccine_dose=="Pfizer (Dose 1)" | type_vaccine_dose=="Astrazeneca (Dose 1)", 0.1*hosp_risk_upper, 0.7*hosp_risk_upper)
  hosp_risk_lower<-ifelse (type_vaccine_dose=="Moderna (Dose 1)" | type_vaccine_dose=="Pfizer (Dose 1)" | type_vaccine_dose=="Astrazeneca (Dose 1)", 0.1*death_risk_lower, 0.7*death_risk_lower)
  hosp_risk_lower<-ifelse (type_vaccine_dose=="Moderna (Dose 1)" | type_vaccine_dose=="Pfizer (Dose 1)" | type_vaccine_dose=="Astrazeneca (Dose 1)", 0.1*death_risk_upper, 0.7*death_risk_upper)
  
  
  risk_factor_indoor = risk_factor_indoor*100
  risk_factor_outdoor = risk_factor_outdoor*100
  cum_risk_factor = cum_risk_factor*100
  cum_risk_factor_u = cum_risk_factor_u*100
  hosp_risk_lower = hosp_risk_lower*100
  hosp_risk_upper = hosp_risk_upper*100
  death_risk_lower = death_risk_lower*100
  death_risk_upper = death_risk_upper*100
  
  if (risk_factor_indoor>99){
    risk_factor_indoor = 99
  }
  if (risk_factor_indoor_u>99){
    risk_factor_indoor_u = 99
  }
  if (risk_factor_outdoor>99){
    risk_factor_outdoor = 99
  }
  if (risk_factor_outdoor_u>99){
    risk_factor_outdoor_u = 99
  }
  if (cum_risk_factor>99){
    cum_risk_factor = 99
  }
  if (cum_risk_factor_u>99){
    cum_risk_factor_u = 99
  }
  if (hosp_risk_lower>99){
    hosp_risk_lower = 99
  }
  if (hosp_risk_upper>99){
    hosp_risk_upper = 99
  }
  if (death_risk_lower>99){
    death_risk_lower = 99
  }
  if (death_risk_upper>99){
    death_risk_upper = 99
  }
  
  risk_factor_outdoor #Upper value of indoor risk value
  risk_factor_indoor #Upper value of outdoor risk value
  
  risk_factor_indoor_u #Lower value of indoor risk value
  risk_factor_outdoor_u #Lower value of outdoor risk value
  
  cum_risk_factor_u #Lower value of risk factor
  cum_risk_factor #Upper value of risk factor
  
  hosp_risk_lower
  hosp_risk_upper
  
  death_risk_lower
  death_risk_upper
  
  if (cum_risk_factor<=0.1) {
    f_risk = "Low Risk"
  } else if(cum_risk_factor>0.1 & cum_risk_factor<=1)
  {f_risk = "Medium Risk"
  } else if(cum_risk_factor>1){
    f_risk = "High Risk"
  } else if(cum_risk_factor==0){
    f_risk = "Error!"
  }
  
  f_risk #Stores value for which level of risk it is 
  
  #f_risk #Stores value for which level of risk it is 
  print(as.Date(date))
  print(cum_risk_factor)
  print(cum_risk_factor_u)
  #normal_date <- as.Date(date)
  var <- c(date, fac_aggregate_cases, fac_aggregate_cases_per_pop, cum_risk_factor, cum_risk_factor_u, hosp_risk_lower, hosp_risk_upper, death_risk_lower, death_risk_upper, f_risk)
  graphical <- rbind(var,graphical)

}
write.csv(graphical,'graphical_usa_30_n95.csv')








rm(list=ls())
library(dplyr)
library(curl)
library(tidyverse)

library(aws.s3)
Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIAQKKGYNQC433Z2ZNA",
           "AWS_SECRET_ACCESS_KEY" = "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")

s3_bucket="covid-data-overall"

readFromS3 = function(filename, bucket, sep = ','){
  return(s3read_using(FUN=read.csv, 
                      bucket = bucket, 
                      object=filename,
                      sep = sep, header=T))
}

graphical = data.frame()
country_list <- read.csv('C:/Users/shrey/Downloads/covid/country_name_code.csv')
#country_list <- read.csv(curl("https://raw.githubusercontent.com/Volunteer-Collab/risk_calculator/main/Risk_calculator/risk_calculator_simple/custom_dataset/country_name_code.csv?token=ACZDUYBLH4IUWYMBZBM7IKTBBP62I"))
country_list <- country_list[-c(1,2)]
#==============Define the inputs======================================================================================
country_name ="United States"
people_passed_outdoor = 10
people_passed_indoor = 5
#date = "2021-10-10"
region = "Massachusetts"
mask_type = "No Mask"
type_vaccine_dose = "Moderna (Dose 2)"
age_group = 30
gender = "Male"
chronic_illness = "No" #Diabetes, Heart Disease, Cancer, Lung disease, High Blood Pressure, Immunocompromised, Asthma, Kidney Disease, Obesity, Sickle Cell Anemia, HIV, Liver Disease

#Suggestive evidence: Qian et al. study of cases in China, Jimenez's Aerosol Transmission Model, lack of surge from BLM protests, anecdotal CO2 data from protests, zero outdoor outbreaks of any kind, many indoor dining outbreaks, despite both indoor and outdoor dining being open in the US
#=====================================================================================================================
country_code = country_list$CountryCode_2[country_list$CountryName==country_name]
country_code_csv = paste(country_code,"csv",sep = ".")
#=================Filtering the US Dataset from Oxford Dataset========================================================
#Just use the covid policy tracker for everything rather than coronasurveys

if (country_code=="US") {
  #jhu_dataset <- read.csv(curl("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv"))
  jhu_dataset <- readFromS3('s3://covid-data-overall/processed/risk-calculator-data/USStates_confirmed_cases_deaths.csv',s3_bucket,sep = ',')
  #jhu_dataset <- jhu_dataset[jhu_dataset$CountryName=="United States",]
  #jhu_dataset <- jhu_dataset[c(1,3,6,40,41)]
  jhu_dataset <- jhu_dataset %>% mutate_if(is.character,as.factor)
  jhu_dataset <- jhu_dataset[jhu_dataset$RegionName!="",]
  jhu_dataset$Date<-as.Date(as.character(jhu_dataset$Date),format="%Y%m%d")
  summary(jhu_dataset)
  jhu_dataset <- jhu_dataset[-c(2,6,7)]
  colnames(jhu_dataset) <- c("date","region","Confirmed","Deaths")
} else {
  jhu_dataset <- read.csv(curl(paste("https://raw.githubusercontent.com/GCGImdea/coronasurveys/master/data/jhu/region/",country_code_csv, sep="")))
  jhu_dataset<-jhu_dataset[order(jhu_dataset[,1] ),]
  
  jhu_dataset <- jhu_dataset[,-c(1,8,9)]
  jhu_dataset <- jhu_dataset[-c(4,5,6)]
  
}  

if(country_code=="CA"){
  #==============Functions to Remove some common errors in JHU=================
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Unknown",]
  #======Functions to scrape out Canada JHU Dataset============================
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Recovered",]
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Diamond Princess",]
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Grand Princess",]
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Repatriated Travellers",]
  
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Montreal, QC",]
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Calgary, Alberta",]
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Edmonton, Alberta",]
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Toronto, ON",]
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="London, ON",]
  
  #jhu_dataset$region[jhu_dataset$region=="Montreal, QC"] <- "Quebec"
  #jhu_dataset$region[jhu_dataset$region=="Calgary, Alberta"] = "Alberta"
  #hu_dataset$region[jhu_dataset$region=="Edmonton, Alberta"] = "Alberta"
  #jhu_dataset$region[jhu_dataset$region=="Toronto, ON"] = "Ontario"
  #jhu_dataset$region[jhu_dataset$region=="London, ON"] = "Ontario"
  #=============================================================================
}


ratio_dataset <- read.csv("C:/Users/shrey/Downloads/covid/Ratios_Survey.csv")
#ratio_dataset <- read.csv(curl("https://raw.githubusercontent.com/Volunteer-Collab/risk_calculator/main/Risk_calculator/risk_calculator_simple/custom_dataset/Ratios_Survey.csv?token=ACZDUYD4JVF3ZIVRPC3ZV5LBBP7RW"))
ratio_dataset <- ratio_dataset[-c(1:6)]
ratio = ratio_dataset$Survey_Reported_Ratio[ratio_dataset$Country==country_name]
jhu_dataset$Confirmed_u <- jhu_dataset$Confirmed*ratio

population_full <- read.csv("C:/Users/shrey/Downloads/covid/UID_ISO_FIPS_LookUp_Table_population_Countries_Regions.csv")
#population_full <- read.csv(curl("https://raw.githubusercontent.com/Volunteer-Collab/risk_calculator/main/Risk_calculator/risk_calculator_simple/custom_dataset/UID_ISO_FIPS_LookUp_Table_population_Countries_Regions.csv?token=ACZDUYH5CI5OQ6DZWPN44P3BBP6VM"))
population_full <- population_full[-c(1,2,3,4,5,6,9,10,11)]

population_full <- population_full[!is.na(population_full$Population),]
population_full <- population_full %>% mutate_if(is.character,as.factor)
population_full$Country_Region <- gsub("US", "United States", population_full$Country_Region)

if(!is.na(region)){
  pop_var <- population_full$Population[population_full$Province_State==region & population_full$Country_Region==country_name]
  pop_var <- pop_var[1]
}else{
  pop_var<-population_full$Population[population_full$Country_Region==country_name]
}

library(zoo)
#======================================================================================================
#======================================================================================================
if(!is.na(region)){
  jhu_dataset<-jhu_dataset %>%
    group_by(region) %>%
    mutate(NewCases = Confirmed - lag(Confirmed, default = first(Confirmed)))
  jhu_dataset <- jhu_dataset %>%
    dplyr::group_by(region) %>%
    dplyr::mutate(fourteen_day_agg = rollapply(NewCases, 14, sum, align="right", fill= NA))
  
}else{
  jhu_dataset<-jhu_dataset %>%    
    mutate(NewCases = Confirmed - lag(Confirmed, default = first(Confirmed)))
  jhu_dataset <- jhu_dataset %>%
    dplyr::mutate(fourteen_day_agg = rollapply(NewCases, 14, sum, align="right", fill= NA))  
}
#===============================================================================================
if(!is.na(region)){
  jhu_dataset<-jhu_dataset %>%
    group_by(region) %>%
    mutate(NewCases_u = Confirmed_u - lag(Confirmed_u, default = first(Confirmed_u)))
  jhu_dataset <- jhu_dataset %>%
    dplyr::group_by(region) %>%
    dplyr::mutate(fourteen_day_agg_u = rollapply(NewCases_u, 14, sum, align="right", fill= NA))
  
}else{
  jhu_dataset<-jhu_dataset %>%    
    mutate(NewCases_u = Confirmed_u - lag(Confirmed, default = first(Confirmed)))
  jhu_dataset <- jhu_dataset %>%
    dplyr::mutate(fourteen_day_agg_u = rollapply(NewCases_u, 14, sum, align="right", fill= NA))  
}
#======================================================================================================
#======================================================================================================
jhu_dataset_agg <- jhu_dataset[!is.na(jhu_dataset$fourteen_day_agg),]
jhu_dataset_agg <- jhu_dataset_agg %>% mutate_if(is.character,as.factor)
#======================================================================================================
jhu_dataset_agg_u <- jhu_dataset[!is.na(jhu_dataset$fourteen_day_agg_u),]
jhu_dataset_agg_u <- jhu_dataset_agg_u %>% mutate_if(is.character,as.factor)
#======================================================================================================
#======================================================================================================
jhu_dataset_date <- jhu_dataset_agg[which(jhu_dataset_agg$region==region),]

for (date in jhu_dataset_date$date){
  if(!is.na(region)){
    fac_aggregate_cases <- jhu_dataset_agg$fourteen_day_agg[which(jhu_dataset_agg$region==region & as.Date(jhu_dataset_agg$date)==as.Date(date))]
  }else{
    fac_aggregate_cases <- jhu_dataset_agg$fourteen_day_agg[as.Date(jhu_dataset_agg$date)==as.Date(date)]
  }
  #======================================================================================================
  if(!is.na(region)){
    fac_aggregate_cases_u <- jhu_dataset_agg$fourteen_day_agg_u[which(jhu_dataset_agg_u$region==region & as.Date(jhu_dataset_agg_u$date)==as.Date(date))]
  }else{
    fac_aggregate_cases_u <- jhu_dataset_agg$fourteen_day_agg_u[as.Date(jhu_dataset_agg_u$date)==as.Date(date)]
  }
  #======================================================================================================
  #======================================================================================================
  fac_aggregate_cases_per_pop <- fac_aggregate_cases/pop_var 
  fac_aggregate_cases_per_pop_u <- fac_aggregate_cases_u/pop_var 
  #======================================================================================================
  #======================================================================================================
  
  mask <- read.csv("C:/Users/shrey/Downloads/covid/mask.csv") #Ref: https://www.epa.gov/sciencematters/epa-researchers-test-effectiveness-face-masks-disinfection-methods-against-covid-19
  #mask <- read.csv(curl("https://raw.githubusercontent.com/Volunteer-Collab/risk_calculator/main/Risk_calculator/risk_calculator_simple/custom_dataset/mask.csv?token=ACZDUYEEPMIQ2GRZNQXAPYLBBP6K6"))
  mask<- mask %>% mutate_if(is.character,as.factor)
  mask$risk <- 1-mask$FFE
  summary(mask)
  vaccine_eff <- read.csv("C:/Users/shrey/Downloads/covid/vaccine_eff.csv", sep = ",") #Ref: https://yourlocalepidemiologist.substack.com/p/vaccine-table-update-lots-on-delta
  #vaccine_eff <- read.csv(curl("https://raw.githubusercontent.com/Volunteer-Collab/risk_calculator/main/Risk_calculator/risk_calculator_simple/custom_dataset/vaccine_eff.csv?token=ACZDUYEVXTJVJEBGNSWYYV3BBP6RI"))
  vaccine_eff$vaccine <- as.factor(vaccine_eff$vaccine)
  
  #================data pulling script======================== 
  #variants_dataset <- read.csv(curl("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv"))
  
  variants_dataset <- readFromS3('s3://covid-data-overall/processed/risk-calculator-data/all_countries_galphabet_variants.csv',s3_bucket,sep = ',' )
  #variants_dataset <- readFromS3('s3://covid-data-overall/processed/risk-calculator-data/OxCGRT_latest.csv', s3_bucket, sep=',')
  summary(variants_dataset)
  variants_dataset$Date <- as.Date(variants_dataset$Date)
  
  variants_dataset <- variants_dataset[variants_dataset$CountryName == country_name,]
  variants_dataset <- variants_dataset[-c(2,4)]
  variants_date = variants_dataset[variants_dataset$Date==as.Date(date)-31,]
  variants_date[is.na(variants_date)] <- 0
  variants_date <- variants_date[-c(2)]
  
  
  #===============Alpha Variant===========================================
  if (!is.na(variants_date$Alpha)) {
    factor_variant_alpha = variants_date$Alpha
  } else {factor_variant_alpha = 0}
  
  #===============Beta Variant============================================
  #variants_date$prevalence_gaussian5_b.1.351 = variants_date$prevalence_gaussian5_b.1.351+variants_date$prevalence_gaussian5_b.1.351.2+variants_date$prevalence_gaussian5_b.1.351.3
  if(!is.na(variants_date$Beta)){
    factor_variant_beta = variants_date$Beta
  } else {factor_variant_beta = 0}
  
  #===============Gamma Variant===========================================
  #variants_date$prevalence_gaussian5_p.1 = variants_date$prevalence_gaussian5_p.1+variants_date$prevalence_gaussian5_p.1.1+variants_date$prevalence_gaussian5_p.1.2+variants_date$prevalence_gaussian5_p.1.3+variants_date$prevalence_gaussian5_p.1.4+variants_date$prevalence_gaussian5_p.1.6+variants_date$prevalence_gaussian5_p.1.7+variants_date$prevalence_gaussian5_p.1.8
  if (!is.na(variants_date$Gamma)){
    factor_variant_gamma = variants_date$Gamma
  } else {factor_variant_gamma = 0}
  
  #===============Delta Variant===========================================
  #variants_date$prevalence_gaussian5_b.1.617.2 = variants_date$prevalence_gaussian5_b.1.617.2+variants_date$prevalence_gaussian5_ay.2+variants_date$prevalence_gaussian5_ay.3+variants_date$prevalence_gaussian5_ay.3.1+variants_date$prevalence_gaussian5_ay.4+variants_date$prevalence_gaussian5_ay.5+variants_date$prevalence_gaussian5_ay.6+variants_date$prevalence_gaussian5_ay.7+variants_date$prevalence_gaussian5_ay.9+variants_date$prevalence_gaussian5_ay.10+variants_date$prevalence_gaussian5_ay.11+variants_date$prevalence_gaussian5_ay.12
  if (!is.na(variants_date$Delta)) {
    factor_variant_delta = variants_date$Delta
  } else {factor_variant_delta = 0}
  
  fac_mask = mask$risk[which(mask$mask_type==mask_type)]
  if (!is.na(1-(factor_variant_alpha+factor_variant_beta+factor_variant_gamma+factor_variant_delta))) {
    factor_variant_normal = 1-(factor_variant_alpha+factor_variant_beta+factor_variant_gamma+factor_variant_delta)
  } else {factor_variant_normal = 0}
  
  
  #fac_variants = vaccine risk factor reduction
  #Can add more variants data here
  
  fac_vaccine_normal = 1-(vaccine_eff$normal[which(vaccine_eff$vaccine == type_vaccine_dose)]) #1- 80-95% efficacy
  fac_vaccine_alpha = 1-(vaccine_eff$alpha[which(vaccine_eff$vaccine == type_vaccine_dose)])
  fac_vaccine_beta = 1-(vaccine_eff$beta[which(vaccine_eff$vaccine == type_vaccine_dose)])
  fac_vaccine_gamma = 1-(vaccine_eff$gamma[which(vaccine_eff$vaccine == type_vaccine_dose)])
  fac_vaccine_delta = 1-(vaccine_eff$delta[which(vaccine_eff$vaccine == type_vaccine_dose)])
  
  fac_vaccine_normal_u = 1-(vaccine_eff$normal_u[which(vaccine_eff$vaccine == type_vaccine_dose)]) #1- 80-95% efficacy
  fac_vaccine_alpha_u = 1-(vaccine_eff$alpha_u[which(vaccine_eff$vaccine == type_vaccine_dose)])
  fac_vaccine_beta_u = 1-(vaccine_eff$beta_u[which(vaccine_eff$vaccine == type_vaccine_dose)])
  fac_vaccine_gamma_u = 1-(vaccine_eff$gamma_u[which(vaccine_eff$vaccine == type_vaccine_dose)])
  fac_vaccine_delta_u = 1-(vaccine_eff$delta_u[which(vaccine_eff$vaccine == type_vaccine_dose)])
  
  #======================================================================================================
  #======================================================================================================
  fac_variants_u = factor_variant_normal*fac_vaccine_normal + factor_variant_alpha*fac_vaccine_alpha + factor_variant_beta*fac_vaccine_beta+ factor_variant_gamma*fac_vaccine_gamma+ factor_variant_delta*fac_vaccine_delta
  #======================================================================================================
  fac_variants = factor_variant_normal*fac_vaccine_normal_u + factor_variant_alpha*fac_vaccine_alpha_u + factor_variant_beta*fac_vaccine_beta_u+ factor_variant_gamma*fac_vaccine_gamma_u+ factor_variant_delta*fac_vaccine_delta_u
  #======================================================================================================
  #======================================================================================================
  
  if(fac_variants!=0){
    risk_factor_outdoor = 0.05*people_passed_outdoor*(fac_aggregate_cases_per_pop*fac_mask*fac_variants)
    risk_factor_indoor = people_passed_indoor*(fac_aggregate_cases_per_pop*fac_mask*fac_variants)
  }else{
    fac_aggregate_cases_per_pop <- fac_aggregate_cases_per_pop*fac_vaccine_normal
    risk_factor_outdoor = 0.05*people_passed_outdoor*(fac_aggregate_cases_per_pop*fac_mask)
    risk_factor_indoor = people_passed_indoor*(fac_aggregate_cases_per_pop*fac_mask)
  }
  #======================================================================================================
  if(fac_variants_u!=0){
    risk_factor_outdoor_u = 0.05*people_passed_outdoor*(fac_aggregate_cases_per_pop_u*fac_mask*fac_variants_u)
    risk_factor_indoor_u = people_passed_indoor*(fac_aggregate_cases_per_pop_u*fac_mask*fac_variants_u)
  }else{
    fac_aggregate_cases_per_pop_u <- fac_aggregate_cases_per_pop_u*fac_vaccine_normal_u
    risk_factor_outdoor_u = 0.05*people_passed_outdoor*(fac_aggregate_cases_per_pop_u*fac_mask)
    risk_factor_indoor_u = people_passed_indoor*(fac_aggregate_cases_per_pop_u*fac_mask)
  }
  #======================================================================================================
  #======================================================================================================
  
  cum_risk_factor <- risk_factor_outdoor+risk_factor_indoor
  
  cum_risk_factor_u <- risk_factor_outdoor_u+risk_factor_indoor_u
  
  if(age_group>=0 & age_group<=17){
    hosp_risk_lower = cum_risk_factor_u*0.008
    death_risk_lower = cum_risk_factor_u*0.000015
    
    hosp_risk_upper = cum_risk_factor*0.008
    death_risk_upper = cum_risk_factor*0.000015
  }
  if(age_group>=18 & age_group<=49){
    hosp_risk_lower = cum_risk_factor_u*0.025
    death_risk_lower = cum_risk_factor_u*0.0007
    hosp_risk_upper = cum_risk_factor*0.025
    death_risk_upper = cum_risk_factor*0.0007
  }
  if(age_group>=50 & age_group<=64){
    hosp_risk_lower = cum_risk_factor_u*0.079
    death_risk_lower = cum_risk_factor_u*0.007
    hosp_risk_upper = cum_risk_factor*0.079
    death_risk_upper = cum_risk_factor*0.007
  }
  if(age_group>=65){
    hosp_risk_lower = cum_risk_factor_u*0.23
    death_risk_lower = cum_risk_factor_u*0.06
    hosp_risk_upper = cum_risk_factor*0.23
    death_risk_upper = cum_risk_factor*0.06
  }
  
  if (chronic_illness == "Yes"){
    hosp_risk_lower = hosp_risk_lower*2.5
    hosp_risk_upper = hosp_risk_upper*2.5
    
    death_risk_lower = death_risk_lower*1.2
    death_risk_upper = death_risk_upper*6.9
  }
  if(chronic_illness == "No"){
    hosp_risk_lower = hosp_risk_lower
    hosp_risk_upper = hosp_risk_upper
    
    death_risk_lower = death_risk_lower
    death_risk_upper = death_risk_upper
  }
  
  if(gender == "Male"){
    death_risk_lower = death_risk_lower*1.5
    death_risk_upper = death_risk_upper*2.3
  }
  
  if(!is.na(variants_date$Alpha)){
    hosp_risk_lower=1.5*hosp_risk_lower
    hosp_risk_upper=1.6*hosp_risk_upper
    death_risk_lower=1.4*death_risk_lower
    death_risk_upper=1.7*death_risk_upper
  }
  
  #if(!is.na(variants_date$prevalence_gaussian5_b.1.617.2)){
  #  hosp_risk_lower=1.5*hosp_risk_lower
  #  hosp_risk_upper=1.6*hosp_risk_upper
  #  death_risk_lower=1.4*death_risk_lower
  #  death_risk_upper=1.7*death_risk_upper
  #}
  
  if(!is.na(variants_date$Gamma)){
    #  hosp_risk_lower=1.5*hosp_risk_lower
    #  hosp_risk_upper=1.6*hosp_risk_upper
    death_risk_lower=1.2*death_risk_lower
    death_risk_upper=1.9*death_risk_upper
  }
  if(!is.na(variants_date$Delta)){
    hosp_risk_lower=1.9*hosp_risk_lower
    hosp_risk_upper=3*hosp_risk_upper
    death_risk_lower=1.5*death_risk_lower
    death_risk_upper=3.3*death_risk_upper
  }
  
  hosp_risk_lower<-ifelse (type_vaccine_dose=="Moderna (Dose 1)" | type_vaccine_dose=="Pfizer (Dose 1)" | type_vaccine_dose=="Astrazeneca (Dose 1)", 0.1*hosp_risk_lower, 0.7*hosp_risk_lower)
  hosp_risk_upper<-ifelse (type_vaccine_dose=="Moderna (Dose 1)" | type_vaccine_dose=="Pfizer (Dose 1)" | type_vaccine_dose=="Astrazeneca (Dose 1)", 0.1*hosp_risk_upper, 0.7*hosp_risk_upper)
  hosp_risk_lower<-ifelse (type_vaccine_dose=="Moderna (Dose 1)" | type_vaccine_dose=="Pfizer (Dose 1)" | type_vaccine_dose=="Astrazeneca (Dose 1)", 0.1*death_risk_lower, 0.7*death_risk_lower)
  hosp_risk_lower<-ifelse (type_vaccine_dose=="Moderna (Dose 1)" | type_vaccine_dose=="Pfizer (Dose 1)" | type_vaccine_dose=="Astrazeneca (Dose 1)", 0.1*death_risk_upper, 0.7*death_risk_upper)
  
  
  risk_factor_indoor = risk_factor_indoor*100
  risk_factor_outdoor = risk_factor_outdoor*100
  cum_risk_factor = cum_risk_factor*100
  cum_risk_factor_u = cum_risk_factor_u*100
  hosp_risk_lower = hosp_risk_lower*100
  hosp_risk_upper = hosp_risk_upper*100
  death_risk_lower = death_risk_lower*100
  death_risk_upper = death_risk_upper*100
  
  if (risk_factor_indoor>99){
    risk_factor_indoor = 99
  }
  if (risk_factor_indoor_u>99){
    risk_factor_indoor_u = 99
  }
  if (risk_factor_outdoor>99){
    risk_factor_outdoor = 99
  }
  if (risk_factor_outdoor_u>99){
    risk_factor_outdoor_u = 99
  }
  if (cum_risk_factor>99){
    cum_risk_factor = 99
  }
  if (cum_risk_factor_u>99){
    cum_risk_factor_u = 99
  }
  if (hosp_risk_lower>99){
    hosp_risk_lower = 99
  }
  if (hosp_risk_upper>99){
    hosp_risk_upper = 99
  }
  if (death_risk_lower>99){
    death_risk_lower = 99
  }
  if (death_risk_upper>99){
    death_risk_upper = 99
  }
  
  risk_factor_outdoor #Upper value of indoor risk value
  risk_factor_indoor #Upper value of outdoor risk value
  
  risk_factor_indoor_u #Lower value of indoor risk value
  risk_factor_outdoor_u #Lower value of outdoor risk value
  
  cum_risk_factor_u #Lower value of risk factor
  cum_risk_factor #Upper value of risk factor
  
  hosp_risk_lower
  hosp_risk_upper
  
  death_risk_lower
  death_risk_upper
  
  if (cum_risk_factor<=0.1) {
    f_risk = "Low Risk"
  } else if(cum_risk_factor>0.1 & cum_risk_factor<=1)
  {f_risk = "Medium Risk"
  } else if(cum_risk_factor>1){
    f_risk = "High Risk"
  } else if(cum_risk_factor==0){
    f_risk = "Error!"
  }
  
  f_risk #Stores value for which level of risk it is 
  
  #f_risk #Stores value for which level of risk it is 
  print(as.Date(date))
  print(cum_risk_factor)
  print(cum_risk_factor_u)
  #normal_date <- as.Date(date)
  var <- c(date, fac_aggregate_cases, fac_aggregate_cases_per_pop, cum_risk_factor, cum_risk_factor_u, hosp_risk_lower, hosp_risk_upper, death_risk_lower, death_risk_upper, f_risk)
  graphical <- rbind(var,graphical)
  
}
write.csv(graphical,'graphical_usa_30_m2.csv')






rm(list=ls())
library(dplyr)
library(curl)
library(tidyverse)

library(aws.s3)
Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIAQKKGYNQC433Z2ZNA",
           "AWS_SECRET_ACCESS_KEY" = "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")

s3_bucket="covid-data-overall"

readFromS3 = function(filename, bucket, sep = ','){
  return(s3read_using(FUN=read.csv, 
                      bucket = bucket, 
                      object=filename,
                      sep = sep, header=T))
}

graphical = data.frame()
country_list <- read.csv('C:/Users/shrey/Downloads/covid/country_name_code.csv')
#country_list <- read.csv(curl("https://raw.githubusercontent.com/Volunteer-Collab/risk_calculator/main/Risk_calculator/risk_calculator_simple/custom_dataset/country_name_code.csv?token=ACZDUYBLH4IUWYMBZBM7IKTBBP62I"))
country_list <- country_list[-c(1,2)]
#==============Define the inputs======================================================================================
country_name ="United States"
people_passed_outdoor = 10
people_passed_indoor = 5
#date = "2021-10-10"
region = "Massachusetts"
mask_type = "No Mask"#"Procedure mask with Clawed hair clip"
type_vaccine_dose = "No Vaccine"
age_group = 65
gender = "Male"
chronic_illness = "No" #Diabetes, Heart Disease, Cancer, Lung disease, High Blood Pressure, Immunocompromised, Asthma, Kidney Disease, Obesity, Sickle Cell Anemia, HIV, Liver Disease

#Suggestive evidence: Qian et al. study of cases in China, Jimenez's Aerosol Transmission Model, lack of surge from BLM protests, anecdotal CO2 data from protests, zero outdoor outbreaks of any kind, many indoor dining outbreaks, despite both indoor and outdoor dining being open in the US
#=====================================================================================================================
country_code = country_list$CountryCode_2[country_list$CountryName==country_name]
country_code_csv = paste(country_code,"csv",sep = ".")
#=================Filtering the US Dataset from Oxford Dataset========================================================
#Just use the covid policy tracker for everything rather than coronasurveys

if (country_code=="US") {
  #jhu_dataset <- read.csv(curl("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv"))
  jhu_dataset <- readFromS3('s3://covid-data-overall/processed/risk-calculator-data/USStates_confirmed_cases_deaths.csv',s3_bucket,sep = ',')
  #jhu_dataset <- jhu_dataset[jhu_dataset$CountryName=="United States",]
  #jhu_dataset <- jhu_dataset[c(1,3,6,40,41)]
  jhu_dataset <- jhu_dataset %>% mutate_if(is.character,as.factor)
  jhu_dataset <- jhu_dataset[jhu_dataset$RegionName!="",]
  jhu_dataset$Date<-as.Date(as.character(jhu_dataset$Date),format="%Y%m%d")
  summary(jhu_dataset)
  jhu_dataset <- jhu_dataset[-c(2,6,7)]
  colnames(jhu_dataset) <- c("date","region","Confirmed","Deaths")
} else {
  jhu_dataset <- read.csv(curl(paste("https://raw.githubusercontent.com/GCGImdea/coronasurveys/master/data/jhu/region/",country_code_csv, sep="")))
  jhu_dataset<-jhu_dataset[order(jhu_dataset[,1] ),]
  
  jhu_dataset <- jhu_dataset[,-c(1,8,9)]
  jhu_dataset <- jhu_dataset[-c(4,5,6)]
  
}  

if(country_code=="CA"){
  #==============Functions to Remove some common errors in JHU=================
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Unknown",]
  #======Functions to scrape out Canada JHU Dataset============================
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Recovered",]
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Diamond Princess",]
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Grand Princess",]
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Repatriated Travellers",]
  
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Montreal, QC",]
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Calgary, Alberta",]
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Edmonton, Alberta",]
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Toronto, ON",]
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="London, ON",]
  
  #jhu_dataset$region[jhu_dataset$region=="Montreal, QC"] <- "Quebec"
  #jhu_dataset$region[jhu_dataset$region=="Calgary, Alberta"] = "Alberta"
  #hu_dataset$region[jhu_dataset$region=="Edmonton, Alberta"] = "Alberta"
  #jhu_dataset$region[jhu_dataset$region=="Toronto, ON"] = "Ontario"
  #jhu_dataset$region[jhu_dataset$region=="London, ON"] = "Ontario"
  #=============================================================================
}


ratio_dataset <- read.csv("C:/Users/shrey/Downloads/covid/Ratios_Survey.csv")
#ratio_dataset <- read.csv(curl("https://raw.githubusercontent.com/Volunteer-Collab/risk_calculator/main/Risk_calculator/risk_calculator_simple/custom_dataset/Ratios_Survey.csv?token=ACZDUYD4JVF3ZIVRPC3ZV5LBBP7RW"))
ratio_dataset <- ratio_dataset[-c(1:6)]
ratio = ratio_dataset$Survey_Reported_Ratio[ratio_dataset$Country==country_name]
jhu_dataset$Confirmed_u <- jhu_dataset$Confirmed*ratio

population_full <- read.csv("C:/Users/shrey/Downloads/covid/UID_ISO_FIPS_LookUp_Table_population_Countries_Regions.csv")
#population_full <- read.csv(curl("https://raw.githubusercontent.com/Volunteer-Collab/risk_calculator/main/Risk_calculator/risk_calculator_simple/custom_dataset/UID_ISO_FIPS_LookUp_Table_population_Countries_Regions.csv?token=ACZDUYH5CI5OQ6DZWPN44P3BBP6VM"))
population_full <- population_full[-c(1,2,3,4,5,6,9,10,11)]

population_full <- population_full[!is.na(population_full$Population),]
population_full <- population_full %>% mutate_if(is.character,as.factor)
population_full$Country_Region <- gsub("US", "United States", population_full$Country_Region)

if(!is.na(region)){
  pop_var <- population_full$Population[population_full$Province_State==region & population_full$Country_Region==country_name]
  pop_var <- pop_var[1]
}else{
  pop_var<-population_full$Population[population_full$Country_Region==country_name]
}

library(zoo)
#======================================================================================================
#======================================================================================================
if(!is.na(region)){
  jhu_dataset<-jhu_dataset %>%
    group_by(region) %>%
    mutate(NewCases = Confirmed - lag(Confirmed, default = first(Confirmed)))
  jhu_dataset <- jhu_dataset %>%
    dplyr::group_by(region) %>%
    dplyr::mutate(fourteen_day_agg = rollapply(NewCases, 14, sum, align="right", fill= NA))
  
}else{
  jhu_dataset<-jhu_dataset %>%    
    mutate(NewCases = Confirmed - lag(Confirmed, default = first(Confirmed)))
  jhu_dataset <- jhu_dataset %>%
    dplyr::mutate(fourteen_day_agg = rollapply(NewCases, 14, sum, align="right", fill= NA))  
}
#===============================================================================================
if(!is.na(region)){
  jhu_dataset<-jhu_dataset %>%
    group_by(region) %>%
    mutate(NewCases_u = Confirmed_u - lag(Confirmed_u, default = first(Confirmed_u)))
  jhu_dataset <- jhu_dataset %>%
    dplyr::group_by(region) %>%
    dplyr::mutate(fourteen_day_agg_u = rollapply(NewCases_u, 14, sum, align="right", fill= NA))
  
}else{
  jhu_dataset<-jhu_dataset %>%    
    mutate(NewCases_u = Confirmed_u - lag(Confirmed, default = first(Confirmed)))
  jhu_dataset <- jhu_dataset %>%
    dplyr::mutate(fourteen_day_agg_u = rollapply(NewCases_u, 14, sum, align="right", fill= NA))  
}
#======================================================================================================
#======================================================================================================
jhu_dataset_agg <- jhu_dataset[!is.na(jhu_dataset$fourteen_day_agg),]
jhu_dataset_agg <- jhu_dataset_agg %>% mutate_if(is.character,as.factor)
#======================================================================================================
jhu_dataset_agg_u <- jhu_dataset[!is.na(jhu_dataset$fourteen_day_agg_u),]
jhu_dataset_agg_u <- jhu_dataset_agg_u %>% mutate_if(is.character,as.factor)
#======================================================================================================
#======================================================================================================
jhu_dataset_date <- jhu_dataset_agg[which(jhu_dataset_agg$region==region),]

for (date in jhu_dataset_date$date){
  if(!is.na(region)){
    fac_aggregate_cases <- jhu_dataset_agg$fourteen_day_agg[which(jhu_dataset_agg$region==region & as.Date(jhu_dataset_agg$date)==as.Date(date))]
  }else{
    fac_aggregate_cases <- jhu_dataset_agg$fourteen_day_agg[as.Date(jhu_dataset_agg$date)==as.Date(date)]
  }
  #======================================================================================================
  if(!is.na(region)){
    fac_aggregate_cases_u <- jhu_dataset_agg$fourteen_day_agg_u[which(jhu_dataset_agg_u$region==region & as.Date(jhu_dataset_agg_u$date)==as.Date(date))]
  }else{
    fac_aggregate_cases_u <- jhu_dataset_agg$fourteen_day_agg_u[as.Date(jhu_dataset_agg_u$date)==as.Date(date)]
  }
  #======================================================================================================
  #======================================================================================================
  fac_aggregate_cases_per_pop <- fac_aggregate_cases/pop_var 
  fac_aggregate_cases_per_pop_u <- fac_aggregate_cases_u/pop_var 
  #======================================================================================================
  #======================================================================================================
  
  mask <- read.csv("C:/Users/shrey/Downloads/covid/mask.csv") #Ref: https://www.epa.gov/sciencematters/epa-researchers-test-effectiveness-face-masks-disinfection-methods-against-covid-19
  #mask <- read.csv(curl("https://raw.githubusercontent.com/Volunteer-Collab/risk_calculator/main/Risk_calculator/risk_calculator_simple/custom_dataset/mask.csv?token=ACZDUYEEPMIQ2GRZNQXAPYLBBP6K6"))
  mask<- mask %>% mutate_if(is.character,as.factor)
  mask$risk <- 1-mask$FFE
  summary(mask)
  vaccine_eff <- read.csv("C:/Users/shrey/Downloads/covid/vaccine_eff.csv", sep = ",") #Ref: https://yourlocalepidemiologist.substack.com/p/vaccine-table-update-lots-on-delta
  #vaccine_eff <- read.csv(curl("https://raw.githubusercontent.com/Volunteer-Collab/risk_calculator/main/Risk_calculator/risk_calculator_simple/custom_dataset/vaccine_eff.csv?token=ACZDUYEVXTJVJEBGNSWYYV3BBP6RI"))
  vaccine_eff$vaccine <- as.factor(vaccine_eff$vaccine)
  
  #================data pulling script======================== 
  #variants_dataset <- read.csv(curl("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv"))
  
  variants_dataset <- readFromS3('s3://covid-data-overall/processed/risk-calculator-data/all_countries_galphabet_variants.csv',s3_bucket,sep = ',' )
  #variants_dataset <- readFromS3('s3://covid-data-overall/processed/risk-calculator-data/OxCGRT_latest.csv', s3_bucket, sep=',')
  summary(variants_dataset)
  variants_dataset$Date <- as.Date(variants_dataset$Date)
  
  variants_dataset <- variants_dataset[variants_dataset$CountryName == country_name,]
  variants_dataset <- variants_dataset[-c(2,4)]
  variants_date = variants_dataset[variants_dataset$Date==as.Date(date)-31,]
  variants_date[is.na(variants_date)] <- 0
  variants_date <- variants_date[-c(2)]
  
  
  #===============Alpha Variant===========================================
  if (!is.na(variants_date$Alpha)) {
    factor_variant_alpha = variants_date$Alpha
  } else {factor_variant_alpha = 0}
  
  #===============Beta Variant============================================
  #variants_date$prevalence_gaussian5_b.1.351 = variants_date$prevalence_gaussian5_b.1.351+variants_date$prevalence_gaussian5_b.1.351.2+variants_date$prevalence_gaussian5_b.1.351.3
  if(!is.na(variants_date$Beta)){
    factor_variant_beta = variants_date$Beta
  } else {factor_variant_beta = 0}
  
  #===============Gamma Variant===========================================
  #variants_date$prevalence_gaussian5_p.1 = variants_date$prevalence_gaussian5_p.1+variants_date$prevalence_gaussian5_p.1.1+variants_date$prevalence_gaussian5_p.1.2+variants_date$prevalence_gaussian5_p.1.3+variants_date$prevalence_gaussian5_p.1.4+variants_date$prevalence_gaussian5_p.1.6+variants_date$prevalence_gaussian5_p.1.7+variants_date$prevalence_gaussian5_p.1.8
  if (!is.na(variants_date$Gamma)){
    factor_variant_gamma = variants_date$Gamma
  } else {factor_variant_gamma = 0}
  
  #===============Delta Variant===========================================
  #variants_date$prevalence_gaussian5_b.1.617.2 = variants_date$prevalence_gaussian5_b.1.617.2+variants_date$prevalence_gaussian5_ay.2+variants_date$prevalence_gaussian5_ay.3+variants_date$prevalence_gaussian5_ay.3.1+variants_date$prevalence_gaussian5_ay.4+variants_date$prevalence_gaussian5_ay.5+variants_date$prevalence_gaussian5_ay.6+variants_date$prevalence_gaussian5_ay.7+variants_date$prevalence_gaussian5_ay.9+variants_date$prevalence_gaussian5_ay.10+variants_date$prevalence_gaussian5_ay.11+variants_date$prevalence_gaussian5_ay.12
  if (!is.na(variants_date$Delta)) {
    factor_variant_delta = variants_date$Delta
  } else {factor_variant_delta = 0}
  
  fac_mask = mask$risk[which(mask$mask_type==mask_type)]
  if (!is.na(1-(factor_variant_alpha+factor_variant_beta+factor_variant_gamma+factor_variant_delta))) {
    factor_variant_normal = 1-(factor_variant_alpha+factor_variant_beta+factor_variant_gamma+factor_variant_delta)
  } else {factor_variant_normal = 0}
  
  
  #fac_variants = vaccine risk factor reduction
  #Can add more variants data here
  
  fac_vaccine_normal = 1-(vaccine_eff$normal[which(vaccine_eff$vaccine == type_vaccine_dose)]) #1- 80-95% efficacy
  fac_vaccine_alpha = 1-(vaccine_eff$alpha[which(vaccine_eff$vaccine == type_vaccine_dose)])
  fac_vaccine_beta = 1-(vaccine_eff$beta[which(vaccine_eff$vaccine == type_vaccine_dose)])
  fac_vaccine_gamma = 1-(vaccine_eff$gamma[which(vaccine_eff$vaccine == type_vaccine_dose)])
  fac_vaccine_delta = 1-(vaccine_eff$delta[which(vaccine_eff$vaccine == type_vaccine_dose)])
  
  fac_vaccine_normal_u = 1-(vaccine_eff$normal_u[which(vaccine_eff$vaccine == type_vaccine_dose)]) #1- 80-95% efficacy
  fac_vaccine_alpha_u = 1-(vaccine_eff$alpha_u[which(vaccine_eff$vaccine == type_vaccine_dose)])
  fac_vaccine_beta_u = 1-(vaccine_eff$beta_u[which(vaccine_eff$vaccine == type_vaccine_dose)])
  fac_vaccine_gamma_u = 1-(vaccine_eff$gamma_u[which(vaccine_eff$vaccine == type_vaccine_dose)])
  fac_vaccine_delta_u = 1-(vaccine_eff$delta_u[which(vaccine_eff$vaccine == type_vaccine_dose)])
  
  #======================================================================================================
  #======================================================================================================
  fac_variants_u = factor_variant_normal*fac_vaccine_normal + factor_variant_alpha*fac_vaccine_alpha + factor_variant_beta*fac_vaccine_beta+ factor_variant_gamma*fac_vaccine_gamma+ factor_variant_delta*fac_vaccine_delta
  #======================================================================================================
  fac_variants = factor_variant_normal*fac_vaccine_normal_u + factor_variant_alpha*fac_vaccine_alpha_u + factor_variant_beta*fac_vaccine_beta_u+ factor_variant_gamma*fac_vaccine_gamma_u+ factor_variant_delta*fac_vaccine_delta_u
  #======================================================================================================
  #======================================================================================================
  
  if(fac_variants!=0){
    risk_factor_outdoor = 0.05*people_passed_outdoor*(fac_aggregate_cases_per_pop*fac_mask*fac_variants)
    risk_factor_indoor = people_passed_indoor*(fac_aggregate_cases_per_pop*fac_mask*fac_variants)
  }else{
    fac_aggregate_cases_per_pop <- fac_aggregate_cases_per_pop*fac_vaccine_normal
    risk_factor_outdoor = 0.05*people_passed_outdoor*(fac_aggregate_cases_per_pop*fac_mask)
    risk_factor_indoor = people_passed_indoor*(fac_aggregate_cases_per_pop*fac_mask)
  }
  #======================================================================================================
  if(fac_variants_u!=0){
    risk_factor_outdoor_u = 0.05*people_passed_outdoor*(fac_aggregate_cases_per_pop_u*fac_mask*fac_variants_u)
    risk_factor_indoor_u = people_passed_indoor*(fac_aggregate_cases_per_pop_u*fac_mask*fac_variants_u)
  }else{
    fac_aggregate_cases_per_pop_u <- fac_aggregate_cases_per_pop_u*fac_vaccine_normal_u
    risk_factor_outdoor_u = 0.05*people_passed_outdoor*(fac_aggregate_cases_per_pop_u*fac_mask)
    risk_factor_indoor_u = people_passed_indoor*(fac_aggregate_cases_per_pop_u*fac_mask)
  }
  #======================================================================================================
  #======================================================================================================
  
  cum_risk_factor <- risk_factor_outdoor+risk_factor_indoor
  
  cum_risk_factor_u <- risk_factor_outdoor_u+risk_factor_indoor_u
  
  if(age_group>=0 & age_group<=17){
    hosp_risk_lower = cum_risk_factor_u*0.008
    death_risk_lower = cum_risk_factor_u*0.000015
    
    hosp_risk_upper = cum_risk_factor*0.008
    death_risk_upper = cum_risk_factor*0.000015
  }
  if(age_group>=18 & age_group<=49){
    hosp_risk_lower = cum_risk_factor_u*0.025
    death_risk_lower = cum_risk_factor_u*0.0007
    hosp_risk_upper = cum_risk_factor*0.025
    death_risk_upper = cum_risk_factor*0.0007
  }
  if(age_group>=50 & age_group<=64){
    hosp_risk_lower = cum_risk_factor_u*0.079
    death_risk_lower = cum_risk_factor_u*0.007
    hosp_risk_upper = cum_risk_factor*0.079
    death_risk_upper = cum_risk_factor*0.007
  }
  if(age_group>=65){
    hosp_risk_lower = cum_risk_factor_u*0.23
    death_risk_lower = cum_risk_factor_u*0.06
    hosp_risk_upper = cum_risk_factor*0.23
    death_risk_upper = cum_risk_factor*0.06
  }
  
  if (chronic_illness == "Yes"){
    hosp_risk_lower = hosp_risk_lower*2.5
    hosp_risk_upper = hosp_risk_upper*2.5
    
    death_risk_lower = death_risk_lower*1.2
    death_risk_upper = death_risk_upper*6.9
  }
  if(chronic_illness == "No"){
    hosp_risk_lower = hosp_risk_lower
    hosp_risk_upper = hosp_risk_upper
    
    death_risk_lower = death_risk_lower
    death_risk_upper = death_risk_upper
  }
  
  if(gender == "Male"){
    death_risk_lower = death_risk_lower*1.5
    death_risk_upper = death_risk_upper*2.3
  }
  
  if(!is.na(variants_date$Alpha)){
    hosp_risk_lower=1.5*hosp_risk_lower
    hosp_risk_upper=1.6*hosp_risk_upper
    death_risk_lower=1.4*death_risk_lower
    death_risk_upper=1.7*death_risk_upper
  }
  
  #if(!is.na(variants_date$prevalence_gaussian5_b.1.617.2)){
  #  hosp_risk_lower=1.5*hosp_risk_lower
  #  hosp_risk_upper=1.6*hosp_risk_upper
  #  death_risk_lower=1.4*death_risk_lower
  #  death_risk_upper=1.7*death_risk_upper
  #}
  
  if(!is.na(variants_date$Gamma)){
    #  hosp_risk_lower=1.5*hosp_risk_lower
    #  hosp_risk_upper=1.6*hosp_risk_upper
    death_risk_lower=1.2*death_risk_lower
    death_risk_upper=1.9*death_risk_upper
  }
  if(!is.na(variants_date$Delta)){
    hosp_risk_lower=1.9*hosp_risk_lower
    hosp_risk_upper=3*hosp_risk_upper
    death_risk_lower=1.5*death_risk_lower
    death_risk_upper=3.3*death_risk_upper
  }
  
  hosp_risk_lower<-ifelse (type_vaccine_dose=="Moderna (Dose 1)" | type_vaccine_dose=="Pfizer (Dose 1)" | type_vaccine_dose=="Astrazeneca (Dose 1)", 0.1*hosp_risk_lower, 0.7*hosp_risk_lower)
  hosp_risk_upper<-ifelse (type_vaccine_dose=="Moderna (Dose 1)" | type_vaccine_dose=="Pfizer (Dose 1)" | type_vaccine_dose=="Astrazeneca (Dose 1)", 0.1*hosp_risk_upper, 0.7*hosp_risk_upper)
  hosp_risk_lower<-ifelse (type_vaccine_dose=="Moderna (Dose 1)" | type_vaccine_dose=="Pfizer (Dose 1)" | type_vaccine_dose=="Astrazeneca (Dose 1)", 0.1*death_risk_lower, 0.7*death_risk_lower)
  hosp_risk_lower<-ifelse (type_vaccine_dose=="Moderna (Dose 1)" | type_vaccine_dose=="Pfizer (Dose 1)" | type_vaccine_dose=="Astrazeneca (Dose 1)", 0.1*death_risk_upper, 0.7*death_risk_upper)
  
  
  risk_factor_indoor = risk_factor_indoor*100
  risk_factor_outdoor = risk_factor_outdoor*100
  cum_risk_factor = cum_risk_factor*100
  cum_risk_factor_u = cum_risk_factor_u*100
  hosp_risk_lower = hosp_risk_lower*100
  hosp_risk_upper = hosp_risk_upper*100
  death_risk_lower = death_risk_lower*100
  death_risk_upper = death_risk_upper*100
  
  if (risk_factor_indoor>99){
    risk_factor_indoor = 99
  }
  if (risk_factor_indoor_u>99){
    risk_factor_indoor_u = 99
  }
  if (risk_factor_outdoor>99){
    risk_factor_outdoor = 99
  }
  if (risk_factor_outdoor_u>99){
    risk_factor_outdoor_u = 99
  }
  if (cum_risk_factor>99){
    cum_risk_factor = 99
  }
  if (cum_risk_factor_u>99){
    cum_risk_factor_u = 99
  }
  if (hosp_risk_lower>99){
    hosp_risk_lower = 99
  }
  if (hosp_risk_upper>99){
    hosp_risk_upper = 99
  }
  if (death_risk_lower>99){
    death_risk_lower = 99
  }
  if (death_risk_upper>99){
    death_risk_upper = 99
  }
  
  risk_factor_outdoor #Upper value of indoor risk value
  risk_factor_indoor #Upper value of outdoor risk value
  
  risk_factor_indoor_u #Lower value of indoor risk value
  risk_factor_outdoor_u #Lower value of outdoor risk value
  
  cum_risk_factor_u #Lower value of risk factor
  cum_risk_factor #Upper value of risk factor
  
  hosp_risk_lower
  hosp_risk_upper
  
  death_risk_lower
  death_risk_upper
  
  if (cum_risk_factor<=0.1) {
    f_risk = "Low Risk"
  } else if(cum_risk_factor>0.1 & cum_risk_factor<=1)
  {f_risk = "Medium Risk"
  } else if(cum_risk_factor>1){
    f_risk = "High Risk"
  } else if(cum_risk_factor==0){
    f_risk = "Error!"
  }
  
  f_risk #Stores value for which level of risk it is 
  
  #f_risk #Stores value for which level of risk it is 
  print(as.Date(date))
  print(cum_risk_factor)
  print(cum_risk_factor_u)
  #normal_date <- as.Date(date)
  var <- c(date, fac_aggregate_cases, fac_aggregate_cases_per_pop, cum_risk_factor, cum_risk_factor_u, hosp_risk_lower, hosp_risk_upper, death_risk_lower, death_risk_upper, f_risk)
  graphical <- rbind(var,graphical)
  
}
write.csv(graphical,'graphical_usa_65_time.csv')




rm(list=ls())
library(dplyr)
library(curl)
library(tidyverse)

library(aws.s3)
Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIAQKKGYNQC433Z2ZNA",
           "AWS_SECRET_ACCESS_KEY" = "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")

s3_bucket="covid-data-overall"

readFromS3 = function(filename, bucket, sep = ','){
  return(s3read_using(FUN=read.csv, 
                      bucket = bucket, 
                      object=filename,
                      sep = sep, header=T))
}

graphical = data.frame()
country_list <- read.csv('C:/Users/shrey/Downloads/covid/country_name_code.csv')
#country_list <- read.csv(curl("https://raw.githubusercontent.com/Volunteer-Collab/risk_calculator/main/Risk_calculator/risk_calculator_simple/custom_dataset/country_name_code.csv?token=ACZDUYBLH4IUWYMBZBM7IKTBBP62I"))
country_list <- country_list[-c(1,2)]
#==============Define the inputs======================================================================================
country_name ="India"
people_passed_outdoor = 10
people_passed_indoor = 5
#date = "2021-10-10"
region = "Delhi"
mask_type = "N95 respirator"
type_vaccine_dose = "No Vaccine"
age_group = 30
gender = "Male"
chronic_illness = "No" #Diabetes, Heart Disease, Cancer, Lung disease, High Blood Pressure, Immunocompromised, Asthma, Kidney Disease, Obesity, Sickle Cell Anemia, HIV, Liver Disease

#Suggestive evidence: Qian et al. study of cases in China, Jimenez's Aerosol Transmission Model, lack of surge from BLM protests, anecdotal CO2 data from protests, zero outdoor outbreaks of any kind, many indoor dining outbreaks, despite both indoor and outdoor dining being open in the US
#=====================================================================================================================
country_code = country_list$CountryCode_2[country_list$CountryName==country_name]
country_code_csv = paste(country_code,"csv",sep = ".")
#=================Filtering the US Dataset from Oxford Dataset========================================================
#Just use the covid policy tracker for everything rather than coronasurveys

if (country_code=="US") {
  #jhu_dataset <- read.csv(curl("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv"))
  jhu_dataset <- readFromS3('s3://covid-data-overall/processed/risk-calculator-data/USStates_confirmed_cases_deaths.csv',s3_bucket,sep = ',')
  #jhu_dataset <- jhu_dataset[jhu_dataset$CountryName=="United States",]
  #jhu_dataset <- jhu_dataset[c(1,3,6,40,41)]
  jhu_dataset <- jhu_dataset %>% mutate_if(is.character,as.factor)
  jhu_dataset <- jhu_dataset[jhu_dataset$RegionName!="",]
  jhu_dataset$Date<-as.Date(as.character(jhu_dataset$Date),format="%Y%m%d")
  summary(jhu_dataset)
  jhu_dataset <- jhu_dataset[-c(2,6,7)]
  colnames(jhu_dataset) <- c("date","region","Confirmed","Deaths")
} else {
  jhu_dataset <- read.csv(curl(paste("https://raw.githubusercontent.com/GCGImdea/coronasurveys/master/data/jhu/region/",country_code_csv, sep="")))
  jhu_dataset<-jhu_dataset[order(jhu_dataset[,1] ),]
  
  jhu_dataset <- jhu_dataset[,-c(1,8,9)]
  jhu_dataset <- jhu_dataset[-c(4,5,6)]
  
}  

if(country_code=="CA"){
  #==============Functions to Remove some common errors in JHU=================
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Unknown",]
  #======Functions to scrape out Canada JHU Dataset============================
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Recovered",]
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Diamond Princess",]
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Grand Princess",]
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Repatriated Travellers",]
  
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Montreal, QC",]
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Calgary, Alberta",]
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Edmonton, Alberta",]
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Toronto, ON",]
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="London, ON",]
  
  #jhu_dataset$region[jhu_dataset$region=="Montreal, QC"] <- "Quebec"
  #jhu_dataset$region[jhu_dataset$region=="Calgary, Alberta"] = "Alberta"
  #hu_dataset$region[jhu_dataset$region=="Edmonton, Alberta"] = "Alberta"
  #jhu_dataset$region[jhu_dataset$region=="Toronto, ON"] = "Ontario"
  #jhu_dataset$region[jhu_dataset$region=="London, ON"] = "Ontario"
  #=============================================================================
}


ratio_dataset <- read.csv("C:/Users/shrey/Downloads/covid/Ratios_Survey.csv")
#ratio_dataset <- read.csv(curl("https://raw.githubusercontent.com/Volunteer-Collab/risk_calculator/main/Risk_calculator/risk_calculator_simple/custom_dataset/Ratios_Survey.csv?token=ACZDUYD4JVF3ZIVRPC3ZV5LBBP7RW"))
ratio_dataset <- ratio_dataset[-c(1:6)]
ratio = ratio_dataset$Survey_Reported_Ratio[ratio_dataset$Country==country_name]
jhu_dataset$Confirmed_u <- jhu_dataset$Confirmed*ratio

population_full <- read.csv("C:/Users/shrey/Downloads/covid/UID_ISO_FIPS_LookUp_Table_population_Countries_Regions.csv")
#population_full <- read.csv(curl("https://raw.githubusercontent.com/Volunteer-Collab/risk_calculator/main/Risk_calculator/risk_calculator_simple/custom_dataset/UID_ISO_FIPS_LookUp_Table_population_Countries_Regions.csv?token=ACZDUYH5CI5OQ6DZWPN44P3BBP6VM"))
population_full <- population_full[-c(1,2,3,4,5,6,9,10,11)]

population_full <- population_full[!is.na(population_full$Population),]
population_full <- population_full %>% mutate_if(is.character,as.factor)
population_full$Country_Region <- gsub("US", "United States", population_full$Country_Region)

if(!is.na(region)){
  pop_var <- population_full$Population[population_full$Province_State==region & population_full$Country_Region==country_name]
  pop_var <- pop_var[1]
}else{
  pop_var<-population_full$Population[population_full$Country_Region==country_name]
}

library(zoo)
#======================================================================================================
#======================================================================================================
if(!is.na(region)){
  jhu_dataset<-jhu_dataset %>%
    group_by(region) %>%
    mutate(NewCases = Confirmed - lag(Confirmed, default = first(Confirmed)))
  jhu_dataset <- jhu_dataset %>%
    dplyr::group_by(region) %>%
    dplyr::mutate(fourteen_day_agg = rollapply(NewCases, 14, sum, align="right", fill= NA))
  
}else{
  jhu_dataset<-jhu_dataset %>%    
    mutate(NewCases = Confirmed - lag(Confirmed, default = first(Confirmed)))
  jhu_dataset <- jhu_dataset %>%
    dplyr::mutate(fourteen_day_agg = rollapply(NewCases, 14, sum, align="right", fill= NA))  
}
#===============================================================================================
if(!is.na(region)){
  jhu_dataset<-jhu_dataset %>%
    group_by(region) %>%
    mutate(NewCases_u = Confirmed_u - lag(Confirmed_u, default = first(Confirmed_u)))
  jhu_dataset <- jhu_dataset %>%
    dplyr::group_by(region) %>%
    dplyr::mutate(fourteen_day_agg_u = rollapply(NewCases_u, 14, sum, align="right", fill= NA))
  
}else{
  jhu_dataset<-jhu_dataset %>%    
    mutate(NewCases_u = Confirmed_u - lag(Confirmed, default = first(Confirmed)))
  jhu_dataset <- jhu_dataset %>%
    dplyr::mutate(fourteen_day_agg_u = rollapply(NewCases_u, 14, sum, align="right", fill= NA))  
}
#======================================================================================================
#======================================================================================================
jhu_dataset_agg <- jhu_dataset[!is.na(jhu_dataset$fourteen_day_agg),]
jhu_dataset_agg <- jhu_dataset_agg %>% mutate_if(is.character,as.factor)
#======================================================================================================
jhu_dataset_agg_u <- jhu_dataset[!is.na(jhu_dataset$fourteen_day_agg_u),]
jhu_dataset_agg_u <- jhu_dataset_agg_u %>% mutate_if(is.character,as.factor)
#======================================================================================================
#======================================================================================================
jhu_dataset_date <- jhu_dataset_agg[which(jhu_dataset_agg$region==region),]

for (date in jhu_dataset_date$date){
  if(!is.na(region)){
    fac_aggregate_cases <- jhu_dataset_agg$fourteen_day_agg[which(jhu_dataset_agg$region==region & as.Date(jhu_dataset_agg$date)==as.Date(date))]
  }else{
    fac_aggregate_cases <- jhu_dataset_agg$fourteen_day_agg[as.Date(jhu_dataset_agg$date)==as.Date(date)]
  }
  #======================================================================================================
  if(!is.na(region)){
    fac_aggregate_cases_u <- jhu_dataset_agg$fourteen_day_agg_u[which(jhu_dataset_agg_u$region==region & as.Date(jhu_dataset_agg_u$date)==as.Date(date))]
  }else{
    fac_aggregate_cases_u <- jhu_dataset_agg$fourteen_day_agg_u[as.Date(jhu_dataset_agg_u$date)==as.Date(date)]
  }
  #======================================================================================================
  #======================================================================================================
  fac_aggregate_cases_per_pop <- fac_aggregate_cases/pop_var 
  fac_aggregate_cases_per_pop_u <- fac_aggregate_cases_u/pop_var 
  #======================================================================================================
  #======================================================================================================
  
  mask <- read.csv("C:/Users/shrey/Downloads/covid/mask.csv") #Ref: https://www.epa.gov/sciencematters/epa-researchers-test-effectiveness-face-masks-disinfection-methods-against-covid-19
  #mask <- read.csv(curl("https://raw.githubusercontent.com/Volunteer-Collab/risk_calculator/main/Risk_calculator/risk_calculator_simple/custom_dataset/mask.csv?token=ACZDUYEEPMIQ2GRZNQXAPYLBBP6K6"))
  mask<- mask %>% mutate_if(is.character,as.factor)
  mask$risk <- 1-mask$FFE
  summary(mask)
  vaccine_eff <- read.csv("C:/Users/shrey/Downloads/covid/vaccine_eff.csv", sep = ",") #Ref: https://yourlocalepidemiologist.substack.com/p/vaccine-table-update-lots-on-delta
  #vaccine_eff <- read.csv(curl("https://raw.githubusercontent.com/Volunteer-Collab/risk_calculator/main/Risk_calculator/risk_calculator_simple/custom_dataset/vaccine_eff.csv?token=ACZDUYEVXTJVJEBGNSWYYV3BBP6RI"))
  vaccine_eff$vaccine <- as.factor(vaccine_eff$vaccine)
  
  #================data pulling script======================== 
  #variants_dataset <- read.csv(curl("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv"))
  
  variants_dataset <- readFromS3('s3://covid-data-overall/processed/risk-calculator-data/all_countries_galphabet_variants.csv',s3_bucket,sep = ',' )
  #variants_dataset <- readFromS3('s3://covid-data-overall/processed/risk-calculator-data/OxCGRT_latest.csv', s3_bucket, sep=',')
  summary(variants_dataset)
  variants_dataset$Date <- as.Date(variants_dataset$Date)
  
  variants_dataset <- variants_dataset[variants_dataset$CountryName == country_name,]
  variants_dataset <- variants_dataset[-c(2,4)]
  variants_date = variants_dataset[variants_dataset$Date==as.Date(date)-31,]
  variants_date[is.na(variants_date)] <- 0
  variants_date <- variants_date[-c(2)]
  
  
  #===============Alpha Variant===========================================
  if (!is.na(variants_date$Alpha)) {
    factor_variant_alpha = variants_date$Alpha
  } else {factor_variant_alpha = 0}
  
  #===============Beta Variant============================================
  #variants_date$prevalence_gaussian5_b.1.351 = variants_date$prevalence_gaussian5_b.1.351+variants_date$prevalence_gaussian5_b.1.351.2+variants_date$prevalence_gaussian5_b.1.351.3
  if(!is.na(variants_date$Beta)){
    factor_variant_beta = variants_date$Beta
  } else {factor_variant_beta = 0}
  
  #===============Gamma Variant===========================================
  #variants_date$prevalence_gaussian5_p.1 = variants_date$prevalence_gaussian5_p.1+variants_date$prevalence_gaussian5_p.1.1+variants_date$prevalence_gaussian5_p.1.2+variants_date$prevalence_gaussian5_p.1.3+variants_date$prevalence_gaussian5_p.1.4+variants_date$prevalence_gaussian5_p.1.6+variants_date$prevalence_gaussian5_p.1.7+variants_date$prevalence_gaussian5_p.1.8
  if (!is.na(variants_date$Gamma)){
    factor_variant_gamma = variants_date$Gamma
  } else {factor_variant_gamma = 0}
  
  #===============Delta Variant===========================================
  #variants_date$prevalence_gaussian5_b.1.617.2 = variants_date$prevalence_gaussian5_b.1.617.2+variants_date$prevalence_gaussian5_ay.2+variants_date$prevalence_gaussian5_ay.3+variants_date$prevalence_gaussian5_ay.3.1+variants_date$prevalence_gaussian5_ay.4+variants_date$prevalence_gaussian5_ay.5+variants_date$prevalence_gaussian5_ay.6+variants_date$prevalence_gaussian5_ay.7+variants_date$prevalence_gaussian5_ay.9+variants_date$prevalence_gaussian5_ay.10+variants_date$prevalence_gaussian5_ay.11+variants_date$prevalence_gaussian5_ay.12
  if (!is.na(variants_date$Delta)) {
    factor_variant_delta = variants_date$Delta
  } else {factor_variant_delta = 0}
  
  fac_mask = mask$risk[which(mask$mask_type==mask_type)]
  if (!is.na(1-(factor_variant_alpha+factor_variant_beta+factor_variant_gamma+factor_variant_delta))) {
    factor_variant_normal = 1-(factor_variant_alpha+factor_variant_beta+factor_variant_gamma+factor_variant_delta)
  } else {factor_variant_normal = 0}
  
  
  #fac_variants = vaccine risk factor reduction
  #Can add more variants data here
  
  fac_vaccine_normal = 1-(vaccine_eff$normal[which(vaccine_eff$vaccine == type_vaccine_dose)]) #1- 80-95% efficacy
  fac_vaccine_alpha = 1-(vaccine_eff$alpha[which(vaccine_eff$vaccine == type_vaccine_dose)])
  fac_vaccine_beta = 1-(vaccine_eff$beta[which(vaccine_eff$vaccine == type_vaccine_dose)])
  fac_vaccine_gamma = 1-(vaccine_eff$gamma[which(vaccine_eff$vaccine == type_vaccine_dose)])
  fac_vaccine_delta = 1-(vaccine_eff$delta[which(vaccine_eff$vaccine == type_vaccine_dose)])
  
  fac_vaccine_normal_u = 1-(vaccine_eff$normal_u[which(vaccine_eff$vaccine == type_vaccine_dose)]) #1- 80-95% efficacy
  fac_vaccine_alpha_u = 1-(vaccine_eff$alpha_u[which(vaccine_eff$vaccine == type_vaccine_dose)])
  fac_vaccine_beta_u = 1-(vaccine_eff$beta_u[which(vaccine_eff$vaccine == type_vaccine_dose)])
  fac_vaccine_gamma_u = 1-(vaccine_eff$gamma_u[which(vaccine_eff$vaccine == type_vaccine_dose)])
  fac_vaccine_delta_u = 1-(vaccine_eff$delta_u[which(vaccine_eff$vaccine == type_vaccine_dose)])
  
  #======================================================================================================
  #======================================================================================================
  fac_variants_u = factor_variant_normal*fac_vaccine_normal + factor_variant_alpha*fac_vaccine_alpha + factor_variant_beta*fac_vaccine_beta+ factor_variant_gamma*fac_vaccine_gamma+ factor_variant_delta*fac_vaccine_delta
  #======================================================================================================
  fac_variants = factor_variant_normal*fac_vaccine_normal_u + factor_variant_alpha*fac_vaccine_alpha_u + factor_variant_beta*fac_vaccine_beta_u+ factor_variant_gamma*fac_vaccine_gamma_u+ factor_variant_delta*fac_vaccine_delta_u
  #======================================================================================================
  #======================================================================================================
  
  if(fac_variants!=0){
    risk_factor_outdoor = 0.05*people_passed_outdoor*(fac_aggregate_cases_per_pop*fac_mask*fac_variants)
    risk_factor_indoor = people_passed_indoor*(fac_aggregate_cases_per_pop*fac_mask*fac_variants)
  }else{
    fac_aggregate_cases_per_pop <- fac_aggregate_cases_per_pop*fac_vaccine_normal
    risk_factor_outdoor = 0.05*people_passed_outdoor*(fac_aggregate_cases_per_pop*fac_mask)
    risk_factor_indoor = people_passed_indoor*(fac_aggregate_cases_per_pop*fac_mask)
  }
  #======================================================================================================
  if(fac_variants_u!=0){
    risk_factor_outdoor_u = 0.05*people_passed_outdoor*(fac_aggregate_cases_per_pop_u*fac_mask*fac_variants_u)
    risk_factor_indoor_u = people_passed_indoor*(fac_aggregate_cases_per_pop_u*fac_mask*fac_variants_u)
  }else{
    fac_aggregate_cases_per_pop_u <- fac_aggregate_cases_per_pop_u*fac_vaccine_normal_u
    risk_factor_outdoor_u = 0.05*people_passed_outdoor*(fac_aggregate_cases_per_pop_u*fac_mask)
    risk_factor_indoor_u = people_passed_indoor*(fac_aggregate_cases_per_pop_u*fac_mask)
  }
  #======================================================================================================
  #======================================================================================================
  
  cum_risk_factor <- risk_factor_outdoor+risk_factor_indoor
  
  cum_risk_factor_u <- risk_factor_outdoor_u+risk_factor_indoor_u
  
  if(age_group>=0 & age_group<=17){
    hosp_risk_lower = cum_risk_factor_u*0.008
    death_risk_lower = cum_risk_factor_u*0.000015
    
    hosp_risk_upper = cum_risk_factor*0.008
    death_risk_upper = cum_risk_factor*0.000015
  }
  if(age_group>=18 & age_group<=49){
    hosp_risk_lower = cum_risk_factor_u*0.025
    death_risk_lower = cum_risk_factor_u*0.0007
    hosp_risk_upper = cum_risk_factor*0.025
    death_risk_upper = cum_risk_factor*0.0007
  }
  if(age_group>=50 & age_group<=64){
    hosp_risk_lower = cum_risk_factor_u*0.079
    death_risk_lower = cum_risk_factor_u*0.007
    hosp_risk_upper = cum_risk_factor*0.079
    death_risk_upper = cum_risk_factor*0.007
  }
  if(age_group>=65){
    hosp_risk_lower = cum_risk_factor_u*0.23
    death_risk_lower = cum_risk_factor_u*0.06
    hosp_risk_upper = cum_risk_factor*0.23
    death_risk_upper = cum_risk_factor*0.06
  }
  
  if (chronic_illness == "Yes"){
    hosp_risk_lower = hosp_risk_lower*2.5
    hosp_risk_upper = hosp_risk_upper*2.5
    
    death_risk_lower = death_risk_lower*1.2
    death_risk_upper = death_risk_upper*6.9
  }
  if(chronic_illness == "No"){
    hosp_risk_lower = hosp_risk_lower
    hosp_risk_upper = hosp_risk_upper
    
    death_risk_lower = death_risk_lower
    death_risk_upper = death_risk_upper
  }
  
  if(gender == "Male"){
    death_risk_lower = death_risk_lower*1.5
    death_risk_upper = death_risk_upper*2.3
  }
  
  if(!is.na(variants_date$Alpha)){
    hosp_risk_lower=1.5*hosp_risk_lower
    hosp_risk_upper=1.6*hosp_risk_upper
    death_risk_lower=1.4*death_risk_lower
    death_risk_upper=1.7*death_risk_upper
  }
  
  #if(!is.na(variants_date$prevalence_gaussian5_b.1.617.2)){
  #  hosp_risk_lower=1.5*hosp_risk_lower
  #  hosp_risk_upper=1.6*hosp_risk_upper
  #  death_risk_lower=1.4*death_risk_lower
  #  death_risk_upper=1.7*death_risk_upper
  #}
  
  if(!is.na(variants_date$Gamma)){
    #  hosp_risk_lower=1.5*hosp_risk_lower
    #  hosp_risk_upper=1.6*hosp_risk_upper
    death_risk_lower=1.2*death_risk_lower
    death_risk_upper=1.9*death_risk_upper
  }
  if(!is.na(variants_date$Delta)){
    hosp_risk_lower=1.9*hosp_risk_lower
    hosp_risk_upper=3*hosp_risk_upper
    death_risk_lower=1.5*death_risk_lower
    death_risk_upper=3.3*death_risk_upper
  }
  
  hosp_risk_lower<-ifelse (type_vaccine_dose=="Moderna (Dose 1)" | type_vaccine_dose=="Pfizer (Dose 1)" | type_vaccine_dose=="Astrazeneca (Dose 1)", 0.1*hosp_risk_lower, 0.7*hosp_risk_lower)
  hosp_risk_upper<-ifelse (type_vaccine_dose=="Moderna (Dose 1)" | type_vaccine_dose=="Pfizer (Dose 1)" | type_vaccine_dose=="Astrazeneca (Dose 1)", 0.1*hosp_risk_upper, 0.7*hosp_risk_upper)
  hosp_risk_lower<-ifelse (type_vaccine_dose=="Moderna (Dose 1)" | type_vaccine_dose=="Pfizer (Dose 1)" | type_vaccine_dose=="Astrazeneca (Dose 1)", 0.1*death_risk_lower, 0.7*death_risk_lower)
  hosp_risk_lower<-ifelse (type_vaccine_dose=="Moderna (Dose 1)" | type_vaccine_dose=="Pfizer (Dose 1)" | type_vaccine_dose=="Astrazeneca (Dose 1)", 0.1*death_risk_upper, 0.7*death_risk_upper)
  
  
  risk_factor_indoor = risk_factor_indoor*100
  risk_factor_outdoor = risk_factor_outdoor*100
  cum_risk_factor = cum_risk_factor*100
  cum_risk_factor_u = cum_risk_factor_u*100
  hosp_risk_lower = hosp_risk_lower*100
  hosp_risk_upper = hosp_risk_upper*100
  death_risk_lower = death_risk_lower*100
  death_risk_upper = death_risk_upper*100
  
  if (risk_factor_indoor>99){
    risk_factor_indoor = 99
  }
  if (risk_factor_indoor_u>99){
    risk_factor_indoor_u = 99
  }
  if (risk_factor_outdoor>99){
    risk_factor_outdoor = 99
  }
  if (risk_factor_outdoor_u>99){
    risk_factor_outdoor_u = 99
  }
  if (cum_risk_factor>99){
    cum_risk_factor = 99
  }
  if (cum_risk_factor_u>99){
    cum_risk_factor_u = 99
  }
  if (hosp_risk_lower>99){
    hosp_risk_lower = 99
  }
  if (hosp_risk_upper>99){
    hosp_risk_upper = 99
  }
  if (death_risk_lower>99){
    death_risk_lower = 99
  }
  if (death_risk_upper>99){
    death_risk_upper = 99
  }
  
  risk_factor_outdoor #Upper value of indoor risk value
  risk_factor_indoor #Upper value of outdoor risk value
  
  risk_factor_indoor_u #Lower value of indoor risk value
  risk_factor_outdoor_u #Lower value of outdoor risk value
  
  cum_risk_factor_u #Lower value of risk factor
  cum_risk_factor #Upper value of risk factor
  
  hosp_risk_lower
  hosp_risk_upper
  
  death_risk_lower
  death_risk_upper
  
  if (cum_risk_factor<=0.1) {
    f_risk = "Low Risk"
  } else if(cum_risk_factor>0.1 & cum_risk_factor<=1)
  {f_risk = "Medium Risk"
  } else if(cum_risk_factor>1){
    f_risk = "High Risk"
  } else if(cum_risk_factor==0){
    f_risk = "Error!"
  }
  
  f_risk #Stores value for which level of risk it is 
  
  #f_risk #Stores value for which level of risk it is 
  print(as.Date(date))
  print(cum_risk_factor)
  print(cum_risk_factor_u)
  #normal_date <- as.Date(date)
  var <- c(date, fac_aggregate_cases, fac_aggregate_cases_per_pop, cum_risk_factor, cum_risk_factor_u, hosp_risk_lower, hosp_risk_upper, death_risk_lower, death_risk_upper, f_risk)
  graphical <- rbind(var,graphical)
  
}
write.csv(graphical,'graphical_delhi_30_n95.csv')




rm(list=ls())
library(dplyr)
library(curl)
library(tidyverse)

library(aws.s3)
Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIAQKKGYNQC433Z2ZNA",
           "AWS_SECRET_ACCESS_KEY" = "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")

s3_bucket="covid-data-overall"

readFromS3 = function(filename, bucket, sep = ','){
  return(s3read_using(FUN=read.csv, 
                      bucket = bucket, 
                      object=filename,
                      sep = sep, header=T))
}

graphical = data.frame()
country_list <- read.csv('C:/Users/shrey/Downloads/covid/country_name_code.csv')
#country_list <- read.csv(curl("https://raw.githubusercontent.com/Volunteer-Collab/risk_calculator/main/Risk_calculator/risk_calculator_simple/custom_dataset/country_name_code.csv?token=ACZDUYBLH4IUWYMBZBM7IKTBBP62I"))
country_list <- country_list[-c(1,2)]
#==============Define the inputs======================================================================================
country_name ="India"
people_passed_outdoor = 10
people_passed_indoor = 5
#date = "2021-10-10"
region = "Delhi"
mask_type = "Procedure mask with Clawed hair clip"
type_vaccine_dose = "No Vaccine"
age_group = 30
gender = "Male"
chronic_illness = "No" #Diabetes, Heart Disease, Cancer, Lung disease, High Blood Pressure, Immunocompromised, Asthma, Kidney Disease, Obesity, Sickle Cell Anemia, HIV, Liver Disease

#Suggestive evidence: Qian et al. study of cases in China, Jimenez's Aerosol Transmission Model, lack of surge from BLM protests, anecdotal CO2 data from protests, zero outdoor outbreaks of any kind, many indoor dining outbreaks, despite both indoor and outdoor dining being open in the US
#=====================================================================================================================
country_code = country_list$CountryCode_2[country_list$CountryName==country_name]
country_code_csv = paste(country_code,"csv",sep = ".")
#=================Filtering the US Dataset from Oxford Dataset========================================================
#Just use the covid policy tracker for everything rather than coronasurveys

if (country_code=="US") {
  #jhu_dataset <- read.csv(curl("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv"))
  jhu_dataset <- readFromS3('s3://covid-data-overall/processed/risk-calculator-data/USStates_confirmed_cases_deaths.csv',s3_bucket,sep = ',')
  #jhu_dataset <- jhu_dataset[jhu_dataset$CountryName=="United States",]
  #jhu_dataset <- jhu_dataset[c(1,3,6,40,41)]
  jhu_dataset <- jhu_dataset %>% mutate_if(is.character,as.factor)
  jhu_dataset <- jhu_dataset[jhu_dataset$RegionName!="",]
  jhu_dataset$Date<-as.Date(as.character(jhu_dataset$Date),format="%Y%m%d")
  summary(jhu_dataset)
  jhu_dataset <- jhu_dataset[-c(2,6,7)]
  colnames(jhu_dataset) <- c("date","region","Confirmed","Deaths")
} else {
  jhu_dataset <- read.csv(curl(paste("https://raw.githubusercontent.com/GCGImdea/coronasurveys/master/data/jhu/region/",country_code_csv, sep="")))
  jhu_dataset<-jhu_dataset[order(jhu_dataset[,1] ),]
  
  jhu_dataset <- jhu_dataset[,-c(1,8,9)]
  jhu_dataset <- jhu_dataset[-c(4,5,6)]
  
}  

if(country_code=="CA"){
  #==============Functions to Remove some common errors in JHU=================
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Unknown",]
  #======Functions to scrape out Canada JHU Dataset============================
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Recovered",]
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Diamond Princess",]
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Grand Princess",]
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Repatriated Travellers",]
  
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Montreal, QC",]
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Calgary, Alberta",]
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Edmonton, Alberta",]
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="Toronto, ON",]
  jhu_dataset <- jhu_dataset[jhu_dataset$region!="London, ON",]
  
  #jhu_dataset$region[jhu_dataset$region=="Montreal, QC"] <- "Quebec"
  #jhu_dataset$region[jhu_dataset$region=="Calgary, Alberta"] = "Alberta"
  #hu_dataset$region[jhu_dataset$region=="Edmonton, Alberta"] = "Alberta"
  #jhu_dataset$region[jhu_dataset$region=="Toronto, ON"] = "Ontario"
  #jhu_dataset$region[jhu_dataset$region=="London, ON"] = "Ontario"
  #=============================================================================
}


ratio_dataset <- read.csv("C:/Users/shrey/Downloads/covid/Ratios_Survey.csv")
#ratio_dataset <- read.csv(curl("https://raw.githubusercontent.com/Volunteer-Collab/risk_calculator/main/Risk_calculator/risk_calculator_simple/custom_dataset/Ratios_Survey.csv?token=ACZDUYD4JVF3ZIVRPC3ZV5LBBP7RW"))
ratio_dataset <- ratio_dataset[-c(1:6)]
ratio = ratio_dataset$Survey_Reported_Ratio[ratio_dataset$Country==country_name]
jhu_dataset$Confirmed_u <- jhu_dataset$Confirmed*ratio

population_full <- read.csv("C:/Users/shrey/Downloads/covid/UID_ISO_FIPS_LookUp_Table_population_Countries_Regions.csv")
#population_full <- read.csv(curl("https://raw.githubusercontent.com/Volunteer-Collab/risk_calculator/main/Risk_calculator/risk_calculator_simple/custom_dataset/UID_ISO_FIPS_LookUp_Table_population_Countries_Regions.csv?token=ACZDUYH5CI5OQ6DZWPN44P3BBP6VM"))
population_full <- population_full[-c(1,2,3,4,5,6,9,10,11)]

population_full <- population_full[!is.na(population_full$Population),]
population_full <- population_full %>% mutate_if(is.character,as.factor)
population_full$Country_Region <- gsub("US", "United States", population_full$Country_Region)

if(!is.na(region)){
  pop_var <- population_full$Population[population_full$Province_State==region & population_full$Country_Region==country_name]
  pop_var <- pop_var[1]
}else{
  pop_var<-population_full$Population[population_full$Country_Region==country_name]
}

library(zoo)
#======================================================================================================
#======================================================================================================
if(!is.na(region)){
  jhu_dataset<-jhu_dataset %>%
    group_by(region) %>%
    mutate(NewCases = Confirmed - lag(Confirmed, default = first(Confirmed)))
  jhu_dataset <- jhu_dataset %>%
    dplyr::group_by(region) %>%
    dplyr::mutate(fourteen_day_agg = rollapply(NewCases, 14, sum, align="right", fill= NA))
  
}else{
  jhu_dataset<-jhu_dataset %>%    
    mutate(NewCases = Confirmed - lag(Confirmed, default = first(Confirmed)))
  jhu_dataset <- jhu_dataset %>%
    dplyr::mutate(fourteen_day_agg = rollapply(NewCases, 14, sum, align="right", fill= NA))  
}
#===============================================================================================
if(!is.na(region)){
  jhu_dataset<-jhu_dataset %>%
    group_by(region) %>%
    mutate(NewCases_u = Confirmed_u - lag(Confirmed_u, default = first(Confirmed_u)))
  jhu_dataset <- jhu_dataset %>%
    dplyr::group_by(region) %>%
    dplyr::mutate(fourteen_day_agg_u = rollapply(NewCases_u, 14, sum, align="right", fill= NA))
  
}else{
  jhu_dataset<-jhu_dataset %>%    
    mutate(NewCases_u = Confirmed_u - lag(Confirmed, default = first(Confirmed)))
  jhu_dataset <- jhu_dataset %>%
    dplyr::mutate(fourteen_day_agg_u = rollapply(NewCases_u, 14, sum, align="right", fill= NA))  
}
#======================================================================================================
#======================================================================================================
jhu_dataset_agg <- jhu_dataset[!is.na(jhu_dataset$fourteen_day_agg),]
jhu_dataset_agg <- jhu_dataset_agg %>% mutate_if(is.character,as.factor)
#======================================================================================================
jhu_dataset_agg_u <- jhu_dataset[!is.na(jhu_dataset$fourteen_day_agg_u),]
jhu_dataset_agg_u <- jhu_dataset_agg_u %>% mutate_if(is.character,as.factor)
#======================================================================================================
#======================================================================================================
jhu_dataset_date <- jhu_dataset_agg[which(jhu_dataset_agg$region==region),]

for (date in jhu_dataset_date$date){
  if(!is.na(region)){
    fac_aggregate_cases <- jhu_dataset_agg$fourteen_day_agg[which(jhu_dataset_agg$region==region & as.Date(jhu_dataset_agg$date)==as.Date(date))]
  }else{
    fac_aggregate_cases <- jhu_dataset_agg$fourteen_day_agg[as.Date(jhu_dataset_agg$date)==as.Date(date)]
  }
  #======================================================================================================
  if(!is.na(region)){
    fac_aggregate_cases_u <- jhu_dataset_agg$fourteen_day_agg_u[which(jhu_dataset_agg_u$region==region & as.Date(jhu_dataset_agg_u$date)==as.Date(date))]
  }else{
    fac_aggregate_cases_u <- jhu_dataset_agg$fourteen_day_agg_u[as.Date(jhu_dataset_agg_u$date)==as.Date(date)]
  }
  #======================================================================================================
  #======================================================================================================
  fac_aggregate_cases_per_pop <- fac_aggregate_cases/pop_var 
  fac_aggregate_cases_per_pop_u <- fac_aggregate_cases_u/pop_var 
  #======================================================================================================
  #======================================================================================================
  
  mask <- read.csv("C:/Users/shrey/Downloads/covid/mask.csv") #Ref: https://www.epa.gov/sciencematters/epa-researchers-test-effectiveness-face-masks-disinfection-methods-against-covid-19
  #mask <- read.csv(curl("https://raw.githubusercontent.com/Volunteer-Collab/risk_calculator/main/Risk_calculator/risk_calculator_simple/custom_dataset/mask.csv?token=ACZDUYEEPMIQ2GRZNQXAPYLBBP6K6"))
  mask<- mask %>% mutate_if(is.character,as.factor)
  mask$risk <- 1-mask$FFE
  summary(mask)
  vaccine_eff <- read.csv("C:/Users/shrey/Downloads/covid/vaccine_eff.csv", sep = ",") #Ref: https://yourlocalepidemiologist.substack.com/p/vaccine-table-update-lots-on-delta
  #vaccine_eff <- read.csv(curl("https://raw.githubusercontent.com/Volunteer-Collab/risk_calculator/main/Risk_calculator/risk_calculator_simple/custom_dataset/vaccine_eff.csv?token=ACZDUYEVXTJVJEBGNSWYYV3BBP6RI"))
  vaccine_eff$vaccine <- as.factor(vaccine_eff$vaccine)
  
  #================data pulling script======================== 
  #variants_dataset <- read.csv(curl("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv"))
  
  variants_dataset <- readFromS3('s3://covid-data-overall/processed/risk-calculator-data/all_countries_galphabet_variants.csv',s3_bucket,sep = ',' )
  #variants_dataset <- readFromS3('s3://covid-data-overall/processed/risk-calculator-data/OxCGRT_latest.csv', s3_bucket, sep=',')
  summary(variants_dataset)
  variants_dataset$Date <- as.Date(variants_dataset$Date)
  
  variants_dataset <- variants_dataset[variants_dataset$CountryName == country_name,]
  variants_dataset <- variants_dataset[-c(2,4)]
  variants_date = variants_dataset[variants_dataset$Date==as.Date(date)-31,]
  variants_date[is.na(variants_date)] <- 0
  variants_date <- variants_date[-c(2)]
  
  
  #===============Alpha Variant===========================================
  if (!is.na(variants_date$Alpha)) {
    factor_variant_alpha = variants_date$Alpha
  } else {factor_variant_alpha = 0}
  
  #===============Beta Variant============================================
  #variants_date$prevalence_gaussian5_b.1.351 = variants_date$prevalence_gaussian5_b.1.351+variants_date$prevalence_gaussian5_b.1.351.2+variants_date$prevalence_gaussian5_b.1.351.3
  if(!is.na(variants_date$Beta)){
    factor_variant_beta = variants_date$Beta
  } else {factor_variant_beta = 0}
  
  #===============Gamma Variant===========================================
  #variants_date$prevalence_gaussian5_p.1 = variants_date$prevalence_gaussian5_p.1+variants_date$prevalence_gaussian5_p.1.1+variants_date$prevalence_gaussian5_p.1.2+variants_date$prevalence_gaussian5_p.1.3+variants_date$prevalence_gaussian5_p.1.4+variants_date$prevalence_gaussian5_p.1.6+variants_date$prevalence_gaussian5_p.1.7+variants_date$prevalence_gaussian5_p.1.8
  if (!is.na(variants_date$Gamma)){
    factor_variant_gamma = variants_date$Gamma
  } else {factor_variant_gamma = 0}
  
  #===============Delta Variant===========================================
  #variants_date$prevalence_gaussian5_b.1.617.2 = variants_date$prevalence_gaussian5_b.1.617.2+variants_date$prevalence_gaussian5_ay.2+variants_date$prevalence_gaussian5_ay.3+variants_date$prevalence_gaussian5_ay.3.1+variants_date$prevalence_gaussian5_ay.4+variants_date$prevalence_gaussian5_ay.5+variants_date$prevalence_gaussian5_ay.6+variants_date$prevalence_gaussian5_ay.7+variants_date$prevalence_gaussian5_ay.9+variants_date$prevalence_gaussian5_ay.10+variants_date$prevalence_gaussian5_ay.11+variants_date$prevalence_gaussian5_ay.12
  if (!is.na(variants_date$Delta)) {
    factor_variant_delta = variants_date$Delta
  } else {factor_variant_delta = 0}
  
  fac_mask = mask$risk[which(mask$mask_type==mask_type)]
  if (!is.na(1-(factor_variant_alpha+factor_variant_beta+factor_variant_gamma+factor_variant_delta))) {
    factor_variant_normal = 1-(factor_variant_alpha+factor_variant_beta+factor_variant_gamma+factor_variant_delta)
  } else {factor_variant_normal = 0}
  
  
  #fac_variants = vaccine risk factor reduction
  #Can add more variants data here
  
  fac_vaccine_normal = 1-(vaccine_eff$normal[which(vaccine_eff$vaccine == type_vaccine_dose)]) #1- 80-95% efficacy
  fac_vaccine_alpha = 1-(vaccine_eff$alpha[which(vaccine_eff$vaccine == type_vaccine_dose)])
  fac_vaccine_beta = 1-(vaccine_eff$beta[which(vaccine_eff$vaccine == type_vaccine_dose)])
  fac_vaccine_gamma = 1-(vaccine_eff$gamma[which(vaccine_eff$vaccine == type_vaccine_dose)])
  fac_vaccine_delta = 1-(vaccine_eff$delta[which(vaccine_eff$vaccine == type_vaccine_dose)])
  
  fac_vaccine_normal_u = 1-(vaccine_eff$normal_u[which(vaccine_eff$vaccine == type_vaccine_dose)]) #1- 80-95% efficacy
  fac_vaccine_alpha_u = 1-(vaccine_eff$alpha_u[which(vaccine_eff$vaccine == type_vaccine_dose)])
  fac_vaccine_beta_u = 1-(vaccine_eff$beta_u[which(vaccine_eff$vaccine == type_vaccine_dose)])
  fac_vaccine_gamma_u = 1-(vaccine_eff$gamma_u[which(vaccine_eff$vaccine == type_vaccine_dose)])
  fac_vaccine_delta_u = 1-(vaccine_eff$delta_u[which(vaccine_eff$vaccine == type_vaccine_dose)])
  
  #======================================================================================================
  #======================================================================================================
  fac_variants_u = factor_variant_normal*fac_vaccine_normal + factor_variant_alpha*fac_vaccine_alpha + factor_variant_beta*fac_vaccine_beta+ factor_variant_gamma*fac_vaccine_gamma+ factor_variant_delta*fac_vaccine_delta
  #======================================================================================================
  fac_variants = factor_variant_normal*fac_vaccine_normal_u + factor_variant_alpha*fac_vaccine_alpha_u + factor_variant_beta*fac_vaccine_beta_u+ factor_variant_gamma*fac_vaccine_gamma_u+ factor_variant_delta*fac_vaccine_delta_u
  #======================================================================================================
  #======================================================================================================
  
  if(fac_variants!=0){
    risk_factor_outdoor = 0.05*people_passed_outdoor*(fac_aggregate_cases_per_pop*fac_mask*fac_variants)
    risk_factor_indoor = people_passed_indoor*(fac_aggregate_cases_per_pop*fac_mask*fac_variants)
  }else{
    fac_aggregate_cases_per_pop <- fac_aggregate_cases_per_pop*fac_vaccine_normal
    risk_factor_outdoor = 0.05*people_passed_outdoor*(fac_aggregate_cases_per_pop*fac_mask)
    risk_factor_indoor = people_passed_indoor*(fac_aggregate_cases_per_pop*fac_mask)
  }
  #======================================================================================================
  if(fac_variants_u!=0){
    risk_factor_outdoor_u = 0.05*people_passed_outdoor*(fac_aggregate_cases_per_pop_u*fac_mask*fac_variants_u)
    risk_factor_indoor_u = people_passed_indoor*(fac_aggregate_cases_per_pop_u*fac_mask*fac_variants_u)
  }else{
    fac_aggregate_cases_per_pop_u <- fac_aggregate_cases_per_pop_u*fac_vaccine_normal_u
    risk_factor_outdoor_u = 0.05*people_passed_outdoor*(fac_aggregate_cases_per_pop_u*fac_mask)
    risk_factor_indoor_u = people_passed_indoor*(fac_aggregate_cases_per_pop_u*fac_mask)
  }
  #======================================================================================================
  #======================================================================================================
  
  cum_risk_factor <- risk_factor_outdoor+risk_factor_indoor
  
  cum_risk_factor_u <- risk_factor_outdoor_u+risk_factor_indoor_u
  
  if(age_group>=0 & age_group<=17){
    hosp_risk_lower = cum_risk_factor_u*0.008
    death_risk_lower = cum_risk_factor_u*0.000015
    
    hosp_risk_upper = cum_risk_factor*0.008
    death_risk_upper = cum_risk_factor*0.000015
  }
  if(age_group>=18 & age_group<=49){
    hosp_risk_lower = cum_risk_factor_u*0.025
    death_risk_lower = cum_risk_factor_u*0.0007
    hosp_risk_upper = cum_risk_factor*0.025
    death_risk_upper = cum_risk_factor*0.0007
  }
  if(age_group>=50 & age_group<=64){
    hosp_risk_lower = cum_risk_factor_u*0.079
    death_risk_lower = cum_risk_factor_u*0.007
    hosp_risk_upper = cum_risk_factor*0.079
    death_risk_upper = cum_risk_factor*0.007
  }
  if(age_group>=65){
    hosp_risk_lower = cum_risk_factor_u*0.23
    death_risk_lower = cum_risk_factor_u*0.06
    hosp_risk_upper = cum_risk_factor*0.23
    death_risk_upper = cum_risk_factor*0.06
  }
  
  if (chronic_illness == "Yes"){
    hosp_risk_lower = hosp_risk_lower*2.5
    hosp_risk_upper = hosp_risk_upper*2.5
    
    death_risk_lower = death_risk_lower*1.2
    death_risk_upper = death_risk_upper*6.9
  }
  if(chronic_illness == "No"){
    hosp_risk_lower = hosp_risk_lower
    hosp_risk_upper = hosp_risk_upper
    
    death_risk_lower = death_risk_lower
    death_risk_upper = death_risk_upper
  }
  
  if(gender == "Male"){
    death_risk_lower = death_risk_lower*1.5
    death_risk_upper = death_risk_upper*2.3
  }
  
  if(!is.na(variants_date$Alpha)){
    hosp_risk_lower=1.5*hosp_risk_lower
    hosp_risk_upper=1.6*hosp_risk_upper
    death_risk_lower=1.4*death_risk_lower
    death_risk_upper=1.7*death_risk_upper
  }
  
  #if(!is.na(variants_date$prevalence_gaussian5_b.1.617.2)){
  #  hosp_risk_lower=1.5*hosp_risk_lower
  #  hosp_risk_upper=1.6*hosp_risk_upper
  #  death_risk_lower=1.4*death_risk_lower
  #  death_risk_upper=1.7*death_risk_upper
  #}
  
  if(!is.na(variants_date$Gamma)){
    #  hosp_risk_lower=1.5*hosp_risk_lower
    #  hosp_risk_upper=1.6*hosp_risk_upper
    death_risk_lower=1.2*death_risk_lower
    death_risk_upper=1.9*death_risk_upper
  }
  if(!is.na(variants_date$Delta)){
    hosp_risk_lower=1.9*hosp_risk_lower
    hosp_risk_upper=3*hosp_risk_upper
    death_risk_lower=1.5*death_risk_lower
    death_risk_upper=3.3*death_risk_upper
  }
  
  hosp_risk_lower<-ifelse (type_vaccine_dose=="Moderna (Dose 1)" | type_vaccine_dose=="Pfizer (Dose 1)" | type_vaccine_dose=="Astrazeneca (Dose 1)", 0.1*hosp_risk_lower, 0.7*hosp_risk_lower)
  hosp_risk_upper<-ifelse (type_vaccine_dose=="Moderna (Dose 1)" | type_vaccine_dose=="Pfizer (Dose 1)" | type_vaccine_dose=="Astrazeneca (Dose 1)", 0.1*hosp_risk_upper, 0.7*hosp_risk_upper)
  hosp_risk_lower<-ifelse (type_vaccine_dose=="Moderna (Dose 1)" | type_vaccine_dose=="Pfizer (Dose 1)" | type_vaccine_dose=="Astrazeneca (Dose 1)", 0.1*death_risk_lower, 0.7*death_risk_lower)
  hosp_risk_lower<-ifelse (type_vaccine_dose=="Moderna (Dose 1)" | type_vaccine_dose=="Pfizer (Dose 1)" | type_vaccine_dose=="Astrazeneca (Dose 1)", 0.1*death_risk_upper, 0.7*death_risk_upper)
  
  
  risk_factor_indoor = risk_factor_indoor*100
  risk_factor_outdoor = risk_factor_outdoor*100
  cum_risk_factor = cum_risk_factor*100
  cum_risk_factor_u = cum_risk_factor_u*100
  hosp_risk_lower = hosp_risk_lower*100
  hosp_risk_upper = hosp_risk_upper*100
  death_risk_lower = death_risk_lower*100
  death_risk_upper = death_risk_upper*100
  
  if (risk_factor_indoor>99){
    risk_factor_indoor = 99
  }
  if (risk_factor_indoor_u>99){
    risk_factor_indoor_u = 99
  }
  if (risk_factor_outdoor>99){
    risk_factor_outdoor = 99
  }
  if (risk_factor_outdoor_u>99){
    risk_factor_outdoor_u = 99
  }
  if (cum_risk_factor>99){
    cum_risk_factor = 99
  }
  if (cum_risk_factor_u>99){
    cum_risk_factor_u = 99
  }
  if (hosp_risk_lower>99){
    hosp_risk_lower = 99
  }
  if (hosp_risk_upper>99){
    hosp_risk_upper = 99
  }
  if (death_risk_lower>99){
    death_risk_lower = 99
  }
  if (death_risk_upper>99){
    death_risk_upper = 99
  }
  
  risk_factor_outdoor #Upper value of indoor risk value
  risk_factor_indoor #Upper value of outdoor risk value
  
  risk_factor_indoor_u #Lower value of indoor risk value
  risk_factor_outdoor_u #Lower value of outdoor risk value
  
  cum_risk_factor_u #Lower value of risk factor
  cum_risk_factor #Upper value of risk factor
  
  hosp_risk_lower
  hosp_risk_upper
  
  death_risk_lower
  death_risk_upper
  
  if (cum_risk_factor<=0.1) {
    f_risk = "Low Risk"
  } else if(cum_risk_factor>0.1 & cum_risk_factor<=1)
  {f_risk = "Medium Risk"
  } else if(cum_risk_factor>1){
    f_risk = "High Risk"
  } else if(cum_risk_factor==0){
    f_risk = "Error!"
  }
  
  f_risk #Stores value for which level of risk it is 
  
  #f_risk #Stores value for which level of risk it is 
  print(as.Date(date))
  print(cum_risk_factor)
  print(cum_risk_factor_u)
  #normal_date <- as.Date(date)
  var <- c(date, fac_aggregate_cases, fac_aggregate_cases_per_pop, cum_risk_factor, cum_risk_factor_u, hosp_risk_lower, hosp_risk_upper, death_risk_lower, death_risk_upper, f_risk)
  graphical <- rbind(var,graphical)
  
}
write.csv(graphical,'graphical_delhi_30_procedure.csv')

