rm(list=ls())
library(dplyr)
library(curl)
library(tidyverse)
library(ggplot2)

df_massachusetts_usa <- read.csv('/home/natraj/franklin_usa_30_no_safety_fixed.csv', skip = 1, header = F)
df_massachusetts_usa_m1 <- read.csv('/home/natraj/franklin_usa_30_p1_no_mask_fixed.csv', skip = 1, header = F)
df_massachusetts_usa_m2 <- read.csv('/home/natraj/franklin_usa_30_p2_no_mask_fixed.csv', skip = 1, header = F)
df_massachusetts_usa_m3 <- read.csv('/home/natraj/franklin_usa_30_p3_no_mask_fixed.csv', skip = 1, header = F)
df_massachusetts_usa_procedure <- read.csv('/home/natraj/franklin_usa_30_no_vaccine_surgical_mask_fixed.csv', skip = 1, header = F)
df_massachusetts_usa_n95 <- read.csv('/home/natraj/franklin_usa_30_no_vaccine_n95_fixed.csv', skip = 1, header = F)

df_massachusetts_usa_60 <- read.csv('/home/natraj/franklin_usa_60_no_safety_fixed.csv', skip = 1, header = F)

df_delhi_india <- read.csv('/home/natraj/delhi_india_30_no_safety.csv', skip = 1, header = F)
df_delhi_india_az1 <- read.csv('/home/natraj/delhi_india_30_az1_no_mask.csv', skip = 1, header = F)
df_delhi_india_az2 <- read.csv('/home/natraj/delhi_india_30_az2_no_mask.csv', skip = 1, header = F)
df_delhi_india_az3 <- read.csv('/home/natraj/delhi_india_30_az3_no_mask.csv', skip = 1, header = F)
df_delhi_india_procedure <- read.csv('/home/natraj/delhi_india_30_no_vaccine_surgical_mask.csv', skip = 1, header = F)
df_delhi_india_n95 <- read.csv('/home/natraj/delhi_india_30_no_vaccine_n95.csv', skip = 1, header = F)

df_delhi_india_60 <- read.csv('/home/natraj/delhi_india_60_no_safety.csv', skip = 1, header = F)

df_delhi_india <- df_delhi_india[-c(1)]
df_delhi_india_az1 <- df_delhi_india_az1[-c(1)]
df_delhi_india_az2 <- df_delhi_india_az2[-c(1)]
df_delhi_india_procedure <- df_delhi_india_procedure[-c(1)]
df_delhi_india_n95 <- df_delhi_india_n95[-c(1)]

df_delhi_india_60 <- df_delhi_india_60[-c(1)]

df_massachusetts_usa <- df_massachusetts_usa[-c(1)]
df_massachusetts_usa_m1 <- df_massachusetts_usa_m1[-c(1)]
df_massachusetts_usa_m2 <- df_massachusetts_usa_m2[-c(1)]
df_massachusetts_usa_procedure <- df_massachusetts_usa_procedure[-c(1)]
df_massachusetts_usa_n95 <- df_massachusetts_usa_n95[-c(1)]

df_massachusetts_usa_60 <- df_massachusetts_usa_60[-c(1)]

colnames(df_massachusetts_usa) <- c('date','fac_aggregate_cases','fac_aggregate_cases_per_pop','cum_risk_factor','cum_risk_factor_u','hosp_risk_lower','hosp_risk_upper','death_risk_lower','death_risk_upper','f_risk')
colnames(df_massachusetts_usa_m1) <- c('date','fac_aggregate_cases','fac_aggregate_cases_per_pop','cum_risk_factor','cum_risk_factor_u','hosp_risk_lower','hosp_risk_upper','death_risk_lower','death_risk_upper','f_risk')
colnames(df_massachusetts_usa_m2) <- c('date','fac_aggregate_cases','fac_aggregate_cases_per_pop','cum_risk_factor','cum_risk_factor_u','hosp_risk_lower','hosp_risk_upper','death_risk_lower','death_risk_upper','f_risk')
colnames(df_massachusetts_usa_m3) <- c('date','fac_aggregate_cases','fac_aggregate_cases_per_pop','cum_risk_factor','cum_risk_factor_u','hosp_risk_lower','hosp_risk_upper','death_risk_lower','death_risk_upper','f_risk')
colnames(df_massachusetts_usa_procedure) <- c('date','fac_aggregate_cases','fac_aggregate_cases_per_pop','cum_risk_factor','cum_risk_factor_u','hosp_risk_lower','hosp_risk_upper','death_risk_lower','death_risk_upper','f_risk')
colnames(df_massachusetts_usa_n95) <- c('date','fac_aggregate_cases','fac_aggregate_cases_per_pop','cum_risk_factor','cum_risk_factor_u','hosp_risk_lower','hosp_risk_upper','death_risk_lower','death_risk_upper','f_risk')

colnames(df_massachusetts_usa_60) <- c('date','fac_aggregate_cases','fac_aggregate_cases_per_pop','cum_risk_factor','cum_risk_factor_u','hosp_risk_lower','hosp_risk_upper','death_risk_lower','death_risk_upper','f_risk')

colnames(df_delhi_india) <- c('date','fac_aggregate_cases','fac_aggregate_cases_per_pop','cum_risk_factor','cum_risk_factor_u','hosp_risk_lower','hosp_risk_upper','death_risk_lower','death_risk_upper','f_risk')
colnames(df_delhi_india_az1) <- c('date','fac_aggregate_cases','fac_aggregate_cases_per_pop','cum_risk_factor','cum_risk_factor_u','hosp_risk_lower','hosp_risk_upper','death_risk_lower','death_risk_upper','f_risk')
colnames(df_delhi_india_az2) <- c('date','fac_aggregate_cases','fac_aggregate_cases_per_pop','cum_risk_factor','cum_risk_factor_u','hosp_risk_lower','hosp_risk_upper','death_risk_lower','death_risk_upper','f_risk')
colnames(df_delhi_india_az3) <- c('date','fac_aggregate_cases','fac_aggregate_cases_per_pop','cum_risk_factor','cum_risk_factor_u','hosp_risk_lower','hosp_risk_upper','death_risk_lower','death_risk_upper','f_risk')
colnames(df_delhi_india_procedure) <- c('date','fac_aggregate_cases','fac_aggregate_cases_per_pop','cum_risk_factor','cum_risk_factor_u','hosp_risk_lower','hosp_risk_upper','death_risk_lower','death_risk_upper','f_risk')
colnames(df_delhi_india_n95) <- c('date','fac_aggregate_cases','fac_aggregate_cases_per_pop','cum_risk_factor','cum_risk_factor_u','hosp_risk_lower','hosp_risk_upper','death_risk_lower','death_risk_upper','f_risk')

colnames(df_delhi_india_60) <- c('date','fac_aggregate_cases','fac_aggregate_cases_per_pop','cum_risk_factor','cum_risk_factor_u','hosp_risk_lower','hosp_risk_upper','death_risk_lower','death_risk_upper','f_risk')

df_massachusetts_usa$date = as.Date(df_massachusetts_usa$date,origin = "1970-01-01")
df_massachusetts_usa_m1$date = as.Date(df_massachusetts_usa_m1$date,origin = "1970-01-01")
df_massachusetts_usa_m2$date = as.Date(df_massachusetts_usa_m2$date,origin = "1970-01-01")
df_massachusetts_usa_m3$date = as.Date(df_massachusetts_usa_m2$date,origin = "1970-01-01")
df_massachusetts_usa_procedure$date = as.Date(df_massachusetts_usa_procedure$date,origin = "1970-01-01")
df_massachusetts_usa_n95$date = as.Date(df_massachusetts_usa_n95$date,origin = "1970-01-01")

df_massachusetts_usa_60$date = as.Date(df_massachusetts_usa_60$date,origin = "1970-01-01")

df_delhi_india$date = as.Date(df_delhi_india$date, origin = "1970-01-01")
df_delhi_india_az1$date = as.Date(df_delhi_india_az1$date,origin = "1970-01-01")
df_delhi_india_az2$date = as.Date(df_delhi_india_az2$date,origin = "1970-01-01")
df_delhi_india_az3$date = as.Date(df_delhi_india_az2$date,origin = "1970-01-01")
df_delhi_india_procedure$date = as.Date(df_delhi_india_procedure$date,origin = "1970-01-01")
df_delhi_india_n95$date = as.Date(df_delhi_india_n95$date,origin = "1970-01-01")

df_delhi_india_60$date = as.Date(df_delhi_india_60$date,origin = "1970-01-01")

df_massachusetts_usa$fac_aggregate_cases <- df_massachusetts_usa$fac_aggregate_cases/100
df_massachusetts_usa_60$fac_aggregate_cases <- df_massachusetts_usa_60$fac_aggregate_cases/100

df_delhi_india$fac_aggregate_cases <- df_delhi_india$fac_aggregate_cases/10000
df_delhi_india_60$fac_aggregate_cases <- df_delhi_india_60$fac_aggregate_cases/10000

#===================================================================================================================================================
ct_r <- c('Reported active cases (scaled)'='black','Risk of Death (%)'='brown','Risk of Infection (%)'='red','Risk of Hospitalization (%)'='blue')
#===================================================================================================================================================
ggplot(df_delhi_india_60) + 
  geom_line(aes(y = fac_aggregate_cases, x=date, fill = "Reported active cases (scaled)")) + 
  geom_ribbon(aes(ymin = cum_risk_factor, ymax = cum_risk_factor_u, x=date, fill = 'Risk of Hospitalization (%)'), alpha=0.3) + 
  geom_ribbon(aes(ymin = hosp_risk_lower, ymax = hosp_risk_upper, x=date, fill = "Risk of Infection (%)"), alpha = 0.3)+
  geom_ribbon(aes(ymin = death_risk_lower, ymax = death_risk_upper, x=date, fill = 'Risk of Death (%)'), alpha = 0.3)+
  labs(y="Risk Of Infection (%)", x = "Date", colour = "")+
  theme(legend.position = c(0.66, 0.8), legend.background = element_rect(fill = "transparent"), axis.text=element_text(size=24),axis.title=element_text(size=24))+
  scale_fill_manual(values=ct_r, name="", labels = c('Reported active cases (scaled)','Risk of Death (%)','Risk of Hospitalization (%)','Risk of Infection (%)'))+
  theme(legend.key.size = unit(1.5, 'cm'),legend.title = element_text(size=24), legend.text = element_text(size=24))

ggplot(df_massachusetts_usa_60) + 
  geom_line(aes(y = fac_aggregate_cases, x=date, fill = "Reported active cases (scaled)")) + 
  geom_ribbon(aes(ymin = cum_risk_factor, ymax = cum_risk_factor_u, x=date, fill = 'Risk of Infection (%)'), alpha=0.3) + 
  geom_ribbon(aes(ymin = hosp_risk_lower, ymax = hosp_risk_upper, x=date, fill = "Risk of Hospitalization (%)"), alpha = 0.3)+
  geom_ribbon(aes(ymin = death_risk_lower, ymax = death_risk_upper, x=date, fill = 'Risk of Death (%)'), alpha = 0.3)+
  labs(y="Risk Of Infection (%)", x = "Date", colour = "")+
  theme(legend.position = c(0.23, 0.8), legend.background = element_rect(fill = "transparent"), axis.text=element_text(size=24),axis.title=element_text(size=24))+
  #scale_color_manual(values = ct_r)+
  scale_fill_manual(values=ct_r, name="", labels = c('Reported active cases (scaled)','Risk of Death (%)','Risk of Infection (%)','Risk of Hospitalization (%)'))+
  theme(legend.key.size = unit(1.5, 'cm'),legend.title = element_text(size=24), legend.text = element_text(size=24))
#===================================================================================================================================================
#===================================================================================================================================================
ct_rv <- c('Reported active cases (scaled)'='black','Risk (No Vaccine) (%)' = 'red', 'Risk (AZ Dose 1) (%)' = 'orange', 'Risk (AZ Dose 2) (%)' = 'yellow', 'Risk (AZ Dose 2 + AZ Booster) (%)' = 'green')
ct_rv_1 <- c('Reported active cases (scaled)'='black','Risk (No Vaccine) (%)' = 'red', 'Risk (PF Dose 1) (%)' = 'orange','Risk (PF Dose 2) (%)' = 'yellow', 'Risk (PF Dose 2 + PF Booster) (%)' = 'green')
#===================================================================================================================================================
ggplot() + 
  geom_line(data = df_delhi_india, aes(y = fac_aggregate_cases, x=date, fill = "Reported active cases (scaled)")) + 
  geom_ribbon(data = df_delhi_india, aes(ymin = cum_risk_factor, ymax = cum_risk_factor_u, x=date, fill = 'Risk (No Vaccine) (%)'), alpha=0.3) + 
  geom_ribbon(data = df_delhi_india_az1, aes(ymin = cum_risk_factor, ymax = cum_risk_factor_u, x=date, fill = "Risk (AZ Dose 1) (%)"), alpha = 0.3)+
  geom_ribbon(data = df_delhi_india_az2, aes(ymin = cum_risk_factor, ymax = cum_risk_factor_u, x=date, fill = 'Risk (AZ Dose 2) (%)'), alpha = 0.3)+
  geom_ribbon(data = df_delhi_india_az3, aes(ymin = cum_risk_factor, ymax = cum_risk_factor_u, x=date, fill = 'Risk (AZ Dose 2 + AZ Booster) (%)'), alpha = 0.3)+
  labs(y="Risk Of Infection (%)", x = "Date", colour = "")+
  theme(axis.text=element_text(size=24),axis.title=element_text(size=24), legend.position = "bottom")+
  #scale_color_manual(values = ct_rv, name = "Delhi, India")+
  scale_fill_manual(values=ct_rv, name="")+#, labels = c("Reported active cases (scaled)", "Risk of Infection (AstraZeneca Dose 2) (%)", "Risk of Infection (AstraZeneca Dose 1) (%)","Risk of Infection (No Vaccine) (%)"))+
  theme(legend.position = c(0.66, 0.8), legend.background = element_rect(fill = "transparent"),legend.key.size = unit(1.5, 'cm'),legend.title = element_text(size=24), legend.text = element_text(size=23))

ggplot() + 
  geom_line(data = df_massachusetts_usa, aes(y = fac_aggregate_cases, x=date, fill = "Reported active cases (scaled)")) + 
  geom_ribbon(data = df_massachusetts_usa, aes(ymin = cum_risk_factor, ymax = cum_risk_factor_u, x=date, fill = 'Risk (No Vaccine) (%)'), alpha=0.3) + 
  geom_ribbon(data = df_massachusetts_usa_m1, aes(ymin = cum_risk_factor, ymax = cum_risk_factor_u, x=date, fill = "Risk (PF Dose 1) (%)"), alpha = 0.3)+
  geom_ribbon(data = df_massachusetts_usa_m2, aes(ymin = cum_risk_factor, ymax = cum_risk_factor_u, x=date, fill = "Risk (PF Dose 2) (%)"), alpha = 0.3)+
  geom_ribbon(data = df_massachusetts_usa_m3, aes(ymin = cum_risk_factor, ymax = cum_risk_factor_u, x=date, fill = "Risk (PF Dose 2 + PF Booster) (%)"), alpha = 0.3)+
    labs(y="Risk Of Infection (%)", x = "Date", colour = "")+
  theme(legend.position = c(0.23, 0.8), legend.background = element_rect(fill = "transparent"), axis.text=element_text(size=24),axis.title=element_text(size=24))+#,labels=c("Reported active cases (scaled)", "Risk of Infection (No Vaccine) (%)", "Risk of Infection (Moderna Dose 1) (%)","Risk of Infection (Moderna Dose 2) (%)"))+
  #scale_color_manual(values = ct_rv_1, name="")+
  scale_fill_manual(values=ct_rv_1, name="")+
  #scale_fill_discrete(name = "Massachusetts, USA",  limits = c("Reported active cases (scaled)", "Risk of Infection (No Vaccine) (%)", "Risk of Infection (Moderna Dose 1) (%)","Risk of Infection (Moderna Dose 2) (%)"))+
  theme(legend.key.size = unit(1.5, 'cm'),legend.title = element_text(size=24), legend.text = element_text(size=24))
#===================================================================================================================================================
#===================================================================================================================================================
ct_rv_us <- c('Reported active cases (scaled)'='black','Risk (No Mask) (%)' = 'red', 'Risk (Surgical) (%)'= 'orange' ,'Risk (N95) (%)' = 'green')
ct_rv_in <- c('Reported active cases (scaled)'='black','Risk (No Mask) (%)' = 'red','Risk (Surgical) (%)'='orange', 'Risk (N95) (%)' = 'green')
#===================================================================================================================================================
ggplot() + 
  geom_line(data = df_massachusetts_usa, aes(y = fac_aggregate_cases, x=date, fill = "Reported active cases (scaled)")) + 
  geom_ribbon(data = df_massachusetts_usa, aes(ymin = cum_risk_factor, ymax = cum_risk_factor_u, x=date, fill = 'Risk (No Mask) (%)'), alpha=0.3) + 
  geom_ribbon(data = df_massachusetts_usa_procedure, aes(ymin = cum_risk_factor, ymax = cum_risk_factor_u, x=date, fill = "Risk (Surgical) (%)"), alpha = 0.3)+
  geom_ribbon(data = df_massachusetts_usa_n95, aes(ymin = cum_risk_factor, ymax = cum_risk_factor_u, x=date, fill = 'Risk (N95) (%)'), alpha = 0.3)+
  labs(y="Risk Of Infection (%)", x = "Date", colour = "")+
  theme(axis.text=element_text(size=24),axis.title=element_text(size=24))+
  scale_fill_manual(values=ct_rv_us, name="")+#,labels=c("Reported active cases (scaled)", "Risk of Infection (No Mask) (%)", "Risk of Infection (Surgical Mask) (%)","Risk of Infection (N95 Respirator) (%)"))+
  #scale_color_manual(values = ct)+
  theme(legend.position = c(0.23, 0.8), legend.background = element_rect(fill = "transparent"), legend.key.size = unit(1.5, 'cm'),legend.title = element_text(size=24), legend.text = element_text(size=24))

ggplot() + 
  geom_line(data = df_delhi_india, aes(y = fac_aggregate_cases, x=date, fill = "Reported active cases (scaled)")) + 
  geom_ribbon(data = df_delhi_india, aes(ymin = cum_risk_factor, ymax = cum_risk_factor_u, x=date, fill = 'Risk (No Mask) (%)'), alpha=0.3) + 
  geom_ribbon(data = df_delhi_india_procedure, aes(ymin = cum_risk_factor, ymax = cum_risk_factor_u, x=date, fill = 'Risk (Surgical) (%)'), alpha = 0.3)+
  geom_ribbon(data = df_delhi_india_n95, aes(ymin = cum_risk_factor, ymax = cum_risk_factor_u, x=date, fill = 'Risk (N95) (%)'), alpha = 0.3)+
  labs(y="Risk Of Infection (%)", x = "Date", colour = "")+
  theme(legend.position = c(0.66, 0.8), legend.background = element_rect(fill = "transparent"), axis.text=element_text(size=24),axis.title=element_text(size=24))+
  scale_fill_manual(values=ct_rv_in, name="")+#,labels=c("Reported active cases (scaled)", "Risk of Infection (No Mask) (%)", "Risk of Infection (Surgical Mask) (%)","Risk of Infection (N95 Respirator) (%)"))+
  theme(legend.key.size = unit(1.5, 'cm'),legend.title = element_text(size=24), legend.text = element_text(size=24))
#===================================================================================================================================================
#===================================================================================================================================================

