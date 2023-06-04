rm(list=ls())
library(dplyr)
#library(tidyverse)
library(zoo)
library(curl)
library(ggplot2)
library(foreach)
library(doParallel)
library(parallel)
library(data.table)
library(rlang)
library(imputeTS)
library(ggsci)
library(ggpubr)
library(cowplot)
library(scales)

setwd("C:/Users/shrey/Downloads/Synapsy/covARC/updated_final_csv")
files<-list.files(pattern = ".csv")
df_names<-  c("Index",'date','fac_aggregate_cases','fac_aggregate_cases_per_pop','cum_risk_factor','cum_risk_factor_u','hosp_risk_lower','hosp_risk_upper','death_risk_lower','death_risk_upper','f_risk')

error_date<-fread("franklin_usa_30_no_safety_alpha_flt.csv",header=F) %>% 
  set_names(df_names) %>% mutate(date=as.numeric(date) %>% as.Date()) %>% na.omit() %>% filter(fac_aggregate_cases<0) %>% select(date) 

data_preprocessing_no_col<-function(path){
  
  processed<-path %>%fread( header=F) %>% 
    set_names(df_names) %>% mutate(date=as.numeric(date) %>% as.Date()) %>% na.omit()%>%
    mutate_at(vars(3:10), as.numeric) %>% 
    mutate_at(vars(fac_aggregate_cases:death_risk_upper), ~ifelse(date %in% error_date$date, NA, .)) %>% 
    mutate_all(~na_interpolation(., option = "spline")) %>%
    mutate(date = as.Date(date)) %>% arrange(date)
  return (processed)
}

data_preprocessing<-function(path){
  
  processed<-path %>%fread( header=T) %>%
    mutate(date=as.numeric(date) %>% as.Date()) %>% na.omit()%>%
    mutate_at(vars(3:10), as.numeric) %>% 
    mutate_at(vars(fac_aggregate_cases:death_risk_upper), ~ifelse(date %in% error_date$date, NA, .)) %>% 
    mutate_all(~na_interpolation(., option = "spline")) %>%
    mutate(date = as.Date(date)) %>% arrange(date)
  return (processed)
}

custom_comma <- function(x) {
  format_decimal <- function(num) {
    num <- round(num, 10) # Round the number to 10 decimal places
    formatted_num <- format(num, big.mark = ",", nsmall = 10, scientific = FALSE, trim = TRUE)
    return(gsub("\\.?0+$", "", formatted_num))
  }
  sapply(x, format_decimal)
}

plot_fun<-function(df,title_name,font_size,min_y,max_y){ 
  # log_interval <- 2
  # y_breaks <- 10^(seq(floor(log10(min_y)), ceiling(log10(max_y)), by = log_interval))
  # 
  max_y<-100
  min_y<-0.8*min_y
  p <- ggplot(df) + 
    geom_line(aes(y = fac_aggregate_cases/100000, x=date, color = "Reported active cases (Scaled)")) + 
    geom_ribbon(aes(ymin = cum_risk_factor, ymax = cum_risk_factor_u, x=date, fill = 'Risk of Infection (%)'), alpha=0.3) + 
    geom_ribbon(aes(ymin = hosp_risk_lower, ymax = hosp_risk_upper, x=date, fill = "Risk of Hospitalization (%)"), alpha = 0.3)+
    geom_ribbon(aes(ymin = death_risk_lower, ymax = death_risk_upper, x=date, fill = 'Risk of Death (%)'), alpha = 0.5)+
    scale_color_manual(values = c("Reported active cases (Scaled)" = "black")) +
    scale_fill_manual(values =  c('Risk of Death (%)'="#4DBBD5FF",'Risk of Hospitalization (%)'="#00A087FF",'Risk of Infection (%)'="#3C5488FF"))+ 
    guides(color = guide_legend(reverse = TRUE, override.aes = list(linetype = 1, shape = NA), order = 1),
           fill = guide_legend(reverse = TRUE, order = 2)) +
    labs(y="Risk", x = "Date", colour = "", fill = "") +  
    ggtitle(title_name) + 
    theme(legend.key.size = unit(1.5, 'cm'),
          legend.title = element_text(size=font_size, face = "bold"),
          legend.text = element_text(size=font_size), 
          axis.text = element_text(size=font_size),
          axis.title = element_text(size=font_size),
          legend.position = "bottom",
          plot.title = element_text(size=font_size+2, face="bold")) +
    scale_fill_npg() +  
    scale_y_continuous(trans = "log10", limits = c(min_y, max_y), labels = custom_comma)
  
  
 

#theme(legend.key.size = unit(1.5, 'cm'),legend.title = element_text(size=24), legend.text = element_text(size=24))
return(p)
}

 sve_fun<-function(p1_A,name,label_p){
   
   tmp<-ggarrange(p1_A,  legend='bottom',labels=label_p)
   ggsave(paste0(name,'.pdf'), tmp, width = 15, height = 6, units = "in")
   
 }

 
font_size=15

franklin_usa_no_mask_no_vaccine_30<- data_preprocessing_no_col('https://raw.githubusercontent.com/Global-Health-Research-Collective/covARC/main/updated_final_csv/franklin_usa_no_mask_no_vaccine_30.csv')
franklin_us_no_mask_no_vaccine_60<- data_preprocessing_no_col('https://raw.githubusercontent.com/Global-Health-Research-Collective/covARC/main/updated_final_csv/franklin_us_no_mask_no_vaccine_60.csv')
#make the date as the same
max_y<-max(franklin_usa_no_mask_no_vaccine_30$fac_aggregate_cases,franklin_us_no_mask_no_vaccine_60$fac_aggregate_cases)*1.05
min_y<-min(franklin_usa_no_mask_no_vaccine_30$death_risk_lower,franklin_us_no_mask_no_vaccine_60$death_risk_lower)*0.95
p1_A<-plot_fun(franklin_usa_no_mask_no_vaccine_30,"Franklin, MA, USA (30 Year Old)",font_size,min_y,max_y) 
p1_B<-plot_fun(franklin_us_no_mask_no_vaccine_60,"Franklin, MA, USA (60 Year Old)",font_size,min_y,max_y)

sve_fun(p1_A,'p1_A','A')
sve_fun(p1_B,'p1_B','B')

p1<-ggarrange(p1_A, p1_B, ncol = 2, common.legend=T,legend='bottom',labels="AUTO")
p1

################P2
franklin_usa_30_no_vaccine_surgical_mask_fixed<- data_preprocessing('https://raw.githubusercontent.com/Global-Health-Research-Collective/covARC/main/updated_final_csv/franklin_usa_30_no_vaccine_surgical_mask_fixed.csv')
franklin_usa_30_no_vaccine_n95_fixed<- data_preprocessing('https://raw.githubusercontent.com/Global-Health-Research-Collective/covARC/main/updated_final_csv/franklin_usa_30_no_vaccine_n95_fixed.csv')

max_y<-max(franklin_usa_30_no_vaccine_surgical_mask_fixed$fac_aggregate_cases,franklin_usa_30_no_vaccine_n95_fixed$fac_aggregate_cases)*1.05
min_y<-min(franklin_usa_30_no_vaccine_surgical_mask_fixed$death_risk_lower,franklin_usa_30_no_vaccine_n95_fixed$death_risk_lower)*0.95
p2_A<-plot_fun(franklin_usa_30_no_vaccine_surgical_mask_fixed,"Franklin, MA, USA (Surgical Mask)",font_size,min_y,max_y)
 

p2_B<-plot_fun(franklin_usa_30_no_vaccine_n95_fixed,"Franklin, MA, USA (N95 Respirator)",font_size,min_y,max_y) 

p2<-ggarrange(p1_A, p1_B, p2_A, p2_B, ncol = 2, nrow=2, common.legend=T,legend='bottom',labels="AUTO")

p2

sve_fun(p2_A,'p2_A','A')
sve_fun(p2_B,'p2_B','B')

################P3

franklin_us_no_mask_p1_30<-'https://raw.githubusercontent.com/Global-Health-Research-Collective/covARC/main/updated_final_csv/franklin_us_no_mask_p1_30.csv' %>% 
  data_preprocessing_no_col()
franklin_us_no_mask_p2_30 <-'https://raw.githubusercontent.com/Global-Health-Research-Collective/covARC/main/updated_final_csv/franklin_us_no_mask_p2_30.csv'%>% 
  data_preprocessing_no_col()
franklin_us_no_mask_p3_30 <-'https://raw.githubusercontent.com/Global-Health-Research-Collective/covARC/main/updated_final_csv/franklin_us_no_mask_p3_30.csv' %>% 
  data_preprocessing_no_col()

max_y<-max(franklin_us_no_mask_p1_30$fac_aggregate_cases,
           franklin_us_no_mask_p2_30$fac_aggregate_cases,
           franklin_us_no_mask_p3_30$fac_aggregate_cases)*1.05
min_y<-min(franklin_us_no_mask_p1_30$death_risk_lower,
           franklin_us_no_mask_p2_30$death_risk_lower,
           franklin_us_no_mask_p3_30$death_risk_lower)*0.95
p3_A<-plot_fun(franklin_us_no_mask_p1_30,"Franklin, MA, USA (PF Dose 1)",font_size,min_y,max_y)#+theme(plot.title = element_text(size = 13))
 

p3_B<-plot_fun(franklin_us_no_mask_p2_30,"Franklin, MA, USA (PF Dose 2)",font_size,min_y,max_y)# +theme(plot.title = element_text(size = 13))

p3_c<-plot_fun(franklin_us_no_mask_p3_30,"Franklin, MA, USA (PF Dose 2) + Booster",font_size,min_y,max_y)# +theme(plot.title = element_text(size = 13))

p3<-ggarrange(p3_A, p3_B,p3_c, ncol = 3, common.legend=T,legend='bottom',labels=c("E","F","G"))

p3

sve_fun(p3_A,'p3_A','A')
sve_fun(p3_B,'p3_B','B')
sve_fun(p3_c,'p3_C','C')
################P4
berlin_germany_no_mask_no_vaccine_30<-'https://raw.githubusercontent.com/Global-Health-Research-Collective/covARC/main/updated_final_csv/berlin_germany_no_mask_no_vaccine_30.csv' %>% data_preprocessing_no_col()
quintana_roo_mexico_no_mask_no_vaccine_30<-'https://raw.githubusercontent.com/Global-Health-Research-Collective/covARC/main/updated_final_csv/quintana_roo_mexico_no_mask_no_vaccine_30.csv'%>% data_preprocessing_no_col()
quebec_canada_no_mask_no_vaccine_30<-'https://raw.githubusercontent.com/Global-Health-Research-Collective/covARC/main/updated_final_csv/quebec_canada_no_mask_no_vaccine_30.csv'%>% data_preprocessing_no_col()
england_uk_no_mask_no_vaccine_30<-'https://raw.githubusercontent.com/Global-Health-Research-Collective/covARC/main/updated_final_csv/england_uk_no_mask_no_vaccine_30.csv'%>% data_preprocessing_no_col()
south_africa_no_mask_no_vaccine_30<-'https://raw.githubusercontent.com/Global-Health-Research-Collective/covARC/main/updated_final_csv/south_africa_no_mask_no_vaccine_30.csv'%>% data_preprocessing_no_col()
osaka_japan_no_mask_no_vaccine_30<-'C:/Users/shrey/Downloads/Synapsy/covARC/updated_final_csv/osaka_japan_no_mask_vaccine_30.csv'%>% data_preprocessing_no_col()

max_y<-max(berlin_germany_no_mask_no_vaccine_30$fac_aggregate_cases,
           quintana_roo_mexico_no_mask_no_vaccine_30$fac_aggregate_cases,
           quebec_canada_no_mask_no_vaccine_30$fac_aggregate_cases,
           england_uk_no_mask_no_vaccine_30$fac_aggregate_cases,
           south_africa_no_mask_no_vaccine_30$fac_aggregate_cases,
           osaka_japan_no_mask_no_vaccine_30$fac_aggregate_cases
           )*1.05
min_y<-min(berlin_germany_no_mask_no_vaccine_30$death_risk_lower,
           quintana_roo_mexico_no_mask_no_vaccine_30$death_risk_lower,
           quebec_canada_no_mask_no_vaccine_30$death_risk_lower,
           england_uk_no_mask_no_vaccine_30$death_risk_lower,
           south_africa_no_mask_no_vaccine_30$death_risk_lower,
           osaka_japan_no_mask_no_vaccine_30$death_risk_lower
           )*0.95
p4_A<-plot_fun(berlin_germany_no_mask_no_vaccine_30,"Berlin, Germany",font_size,min_y,max_y)

p4_B<-plot_fun(quintana_roo_mexico_no_mask_no_vaccine_30,"Quintana Roo, Mexico",font_size,min_y,max_y) 

p4_c<-plot_fun(quebec_canada_no_mask_no_vaccine_30,"Quebec, Canada",font_size,min_y,max_y) 

p4_D<-plot_fun(england_uk_no_mask_no_vaccine_30,"England, UK",font_size,min_y,max_y)

p4_E<-plot_fun(south_africa_no_mask_no_vaccine_30,"South Africa",font_size,min_y,max_y) 
 
p4_F<-plot_fun(osaka_japan_no_mask_no_vaccine_30,"Osaka, Japan",font_size,min_y*0.9,max_y)

p4<-ggarrange(p4_A, p4_B,p4_c, p4_D, p4_E, p4_F, ncol = 2, nrow = 3, common.legend=T,legend='bottom',labels="AUTO")
p4

p5<-ggarrange(p4_c,p4_D, ncol = 2, common.legend=T,legend='bottom',labels=c("C","D"))
p5
p6<-ggarrange(p4_E,p4_F,ncol = 2,  common.legend=T,legend='bottom',labels=c("E","F"))
p6


sve_fun(p4_A,'p4_A','A')
sve_fun(p4_B,'p4_B','B')

sve_fun(p4_c,'p5_C','C')
sve_fun(p4_D,'p5_D','D')
sve_fun(p4_E,'p6_E','E')

###p7
delhi_india_no_mask_no_vaccine_30<-'https://raw.githubusercontent.com/Global-Health-Research-Collective/covARC/main/updated_final_csv/delhi_india_no_mask_no_vaccine_30.csv' %>% data_preprocessing_no_col()
delhi_india_no_mask_no_vaccine_60 <-'https://raw.githubusercontent.com/Global-Health-Research-Collective/covARC/main/updated_final_csv/delhi_india_no_mask_no_vaccine_60.csv' %>% data_preprocessing_no_col()

max_y<-max(delhi_india_no_mask_no_vaccine_30$fac_aggregate_cases,
           delhi_india_no_mask_no_vaccine_60$fac_aggregate_cases
)*1.05
min_y<-min(delhi_india_no_mask_no_vaccine_30$death_risk_lower,
           delhi_india_no_mask_no_vaccine_60$death_risk_lower,
           delhi_india_no_mask_no_vaccine_30$hosp_risk_lower,
           delhi_india_no_mask_no_vaccine_60$hosp_risk_lower,
           delhi_india_no_mask_no_vaccine_30$death_risk_upper,
           delhi_india_no_mask_no_vaccine_60$death_risk_upper,
           delhi_india_no_mask_no_vaccine_30$hosp_risk_upper,
           delhi_india_no_mask_no_vaccine_60$hosp_risk_upper
)*0.95
p7_A<-plot_fun(delhi_india_no_mask_no_vaccine_30,"Delhi, India (30 Year old)",font_size,min_y,max_y)

p7_B<-plot_fun(delhi_india_no_mask_no_vaccine_60,"Delhi, India (60 Year old)",font_size,min_y,max_y) 

p7<-ggarrange(p7_A, p7_B, ncol = 2, common.legend=T,legend='bottom',labels="AUTO")
p7


sve_fun(p7_A,'p7_A','A')
sve_fun(p7_B,'p7_B','B')

###p8

delhi_india_surgical_no_vaccine_30<- 'https://raw.githubusercontent.com/Global-Health-Research-Collective/covARC/main/updated_final_csv/delhi_india_surgical_no_vaccine_30.csv' %>% data_preprocessing_no_col()
delhi_india_n95_no_vaccine_30<-'https://raw.githubusercontent.com/Global-Health-Research-Collective/covARC/main/updated_final_csv/delhi_india_n95_no_vaccine_30.csv' %>% data_preprocessing_no_col()

max_y<-max(delhi_india_surgical_no_vaccine_30$fac_aggregate_cases,
           delhi_india_n95_no_vaccine_30$fac_aggregate_cases
)*1.05
min_y<-min(delhi_india_surgical_no_vaccine_30$death_risk_lower,
           delhi_india_n95_no_vaccine_30$death_risk_lower,
           delhi_india_surgical_no_vaccine_30$death_risk_upper,
           delhi_india_n95_no_vaccine_30$death_risk_upper,
           delhi_india_surgical_no_vaccine_30$hosp_risk_lower,
           delhi_india_n95_no_vaccine_30$hosp_risk_lower,
           delhi_india_surgical_no_vaccine_30$hosp_risk_upper,
           delhi_india_n95_no_vaccine_30$hosp_risk_upper
)*0.95
p8_A<-plot_fun(delhi_india_surgical_no_vaccine_30,"Delhi, India (Surgical Mask)",font_size,min_y,max_y)

p8_B<-plot_fun(delhi_india_n95_no_vaccine_30,"Delhi, India (N95 Respirator)",font_size,min_y,max_y) 

p8<-ggarrange(p7_A, p7_B, p8_A, p8_B, ncol = 2, nrow=2, common.legend=T,legend='bottom',labels="AUTO")
p8

sve_fun(p8_A,'p8_A','A')
sve_fun(p8_B,'p8_B','B')

###p9

delhi_india_no_mask_az1_30<-'https://raw.githubusercontent.com/Global-Health-Research-Collective/covARC/main/updated_final_csv/delhi_india_no_mask_az1_30.csv'%>% data_preprocessing_no_col()
delhi_india_no_mask_az2_30 <-'https://raw.githubusercontent.com/Global-Health-Research-Collective/covARC/main/updated_final_csv/delhi_india_no_mask_az2_30.csv'%>% data_preprocessing_no_col()
delhi_india_no_mask_az3_30 <-'https://raw.githubusercontent.com/Global-Health-Research-Collective/covARC/main/updated_final_csv/delhi_india_no_mask_az3_30.csv'%>% data_preprocessing_no_col()


max_y<-max(delhi_india_no_mask_az1_30$fac_aggregate_cases,
           delhi_india_no_mask_az2_30$fac_aggregate_cases,
           delhi_india_no_mask_az3_30$fac_aggregate_cases
)*1.05
min_y<-min(delhi_india_no_mask_az1_30$death_risk_lower,
           delhi_india_no_mask_az2_30$death_risk_lower,
           delhi_india_no_mask_az3_30$death_risk_lower,
           delhi_india_no_mask_az1_30$hosp_risk_lower,
           delhi_india_no_mask_az2_30$hosp_risk_lower,
           delhi_india_no_mask_az3_30$hosp_risk_lower,
           delhi_india_no_mask_az1_30$death_risk_upper,
           delhi_india_no_mask_az2_30$death_risk_upper,
           delhi_india_no_mask_az3_30$death_risk_upper,
           delhi_india_no_mask_az1_30$hosp_risk_upper,
           delhi_india_no_mask_az2_30$hosp_risk_upper,
           delhi_india_no_mask_az3_30$hosp_risk_upper
)*0.95
p9_A<-plot_fun(delhi_india_no_mask_az1_30,"Delhi, India (AZ Dose 1)",font_size,min_y,max_y) #+theme(plot.title = element_text(size = 13))

p9_B<-plot_fun(delhi_india_no_mask_az2_30,"Delhi, India (AZ Dose 2)",font_size,min_y,max_y) #+theme(plot.title = element_text(size = 13))

p9_c<-plot_fun(delhi_india_no_mask_az3_30,"Delhi, India (AZ Dose 2 + Booster)",font_size,min_y,max_y) #+theme(plot.title = element_text(size = 13))

 

p9<-ggarrange(p9_A, p9_B,p9_c,ncol = 3, common.legend=T,legend='bottom',labels=c("E","F","G"))
p9


sve_fun(p9_A,'p9_A','A')
sve_fun(p9_B,'p9_B','B')
sve_fun(p9_c,'p9_C','C')

###p10

plot_fun1<-function(df,title_name,font_size,min_y,max_y){ 
  # log_interval <- 2
  # y_breaks <- 10^(seq(floor(log10(min_y)), ceiling(log10(max_y)), by = log_interval))
  # 
  max_y<-100
  min_y<-0.8*min_y
  p <- ggplot(df) + 
    geom_line(aes(y = fac_aggregate_cases/100000, x=date, color = "Reported active cases (Scaled)")) + 
    geom_ribbon(aes(ymin = cum_risk_factor, ymax = cum_risk_factor_u, x=date, fill = 'Risk of Infection without vaccine (%)'), alpha=0.3) + 
    geom_ribbon(aes(ymin = cum_risk_factor_1, ymax = cum_risk_factor_u_1, x=date, fill = 'Risk of Infection with vaccine (%)'), alpha = 0.3)+
    #geom_ribbon(aes(ymin = death_risk_lower, ymax = death_risk_upper, x=date, fill = 'Risk of Death (%)'), alpha = 0.3)+
    scale_color_manual(values = c("Reported active cases (Scaled)" = "black")) +
    scale_fill_manual(values =  c( 'Risk of Infection without vaccine (%)'="#3C5488FF",'Risk of Infection with vaccine (%)'="#00A087FF"))+ 
    guides(color = guide_legend(reverse = TRUE, override.aes = list(linetype = 1, shape = NA), order = 1),
           fill = guide_legend(reverse = TRUE, order = 2)) +
    labs(y="Risk", x = "Date", colour = "", fill = "") +  
    ggtitle(title_name) + 
    theme(legend.key.size = unit(1.5, 'cm'),
          legend.title = element_text(size=font_size, face = "bold"),
          legend.text = element_text(size=font_size), 
          axis.text = element_text(size=font_size),
          axis.title = element_text(size=font_size),
          legend.position = "right",
          plot.title = element_text(size=font_size+2, face="bold")) +
    scale_fill_npg() +  
    scale_y_continuous(trans = "log10", limits = c(min_y, max_y), labels = custom_comma)
  
  
  
  
  #theme(legend.key.size = unit(1.5, 'cm'),legend.title = element_text(size=24), legend.text = element_text(size=24))
  return(p)
}


franklin_us_no_mask_no_vaccine_30<-'franklin_usa_no_mask_vaccine_variants_30.csv'%>% data_preprocessing_no_col()
franklin_us_no_mask_no_variant_p1_30<-'franklin_usa_no_mask_dose_1_vaccine_variants_30.csv'%>% data_preprocessing_no_col()
colnames(franklin_us_no_mask_no_variant_p1_30)<-paste0(colnames(franklin_us_no_mask_no_variant_p1_30),"_1")
p10_data_A<-franklin_us_no_mask_no_vaccine_30 %>% cbind(franklin_us_no_mask_no_variant_p1_30) %>% select(date,fac_aggregate_cases,cum_risk_factor,cum_risk_factor_u,cum_risk_factor_1,cum_risk_factor_u_1)

franklin_usa_30_no_safety_alpha_flt<-'https://raw.githubusercontent.com/Global-Health-Research-Collective/covARC/main/updated_final_csv/franklin_usa_30_no_safety_alpha_flt.csv'%>% data_preprocessing_no_col()
franklin_usa_no_mask_30_alpha_p1<-'franklin_us_no_mask_30_alpha_p1.csv'%>% data_preprocessing_no_col()
colnames(franklin_usa_no_mask_30_alpha_p1)<-paste0(colnames(franklin_usa_no_mask_30_alpha_p1),"_1")
p10_data_B<-franklin_usa_30_no_safety_alpha_flt %>% cbind(franklin_usa_no_mask_30_alpha_p1) %>% select(date,fac_aggregate_cases,cum_risk_factor,cum_risk_factor_u,cum_risk_factor_1,cum_risk_factor_u_1)

franklin_usa_30_no_safety_beta_flt<-'https://raw.githubusercontent.com/Global-Health-Research-Collective/covARC/main/updated_final_csv/franklin_usa_30_no_safety_beta_flt.csv' %>% data_preprocessing_no_col()
franklin_us_no_mask_30_beta_p1<-'franklin_us_no_mask_30_beta_p1.csv' %>% data_preprocessing_no_col()
colnames(franklin_us_no_mask_30_beta_p1)<-paste0(colnames(franklin_us_no_mask_30_beta_p1),"_1")
p10_data_C<-franklin_usa_30_no_safety_beta_flt %>% cbind(franklin_us_no_mask_30_beta_p1) %>% select(date,fac_aggregate_cases,cum_risk_factor,cum_risk_factor_u,cum_risk_factor_1,cum_risk_factor_u_1)



 
max_y<-max(franklin_usa_30_no_safety_alpha_flt$fac_aggregate_cases,
           franklin_usa_no_mask_30_alpha_p1$fac_aggregate_cases,
           franklin_usa_30_no_safety_beta_flt$fac_aggregate_cases,
           
           franklin_us_no_mask_30_beta_p1$fac_aggregate_cases
)*1.05
min_y<-min(franklin_usa_30_no_safety_alpha_flt$death_risk_lower,
           franklin_usa_no_mask_30_alpha_p1$death_risk_lower,
           franklin_usa_30_no_safety_alpha_flt$hosp_risk_lower,
           franklin_usa_no_mask_30_alpha_p1$hosp_risk_lower,
           franklin_usa_30_no_safety_alpha_flt$death_risk_upper,
           franklin_usa_no_mask_30_alpha_p1$death_risk_upper,
           franklin_usa_30_no_safety_alpha_flt$hosp_risk_upper,
           franklin_usa_no_mask_30_alpha_p1$hosp_risk_upper,
           franklin_usa_30_no_safety_beta_flt$death_risk_lower,
           franklin_us_no_mask_30_beta_p1$death_risk_lower,
           franklin_usa_30_no_safety_beta_flt$hosp_risk_lower,
           franklin_us_no_mask_30_beta_p1$hosp_risk_lower,
           franklin_usa_30_no_safety_beta_flt$death_risk_upper,
           franklin_us_no_mask_30_beta_p1$death_risk_upper,
           franklin_usa_30_no_safety_beta_flt$hosp_risk_upper,
           franklin_us_no_mask_30_beta_p1$hosp_risk_upper
)*0.95

p10_A<-plot_fun1(p10_data_A,"Franklin, MA, USA (No Variant) (No Vaccine/Dose 1)",font_size,min_y,max_y)  

p10_B<-plot_fun1(p10_data_A,"Franklin, MA, USA (Alpha Variant) (No Vaccine/Dose 1)",font_size,min_y,max_y)  

p10_C<-plot_fun1(p10_data_B,"Franklin, MA, USA (Beta Variant) (No Vaccine/Dose 1)",font_size,min_y,max_y) 

 

#p10<-ggarrange(p10_A, p10_B, ncol =2, nrow=1, common.legend=T,legend='bottom',labels=c("A","B") )
#p10


sve_fun(p10_A,'p10_A','A')
sve_fun(p10_B,'p10_B','B')
 
#p11
franklin_usa_30_no_safety_gamma_flt<-'https://raw.githubusercontent.com/Global-Health-Research-Collective/covARC/main/updated_final_csv/franklin_usa_30_no_safety_gamma_flt.csv' %>% data_preprocessing_no_col()
franklin_us_no_mask_30_gamma_p1<-'franklin_us_no_mask_30_gamma_p1.csv' %>% data_preprocessing_no_col()
colnames(franklin_us_no_mask_30_gamma_p1)<-paste0(colnames(franklin_us_no_mask_30_gamma_p1),"_1")
p11_data_A<-franklin_usa_30_no_safety_gamma_flt %>% cbind(franklin_us_no_mask_30_gamma_p1) %>% select(date,fac_aggregate_cases,cum_risk_factor,cum_risk_factor_u,cum_risk_factor_1,cum_risk_factor_u_1)




franklin_usa_30_no_safety_delta_flt<-'https://raw.githubusercontent.com/Global-Health-Research-Collective/covARC/main/updated_final_csv/franklin_usa_30_no_safety_delta_flt.csv' %>% data_preprocessing_no_col()
franklin_us_no_mask_30_delta_p2<-'https://raw.githubusercontent.com/Global-Health-Research-Collective/covARC/main/updated_final_csv/franklin_us_no_mask_30_delta_p2.csv' %>% data_preprocessing_no_col()
colnames(franklin_us_no_mask_30_delta_p2)<-paste0(colnames(franklin_us_no_mask_30_delta_p2),"_1")
p11_data_B<-franklin_usa_30_no_safety_delta_flt %>% cbind(franklin_us_no_mask_30_delta_p2) %>% select(date,fac_aggregate_cases,cum_risk_factor,cum_risk_factor_u,cum_risk_factor_1,cum_risk_factor_u_1)




max_y<-max(franklin_usa_30_no_safety_gamma_flt$fac_aggregate_cases,
           franklin_us_no_mask_30_gamma_p1$fac_aggregate_cases,
           franklin_usa_30_no_safety_delta_flt$fac_aggregate_cases,
           franklin_us_no_mask_30_delta_p2$fac_aggregate_cases
)*1.05
min_y<-min(franklin_usa_30_no_safety_gamma_flt$death_risk_lower,
           franklin_us_no_mask_30_gamma_p1$death_risk_lower,
           franklin_usa_30_no_safety_gamma_flt$hosp_risk_lower,
           franklin_us_no_mask_30_gamma_p1$hosp_risk_lower,
           franklin_usa_30_no_safety_gamma_flt$death_risk_upper,
           franklin_us_no_mask_30_gamma_p1$death_risk_upper,
           franklin_usa_30_no_safety_gamma_flt$hosp_risk_upper,
           franklin_us_no_mask_30_gamma_p1$hosp_risk_upper,
           franklin_usa_30_no_safety_delta_flt$death_risk_lower,
           franklin_us_no_mask_30_delta_p2$death_risk_lower
)*0.95
p11_A<-plot_fun1(p11_data_A,"Franklin, MA, USA (Gamma Variant) (No Vaccine/Dose 1)",font_size,min_y,max_y) 

p11_B<-plot_fun1(p11_data_B,"Franklin, MA, USA (Delta Variant) (No Vaccine/Dose 2)",font_size,min_y,max_y) 


#p11<-ggarrange(p11_A, p11_B , ncol = 2 ,nrow=1,  common.legend=T,legend='bottom',labels= c("C","D"))
#p11

#sve_fun(p11_A,'p11_C','C')
#sve_fun(p11_B,'p11_D','D')

#p12
franklin_usa_30_no_safety_omicron_flt<-'https://raw.githubusercontent.com/Global-Health-Research-Collective/covARC/main/updated_final_csv/franklin_usa_30_no_safety_omicron_flt.csv' %>% data_preprocessing_no_col()
franklin_us_no_mask_30_omicron_p3<-'https://raw.githubusercontent.com/Global-Health-Research-Collective/covARC/main/updated_final_csv/franklin_us_no_mask_30_omicron_p3.csv' %>% data_preprocessing_no_col()
colnames(franklin_us_no_mask_30_omicron_p3)<-paste0(colnames(franklin_us_no_mask_30_omicron_p3),"_1")
p12_data_A<-franklin_usa_30_no_safety_omicron_flt %>% cbind(franklin_us_no_mask_30_omicron_p3) %>% select(date,fac_aggregate_cases,cum_risk_factor,cum_risk_factor_u,cum_risk_factor_1,cum_risk_factor_u_1)



#max_y<-max(franklin_usa_30_no_safety_omicron_flt$fac_aggregate_cases,
#           
#           franklin_us_no_mask_30_omicron_p3$fac_aggregate_cases
#)*1.05
#min_y<-min(franklin_usa_30_no_safety_omicron_flt$death_risk_lower,
           
#           franklin_us_no_mask_30_omicron_p3$death_risk_lower
#)*0.95
p12_A<-plot_fun1(p12_data_A,"Franklin, MA, USA (Omicron Variant) (No Vaccine/Dose 3)",font_size,min_y,max_y) 

p12<-ggarrange(p10_A, p10_B, p10_C, p11_A, p11_B, p12_A,   ncol = 2 , nrow=3,common.legend=T,legend='bottom',labels=c("A","B","C","D","E","F"))
p12


sve_fun(p12_A,'p12_E','E')
 
#p13
england_uk_no_mask_no_vaccine_5<-'https://raw.githubusercontent.com/Global-Health-Research-Collective/covARC/main/updated_final_csv/england_uk_no_mask_no_vaccine_5.csv' %>% data_preprocessing_no_col()
england_uk_no_mask_no_vaccine_21<-'https://raw.githubusercontent.com/Global-Health-Research-Collective/covARC/main/updated_final_csv/england_uk_no_mask_no_vaccine_21.csv' %>% data_preprocessing_no_col()
england_uk_no_mask_no_vaccine_55<-'https://raw.githubusercontent.com/Global-Health-Research-Collective/covARC/main/updated_final_csv/england_uk_no_mask_no_vaccine_55.csv' %>% data_preprocessing_no_col()
england_uk_no_mask_no_vaccine_70<-'https://raw.githubusercontent.com/Global-Health-Research-Collective/covARC/main/updated_final_csv/england_uk_no_mask_no_vaccine_70.csv' %>% data_preprocessing_no_col()


max_y<-max(england_uk_no_mask_no_vaccine_5$fac_aggregate_cases,
           england_uk_no_mask_no_vaccine_21$fac_aggregate_cases,
           england_uk_no_mask_no_vaccine_55$fac_aggregate_cases,
           england_uk_no_mask_no_vaccine_70$fac_aggregate_cases
)*1.05
min_y<-min(england_uk_no_mask_no_vaccine_5$death_risk_lower,
           england_uk_no_mask_no_vaccine_21$death_risk_lower,
           england_uk_no_mask_no_vaccine_55$death_risk_lower,
           england_uk_no_mask_no_vaccine_70$death_risk_lower
)*0.95

p13_A<-plot_fun(england_uk_no_mask_no_vaccine_5,"England, UK (5 Year old)",font_size,min_y,max_y)

p13_B<-plot_fun(england_uk_no_mask_no_vaccine_21,"England, UK (21 Year old)",font_size,min_y,max_y) 

 

p13<-ggarrange(p13_A, p13_B , ncol = 2 ,  common.legend=T,legend='bottom',labels="AUTO")
p13
 
sve_fun(p13_A,'p13_A','A')
sve_fun(p13_B,'p13_B','B')
#p14
england_uk_no_mask_no_vaccine_55<-'https://raw.githubusercontent.com/Global-Health-Research-Collective/covARC/main/updated_final_csv/england_uk_no_mask_no_vaccine_55.csv' %>% data_preprocessing_no_col()
england_uk_no_mask_no_vaccine_70<-'https://raw.githubusercontent.com/Global-Health-Research-Collective/covARC/main/updated_final_csv/england_uk_no_mask_no_vaccine_70.csv' %>% data_preprocessing_no_col()


p14_C<-plot_fun(england_uk_no_mask_no_vaccine_55,"England, UK (55 Year old)",font_size,min_y,max_y)

p14_D<-plot_fun(england_uk_no_mask_no_vaccine_70,"England, UK (70 Year old)",font_size,min_y,max_y) 

p14<-ggarrange(p13_A, p13_B, p14_C,p14_D, labels = c("A","B","C", "D" ), ncol = 2 ,nrow=2 , common.legend=T,legend='bottom' )
p14


sve_fun(p14_A,'p14_C','C')
sve_fun(p14_B,'p14_D','D')


setwd("C:/Users/shrey/Downloads")
ggsave("p1.pdf", p1, width = 20, height = 6, units = "in")
ggsave("p2.pdf", p2, width = 20, height = 10, units = "in")
ggsave("p3.pdf", p3, width = 20, height = 6, units = "in")
ggsave("p4.pdf", p4, width = 20, height = 6, units = "in")
ggsave("p5.pdf", p5, width = 20, height = 6, units = "in")
ggsave("p6.pdf", p6, width = 20, height = 6, units = "in")
ggsave("p7.pdf", p7, width = 20, height = 6, units = "in")
ggsave("p8.pdf", p8, width = 20, height = 10, units = "in")
ggsave("p9.pdf", p9, width = 20, height = 6, units = "in")
ggsave("p10.pdf", p10, width = 20, height = 6, units = "in")
ggsave("p11.pdf", p11, width = 20, height = 6, units = "in")
ggsave("p12.pdf", p12, width = 20, height = 15, units = "in")
ggsave("p13.pdf", p13, width = 20, height = 6, units = "in")
ggsave("p14.pdf", p14, width = 20, height = 10, units = "in")
 



