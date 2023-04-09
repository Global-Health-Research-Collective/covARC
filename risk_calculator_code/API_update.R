setwd("C:/Users/liume/OneDrive - The Pennsylvania State University/XGB/Lineage2022/data")
library(foreach)
library(outbreakinfo)
library(data.table)
library(dplyr)
library(tibble)
library(countrycode)
library(UScensus2010)
library(stringr )
library(raster)
library(reshape2)
library(reshape)
library(zoo)
library(plotly)
library(data.table)
library(RCurl)
library(RSelenium)
library(rvest)
library(httr)
library(keyring)
library(jsonlite)
###############################################################################################
##auto login 
OUTBREAK_INFO_AUTH = "https://api.outbreak.info/genomics/get-auth-token"
response <- POST(OUTBREAK_INFO_AUTH, body = "{}", encode = "raw")
if (status_code(response) == 200) {
  response_content <- content(response)
  auth_token <- response_content$authn_token
  Sys.setenv(OUTBREAK_INFO_TOKEN = auth_token)
  cat(paste("Please open this url in a browser and authenticate with your GISAID credentials.", 
            response_content$authn_url, sep = "\n\n"))
  browseURL(response_content$authn_url)
}
#response_content$authn_url


driver <- rsDriver(browser=c("chrome"),
                   chromever = "latest_compatible")
remote_driver <- driver[["client"]]
remote_driver$open()
remote_driver$navigate(response_content$authn_url)
webElem1 <- remote_driver$findElement(using = "name", "login")
webElem1$sendKeysToElement(list("liumeng595",key="enter"))
webElem2 <- remote_driver$findElement(using = "name", "password")
webElem2$sendKeysToElement(list("Z1vd6(nk",key="enter"))
webElem3 <- remote_driver$findElement(using = "xpath",value="//input[@value='Authenticate']")
webElem3$clickElement()


###############################################################################################
#Get the country name 
country<-fread("country.csv",header = T)
country$ISO3<-countrycode(country$x, origin = 'country.name', destination = 'iso3c')
country$ISO2<-countrycode(country$x, origin = 'country.name', destination = 'iso2c')


#get the variant
omicron_qr <-  lookupSublineages("omicron", returnQueryString = TRUE)
alpha_qr <-  lookupSublineages("alpha", returnQueryString = TRUE)
beta_qr <-  lookupSublineages("beta", returnQueryString = TRUE)
delta_qr <-  lookupSublineages("delta", returnQueryString = TRUE)
gamma_qr<-  lookupSublineages("gamma", returnQueryString = TRUE)
omicron_qr <-  lookupSublineages("omicron", returnQueryString = TRUE)
alpha_qr <-  lookupSublineages("alpha", returnQueryString = TRUE)
beta_qr <-  lookupSublineages("beta", returnQueryString = TRUE)
delta_qr <-  lookupSublineages("delta", returnQueryString = TRUE)
gamma_qr<-  lookupSublineages("gamma", returnQueryString = TRUE)
omicron_qr <-  lookupSublineages("omicron", returnQueryString = TRUE)
#BA.2_qr <-  'BA.2 OR BA.2.1 OR BA.2.2 OR BA.2.3 OR BA.2.3.1 OR BA.2.3.2 OR BA.2.4 OR BA.2.5 OR BA.2.6 OR BA.2.7 OR BA.2.8 OR BA.2.9 OR BA.2.9.1 OR BA.2.10 OR BA.2.10.1 OR BA.2.11 OR BA.2.12 OR BA.2.12.1 OR BA.2.13'

VOC<-c(alpha_qr,beta_qr,gamma_qr,delta_qr,omicron_qr)
VOC_dic<-data.frame(Name=c("Alpha","Beta","Gamma","Delta","Omicron"),
                    QR=c(alpha_qr,beta_qr,gamma_qr,delta_qr,omicron_qr))
#US state
states.names<- getData(country="USA", level=1) 
US_state<-data.frame(Name=states.names$NAME_1,Abb=state.abb[match(states.names$NAME_1,state.name)] )
US_state$Abb[US_state$Name=="District of Columbia"]<-"DC"
US_state$query_name<-paste0("USA_US-",US_state$Abb)

#Canada provinces 
Canada_provinces <- getData(country="Canada", level=1) 
Canada_provinces<-Canada_provinces$NAME_1
Canada_state<-data.frame(Name=Canada_provinces,
                                 Abb=c("AB","BC","MB","NB","NL","NT","NS","NU",
                                       "ON","PE","QC","SK","YT"))
Canada_state$query_name<-paste0("CAN_CA-",Canada_state$Abb)

#India provinces 
India_provinces<-getData(country="India", level=1) 
India_provinces<-India_provinces$NAME_1
India_provinces[25]<-"Delhi"
India_provinces<-sort(India_provinces)
India_state<-data.frame(Name=India_provinces,
                                Abb=c("AN","AP","AR","AS","BR","CH","CT","DN",
                                      "DD","DL","GA","GJ","HR","HP","JK","JH","KA",
                                      "KL","LD","MP","MH","MN","ML","MZ","NL",
                                      "OR","PY","PB","RJ","SK","TN","TG","TR","UP","UT","WB"))
India_state$query_name<-paste0("IND_IN-",India_state$Abb)

###############################################################################################
####Country Level
#Retrieve prevalence of lineages by location
dir.create("Country/original")
dir.create("Country/processed")

Country_lineage<-function(Country,VOC,VOC_dic){
    
    
    c_url<-paste0("https://api.outbreak.info/genomics/prevalence-by-location?location_id=",
                  Country,"&pangolin_lineage=",VOC)
    tep <-foreach(i=1:length(c_url), .combine='rbind') %do% 
      rbind_pages(getGenomicsResponse(c_url[i], T))  
    tep$lineage<-VOC_dic$Name[match(tep$query_key,VOC_dic$QR)]
    tep<-tep %>% group_by(lineage) %>%  arrange(lineage, date) %>% 
      mutate(total_count_rolling_14 = rollapply(total_count,14,mean,align='right',fill=NA)) %>%
      mutate(lineage_count_rolling_14 = rollapply(lineage_count,14,mean,align='right',fill=NA)) %>%
      mutate(proportion_rolling_14 =lineage_count_rolling_14/total_count_rolling_14) %>%
      ungroup()
    tep2<-tep %>% dplyr:: select(date,proportion_rolling_14,lineage) %>%
      reshape:: cast(date~lineage,value = 'proportion_rolling_14') 
    location_out<- data.frame(Date=tep2$date,
                              CountryCode=rep(countrycode(Country, 'country.name', 'iso3c'),length(tep2$date)),
                              CountryName=rep(Country,length(tep2$date)),
                              Alpha= if(is.null(tep2$Alpha)) rep(NA,length(tep2$date)) else tep2$Alpha,
                              
                              Beta=  if(is.null(tep2$Beta)) rep(NA,length(tep2$date)) else tep2$Beta ,
                              Gamma=if(is.null(tep2$Gamma)) rep(NA,length(tep2$date)) else tep2$Gamma,
                              Delta= if(is.null(tep2$Delta)) rep(NA,length(tep2$date)) else tep2$Delta ,
                              Omicron= if(is.null(tep2$Omicron)) rep(NA,length(tep2$date)) else tep2$Omicron )
    location_out[is.na(location_out)]<-0
    
    fwrite(tep,paste0(getwd(),"/","Country/original","/",countrycode(Country,  'iso3c','country.name'),".csv"))
    fwrite(location_out,paste0(getwd(),"/","Country/processed","/",countrycode(Country,  'iso3c','country.name'),".csv"))
}
#save the data
  for (i in country$ISO3){
    try(Country_lineage(i,VOC,VOC_dic))
  }
  

###############################################################################################

#Retrieve prevalence of lineages by state in US
dir.create("US/original")
dir.create("US/processed")
  
US_lineage<-function(State,VOC,VOC_dic){
  c_url<-paste0("https://api.outbreak.info/genomics/prevalence-by-location?location_id=",
                State,"&pangolin_lineage=",VOC)
  tep <-foreach(i=1:length(c_url), .combine='rbind') %do% 
    rbind_pages(getGenomicsResponse(c_url[i], T))  
  tep$lineage<-VOC_dic$Name[match(tep$query_key,VOC_dic$QR)]
  
  tep<-tep %>% group_by(lineage) %>%  arrange(lineage, date) %>% 
    mutate(total_count_rolling_14 = rollapply(total_count,14,mean,align='right',fill=NA)) %>%
    mutate(lineage_count_rolling_14 = rollapply(lineage_count,14,mean,align='right',fill=NA)) %>%
    mutate(proportion_rolling_14 =lineage_count_rolling_14/total_count_rolling_14) %>%
    ungroup()
  tep2<-tep %>% dplyr:: select(date,proportion_rolling_14,lineage) %>%
    reshape:: cast(date~lineage,value = 'proportion_rolling_14') 
  location_out<- data.frame(Date=tep2$date,
                            StateCode=rep(US_state$Abb[match(State,US_state$query_name)],length(tep2$date)),
                            StateName=rep(US_state$Name[match(State,US_state$query_name)],length(tep2$date)),
                            Alpha= if(is.null(tep2$Alpha)) rep(NA,length(tep2$date)) else tep2$Alpha,
                            
                            Beta=  if(is.null(tep2$Beta)) rep(NA,length(tep2$date)) else tep2$Beta ,
                            Gamma=if(is.null(tep2$Gamma)) rep(NA,length(tep2$date)) else tep2$Gamma,
                            Delta= if(is.null(tep2$Delta)) rep(NA,length(tep2$date)) else tep2$Delta ,
                            Omicron= if(is.null(tep2$Omicron)) rep(NA,length(tep2$date)) else tep2$Omicron )
  location_out[is.na(location_out)]<-0
  
  fwrite(tep,paste0(getwd(),"/","US/original","/",US_state$Name[match(State,US_state$query_name)],".csv"))
  fwrite(location_out,paste0(getwd(),"/","US/processed","/",US_state$Name[match(State,US_state$query_name)],".csv"))
}

for (i in US_state$query_name){
  
  try(US_lineage(i,VOC,VOC_dic))
  
}
###############################################################################################
#Canada provinces 
dir.create("Canada/original")
dir.create("Canada/processed")


Canada_lineage<-function(State,VOC,VOC_dic){
  
  c_url<-paste0("https://api.outbreak.info/genomics/prevalence-by-location?location_id=",
                State,"&pangolin_lineage=",VOC)
  tep <-foreach(i=1:length(c_url), .combine='rbind') %do% 
    rbind_pages(getGenomicsResponse(c_url[i], T))  
  tep$lineage<-VOC_dic$Name[match(tep$query_key,VOC_dic$QR)]
  
  tep<-tep %>% group_by(lineage) %>%  arrange(lineage, date) %>% 
    mutate(total_count_rolling_14 = rollapply(total_count,14,mean,align='right',fill=NA)) %>%
    mutate(lineage_count_rolling_14 = rollapply(lineage_count,14,mean,align='right',fill=NA)) %>%
    mutate(proportion_rolling_14 =lineage_count_rolling_14/total_count_rolling_14) %>%
    ungroup()
  tep2<-tep %>% dplyr:: select(date,proportion_rolling_14,lineage) %>%
    reshape:: cast(date~lineage,value = 'proportion_rolling_14') 
  location_out<- data.frame(Date=tep2$date,
                            StateCode=rep(Canada_state$Abb[match(State,Canada_state$query_name)],length(tep2$date)),
                            StateName=rep(Canada_state$Name[match(State,Canada_state$query_name)],length(tep2$date)),
                            Alpha= if(is.null(tep2$Alpha)) rep(NA,length(tep2$date)) else tep2$Alpha,
                            
                            Beta=  if(is.null(tep2$Beta)) rep(NA,length(tep2$date)) else tep2$Beta ,
                            Gamma=if(is.null(tep2$Gamma)) rep(NA,length(tep2$date)) else tep2$Gamma,
                            Delta= if(is.null(tep2$Delta)) rep(NA,length(tep2$date)) else tep2$Delta ,
                            Omicron= if(is.null(tep2$Omicron)) rep(NA,length(tep2$date)) else tep2$Omicron )
  location_out[is.na(location_out)]<-0
  
  fwrite(tep,paste0(getwd(),"/","Canada/original","/",Canada_state$Name[match(State,Canada_state$query_name)],".csv"))
  fwrite(location_out,paste0(getwd(),"/","Canada/processed","/",Canada_state$Name[match(State,Canada_state$query_name)],".csv"))
}

for (i in Canada_state$query_name){
  
  try(Canada_lineage(i,VOC,VOC_dic))
  
}


###############################################################################################
#India provinces
dir.create("India/original")
dir.create("India/processed")

India_lineage<-function(State,VOC,VOC_dic){
  c_url<-paste0("https://api.outbreak.info/genomics/prevalence-by-location?location_id=",
                State,"&pangolin_lineage=",VOC)
  tep <-foreach(i=1:length(c_url), .combine='rbind') %do% 
    rbind_pages(getGenomicsResponse(c_url[i], T))  
  tep$lineage<-VOC_dic$Name[match(tep$query_key,VOC_dic$QR)]
  
  tep<-tep %>% group_by(lineage) %>%  arrange(lineage, date) %>% 
    mutate(total_count_rolling_14 = rollapply(total_count,14,mean,align='right',fill=NA)) %>%
    mutate(lineage_count_rolling_14 = rollapply(lineage_count,14,mean,align='right',fill=NA)) %>%
    mutate(proportion_rolling_14 =lineage_count_rolling_14/total_count_rolling_14) %>%
    ungroup()
  tep2<-tep %>% dplyr:: select(date,proportion_rolling_14,lineage) %>%
    reshape:: cast(date~lineage,value = 'proportion_rolling_14') 
  location_out<- data.frame(Date=tep2$date,
                            StateCode=rep(India_state$Abb[match(State,India_state$query_name)],length(tep2$date)),
                            StateName=rep(India_state$Name[match(State,India_state$query_name)],length(tep2$date)),
                            Alpha= if(is.null(tep2$Alpha)) rep(NA,length(tep2$date)) else tep2$Alpha,
                            
                            Beta=  if(is.null(tep2$Beta)) rep(NA,length(tep2$date)) else tep2$Beta ,
                            Gamma=if(is.null(tep2$Gamma)) rep(NA,length(tep2$date)) else tep2$Gamma,
                            Delta= if(is.null(tep2$Delta)) rep(NA,length(tep2$date)) else tep2$Delta ,
                            Omicron= if(is.null(tep2$Omicron)) rep(NA,length(tep2$date)) else tep2$Omicron )
  location_out[is.na(location_out)]<-0
  
  fwrite(tep,paste0(getwd(),"/","India/original","/",India_state$Name[match(State,India_state$query_name)],".csv"))
  fwrite(location_out,paste0(getwd(),"/","India/processed","/",India_state$Name[match(State,India_state$query_name)],".csv"))
}

for (i in India_state$query_name){
  
  try(India_lineage(i,VOC,VOC_dic))
  
}


############################################################################################


# x<-getAllLineagesByLocation(
#   "united states",
#   other_threshold = 0.001,
#   nday_threshold = 1,
#   ndays = 730,
#   cumulative = F
# )

dir.create("All Lineages by Country")  
AllLineages<-function(Country,other_threshold,nday_threshold,ndays,cumulative){
  c_url<-paste0("https://api.outbreak.info/genomics/prevalence-by-location-all-lineages?location_id=",
                Country,"&cumulative=",cumulative,"&ndays=",ndays,
                "&nday_threshold=",nday_threshold,
                "&other_threshold=",other_threshold
                
                )
  res<-getGenomicsResponse(c_url, T) %>% rbind_pages()
  res$Country<-country$x[match(Country,country$ISO3)]
  fwrite(res,paste0(getwd(),"/","All Lineages by Country","/",res$Country[1],".csv"))
  }
  
for (i in country$ISO3){
  try(AllLineages(i,other_threshold= 0.001,nday_threshold = 1,ndays = 730,cumulative = "false"))
}



#"https://api.outbreak.info/genomics/prevalence-by-location?location_id=USA_US-PA&pangolin_lineage=B.1.1.7 OR Q.1 OR Q.2 OR Q.3 OR Q.4 OR Q.5 OR Q.6 OR Q.7 OR Q.8"


#########All Lineages in US
dir.create("All Lineages in US")  
AllLineages_US<-function(State,other_threshold,nday_threshold,ndays,cumulative){
  c_url<-paste0("https://api.outbreak.info/genomics/prevalence-by-location-all-lineages?location_id=",
                State,"&cumulative=",cumulative,"&ndays=",ndays,
                "&nday_threshold=",nday_threshold,
                "&other_threshold=",other_threshold
                
  )
  res<-getGenomicsResponse(c_url, T) %>% rbind_pages()
  res$State<-US_state$Name[match(State,US_state$query_name)]
  fwrite(res,paste0(getwd(),"/","All Lineages in US","/",res$State[1],".csv"))
}

for (i in US_state$query_name){
  try(AllLineages_US(i,other_threshold= 0.0001,nday_threshold = 1,ndays = 730,cumulative = "false"))
}



###########All Lineages in Canada
dir.create("All Lineages in Canada")  
AllLineages_Canada<-function(State,other_threshold,nday_threshold,ndays,cumulative){
  c_url<-paste0("https://api.outbreak.info/genomics/prevalence-by-location-all-lineages?location_id=",
                State,"&cumulative=",cumulative,"&ndays=",ndays,
                "&nday_threshold=",nday_threshold,
                "&other_threshold=",other_threshold
                
  )
  res<-getGenomicsResponse(c_url, T) %>% rbind_pages()
  res$State<-Canada_state$Name[match(State,Canada_state$query_name)]
  fwrite(res,paste0(getwd(),"/","All Lineages in Canada","/",res$State[1],".csv"))
}

for (i in Canada_state$query_name){
  try(AllLineages_Canada(i,other_threshold= 0.0001,nday_threshold = 1,ndays = 730,cumulative = "false"))
}


