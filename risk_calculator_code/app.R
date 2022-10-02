#Brazil Population Data: https://en.wikipedia.org/wiki/List_of_Brazilian_states_by_population
#India Population Data: https://www.findeasy.in/top-indian-states-by-population/
#USA Population Data: https://github.com/OxCGRT/covid-policy-tracker
rm(list=ls())
library(shiny)
library(dplyr)
library(curl)
library(tidyverse)
library(shinythemes)
library(shinycssloaders)
library(shinyBS)
library(flexdashboard)
library(zoo)
# Options for Spinner
options(spinner.color="#3D3D3D", spinner.color.background="#ffffff", spinner.size=1)

options(scipen=999)

readFromS3 = function(filename, bucket, sep = ','){
  return(s3read_using(FUN=read.csv, 
                      bucket = bucket, 
                      object=filename,
                      sep = sep, header=T))
}


#Currently only scaled for Brazil, India, USA and Canada
#country_list <- data.frame(country=c("US","IN","BR","CA"))
country_list <- read.csv('./custom_dataset/country_name_code.csv')

population_full <- read.csv(curl("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv"))
#population_full <- read.csv(curl("https://raw.githubusercontent.com/Volunteer-Collab/risk_calculator/main/Risk_calculator/risk_calculator_simple/custom_dataset/UID_ISO_FIPS_LookUp_Table_population_Countries_Regions.csv?token=ACZDUYH5CI5OQ6DZWPN44P3BBP6VM"))
population_full <- population_full[-c(1,2,3,4,5,9,10,11)]
population_full <- population_full[!is.na(population_full$Population),]
population_full <- population_full %>% mutate_if(is.character,as.factor)
population_full$Country_Region <- gsub("US", "United States", population_full$Country_Region)

mask <- read.csv("./custom_dataset/mask.csv") #Ref: https://www.epa.gov/sciencematters/epa-researchers-test-effectiveness-face-masks-disinfection-methods-against-covid-19

vaccine_eff <- read.csv("./custom_dataset/vaccine_eff.csv", sep = ",") #Ref: https://yourlocalepidemiologist.substack.com/p/vaccine-table-update-lots-on-delta

ratio_dataset <- read.csv("./custom_dataset/Ratios_Survey.csv")

#==============Define the inputs======================================================================================
#country_name ="India"
#people_passed_outdoor = 50
#people_passed_indoor = 50
#date = "2021-07-01"
#region = "Delhi"
#mask_type = "No Mask"
#type_vaccine_dose = "No Vaccine"

# Define UI
ui <- fluidPage(theme = shinytheme("flatly"),
                navbarPage(
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
                  "",
                  tabPanel("Risk Calculator",
                           sidebarPanel(
                             tags$h3("Information needed to estimate your risk:"),
                             p("Please enter in approximate values into each field and click submit to receive a risk percentage based on your information."),
                             br(),
                             selectInput(
                               "country",
                               tipify(p(id = "country_hover", "Country", icon("info-circle", "fa-xs")), "This describes the country where the activity will take place.", placement="right", trigger = "hover"),
                               country_list$CountryName,
                               selected = FALSE,
                               multiple = FALSE,
                               selectize = TRUE,
                             ),
                             selectInput(
                               "region",
                               tipify(p(id = "region_hover", "Region", icon("info-circle", "fa-xs")), "Specific region within a selected country if applicable.", placement="right", trigger = "hover"),
                               choices = character(0),
                               selected = FALSE,
                               multiple = FALSE,
                               selectize = TRUE,
                             ),
                             
                             selectInput(
                               "county",
                               tipify(p(id = "county_hover", "County", icon("info-circle", "fa-xs")), "Specific county within a selected region if applicable.", placement="right", trigger = "hover"),
                               choices = character(0),
                               selected = FALSE,
                               multiple = FALSE,
                               selectize = TRUE,
                             ),
                             dateInput(
                               "date",
                               tipify(p(id = "date_hover", "Date", icon("info-circle", "fa-xs")), "Enter the approximate date of the activity", placement="right", trigger = "hover"),
                               value = Sys.Date(),
                               min = NULL,
                               max = Sys.Date(),
                               format = "yyyy-mm-dd",
                               startview = "month",
                               weekstart = 0,
                               language = "en",
                             ),
                             selectInput(
                               "mask",
                               tipify(p(id = "mask_hover", "Mask Type", icon("info-circle", "fa-xs"), 
                                        a("What mask do you have?", href="https://www.epa.gov/sciencematters/epa-researchers-test-effectiveness-face-masks-disinfection-methods-against-covid-19")), "Select the type of mask which will be used", placement="right", trigger = "hover"),
                               mask$mask_type,
                               selected = NULL,
                               multiple = FALSE,
                               selectize = TRUE,
                             ),
                             selectInput(
                               "vaccine_type_dose",
                               tipify(p(id = "vaccine_hover", "Vaccine Type and Dosage", icon("info-circle", "fa-xs")), "This option factors in the efficacy of different COVID vaccines at different dosage levels", placement="right", trigger = "hover"),
                               vaccine_eff$vaccine,
                               selected = NULL,
                               multiple = FALSE,
                               selectize = TRUE,
                             ),
                             selectInput(
                               "past_covid_infection",
                               tipify(p(id = "past_covid_hover", "Past COVID Infection", icon("info-circle", "fa-xs")), "Previous COVID diagnosis", placement="right", trigger = "hover"),
                               c("No", "Yes"),
                               selected = TRUE,
                               multiple = FALSE,
                               selectize = TRUE,
                             ),
                             numericInput(
                               "age_group",
                               tipify(p(id = "age_hover", "Age", icon("info-circle", "fa-xs")), "Your age group will influence your succeptibility to infection", placement="right", trigger = "hover"),
                               18,
                               min = 1,
                               max = NA,
                               step = NA,
                               width = NULL
                             ),
                             selectInput(
                               "gender",
                               tipify(p(id = "gender_hover", "Sex", icon("info-circle", "fa-xs")), "We recognize that these gender options are not inclusive of every gender and regret that the required data has not been collected or is not accessible.", placement="right", trigger = "hover"),
                               c("Male", "Female"),
                               selected = NULL,
                               multiple = FALSE,
                               selectize = TRUE,
                             ),
                             selectInput(
                               "chronic_illness",
                               tipify(p(id = "illness_hover", "Chronic Illnesses", icon("info-circle", "fa-xs")), "Having an underlying condition such as diabetes, heart disease, cancer, lung disease, high blood pressure, immune disease, asthma, kidney disease, obesity, sickle cell anemia, HIV, or liver disease may influence your risk.", placement="right", trigger = "hover"),
                               c("No", "Yes"),
                               selected = NULL,
                               multiple = FALSE,
                               selectize = TRUE,
                             ),
                             numericInput(
                               "people_passed_indoor",
                               tipify(p(id = "people_indoor_hover", "People Passed Indoor During Activity", icon("info-circle", "fa-xs")), "Estimate the number of people you will pass within six feet of indoors during this activity.", placement="right", trigger = "hover"),
                               0,
                               min = 0,
                               max = NA,
                               step = NA,
                               width = NULL
                             ),
                             numericInput(
                               "people_passed_outdoor",
                               tipify(p(id = "people_outdoor_hover", "People Passed Outdoors During Activity", icon("info-circle", "fa-xs")), "Estimate the number of people you will pass within six feet of outdoors during this activity.", placement="right", trigger = "hover"),
                               0,
                               min = 0,
                               max = NA,
                               step = NA,
                               width = NULL
                             ),
                             actionButton(
                               "submit_button",
                               "Submit",
                               class = "btn btn-primary",
                             ),
                             actionButton(
                               "reset_button",
                               "Reset",
                               class = "btn btn-primary",
                             ),
                           ),
                           # sidebarPanel
                           column(4, offset = 1,
                                  
                                  h1("Activity Risk Calculator",img(src = "https://ucarecdn.com/dacb4d34-f833-4d2f-8453-9485ce07348b/AFS_90506removebgpreview.png", height = 60, width = 60)),
                                  
                                  h4("Risk of Being Infected with Covid-19"),
                                  p("This interactive risk calculator functions as a quick and easy way to gauge the odds of catching, being hospitalized with, and dying from COVID-19 for various activities you might do. View ", a("this paper", href="https://www.researchgate.net/publication/356884702_A_COVID-19_Activity_Risk_Calculator_as_a_Gamified_Public_Health_Intervention"), " to see more info on how it works"),
                                  hr(),
                                  h1("The following is the estimated risk of carrying out an activity:"),
                                  withSpinner(uiOutput("f_risk"),type=1),
                                  #withSpinner(gaugeOutput("gauge"), type=1)
                           ),
                           column(4, offset = 1, id = "footer",
                                  hr(),
                                  h4(id = "donate", "Do you find the AFS Activity Risk Calculator useful? Consider supporting our work by donating to our non-profit ", a("here", href="https://www.paypal.com/paypalme/AFSdonations"), " and/or filling out our ", a("CrossPaths survey here", href="https://docs.google.com/forms/d/e/1FAIpQLScaUKpZgsHaHtt2HvTEiLgtIurBeY6mRVGxrK1bo9rh001t_A/viewform"), " to help our epidemiologists continue to improve our technology."),
                           ),
                           br(),
                           br(),
                           br(),
                           br(),
                           br(),
                           br(),
                           column(10, offset = 1, id = "footer",
                                  h4("Disclaimer"),
                                  p(id = "disclaimer", "This calculator should not be taken as advice on how much risk to accept for yourself or others. Unforeseen events such as delays or inaccuracies in federally reported data or super spreader events could make the risk results less accurate temporarily. We may also continue to update the algorithm as more scientific data becomes available. Please always continue to follow the guidance and mandates of your local health officials when posted."),
                                  hr(),
                           )
                  ), # Navbar 1, tabPanel
                  
                  tabPanel("About this Calculator",
                           column(6, offset=3,
                                  h3("Why did we make this activity risk calculator?"),
                                  p("Since the COVID-19 pandemic began, millions of people have been struggling with the same questions about their personal risk. Questions like \"how safe is it to go to this restaurant today or to have a birthday party for my kid?\" and \"how much would my risk be reduced if I am vaccinated with this vaccine vs. another?\" and \"how does this variant change my risk?.\" Risk for activities has varied widely during the pandemic and has been very complicated for people to figure out. Here we provide an easy to use solution to calculate risk. We aim to empower people to live their lives, and to do it as safely as possible, by giving them a tool to make choices backed by data."),
                                  h3("What data does the calculator use and is it trustworthy?"),
                                  p("We use datasets from government and international repositories that are widely considered high quality by experts. These datasets include the Johns Hopkins University Covid repository, Oxford University's Covid repository, and the GISAID variants repository."),
                                  h3("What factors does the calculator take into account?"),
                                  tags$ul(
                                    tags$li("Community levels of Covid on the state level, which are updated daily."),
                                    tags$li("Variants in the community on the state level, which are updated daily."),
                                    tags$li("Protection by vaccines and masks."),
                                    tags$li("How well different vaccines work to protect from different variants."),
                                    tags$li("Indoor vs. outdoor risk."),
                                    tags$li("How age, gender, and health affect chances of hospitalization and death."),
                                    tags$li("The number of people that you come in close contact with during the activity."),
                                  ),
                                  h3("What factors does the calculator not currently take into account?"),
                                  tags$ul(
                                    tags$li("Being previously infected with Covid or \"natural immunity\"- the data is less clear for this so far but protection is likely 50% or less compared to being fully vaccinated. It also wanes over time. It does confer some protection. We may add this as a factor in the future as the data become clearer."),
                                    tags$li("Other people's masking or vaccination status at the activity- for simplicity we err on the side of caution and assume others are unmasked and unvaccinated. If people at your activity are masked and/or vaccinated it will substantially decrease your risk, although not eliminate it, as vaccinated/masked people can still catch and transmit Covid."),
                                    tags$li("Quality of air filtration system/open windows at indoor activities. We have not yet added this feature to the code but high quality air filtration and open windows do decrease risk of transmission."),
                                    tags$li("Time since vaccination. Vaccines do seem to become less effective starting at the 6-8 month mark. As the data becomes clearer, we may add this as a factor.")
                                  ),
                                  h3("How does the code for the calculator work?"),
                                  p("You can view a preprint describing the code for the calculator at this link."),
                                  a("Shreyasvi Natraj, Nathan Yap, Agrima Seth, Malhar Bhide, Leila Orszag, Pawan Nandakishore, Nina Rescic, Shanice Hudson, Carlos Baquero6, Davide Frey, Antonio Fernandez Anta, and Christin Glorioso. The COVID-19 Activity Risk Calculator as a Gamified Public Health Intervention.  DOI: 10.13140/RG.2.2.23583.89765", href="https://www.researchgate.net/publication/356884702_A_COVID-19_Activity_Risk_Calculator_as_a_Gamified_Public_Health_Intervention"),
                                  br(),
                                  br(),
                                  p("Questions or comments? Feel free to reach out to Dr. Glorioso on twitter @DrGlorioso or ", a("LinkedIn", href="https://www.linkedin.com/in/christin-glorioso-md-phd-39627719/")),
                                  br(),br(),br(),br()
                           )
                  ),
                  
                  tabPanel("About Us",
                           column(6, offset=3, h2("Who we are"),
                                  h4("This project is led by ", a("Dr. Christin Glorioso.", href="https://www.linkedin.com/in/christin-glorioso-md-phd-39627719/"), " an MIT- trained Epidemiologist, Physician, and Founder and CEO of the non-profit research organization, ",  a("Academics for the Future of Sciences (AFS)", href="https://www.facebook.com/academicsforthefutureofscience"), ". She is the Chief Strategist of the Bakar Aging Research Institute at the University of California, San Francisco School of Medicine. The talented volunteers that contributed to the writing of the scientific code, the UI, and the paper are credited below."),
                                  #img(src = "https://www.epa.gov/sites/default/files/2021-04/medical-mask-performance-resize.png", height = 920, width = 920),
                                  br(),
                                  h2("Our Team"),
                                  #https://app.uploadcare.com/accounts/settings/projects/df88dbeba8c170c16b58/files/?limit=25&ordering=-datetime_uploaded
                                  column(12, id="boxblue",
                                         column(3, id="imagecontent", img(src = "https://ucarecdn.com/35330bc4-8755-475a-9c58-ca8fe5a030ed/christin.png")),
                                         h3("Christin Glorioso, MD, PhD"), 
                                         p(id="imagecontent", "is an MIT-trained Epidemiologist and Co-Founder and CEO of the non-profit MIT spin-off, Academics for the Future of Science, and Founder and CEO of the Real Science Community. You can follow her on Twitter @DrGlorioso.")
                                  ),
                                  column(12, id="boxblue",
                                         column(3, id="imagecontent", img(src = "https://ucarecdn.com/cf5ae847-1c33-4e98-bd10-b534174955a8/shrey.jpg", height=150, width=120)),
                                         h3("Shreyasvi Natraj"), 
                                         p(id="imagecontent", "is a masters student in neuroscience at the University of Geneva working as a technical student at CERN in IT-DI-EFP group and under Prof. Marie Schaer's group at NCCR Synapsy, Campus Biotech. His current projects involve using high performance computing for benchmarking several high energy physics and deep learning workloads as well as working in data science to develop deep learning systems for automated screening of neurological disorders.")
                                  ),
                                  column(12, id="boxblue",
                                         column(3, id="imagecontent", img(src = "https://ucarecdn.com/2693dfb4-9ebe-4dd8-835f-613b58bd5d68/nathan128x1281.jpg", height=150, width=120)),
                                         h3("Nathan Yap"), 
                                         p(id="imagecontent", "is a student at Eastlake High School set to graduate in 2022. He has had experience working as an intern for web development, labelling tool development, and research projects for MIT-affiliated organizations and other tech startups.")
                                  ),
                                  column(12, id="boxblue",
                                         column(3, id="imagecontent", img(src = "https://ucarecdn.com/f5866dbb-c1c1-44df-9efe-3bd32cda4454/agrima.png", height=150, width=120)),
                                         h3("Agrima Seth"), 
                                         p(id="imagecontent", "is a Ph.D. Student studying computational social science at the School of Information at the University of Michigan. You can connect with her on Twitter: @agrima_seth")
                                  ),
                                  column(12, id="boxblue",
                                         column(3, id="imagecontent", img(src = "https://ucarecdn.com/346d4300-f867-4b74-af50-3d27f01cde5f/image12.png", height=150, width=120)),
                                         h3("Malhar Bhide"), 
                                         p(id="imagecontent", "is a student at Hill Spring International School set to graduate in 2022. He has previously worked with non-profits and other organizations on different Machine Learning and Data Science projects.")
                                  ),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  tags$head(tags$style("
                                    #boxblue{
                                    background-color: #d4f0ff;
                                    border-radius: 10px;
                                    margin: 10px;
                                    }
                                    #imagecontent{
                                    font-size: 15px;
                                    padding: 15px;
                                    font-weight: 700;
                                    }
                                    #imagecontent img {max-width: 100%; width: 100%; height: auto}
                                    "
                                  )),
                           )
                  )
                  #tabPanel("Navbar 2", "This panel is intentionally left blank"),
                  #tabPanel("Navbar 3", "This panel is intentionally left blank")
                  
                ) # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output, session) {
  
  datasetInput <- reactive({
    
    validate(
      need(input$region != "", "Please select a region"),
      need(input$county, "Please select a county")
    )
    
    people_passed_outdoor = input$people_passed_outdoor
    people_passed_indoor = input$people_passed_indoor
    date = toString(input$date-9)
    region = input$region
    mask_type = input$mask
    type_vaccine_dose = input$vaccine_type_dose
    country_name = input$country
    age_group = input$age_group
    gender = input$gender
    chronic_illness = input$chronic_illness
    past_covid_infection = input$past_covid_infection
    county = input$county
    
    if (country_name=="United States") {
      jhu_dataset <- read.csv(curl(paste0(paste0("https://raw.githubusercontent.com/Global-Health-Research-Collective/covARC/main/filtered/US/", str_replace(region," ", "%20")),".csv")))
      jhu_dataset <- jhu_dataset %>% mutate_if(is.character,as.factor)
      jhu_dataset <- jhu_dataset[jhu_dataset$Admin2!="",]
      jhu_dataset$Date<-as.Date(as.character(jhu_dataset$Date),format="%Y-%m-%d")
      colnames(jhu_dataset) <- c("city","region","country","Lat","Lon","Confirmed","Deaths","date","time")
      summary(jhu_dataset)
      
    } else {
      jhu_dataset <- read.csv(curl(paste0(paste0("https://raw.githubusercontent.com/Global-Health-Research-Collective/covARC/main/filtered/", str_replace(country_name," ", "%20")),".csv")))
      jhu_dataset <- jhu_dataset %>% mutate_if(is.character,as.factor)
      jhu_dataset$Date<-as.Date(as.character(jhu_dataset$Date),format="%Y-%m-%d")
      colnames(jhu_dataset) <- c("region","country","Lat","Lon","Confirmed","Deaths","date","time")
    }  
    
    if(country_name=="Canada"){
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
    
    #ratio_dataset <- read.csv("C:/Users/shrey/Downloads/covid/Ratios_Survey.csv")
    #ratio_dataset <- read.csv(curl("https://raw.githubusercontent.com/Volunteer-Collab/risk_calculator/main/Risk_calculator/risk_calculator_simple/custom_dataset/Ratios_Survey.csv?token=ACZDUYD4JVF3ZIVRPC3ZV5LBBP7RW"))
    ratio_dataset <- ratio_dataset[-c(1:6)]
    ratio = ratio_dataset$Survey_Reported_Ratio[ratio_dataset$Country==country_name]
    jhu_dataset$Confirmed_u <- jhu_dataset$Confirmed*ratio

    if(country_name=="United States"){
      if(county!="Not applicable"){
        pop_var <- population_full$Population[population_full$Province_State==region & population_full$Country_Region==country_name & population_full$Admin2==county]
        pop_var <- pop_var[1]
      }else{
        pop_var<-population_full$Population[population_full$Province_State==region]
      }
      
    }else{
      if(region!="Not applicable"){
        pop_var <- population_full$Population[population_full$Province_State==region & population_full$Country_Region==country_name]
        pop_var <- pop_var[1]
      }else{
        pop_var<-population_full$Population[population_full$Country_Region==country_name]
      }
    }
    
    #======================================================================================================
    #======================================================================================================
    
    if(country_name=="United States"){
      if(county!="Not applicable"){
        jhu_dataset<-jhu_dataset %>%
          group_by(city) %>%
          mutate(NewCases = Confirmed - lag(Confirmed, default = first(Confirmed)))
        jhu_dataset <- jhu_dataset %>%
          dplyr::group_by(city) %>%
          dplyr::mutate(fourteen_day_agg = rollapply(NewCases, 14, sum, align="right", fill= NA))
        
      }else{
        jhu_dataset<-jhu_dataset %>%    
          mutate(NewCases = Confirmed - lag(Confirmed, default = first(Confirmed)))
        jhu_dataset <- jhu_dataset %>%
          dplyr::mutate(fourteen_day_agg = rollapply(NewCases, 14, sum, align="right", fill= NA))
      }
    }else{
      if(region!="Not applicable"){
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
    }
    
    #===============================================================================================
    if(country_name=="United States"){
      if(county!="Not applicable"){
        jhu_dataset<-jhu_dataset %>%
          group_by(city) %>%
          mutate(NewCases_u = Confirmed_u - lag(Confirmed_u, default = first(Confirmed_u)))
        jhu_dataset <- jhu_dataset %>%
          dplyr::group_by(city) %>%
          dplyr::mutate(fourteen_day_agg_u = rollapply(NewCases_u, 14, sum, align="right", fill= NA))
        
      }else{
        jhu_dataset<-jhu_dataset %>%    
          mutate(NewCases_u = Confirmed_u - lag(Confirmed, default = first(Confirmed)))
        jhu_dataset <- jhu_dataset %>%
          dplyr::mutate(fourteen_day_agg_u = rollapply(NewCases_u, 14, sum, align="right", fill= NA))  
      }
    }else{
      if(region!="Not applicable"){
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
    }
    #======================================================================================================
    #======================================================================================================
    jhu_dataset_agg <- jhu_dataset[!is.na(jhu_dataset$fourteen_day_agg),]
    jhu_dataset_agg <- jhu_dataset_agg %>% mutate_if(is.character,as.factor)
    if(county!="Not applicable"){
      jhu_dataset_agg$city <- as.factor(jhu_dataset_agg$city)
    }
    #======================================================================================================
    jhu_dataset_agg_u <- jhu_dataset[!is.na(jhu_dataset$fourteen_day_agg_u),]
    jhu_dataset_agg_u <- jhu_dataset_agg_u %>% mutate_if(is.character,as.factor)
    if(county!="Not applicable"){
      jhu_dataset_agg_u$city <- as.factor(jhu_dataset_agg_u$city)
    }
    #======================================================================================================
    #======================================================================================================
    if(country_name=="United States"){
      if(county!="Not applicable"){
        fac_aggregate_cases <- jhu_dataset_agg$fourteen_day_agg[which(jhu_dataset_agg$city==county & as.Date(jhu_dataset_agg$date)==as.Date(date))]
      }else{
        fac_aggregate_cases <- jhu_dataset_agg$fourteen_day_agg[as.Date(jhu_dataset_agg$date)==as.Date(date)]
      }
    }else{
      if(region!="Not applicable"){
        fac_aggregate_cases <- jhu_dataset_agg$fourteen_day_agg[which(jhu_dataset_agg$region==region & as.Date(jhu_dataset_agg$date)==as.Date(date))]
      }else{
        fac_aggregate_cases <- jhu_dataset_agg$fourteen_day_agg[as.Date(jhu_dataset_agg$date)==as.Date(date)]
      }
    }
    
    #======================================================================================================
    if(country_name=="United States"){
      if(region!="Not applicable"){
        fac_aggregate_cases_u <- jhu_dataset_agg$fourteen_day_agg_u[which(jhu_dataset_agg_u$city==county & as.Date(jhu_dataset_agg_u$date)==as.Date(date))]
      }else{
        fac_aggregate_cases_u <- jhu_dataset_agg$fourteen_day_agg_u[as.Date(jhu_dataset_agg_u$date)==as.Date(date)]
      }
    }else{
      if(region!="Not applicable"){
        fac_aggregate_cases_u <- jhu_dataset_agg$fourteen_day_agg_u[which(jhu_dataset_agg_u$region==region & as.Date(jhu_dataset_agg_u$date)==as.Date(date))]
      }else{
        fac_aggregate_cases_u <- jhu_dataset_agg$fourteen_day_agg_u[as.Date(jhu_dataset_agg_u$date)==as.Date(date)]
      }
    }
    #======================================================================================================
    #======================================================================================================
    fac_aggregate_cases_per_pop <- fac_aggregate_cases/pop_var 
    fac_aggregate_cases_per_pop_u <- fac_aggregate_cases_u/pop_var 
    #======================================================================================================
    #======================================================================================================
    #mask <- read.csv("C:/Users/shrey/Downloads/covid/mask.csv") #Ref: https://www.epa.gov/sciencematters/epa-researchers-test-effectiveness-face-masks-disinfection-methods-against-covid-19
    #mask <- read.csv(curl("https://raw.githubusercontent.com/Volunteer-Collab/risk_calculator/main/Risk_calculator/risk_calculator_simple/custom_dataset/mask.csv?token=ACZDUYEEPMIQ2GRZNQXAPYLBBP6K6"))
    mask<- mask %>% mutate_if(is.character,as.factor)
    mask$risk <- 1-mask$FFE
    summary(mask)
    #vaccine_eff <- read.csv("C:/Users/shrey/Downloads/covid/vaccine_eff.csv", sep = ",") #Ref: https://yourlocalepidemiologist.substack.com/p/vaccine-table-update-lots-on-delta
    #vaccine_eff <- read.csv(curl("https://raw.githubusercontent.com/Volunteer-Collab/risk_calculator/main/Risk_calculator/risk_calculator_simple/custom_dataset/vaccine_eff.csv?token=ACZDUYEVXTJVJEBGNSWYYV3BBP6RI"))
    vaccine_eff[is.na(vaccine_eff)] <- 0
    vaccine_eff$vaccine <- as.factor(vaccine_eff$vaccine)
    
    #================data pulling script======================== 
    dataset_url = paste0(paste0("https://raw.githubusercontent.com/Global-Health-Research-Collective/covARC/main/data/Country/processed/", str_replace(country_name," ", "%20")),".csv")
    dataset_url = str_replace(dataset_url, " ", "%20")
    variants_dataset <- read_csv(curl(dataset_url))
    variants_dataset$Date <- as.Date(variants_dataset$Date)
    #variants_dataset <- variants_dataset[-c(2,3)]
    variants_date = variants_dataset[variants_dataset$Date==as.Date(date)-31,]
    variants_date[is.na(variants_date)] <- 0
    print(as.Date(date)-31)
    print(variants_date$Alpha)
    #variants_date <- variants_date[-c(2)]
    
    #===============Alpha Variant===========================================
    if (!is_empty(variants_date$Alpha)) {
      factor_variant_alpha = variants_date$Alpha
    } else {factor_variant_alpha = 0}
    
    #===============Beta Variant============================================
    #variants_date$prevalence_gaussian5_b.1.351 = variants_date$prevalence_gaussian5_b.1.351+variants_date$prevalence_gaussian5_b.1.351.2+variants_date$prevalence_gaussian5_b.1.351.3
    if(!is_empty(variants_date$Beta)){
      factor_variant_beta = variants_date$Beta
    } else {factor_variant_beta = 0}
    
    #===============Gamma Variant===========================================
    #variants_date$prevalence_gaussian5_p.1 = variants_date$prevalence_gaussian5_p.1+variants_date$prevalence_gaussian5_p.1.1+variants_date$prevalence_gaussian5_p.1.2+variants_date$prevalence_gaussian5_p.1.3+variants_date$prevalence_gaussian5_p.1.4+variants_date$prevalence_gaussian5_p.1.6+variants_date$prevalence_gaussian5_p.1.7+variants_date$prevalence_gaussian5_p.1.8
    if (!is_empty(variants_date$Gamma)){
      factor_variant_gamma = variants_date$Gamma
    } else {factor_variant_gamma = 0}
    
    #===============Delta Variant===========================================
    #variants_date$prevalence_gaussian5_b.1.617.2 = variants_date$prevalence_gaussian5_b.1.617.2+variants_date$prevalence_gaussian5_ay.2+variants_date$prevalence_gaussian5_ay.3+variants_date$prevalence_gaussian5_ay.3.1+variants_date$prevalence_gaussian5_ay.4+variants_date$prevalence_gaussian5_ay.5+variants_date$prevalence_gaussian5_ay.6+variants_date$prevalence_gaussian5_ay.7+variants_date$prevalence_gaussian5_ay.9+variants_date$prevalence_gaussian5_ay.10+variants_date$prevalence_gaussian5_ay.11+variants_date$prevalence_gaussian5_ay.12
    if (!is_empty(variants_date$Delta)) {
      factor_variant_delta = variants_date$Delta
    } else {factor_variant_delta = 0}
    
    #===============omicron Variant===========================================
    if (!is_empty(variants_date$Omicron)) {
      factor_variant_omicron = variants_date$Omicron
    } else {factor_variant_omicron = 0}
    
    #====================Mask Type Processing==================================
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
    fac_vaccine_omicron = 1-(vaccine_eff$omicron[which(vaccine_eff$vaccine == type_vaccine_dose)])
    
    fac_vaccine_normal_u = 1-(vaccine_eff$normal_u[which(vaccine_eff$vaccine == type_vaccine_dose)]) #1- 80-95% efficacy
    fac_vaccine_alpha_u = 1-(vaccine_eff$alpha_u[which(vaccine_eff$vaccine == type_vaccine_dose)])
    fac_vaccine_beta_u = 1-(vaccine_eff$beta_u[which(vaccine_eff$vaccine == type_vaccine_dose)])
    fac_vaccine_gamma_u = 1-(vaccine_eff$gamma_u[which(vaccine_eff$vaccine == type_vaccine_dose)])
    fac_vaccine_delta_u = 1-(vaccine_eff$delta_u[which(vaccine_eff$vaccine == type_vaccine_dose)])
    fac_vaccine_omicron_u = 1-(vaccine_eff$omicron_u[which(vaccine_eff$vaccine == type_vaccine_dose)])
    
    
    if(past_covid_infection=="Yes"){
      fac_vaccine_alpha=0.098*fac_vaccine_alpha
      fac_vaccine_beta=0.143*fac_vaccine_beta
      fac_vaccine_delta=0.08*fac_vaccine_delta
      fac_vaccine_omicron=0.44*fac_vaccine_omicron
      
      fac_vaccine_alpha_u=0.098*fac_vaccine_alpha_u
      fac_vaccine_beta_u=0.143*fac_vaccine_beta_u
      fac_vaccine_delta_u=0.08*fac_vaccine_delta_u
      fac_vaccine_omicron_u=0.44*fac_vaccine_omicron_u
    }
    
    
    #======================================================================================================
    #======================================================================================================
    fac_variants_u = factor_variant_normal*fac_vaccine_normal + factor_variant_alpha*fac_vaccine_alpha + factor_variant_beta*fac_vaccine_beta+ factor_variant_gamma*fac_vaccine_gamma+factor_variant_delta*fac_vaccine_delta+factor_variant_omicron*fac_vaccine_omicron
    #======================================================================================================
    fac_variants = factor_variant_normal*fac_vaccine_normal_u + factor_variant_alpha*fac_vaccine_alpha_u + factor_variant_beta*fac_vaccine_beta_u+ factor_variant_gamma*fac_vaccine_gamma_u+ factor_variant_delta*fac_vaccine_delta_u+factor_variant_omicron*fac_vaccine_omicron_u
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
      fac_aggregate_cases_per_pop <- fac_aggregate_cases_per_pop_u*fac_vaccine_normal_u
      risk_factor_outdoor_u = 0.05*people_passed_outdoor*(fac_aggregate_cases_per_pop_u*fac_mask)
      risk_factor_indoor_u = people_passed_indoor*(fac_aggregate_cases_per_pop_u*fac_mask)
    }
    #======================================================================================================
    #======================================================================================================
    cum_risk_factor <- risk_factor_outdoor+risk_factor_indoor
    
    cum_risk_factor_u <- risk_factor_outdoor_u+risk_factor_indoor_u
    
    
    
    print(cum_risk_factor_u)
    
    print(cum_risk_factor)
    
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
      hosp_risk_lower=1.5*hosp_risk_lower
      hosp_risk_upper=2.3*hosp_risk_upper
      death_risk_lower = death_risk_lower*1.5
      death_risk_upper = death_risk_upper*2.3
    }
    
    if(!is_empty(variants_date$Alpha)){
      hosp_risk_lower=1.5*hosp_risk_lower
      hosp_risk_upper=1.6*hosp_risk_upper
      death_risk_lower=1.4*death_risk_lower
      death_risk_upper=1.7*death_risk_upper
    }
    
    if(!is_empty(variants_date$Gamma)){
      #  hosp_risk_lower=1.5*hosp_risk_lower
      #  hosp_risk_upper=1.6*hosp_risk_upper
      death_risk_lower=1.2*death_risk_lower
      death_risk_upper=1.9*death_risk_upper
    }
    if(!is_empty(variants_date$Delta)){
      hosp_risk_lower=1.9*hosp_risk_lower
      hosp_risk_upper=3*hosp_risk_upper
      death_risk_lower=1.5*death_risk_lower
      death_risk_upper=3.3*death_risk_upper
    }
    
    
    hosp_risk_lower<-ifelse (type_vaccine_dose=="Moderna (Dose 1)" | type_vaccine_dose=="Pfizer (Dose 1)" | type_vaccine_dose=="Astrazeneca (Dose 1)", 0.1*hosp_risk_lower, 0.7*hosp_risk_lower)
    hosp_risk_upper<-ifelse (type_vaccine_dose=="Moderna (Dose 1)" | type_vaccine_dose=="Pfizer (Dose 1)" | type_vaccine_dose=="Astrazeneca (Dose 1)", 0.1*hosp_risk_upper, 0.7*hosp_risk_upper)
    hosp_risk_lower<-ifelse (type_vaccine_dose=="Moderna (Dose 1)" | type_vaccine_dose=="Pfizer (Dose 1)" | type_vaccine_dose=="Astrazeneca (Dose 1)", 0.1*death_risk_lower, 0.7*death_risk_lower)
    hosp_risk_lower<-ifelse (type_vaccine_dose=="Moderna (Dose 1)" | type_vaccine_dose=="Pfizer (Dose 1)" | type_vaccine_dose=="Astrazeneca (Dose 1)", 0.1*death_risk_upper, 0.7*death_risk_upper)
    
    if(!is_empty(variants_date$Omicron)){
      hosp_risk_lower=0.313636363*hosp_risk_lower
      hosp_risk_upper=0.327272727*hosp_risk_upper
      death_risk_lower=0.011*death_risk_lower
      death_risk_upper=0.011*death_risk_upper
    }
    
    risk_factor_indoor = risk_factor_indoor*100
    risk_factor_outdoor = risk_factor_outdoor*100
    risk_factor_outdoor_u = risk_factor_outdoor_u*100
    risk_factor_indoor_u = risk_factor_indoor_u*100
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
    
    risk_factor_outdoor #Lower value of indoor risk value
    risk_factor_indoor #Lower value of outdoor risk value
    
    risk_factor_indoor_u #Upper value of indoor risk value
    risk_factor_outdoor_u #Upper value of outdoor risk value
    
    cum_risk_factor_u #Upper value of risk factor
    cum_risk_factor #Lower value of risk factor
    
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
    
    print("Outdoor")
    risk_factor_outdoor = convert_values(risk_factor_outdoor)
    risk_factor_outdoor_u = convert_values(risk_factor_outdoor_u)
    print("Indoor")
    risk_factor_indoor = convert_values(risk_factor_indoor)
    risk_factor_indoor_u = convert_values(risk_factor_indoor_u)
    print("Cumulative")
    cum_risk_factor = convert_values(cum_risk_factor)
    cum_risk_factor_u = convert_values(cum_risk_factor_u)
    print("Hosp")
    hosp_risk_lower = convert_values(hosp_risk_lower)
    hosp_risk_upper = convert_values(hosp_risk_upper)
    print("death")
    death_risk_lower = convert_values(death_risk_lower)
    death_risk_upper = convert_values(death_risk_upper)
    
    too_low_message = "Low to none"
    
    if (cum_risk_factor_u > 0 && cum_risk_factor > 0) {
      total_chance_str = paste('1/', cum_risk_factor, ' to 1/', cum_risk_factor_u, sep="")
    }
    else {
      total_chance_str = too_low_message
    }
    if (risk_factor_indoor_u > 0 && risk_factor_indoor > 0) {
      indoor_risk_str = paste('1/', risk_factor_indoor_u, ' to 1/', risk_factor_indoor, sep="")
    }
    else {
      indoor_risk_str = too_low_message
    }
    
    if (risk_factor_outdoor_u > 0 && risk_factor_outdoor > 0) {
      outdoor_risk_str = paste('1/', risk_factor_outdoor_u, ' to 1/', risk_factor_outdoor, sep="")
    }
    else {
      outdoor_risk_str = too_low_message
    }
    
    if (hosp_risk_lower > 0 && hosp_risk_upper > 0) {
      hosp_risk_str = paste('1/', hosp_risk_lower, ' to 1/', hosp_risk_upper, sep="")
    }
    else {
      hosp_risk_str = too_low_message
    }
    
    if (death_risk_lower > 0 && death_risk_upper > 0) {
      death_risk_str = paste('1/', death_risk_lower, ' to 1/', death_risk_upper, sep="")
    }
    else {
      death_risk_str = too_low_message
    }
    return(c(total_chance_str, indoor_risk_str, outdoor_risk_str, hosp_risk_str, death_risk_str))
  })
  
  convert_values <- function(val) {
    print(val)
    if(val <= 0) {
      return(0)
    } else {
      val = signif(1/(val/100), 2)
      frac = prettyNum(val, big.mark = ",", scientific=FALSE)
      return(frac)
    }
  }
  
  fill_country <- reactive({
    country = input$country
    reg_list = population_full$Province_State[population_full$Country_Region==country]
    if (length(reg_list) > 1) {
      regions = reg_list
    } else {
      regions = "Not applicable"
    }
    return(regions)
  })
  
  fill_county <- reactive({
    country = input$country
    region = input$region
    if (country == "United States") {
      county_list = population_full$Admin2[population_full$Province_State==region & population_full$Country_Region==country]
      if (length(county_list) > 1) {
        counties = county_list
      } else {
        counties = "Not applicable"
      }
    }
    else {
      counties = "Not applicable"
    }
    return(counties)
  })
  
  observeEvent(input$country, {
    regions = fill_country()
    updateSelectizeInput(session, "region", choices = regions, server = TRUE)
  })
  
  observeEvent(input$region, {
    counties = fill_county()
    updateSelectizeInput(session, "county", choices = counties, server = TRUE)
  })
  
  observeEvent(input$reset_button, {
    output$f_risk <- renderUI({
      h4("Please enter your information to get a calculated risk value.")
    })
  })
  
  output$f_risk <- renderUI({
    h4("Please enter your information to get a calculated risk value.")
  })
  
  observeEvent(input$submit_button, {
    output$f_risk <- renderUI({
      fluidRow(
        column(12, id = "valueboxblue",
               column(12, id = "title", "Total Chance of Infection"),
               column(12, id = "boxcontent", isolate(datasetInput()[1])),
        ),
        column(12, id = "valueboxblue",
               column(12, id = "title", "Indoor Risk of Infection"),
               column(12, id = "boxcontent", isolate(datasetInput()[2])),
        ),
        column(12, id = "valueboxblue",
               column(12, id = "title", "Outdoor Risk of Infection"),
               column(12, id = "boxcontent", isolate(datasetInput()[3])),
        ),
        column(12, id = "valueboxblue",
               column(12, id = "title", "Risk of Hospitalization"),
               column(12, id = "boxcontent", isolate(datasetInput()[4])),
        ),
        column(12, id = "valueboxblue",
               column(12, id = "title", "Risk of Death"),
               column(12, id = "boxcontent", isolate(datasetInput()[5])),
        ),
        tags$head(tags$style(
              "#title{
              font-size: 20px;
              padding: 10px;
              font-weight: 200;
              font-weight: 700;
              }
              #gauge{
              margin:0px;
              }
              #valueboxblue{
              background-color: #90bbde;
              border-radius: 10px;
              margin: 10px;
              }
              #boxcontent{
              font-size: 35px;
              padding: 10px;
              font-weight: 700;
              }
              "
        )),
      )
    })
  })
  
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)
