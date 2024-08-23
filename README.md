# COVID-19 Activity Risk Calculator (CovARC)

CovARC or COVID-19 Activity Risk Calculator is a [web application](https://realsciencecommunity.shinyapps.io/riskcalculator/) developed unde the [Global health Research Collective](https://globalhealthresearchcollective.org/) that aims to develop a system that can by used by the general public for estimating an individual's risk of infection, hospitalization and death when carrying out a day to day activity. [Nature Scientific Reports Paper](https://www.nature.com/articles/s41598-023-40338-8).

CovARC extracts the data regularly from the [COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University](https://github.com/CSSEGISandData/COVID-19) and [GISAID variants](https://gisaid.org/hcov19-variants/) through GitHub Actions and pre-processed them to store in this repository. The data is then utillized by the risk calculator shinyapps web application to calculate individual risk. 

![alt text](https://github.com/Global-Health-Research-Collective/covARC/blob/main/illustrations/screenshot.png)

## Description
The repository consists of several folders which are described as follows:
- `data/Country/processed` folder consists of data related to prevalence of different variants in different countries around the world.
- `filtered` folder consists of filtered data from the [CSSE github repository](https://github.com/CSSEGISandData/COVID-19) that is regularly updated through the `pre_proc.py` script (requires only pandas installation)
- The `risk_calculator_code` consists of 3 R scripts, `app.R` is the source code for the shiny app which is currently being used as a web-interface, `runner.R` which is just the code for the initial local version of CovARC, `plots.R` which used the data from `updated_final_csv` folder to create plots stored in `covid_plots` folder.

The R code was run using [RStudio](https://posit.co/download/rstudio-desktop/) after installing the related dependencies using the IDE.

The risk calculator can be used to calculate individual's risk of catching COVID-19 infection when carrying out for day-to-day activities. The calculator considers the number of people the user will pass (in indoor and outdoor space) when carrying out the daily activity to estimate several risks associated with carrying out the daily activity. Apart from the number of people passed indoors and outdoors, the system also considers the following inputs:
- Country
- Region
- County
- Current Date 
- Type of mask worn
- Vaccines type and dosage
- Past COVID-19 infection
- Age
- Gender
- Past Chronic Illness

Using this we calculate the risk increase due to rise in number of COVID cases or risk reduction due to usage of mask or vaccination. A flow diagram for the same can be observed as follows:

![alt text](https://github.com/Global-Health-Research-Collective/covARC/blob/main/illustrations/wireframe_page-0001.jpg)

## Citation (Bibtex Format)
```
﻿@Article{Natraj2023,
author={Natraj, Shreyasvi
and Bhide, Malhar
and Yap, Nathan
and Liu, Meng
and Seth, Agrima
and Berman, Jonathan
and Glorioso, Christin},
title={COVID-19 activity risk calculator as a gamified public health intervention tool},
journal={Scientific Reports},
year={2023},
month={Aug},
day={11},
volume={13},
number={1},
pages={13056},
abstract={The Coronavirus disease 2019 (COVID-19) pandemic, caused by the virus severe acute respiratory syndrome coronavirus 2 (SARS-CoV-2), has impacted over 200 countries leading to hospitalizations and deaths of millions of people. Public health interventions, such as risk estimators, can reduce the spread of pandemics and epidemics through influencing behavior, which impacts risk of exposure and infection. Current publicly available COVID-19 risk estimation tools have had variable effectiveness during the pandemic due to their dependency on rapidly evolving factors such as community transmission levels and variants. There has also been confusion surrounding certain personal protective strategies such as risk reduction by mask-wearing and vaccination. In order to create a simple easy-to-use tool for estimating different individual risks associated with carrying out daily-life activity, we developed COVID-19 Activity Risk Calculator (CovARC). CovARC is a gamified public health intervention as users can ''play with'' how different risks associated with COVID-19 can change depending on several different factors when carrying out routine daily activities. Empowering the public to make informed, data-driven decisions about safely engaging in activities may help to reduce COVID-19 levels in the community. In this study, we demonstrate a streamlined, scalable and accurate COVID-19 risk calculation system. Our study also demonstrates the quantitative impact of vaccination and mask-wearing during periods of high case counts. Validation of this impact could inform and support policy decisions regarding case thresholds for mask mandates, and other public health interventions.},
issn={2045-2322},
doi={10.1038/s41598-023-40338-8},
url={https://doi.org/10.1038/s41598-023-40338-8}
}
```
Please feel free to inform us in case of any issues or contact `gloriosoca@gmail.com` or `shreyasvi.natraj@unige.ch`
