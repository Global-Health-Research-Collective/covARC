# CovARC
CovARC or COVID-19 Activity Risk Calculator is a [web application](https://realsciencecommunity.shinyapps.io/riskcalculator/) developed unde the [Global health Research Collective](https://globalhealthresearchcollective.org/) that aims to develop a system that can by used by the general public for estimating an individual's risk of infection, hospitalization and death when carrying out a day to day activity [[Paper](https://www.researchsquare.com/article/rs-2372205/v1)].

CovARC extracts the data regularly from the [COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University](https://github.com/CSSEGISandData/COVID-19) and [GISAID variants](https://gisaid.org/hcov19-variants/) through GitHub Actions and pre-processed them to store in this repository. The data is then utillized by the risk calculator shinyapps web application to calculate individual risk. 

![alt text](https://github.com/Global-Health-Research-Collective/covARC/blob/main/illustrations/screenshot.png)

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

## Citation
```
@misc{https://doi.org/10.48550/arxiv.2212.05035,
doi = {10.48550/ARXIV.2212.05035},
url = {https://arxiv.org/abs/2212.05035},
author = {Natraj, Shreyasvi and Bhide, Malhar and Yap, Nathan and Liu, Meng and Seth, Agrima and Berman, Jonathan and Glorioso, Christin},
keywords = {Computers and Society (cs.CY), Information Retrieval (cs.IR), Numerical Analysis (math.NA), Methodology (stat.ME), FOS: Computer and information sciences, FOS: Computer and information sciences, FOS: Mathematics, FOS: Mathematics},
title = {COVID-19 Activity Risk Calculator as a Gamified Public Health Intervention Tool},
publisher = {arXiv},
year = {2022},
copyright = {arXiv.org perpetual, non-exclusive license}
}
```
