import io
import os
import glob
from datetime import date
from datetime import datetime, timedelta
import pandas as pd
import gc

#os.system('cd COVID-19 && git pull')
#os.system('cd ..')

list_ = []

fields = ['CountryName','CountryCode','RegionName','RegionCode','Date','ConfirmedCases','ConfirmedDeaths','PopulationVaccinated']

df = pd.read_csv('https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_nat_latest_combined.csv', usecols = fields)
#df = pd.read_csv('OxCGRT_nat_latest_combined.csv', usecols = fields)

countries = df['CountryName'].unique()

for i in range(0,len(countries)):
        df_temp = df.loc[df['CountryName']==countries[i]]
        print(df_temp.head())
        df_temp.to_csv(os.path.join('ox_filtered/',str(countries[i])+'.csv'), index=False)
        gc.collect()
