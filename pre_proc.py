import io
import os
import glob
from datetime import date
from datetime import datetime, timedelta
import pandas as pd
import gc

#os.system('cd COVID-19 && git pull')
#os.system('cd ..')

new_date = date.today()
old_date = (datetime.today() - timedelta(days=548)).date()

df_dates = pd.date_range(old_date,new_date-timedelta(days=1),freq='d')
list_ = []

for i in range(0,len(df_dates)):
        print(str(df_dates[i].date().strftime('%m-%d-%Y')))
        df = pd.read_csv('COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/'+str(df_dates[i].date().strftime('%m-%d-%Y'))+'.csv')
        #df = pd.read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/'+str(df_dates[i].date().strftime('%m-%d-%Y'))+'.csv')        
        df = df.drop(['FIPS', 'Combined_Key','Recovered','Active'], axis = 1)
        df = df[df.columns[:-2]]
        #print(df.head())
        list_.append(df)
        gc.collect()

frame = pd.concat(list_)
frame[['Date', 'Time']] = frame['Last_Update'].str.split(' ', 1, expand=True)
frame = frame.drop(['Last_Update'],axis=1)

country_names = frame['Country_Region'].unique()
for i in range(0,len(country_names)):
        if country_names[i] == "Taiwan*":
                print(country_names[i])
                df_temp = frame.loc[frame['Country_Region'] == country_names[i]]
                df_temp = df_temp.drop(['Admin2'],axis = 1)
                df_temp.to_csv('filtered/Taiwan.csv',index = False)
        
        elif country_names[i] == "US":
                print(country_names[i])
                df_temp = frame.loc[frame['Country_Region'] == country_names[i]]
                region_names = df_temp['Province_State'].unique()
                for j in range(0,len(region_names)):
                        df_temp_county = df_temp.loc[df_temp['Province_State'] == region_names[j]]
                        df_temp_county.to_csv('filtered/US/'+str(region_names[j])+'.csv', index = False)
                        gc.collect()

        else:
                print(country_names[i])
                df_temp = frame.loc[frame['Country_Region'] == country_names[i]]
                df_temp = df_temp.drop(['Admin2'],axis = 1)
                df_temp.to_csv('filtered/'+str(country_names[i])+'.csv',index = False)
        gc.collect()
