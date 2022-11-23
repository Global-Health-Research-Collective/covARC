import pandas as pd
import glob
import gc
from os.path import basename

files = glob.glob('*.csv')

for file in files:
    df = pd.read_csv(str(file), names = ['index','date', 'fac_aggregate_cases', 'fac_aggregate_cases_per_pop', 'cum_risk_factor', 'cum_risk_factor_u', 'hosp_risk_lower', 'hosp_risk_upper', 'death_risk_lower', 'death_risk_upper', 'f_risk'])
    df = df.iloc[1:]
    print(df.head())
    df = df.drop_duplicates(subset='date', keep="last")
    print(df.head())
    df.to_csv(str(basename(file)).split('.')[0]+'_fixed.csv', index = False)
    gc.collect()