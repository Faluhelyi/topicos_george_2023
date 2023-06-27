import pandas as pd
import numpy as np
import datetime
from dateutil.relativedelta import relativedelta

PATH = 'C:/Users/u00378/Desktop/topicos_george_2023/lista6'

df_y = pd.read_excel(f'{PATH}/BaseDados.xlsx', sheet_name='y')
df_x = pd.read_excel(f'{PATH}/BaseDados.xlsx', sheet_name='x')

df_y = df_y.rename(columns = {'Unnamed: 0':'date'})
df_x = df_x.rename(columns= {'Unnamed: 0':'date'})

df_y['date']= pd.to_datetime(df_y['date'])
df_x['date']= pd.to_datetime(df_x['date'])

df = df_y.merge(df_x, how = 'inner', on = 'date')
#df = df.set_index('date')

rot = df[np.abs(df['y1']) >= 10]
for i in pd.to_datetime(rot['date'].values):
    dates = [str((i - relativedelta(years = j)).date()) for j in range(1, 6)]
    a = df[df['date'].isin(dates)]
    d = str(i.date())
    a = pd.DataFrame(a.iloc[:,1:].mean())
    a = a.T
    a.insert(0, 'date', d)
    index = df[df['date'] == str(i.date())].index
    df.loc[index] = a.values

for i in np.where(pd.isnull(df))[0]:
    date = pd.to_datetime(df.loc[i]['date'].date())
    dates = [str((date - relativedelta(years = j)).date()) for j in range(1, 6)]
    a = np.mean(df[df['date'].isin(dates)].iloc[:, 1:]['x18'])
    b = np.random.normal(loc=0, scale=1, size=1)
    df.loc[i, 'x18'] = a
    #df['x18'] = df['x18'].replace(b[0:], a)

df.to_excel(f'{PATH}/BaseDados_ajustada.xlsx', index = False)