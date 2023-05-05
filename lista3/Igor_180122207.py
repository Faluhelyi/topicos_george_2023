######################
### Data Wrangling ###
######################
import pandas as pd
import numpy as np

INPUT_DIR = 'C:/Users/Igor/Desktop/topicos_george_2023/git_repo/lista3'

df = pd.read_excel(f'{INPUT_DIR}/WorldHappinessReport.xls')


# Drop State of Palestine cus of missing and the Dystopia column
df = df.drop(index = 98, axis = 0)
df = df.drop('Dystopia', axis = 1)

# Normalize variables 
df = (df.set_index('Country name')-df.set_index('Country name').min())/ (df.set_index('Country name').max() - df.set_index('Country name').min())

# Random sampling
best = df.sort_values('Score', ascending=False).head(69)
worst = df.sort_values('Score', ascending=False).tail(69)

best = best.sample(n=20, random_state=0)
worst = worst.sample(n=20, random_state=0)

best['Country name'] = best.index
worst['Country name'] = worst.index

full = pd.concat([best, worst]).to_excel(f'{INPUT_DIR}/sample_whr.xlsx', index = False) #dataset w/ full information


print(best)
print(worst)

for col in df.columns[:-1]:
    full.loc[df.sample(frac=0.03571428571428571).index, col] = np.nan

full.to_excell(f'{INPUT_DIR}/missing_whr.xlsx', index = False) #dataset w/ 25% missing information