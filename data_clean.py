import pandas as pd
import re

lotto = pd.read_csv('test_crawl.csv')
lotto

lotto['prize'].apply(lambda x: re.sub('$','',x))

prize_col = lotto['prize']

prize_col.apply(lambda x: re.sub('$','',x))

df1['Avg_Annual'] = df1['Avg_Annual'].str.replace('$', '')
df1['Avg_Annual'] = df1['Avg_Annual'].astype(int)

lotto['prize']= lotto['prize'].str.replace(',', '')
lotto['prize']= lotto['prize'].str.replace('$', '')
lotto['prize']= lotto['prize'].astype(int)

lotto.sort_values(['date', 'prize'])

date = lotto.groupby('date')
