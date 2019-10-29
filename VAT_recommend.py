# -*- coding: utf-8 -*-
"""
Created on Thu Oct 24 12:14:47 2019

@author: MSmirnov
"""
import time
start = time.time()


import pandas as pd
colnames=['COMPANY', 'POS_TYPE', 'CONTRACTDATUM', 'CONTRACTNO', 'COUNTERPARTY', 'RELNAME', 'CONS_IND', 'LOCATION', 'LOCNAME', 'VAT_NUMBER', 'ADDRESS_LINE', 'ZIP_CD', 'PLACE', 'COUNTRYCD']

import os

os.chdir('C:\\Users\\msmirnov\\Documents\\Проект_адреса')



df_cefctr = pd.read_csv('cefctr.csv', sep=',',encoding='ansi', names=colnames)
df_ltdctr_1 = pd.read_csv('ltdctr first_.csv', sep=',',encoding='ansi', names=colnames)
df_ltdctr_2 = pd.read_csv('ltdctr second_.csv', sep=',',encoding='ansi', names=colnames)

df = pd.concat([df_cefctr, df_ltdctr_1, df_ltdctr_2])

df['aggregat_address'] = df['COUNTERPARTY']+" "+ df['RELNAME'] + " " + df['LOCATION']+ " "+ df ['LOCNAME'] + " "+ df['ADDRESS_LINE']+ " " +df['ZIP_CD']+ " " +df['PLACE']  + " " + df['COUNTRYCD']

# создали новый data frame без строк, содержащих NaN
df_removeNaN= df.dropna()

# убираем дублированные адреса
df_no_duplicated = df_removeNaN.drop_duplicates(['aggregat_address', 'VAT_NUMBER'])




 #  загружаем библиотеки для расчета попарного косинусного расстояния  
#import nltk
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics.pairwise import cosine_similarity

# инициализируем список адресов, для которого попарно будет рассчитываться косинусное расстояние
the_corpus = df_no_duplicated['aggregat_address']
# Vectorise the data
vec = TfidfVectorizer()

# матрица количества вхождений слова в конкретный документ (адрес) (tfidf)
X = vec.fit_transform(the_corpus)

# рассчитываем попарное косинусное расстояние между документами (формируется квадратная матрица для элементов "каждый-с -с каждым") 
S = cosine_similarity(X)

# создаем data frame на базе матрицы с косинусными расстояниями с названиями колонок и рядов, которые являются сравниваемыми адресами
df_sim= pd.DataFrame(data=S, index=df_no_duplicated['aggregat_address'], columns=df_no_duplicated['aggregat_address'])

# создаем data frame, где первая колонка является списком адресов (она является дубликатом рядов). Возможно, это избыточно.
address_list=  pd.DataFrame(df_sim.index.values, columns=['address'])
df_sim= pd.concat([address_list.reset_index(drop=True), df_sim.reset_index(drop=True)], axis=1)

# очищаем память
del df, S, df_cefctr, df_ltdctr_1, df_ltdctr_2
import gc
gc.collect()
 
# формируем data frame из "широкого" формата в "динный", т.е. "квадратный" data frame преобразовывается, для того, 
# чтобы можно было провести сортировку "наборов" адресов с большим коэф. подобия (>0.8) для каждого адреса
address_group= pd.melt(df_sim, id_vars=['address'],var_name='address_in_group', value_name='sim_coef')
address_group= address_group[address_group['sim_coef']>0.8]
 

 
 
 # сводная таблица сколько раз в общей выборке встречается пара адрес-VAT номер
addr_vat_composite_key = df_removeNaN.groupby(['aggregat_address', 'VAT_NUMBER']).size().reset_index(name='counts')
 
 # объединяем сгруппированные адреса с номерами VAT
address_group= pd.merge(address_group, addr_vat_composite_key, how='left', left_on=['address'], right_on=['aggregat_address'])
  
 # расчитываем кол-во уникальных VAT номеров внутри каждой группы
address_group_VAT_count= pd.DataFrame(address_group.groupby('address_in_group').VAT_NUMBER.nunique())
  
 # выбораем только те группы, где VAT номеров больше 1
address_group_VAT_count_sorted = address_group_VAT_count[address_group_VAT_count['VAT_NUMBER']>1]
 
 
 # выбираем только те группы, в которых более 1 адреса VAT
address_group = address_group[address_group['address_in_group'].isin(address_group_VAT_count_sorted.index)]
 

# меняем порядок колонок и названия для наглядности
address_group = address_group.sort_values(['address_in_group', 'sim_coef'], ascending=[True, False])
address_group = address_group.drop(['aggregat_address'], axis=1)
address_group = address_group.reindex(columns= ['address_in_group', 'address', 'sim_coef', 'VAT_NUMBER', 'counts'])



end = time.time()
print(end - start)    
        
