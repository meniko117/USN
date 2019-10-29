# -*- coding: utf-8 -*-
"""
Редактор Spyder

Это временный скриптовый файл.
"""

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
df_no_duplicated = df_removeNaN.drop_duplicates('aggregat_address')

text = df_no_duplicated['aggregat_address']


# создаем матрицу TF IDF (входимость каждого слова в каждом адресе в общий массив слов)
import pandas as pd
from sklearn.feature_extraction.text import CountVectorizer

vec = CountVectorizer()

# дали матрице с признаками неказистое название "X"
X = vec.fit_transform(text)



# data frame c названиями признаков и кол-вом раз, сколько признак встречается в общем корпусе объектов (адресов)
dt_features=pd.DataFrame(X.toarray(), columns=vec.get_feature_names()).sum(axis=0)


# фильтруем список признаков, которые встречаются более 5 раз 

test_features = pd.DataFrame(dt_features)[pd.DataFrame(dt_features)>5].dropna().axes[0].tolist()

# удаляем признаки, которые являются цифрами
features_not_digits =[item for item in test_features if item.isalpha()]

import scipy as scipy

# получаем матрицу частоты признаков, которые встречаются в адресах более заданного количества раз и не являются цифрами
# матрица формируется на основе data frame, где названия колонок это отфильтрованные признаки
X_new=scipy.sparse.csr_matrix(pd.DataFrame(X.toarray(), columns=vec.get_feature_names())[features_not_digits])



# проводим кластеризацию методом MeanShift
import numpy as np
from sklearn.cluster import MeanShift, estimate_bandwidth

# кол-во адресов, которое хотим клсетирзовать из общей выборки (время работы для 12 808 - ок. 5 часов; 2000 адресов - менее 30 мин)
n =500

bandwidth = estimate_bandwidth(X_new[0:n, :].toarray(),quantile=0.7, n_samples=100) #, n_samples=100

clustering = MeanShift(bandwidth=bandwidth).fit(X_new[0:n, :].toarray())

# делаем df с 2-мя колонками с номером кластера для каждого адреса
cluster_map = pd.DataFrame()
cluster_map['data_index'] = df_no_duplicated['aggregat_address'][0:n]
cluster_map['cluster'] = clustering.labels_


cluster_map.describe()

# проверяем, что вошло в кластер
cluster_map[cluster_map.cluster == 1]

cluster_map[cluster_map.cluster == 0]


# кластеризация методом hdbscan
import hdbscan

import time
start = time.time()
data = X_new[0:n, :].toarray()

clusterer = hdbscan.HDBSCAN(min_cluster_size = 2, min_samples=1)
cluster_labels = clusterer.fit_predict(data)

end = time.time()
print(end - start)

# создаем таблицу 
cluster_map_hdbscan = pd.DataFrame()
cluster_map_hdbscan['data_index'] = df_no_duplicated['aggregat_address'][0:n]
cluster_map_hdbscan['cluster'] = cluster_labels


cluster_map_hdbscan[cluster_map_hdbscan['cluster']==38]
cluster_map_hdbscan['cluster'].nunique()


cluster_map_hdbscan_grouped = cluster_map_hdbscan.sort_values(by ='cluster')

