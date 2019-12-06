# -*- coding: utf-8 -*-
"""
Created on Thu Nov 28 10:27:52 2019

@author: MSmirnov
"""
import time
start = time.time()

import pandas as pd

# скачиваем таблицу c ОКП, где в колонке "Item" находятся "наименования", которым нужно прдидать весовые коэффициенты на основе анализа частей речи и падежей 
df_OKP = pd.read_csv('C:\\Users\\msmirnov\\Documents\\Проект Spark\\OKP_group_test.csv', sep=';',encoding='utf-8')


df_OKP['Item'] = df_OKP.fillna({'Item':''})

import pymorphy2
morph = pymorphy2.MorphAnalyzer()

# очистка названия в чеке от лишних символов

import string
from string import digits
import re
# !"#$%&'()*+,-./:;<=>?@[\]^_`{|}~
#s = "string ' ' ? 45826"
table = str.maketrans(dict.fromkeys(string.punctuation))  
#s.translate(table)

# очистка от цифр
remove_digits = str.maketrans('', '', digits)
#s.translate(remove_digits)

stop_words_list = ["перед", "килограмм" , "литр", "вес", "год"]

def remove_symbols (input_string):
    table = str.maketrans(dict.fromkeys(string.punctuation))  
    input_string = input_string.translate(table) # удаляем пунктуацию#    remove_digits = str.maketrans('', '', digits)
    input_string = input_string.translate(remove_digits) # удаляем цифры
    input_string = re.sub(r'\b\w{1,2}\b', '', input_string)# удаляем слова менее 2-х символов
    input_string =[word for word in input_string.split() if not any([phrase in word for phrase in stop_words_list])] # удаляем слова, входящие в список стоп-слов
    input_string = ' '.join(input_string) # конвертируем список в строку
    
    return input_string

def normal_fun (x):
    result = [morph.parse(item)[0].normal_form for item in x.split()]
    result =' '.join(result) # без этого проеобразования в строку, возвращает список элементов
    return result


# вернуть лист слов
def word_list(x):
    wl = []
    wl.extend([item for item in x.split(', ')])
    return wl

# включаем колонку, где все нормальные формы собраны в виде списка, а не строки
def normal_fun_list (x):
    result = [morph.parse(item)[0].normal_form for item in x.split()]
  # result =' '.join(result) # без этого проеобразования в строку, возвращает список элементов
    result = ', '.join(result)
    result = word_list(result)
    return result



# функция для возврата пустой строки, если аргумент "None", что может встретиться при попытке определить падеж, например, глагола или предлога
# равнозначно определению функции def xstr(s): return s or ""
xstr = lambda s: s or ""



def pos_fun (x):
    result = ( [morph.parse(item)[0].tag.POS for item in x.split()]) #+ [morph.parse(item)[0].tag.case for item in x.split()]
   # grammems = [ grammem + " " + grammem for grammem in result]
   
    result = [xstr(item) for item in result]
    result = ', '.join(result)
    result = word_list(result)
    return result

def animacy_fun (x):
    
    result = ( [morph.parse(item)[0][1].animacy for item in x.split()]) #+ [morph.parse(item)[0].tag.case for item in x.split()]
    result = [xstr(item) for item in result]
    result = ', '.join(result)
    result = word_list(result)
    return result



# функция явяляющаяся результатом работы 3-х функций: приведение к нормальной форме, удаление симоволв пунктуации, определение части речи
def clean (x):
    return pos_fun(remove_symbols(normal_fun(x)))



def case_fun (x):
    result = [morph.parse(item)[0].tag.case for item in x.split()]
    result = [xstr(item) for item in result]
    result = ', '.join(result)
    result = word_list(result)
    return result



#def word_multiplicator(a, b):
#    return [5 if a_val == 'NOUN' and b_val == 'nomn' else 3 if a_val == 'NOUN' and b_val == 'accs'  else 1 for a_val, b_val in zip(a, b)]


def word_multiplicator(a, b, c):
    result = [3 if a_val == 'NOUN' and (b_val == 'nomn' or b_val == 'accs') and c_val == 'inan'  
              else 2 if a_val == 'NOUN' and b_val == 'accs' and c_val == 'inan' 
              else 1 for a_val, b_val, c_val, in zip(a, b, c)]
    if result[0] == 3:
        result [0] = result [0]*2
    return result

def weighted_text_string (normal_form_list, multiplicator_list):
    result = sum([[a]*b for a,b in zip(normal_form_list, multiplicator_list)], [])
    return ' '.join(result)



df_OKP['clean_form'] = df_OKP['Item'].apply(remove_symbols)
df_OKP['normal_form'] = df_OKP['clean_form'].apply(normal_fun)
df_OKP['normal_form_list'] = df_OKP['normal_form'].apply(normal_fun_list) 
df_OKP['POS'] = df_OKP['normal_form'].apply(pos_fun)
df_OKP['animacy'] = df_OKP['normal_form'].apply(animacy_fun)
df_OKP['case'] = df_OKP['normal_form'].apply(case_fun)

# передача нескольких колонок-аргументов в функцию
df_OKP['multiplicator'] = df_OKP.apply(lambda x: word_multiplicator(x['POS'], x['case'], x['animacy']), axis =1)
#df_OKP['multiplicator'] = df_OKP.apply(lambda x: [1]*len(x['POS']), axis =1)
df_OKP['normal_form_weighted'] = df_OKP.apply(lambda x: weighted_text_string(x['normal_form_list'], x['multiplicator']), axis =1)

# оставляем нужные колонки
df_OKP_main = df_OKP [['Item', 'Group_number','normal_form_weighted'  ]]
df_OKP_main= df_OKP_main.dropna()
df_OKP_main= df_OKP_main.replace('', 'not defined')



# не нужно
# переставляем колонки для передачи в функции
#df_OKP['Group_name'] = df_OKP['Item']
#df_OKP['Item'] = df_OKP['normal_form_weighted']



#df_OKP= df_OKP.dropna()
#
#df_OKP= df_OKP.replace('', 'not defined')

#df_OKP['Item'].isnull().sum()

#df_OKP[['Item', 'Group_number', 'Group_name']].to_csv('C:\\Users\\msmirnov\\Documents\\Проект Spark\\OKP_group_test_weight.csv', sep=';',  index=False)









## функция для расчета косинусного расстояния
#
#from sklearn.feature_extraction.text import CountVectorizer
#from sklearn.metrics import pairwise_kernels
#
#
## формируем список с группами из ОКП, с которым будем сравнивать каждую позицию из чека
#target = df_OKP_main["normal_form_weighted"]
#
#
#def cos_sim_fun(item):
#    
#
#    item_list = []
#    item_list.append(item)
#    
#    #target = list(target)
#    vec = CountVectorizer() # в скобках можно поставить параметр analyzer='char', если важно подобие с учетом букв
#    vec.fit(target)
#    
#    vec.fit(item_list)
#
#    result = pairwise_kernels(vec.transform(item_list),
#                              vec.transform(target),
#                              metric='cosine')
#    result = list(result[0,:]) # матрицу с коэффицинтами преобразуем в list
#   
#    if max(result)>0 :
#        elem_index = max(range(len(result)), key=result.__getitem__) # индекс элемента с наибольшим коэффициентом коиснусного расстояния
#
#    else:
#        elem_index = None
#            
#    elem =  df_OKP_main["normal_form_weighted"].iloc[elem_index]
#    elem_origin = df_OKP_main["Item"].iloc[elem_index]
#    elem_group_number = df_OKP_main["Group_number"].iloc[elem_index]
#        
#    return  elem, elem_origin, elem_group_number


from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics.pairwise import cosine_similarity

def cos_sim_fun(item):
    

    documents = [item] + list(df_OKP_main["normal_form_weighted"])


# инициализируем список адресов, для которого попарно будет рассчитываться косинусное расстояние
    the_corpus = documents

# функция для векторизации данных
    vec = TfidfVectorizer()

# матрица количества вхождений слова в конкретный документ (адрес) (tfidf)
    X = vec.fit_transform(the_corpus)

# рассчитываем попарное косинусное расстояние между документами (формируется квадратная матрица для элементов "каждый-с -с каждым") 
    S = cosine_similarity(X[0:1], X)


    max_value_index= S.ravel().tolist().index(max(S.ravel().tolist()[1:52940]))
            
    elem =  df_OKP_main["normal_form_weighted"].iloc[max_value_index-1]
    elem_origin = df_OKP_main["Item"].iloc[max_value_index-1]
    elem_group_number = df_OKP_main["Group_number"].iloc[max_value_index-1]
        
    return  elem, elem_origin, elem_group_number

df_item = pd.read_csv('C:\\Users\\msmirnov\\Documents\\Проект Spark\\item_names.csv', sep=';',encoding='utf-8')




df_item['clean_form'] = df_item['Item'].apply(remove_symbols)
df_item['normal_form'] = df_item['clean_form'].apply(normal_fun)
df_item['normal_form_list'] = df_item['normal_form'].apply(normal_fun_list) 
df_item['POS'] = df_item['normal_form'].apply(pos_fun)
df_item['animacy'] = df_item['normal_form'].apply(animacy_fun)
df_item['case'] = df_item['normal_form'].apply(case_fun)

# передача нескольких колонок-аргументов в функцию
df_item['multiplicator'] = df_item.apply(lambda x: word_multiplicator(x['POS'], x['case'], x['animacy']), axis =1)
df_item['normal_form_weighted'] = df_item.apply(lambda x: weighted_text_string(x['normal_form_list'], x['multiplicator']), axis =1)

df_item [['группа ОКП (по косин расстоянию', 'оригинальное название группы ОКП', 'номер гурппы ОКП']] =df_item[0:99: ].apply(lambda x: cos_sim_fun(x['normal_form_weighted']), axis=1, result_type="expand")

df_item_result = df_item [['Item', 'multiplicator', 'группа ОКП (по косин расстоянию','оригинальное название группы ОКП', 'номер гурппы ОКП']]



end = time.time()
print(end - start)  



#a = ['NOUN', 'ADJF', 'NOUN',  'NOUN', None]
#b = ['accs',  '', 'accs', 'nomn', None]
#
#def word_multiplicator(a, b):
#    return [5 if a_val == 'NOUN' and b_val == 'nomn' or a_val == 'NOUN' and b_val == 'accs' else 1 for a_val, b_val in zip(a, b)]
#
#['yes' if v == 1 else 'no' if v == 2 else 'idle' for v in l]
#
#def word_multiplicator(a, b):
#    return [5 if a_val == 'NOUN' and b_val == 'nomn' else 3 if a_val == 'NOUN' and b_val == 'accs'  else 1 for a_val, b_val in zip(a, b)]
#
#normal_fun('водицей')





target = df_OKP_main["normal_form_weighted"]

item = 'хлеб'

from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics.pairwise import cosine_similarity

def cos_sim_fun(item):
    

    documents = [item] + list(df_OKP_main["normal_form_weighted"])


# инициализируем список адресов, для которого попарно будет рассчитываться косинусное расстояние
    the_corpus = documents

# функция для векторизации данных
    vec = TfidfVectorizer()

# матрица количества вхождений слова в конкретный документ (адрес) (tfidf)
    X = vec.fit_transform(the_corpus)

# рассчитываем попарное косинусное расстояние между документами (формируется квадратная матрица для элементов "каждый-с -с каждым") 
    S = cosine_similarity(X[0:1], X)


    max_value_index= S.ravel().tolist().index(max(S.ravel().tolist()[1:52940]))
            
    elem =  df_OKP_main["normal_form_weighted"].iloc[max_value_index-1]
    elem_origin = df_OKP_main["Item"].iloc[max_value_index-1]
    elem_group_number = df_OKP_main["Group_number"].iloc[max_value_index-1]
        
    return  elem, elem_origin, elem_group_number













# работает

documents = ['ножи круглые'] + list(df_OKP_main["normal_form_weighted"])


from sklearn.feature_extraction.text import TfidfVectorizer
tfidf_vectorizer = TfidfVectorizer()
tfidf_matrix = tfidf_vectorizer.fit_transform(documents)


from sklearn.metrics.pairwise import cosine_similarity

result =cosine_similarity(tfidf_matrix[0:1], tfidf_matrix)

result = result.ravel().tolist() # матрицу с коэффицинтами преобразуем в list
 #if max(result)>0 :
elem_index = max(range(1,len(result)), key=result.__getitem__) # индекс элемента с наибольшим коэффициентом коиснусного расстояния

#else:
#    elem_index = None
            
elem =  df_OKP_main["normal_form_weighted"].iloc[elem_index]
elem_origin = df_OKP_main["Item"].iloc[elem_index]
elem_group_number = df_OKP_main["Group_number"].iloc[elem_index]








documents = [item] + list(df_OKP_main["normal_form_weighted"])


# инициализируем список адресов, для которого попарно будет рассчитываться косинусное расстояние
the_corpus = documents

# функция для векторизации данных
vec = TfidfVectorizer()

# матрица количества вхождений слова в конкретный документ (адрес) (tfidf)
X = vec.fit_transform(the_corpus)

# рассчитываем попарное косинусное расстояние между документами (формируется квадратная матрица для элементов "каждый-с -с каждым") 
S = cosine_similarity(X[0:1], X)


max_value_index= S.ravel().tolist().index(max(S.ravel().tolist()[1:52940]))

# создаем data frame на базе матрицы с косинусными расстояниями с названиями колонок и рядов, которые являются сравниваемыми адресами
df_sim= pd.DataFrame(data=S,  index=the_corpus[0:1], columns=the_corpus)

# создаем data frame, где первая колонка является списком адресов (она является дубликатом рядов). Возможно, это избыточно, но наглядно.
address_list=  pd.DataFrame(df_sim.index.values, columns=['address'])
df_sim= pd.concat([address_list.reset_index(drop=True), df_sim.reset_index(drop=True)], axis=1)

# очищаем память
#del df, S, df_cefctr, df_ltdctr_1, df_ltdctr_2

# запускаем сборщик мусора
import gc
#gc.collect()
 
# формируем data frame из "широкого" формата в "динный", т.е. "квадратный" data frame преобразовывается, для того, 
# чтобы можно было провести сортировку "наборов" адресов с большим коэф. подобия (косинусное расстояние >0.8) для каждого адреса
address_group= pd.melt(df_sim, id_vars=['address'],var_name='address_in_group', value_name='sim_coef')
address_group= address_group[address_group['sim_coef']>0.2]

address_group=address_group.sort_values('sim_coef', ascending=False)

max(range(3,len(S.ravel().tolist())), key=result.__getitem__)

max_value_index= S.ravel().tolist().index(max(S.ravel().tolist()[1:52940]))

documents[max_value_index]
df_OKP_main["normal_form_weighted"][max_value_index-1]