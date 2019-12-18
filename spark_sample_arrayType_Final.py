# -*- coding: utf-8 -*-
"""
Created on Thu Oct 31 16:55:50 2019

@author: MSmirnov
"""

'''Print the words and their frequencies in this file'''


from pyspark import SparkContext
from pyspark.sql.types import *
from pyspark.sql import SparkSession
from pyspark.sql.functions import udf

# создание сессии Spark
spark = SparkSession.builder.appName('abc').getOrCreate()


# создание сессии Spark без логирования
#spark = SparkSession.builder.\
#        master('local').\
#        appName('foo').\
#        getOrCreate()
#spark.sparkContext.setLogLevel('WARN')


#data=spark.read.csv('/usr/bin/Sample_code/300K_addresses.csv', header=True)

#data=spark.read.option("delimiter", ";").csv('/usr/bin/Sample_code/300K_addresses.csv')

#data= spark.read.csv('/usr/bin/Sample_code/300K_addresses.csv', header=True, mode="DROPMALFORMED", schema = schema)



schema = StructType([
    StructField("item", StringType(), True),
    StructField("price", IntegerType(), True)])

#
data_DF= (spark.read
    .schema(schema)
    .option("header", "true")
    .option("delimiter", ";")
#   .option("mode", "DROPMALFORMED")
    .csv("/usr/bin/Sample_code/item_names.csv"))

#import pyspark
print ('Hello, Vasya')

import sys  
print(sys.version_info.major)

#import os
#os.chdir('/usr/bin/Sample_code/')

#spark_home = os.chdir("../../../usr/bin/Sample_code/")

#import pandas as pd
#colnames=['COMPANY', 'POS_TYPE', 'CONTRACTDATUM', 'CONTRACTNO', 'COUNTERPARTY', 'RELNAME', 'CONS_IND', 'LOCATION', 'LOCNAME', 'VAT_NUMBER', 'ADDRESS_LINE', 'ZIP_CD', 'PLACE', 'COUNTRYCD']
#

#import databricks.koalas as ks
#import pandas as pd
#data = pd.read_csv('/usr/bin/Sample_code/item_names.csv', sep=';', header=0)

data_DF.createOrReplaceTempView("data")

data_DF.show()




# работает
spark = SparkSession \
    .builder \
    .appName("Python Spark SQL basic example") \
    .config("spark.some.config.option", "some-value") \
    .getOrCreate()

df = spark.read.csv("/usr/bin/Sample_code/item_names.csv",header=True,sep=";");

print(df.collect())

df.show()

# загружаем список групп ОКР
spark = SparkSession \
    .builder \
    .appName("Python Spark SQL basic example") \
    .config("spark.some.config.option", "some-value") \
    .getOrCreate()

df_OKP = spark.read.csv("/usr/bin/Sample_code/OKP_group_test_weight.csv",header=True,sep=";");




from pyspark.sql.functions import lit

#def lower_fun (x):
#    result = x.lower()
#    return result 
#
#from pyspark.sql.functions import lit
#
#udf_myFunction = udf(lower_fun, StringType()) 
#df_res= df.withColumn("message", lit(udf_myFunction("item")))
#
#df_res.show()



# очистка названия в чеке от лишних символов

import string
import re
from string import digits


stop_words_list = ["перед", "килограмм" , "литр", "вес", "год"]

def remove_symbols (input_string):
    table = str.maketrans(dict.fromkeys(string.punctuation))  
    input_string = input_string.translate(table) # удаляем пунктуацию
    remove_digits = str.maketrans('', '', digits)
    input_string = input_string.translate(remove_digits) # удаляем цифры
    input_string = re.sub(r'\b\w{1,2}\b', '', input_string)# удаляем слова менее 2-х символов
    input_string =[word for word in input_string.split() if not any([phrase in word for phrase in stop_words_list])] # удаляем слова, входящие в список стоп-слов
    input_string = ' '.join(input_string) # конвертируем список в строку
    
    return input_string



udf_remove_symbols = udf(remove_symbols , StringType()) 
df_res= df.withColumn("clean_form", lit(udf_remove_symbols("item")))
df_res.show()





import pymorphy2
morph = pymorphy2.MorphAnalyzer()
#morph.parse('яблоки')[0].normal_form
#
#import spacy
#from nltk.tokenize import word_tokenize
#
#
#
#text= "яблоко, груши растительными удобрениями"
#str_list= text.split()

def normal_fun (x):
    result = [morph.parse(item)[0].normal_form for item in x.split()]
    result =' '.join(result) # без этого проеобразования в строку, возвращает список элементов
    return result

udf_normal_fun = udf(normal_fun , StringType()) 
df_res= df_res.withColumn("normal_form", lit(udf_normal_fun("clean_form")))
df_res.show()

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

udf_normal_fun_list = udf(normal_fun_list , ArrayType(StringType())) 
df_res= df_res.withColumn("normal_form_list", lit(udf_normal_fun_list("normal_form")))
df_res.show()


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

udf_pos_fun = udf(pos_fun ,ArrayType(StringType()) ) 
df_res= df_res.withColumn("часть речи", lit(udf_pos_fun("clean_form")))
df_res.show()


def animacy_fun (x):
    
    result = ( [morph.parse(item)[0][1].animacy for item in x.split()]) #+ [morph.parse(item)[0].tag.case for item in x.split()]
    result = [xstr(item) for item in result]
    result = ', '.join(result)
    result = word_list(result)
    return result

udf_animacy_fun = udf(animacy_fun ,ArrayType(StringType()) ) 
df_res= df_res.withColumn("одушевленность", lit(udf_animacy_fun("clean_form")))
df_res.show()




def case_fun (x):
    result = [morph.parse(item)[0].tag.case for item in x.split()]
    result = [xstr(item) for item in result]
    result = ', '.join(result)
    result = word_list(result)
    return result

udf_case_fun = udf(case_fun , ArrayType(StringType())) 
df_res= df_res.withColumn("падеж", lit(udf_case_fun("clean_form")))
df_res.show()



















#
# df_cefctr[["COUNTERPARTY"]][0:10].apply(lambda x: cos_sim(x, ['мясо', 'banana', 'молодой', 'apple23']), axis=1)
#
#
#udf_cos_sim_fun = udf(cos_sim , IntegerType()) 
#df_res= df_res.withColumn("similiarity", lit(udf_cos_sim_fun("normal_form")))
#df_res.show()






#def pos_fun (x):
#    result = [morph.parse(item)[0] for item in x.split()]
#    return result






#
#udf_word_list_fun = udf(word_list , StringType()) 
#df_res= df_res.withColumn("word_list", lit(udf_word_list_fun("Item")))
#df_res.show()
#
#
#def pos_fun (x):
#    result = [morph.parse(item)[0].tag.POS for item in x.split()]
#    return result
#
#udf_pos_fun = udf(pos_fun , StringType()) 
#df_res= df_res.withColumn("POS", lit(udf_pos_fun("word_list")))
##df_res.show()

#print(word_list ("Шоколад Kиндер"))

#df_res= df.withColumn("POS", morph.parse("Item"))

#pos_fun = udf(lambda Item:  morph.parse(Item), StringType())
#df_res= df.withColumn("POS", pos_fun(df.Item))

#df_res.show()
print(pos_fun('красными прыгал'))
print(morph.parse('яблоки')[0].tag.POS)

#print(df_res[0].__getitem__("часть речи"))
#print(df_res[0].asDict()["часть речи"])
#
print(df_res.collect()[0]["Item"]) # является String
print(df_res.collect()[0]["часть речи"]) # является списком
print(df_res.collect()[0]["падеж"]) # является списком


print(df_res.collect()[0]["часть речи"][2]) # в 1-й строке получить 3-й элемент 

print(df_res.collect()[1]["падеж"][0])




def word_multiplicator(a, b, c):
    result = [round(len(a)*0.6) if a_val == 'NOUN' and (b_val == 'nomn' or b_val == 'accs') and c_val == 'inan'  
              else round(len(a)*0.4)*4 if a_val == 'NOUN' and b_val == 'gent' and c_val == 'inan' 
              else 1 for a_val, b_val, c_val, in zip(a, b, c)]
    if result[0] == round(len(a)*0.6): # если первый эелемент массива с весами является сущестительным в им падеже, то придаем ему еще больший вес
        result [0] = result [0]*2
    return result

udf_word_multiplicator = udf(word_multiplicator , ArrayType(IntegerType())) 
df_res= df_res.withColumn("мультипликатор", lit(udf_word_multiplicator( "часть речи", "падеж", "одушевленность")))
df_res.show()



# преобразование строки с учетом весовых коэффициентов
#import operator
#import functools

def weighted_text_string (normal_form_list, multiplicator_list):
    result = sum([[a]*b for a,b in zip(normal_form_list, multiplicator_list)], [])
    return ' '.join(result)

udf_weighted_text_string = udf(weighted_text_string , StringType()) 
df_res= df_res.withColumn("normal_form_weighted", lit(udf_weighted_text_string( "normal_form_list", "мультипликатор")))
df_res.show()


print(word_multiplicator(df_res.collect()[0]["часть речи"], df_res.collect()[0]["падеж"], df_res.collect()[0]["одушевленность"]))

# https://stackoverflow.com/questions/56689048/iterate-over-an-array-column-in-pyspark-with-map
# обработка списков, содарежащихся в одной из колонок
#from pyspark.sql.types import StringType, ArrayType
#from pyspark.sql.functions import udf, col
#
#concat_udf = udf(lambda con_str, arr: [x + con_str for x in arr],
#                   ArrayType(StringType()))
#ret = df \
#  .select(['str1', 'array_of_str']) \
#  .withColumn('concat_result', concat_udf(col("str1"), col("array_of_str")))
#
#ret.show()


#
#
#
#
#
# Возрврат одной колонки








target = df_OKP.select("Item").rdd.flatMap(lambda x: x).collect()
target_group_number = df_OKP.select("Group_number").rdd.flatMap(lambda x: x).collect()
target_group_name = df_OKP.select("Group_name").rdd.flatMap(lambda x: x).collect()

from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics.pairwise import cosine_similarity



def cos_sim_fun(item):
    

    documents = [item] + target


# инициализируем список адресов, для которого попарно будет рассчитываться косинусное расстояние
    the_corpus = documents

# функция для векторизации данных
    vec = TfidfVectorizer()

# матрица количества вхождений слова в конкретный документ (адрес) (tfidf)
    X = vec.fit_transform(the_corpus)

# рассчитываем попарное косинусное расстояние между документами (формируется квадратная матрица для элементов "каждый-с -с каждым") 
    S = cosine_similarity(X[0:1], X)


    max_value_index= S.ravel().tolist().index(max(S.ravel().tolist()[1:52940]))
            
#    elem =  df_OKP["Item"].iloc[max_value_index-1]
    elem_origin = target[max_value_index-1]
#    elem_group_number = df_OKP["Group_number"].iloc[max_value_index-1]
        
    return   elem_origin

    



udf_cos_sim_fun = udf(cos_sim_fun , StringType()) 

print(cos_sim_fun('хлеб'))

df_res = df_res.withColumn("Группа по ОКП", lit(udf_cos_sim_fun( "normal_form_weighted")))
df_res.show()





#
#
## функция для расчета косинусного расстояния
#
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics.pairwise import cosine_similarity

import pyspark.sql.functions as f
import pyspark.sql.types as t 

def cos_sim_fun(item):
    

    documents = [item] + target 


# инициализируем список адресов, для которого попарно будет рассчитываться косинусное расстояние
    the_corpus = documents

# функция для векторизации данных
    vec = TfidfVectorizer()

# матрица количества вхождений слова в конкретный документ (адрес) (tfidf)
    X = vec.fit_transform(the_corpus)

# рассчитываем попарное косинусное расстояние между документами (формируется квадратная матрица для элементов "каждый-с -с каждым") 
    S = cosine_similarity(X[0:1], X)


    max_value_index= S.ravel().tolist().index(max(S.ravel().tolist()[1:52940]))
          
    elem =  target[max_value_index-1]
    elem_origin = target_group_name[max_value_index-1]
    elem_group_number = target_group_number[max_value_index-1]
    
#    elem =  df_OKP["Item"].iloc[max_value_index-1]
#    elem_origin = df_OKP["Group_name"].iloc[max_value_index-1]
#    elem_group_number = df_OKP["Group_number"].iloc[max_value_index-1]
        
    return   t.Row('Out1', 'Out2', 'Out3')(elem, elem_origin, elem_group_number)

    
 

schema = t.StructType([
        t.StructField('Out1', t.StringType(), False),
        t.StructField('Out2', t.StringType(), False),
        t.StructField('Out3', t.StringType(), False)])





udf_cos_sim_fun = f.udf(cos_sim_fun , schema) 

#              df.withColumn("Output", f.explode(f.array(example_udf    (df    ["Number"]))))
df_final = df_res.withColumn("Output", f.explode(f.array(udf_cos_sim_fun(df_res["normal_form_weighted"]))))
df_final = df_final.select("Item", "Output.*")


df_final.show(truncate=False)
df_final.explain()





#
##df_res = df_res.withColumn("Output", lit(udf_cos_sim_fun(df_res["normal_form_weighted"])))
#
#df_res = df_res.withColumn("Output", f.explode(f.array(udf_cos_sim_fun(df_res["normal_form_weighted"]))))
#df_res = df_res.select('Item',  "Output.*")
#
##df_res= df_res.withColumn("косин расстояние", lit(udf_cos_sim_fun("Item", target_list)))
#
#
#df_res.show(truncate=False)
#df_res.show()
#df_res.explain()

#df_res.select("группа ОКП (по косин расстоянию").show(50)


# обработка групп ОКП для создания исходный навзания с учетом весовых коэффициентов

#df_res_OKP= df_OKP.withColumn("clean_form", lit(udf_remove_symbols("item")))
#df_res_OKP.show()
#df_res_OKP= df_res_OKP.withColumn("normal_form", lit(udf_normal_fun("clean_form")))
#df_res_OKP.show()
#df_res_OKP= df_res_OKP.withColumn("normal_form_list", lit(udf_normal_fun_list("normal_form")))
#df_res_OKP.show()
#df_res_OKP= df_res_OKP.withColumn("часть речи", lit(udf_pos_fun("clean_form")))
#df_res_OKP.show()
#df_res_OKP= df_res_OKP.withColumn("падеж", lit(udf_case_fun("clean_form")))
#df_res_OKP.show()
#df_res_OKP= df_res_OKP.withColumn("мультипликатор", lit(udf_word_multiplicator( "часть речи", "падеж")))
#df_res_OKP.show()
#df_res_OKP= df_res_OKP.withColumn("normal_form_weighted", lit(udf_weighted_text_string( "normal_form_list", "мультипликатор")))
#df_res_OKP.show()
#df_res_OKP= df_res_OKP.withColumn("группа ОКП (по косин расстоянию)", lit(udf_cos_sim_fun("normal_form_weighted")))
#df_res_OKP.show()
#
#df1 = df_res_OKP.select("Item", "normal_form_weighted")
#df1.write.csv(path='/usr/bin/Sample_code/weighted_group_OKP.csv', mode="append")
#df1.write.csv('/usr/bin/Sample_code/weighted_group_OKP.csv')









#print(df_res.select(col("часть речи").getItem(2)))

#import pyspark.sql.functions as f
#df_res.withColumn("Elem", f.slice("часть речи",start=2,length=1)).show()
#

#normal_fun ("слива, груши растительными удобрениями")
#pos_fun (" груши груши растительными удобрениями")
#normal_fun (" груши груши растительными удобрениями")
#pos_fun ("груши груши")

#[morph.parse(item)[0].tag.POS for item in str_list]


#reviews_rdd = df.select("Item").rdd.flatMap(lambda x: x)
#reviews_rdd.collect()

#lowerCase_df = df["Item"].map(lambda x : x.lower())

#print (lowerCase_df)
#spark.createDataFrame(df,schema=schema).show()



#print(data.head(5))

#data = data['PLACE']

#mySchema = StructType([ StructField('PLACE', StringType(), True)])

#data_spark = sc.parallelize([data])

#data_spark = spark.createDataFrame(data,schema=mySchema).rdd

#rdd_data = data_DF["Item"].rdd.flatMap(lambda x: x + ("anything", )).toDF()
#
#print(rdd_data)



#reviews_rdd = data_DF.select("Item")
#print(reviews_rdd)

#rdd_data = spark.createDataFrame(data).rdd
#
#concat_rdd = rdd_data.select("RELNAME").rdd.flatMap(lambda x: x.lower())
#print (concat_rdd)



#
#import chardet
#
#
#def find_encoding(fname):
#    r_file = open(fname, 'rb').read()
#    result = chardet.detect(r_file)
#    charenc = result['encoding']
#    return charenc
#
#
#my_encoding = find_encoding('300K_addresses.csv')
#df = pd.read_csv('300K_addresses.csv', sep=';', encoding=my_encoding, names=colnames)
#df.to_csv(df, sep=';', encoding='utf-8')
#
#
#
#
##df_cefctr = pd.read_csv('300K_addresses.csv', sep=';', encoding='ansi', names=colnames)
#
#df.head()


