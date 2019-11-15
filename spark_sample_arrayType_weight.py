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
#spark = SparkSession.builder.appName('abc').getOrCreate()


# отключить логирование
spark = SparkSession.builder.\
        master('local').\
        appName('foo').\
        getOrCreate()
spark.sparkContext.setLogLevel('WARN')


#data=spark.read.csv('/usr/bin/Sample_code/300K_addresses.csv', header=True)

#data=spark.read.option("delimiter", ";").csv('/usr/bin/Sample_code/300K_addresses.csv')

#data= spark.read.csv('/usr/bin/Sample_code/300K_addresses.csv', header=True, mode="DROPMALFORMED", schema = schema)

#schema = StructType([
#    StructField("Item", StringType(), True),
#    StructField("date", StringType(), True),
#    StructField("variation", StringType(), True),
#    StructField("verified_reviews", StringType(), True),
#    StructField("feedback", IntegerType(), True)])

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

df_OKP = spark.read.csv("/usr/bin/Sample_code/OKP_group_test.csv",header=True,sep=";");




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
df_res= df.withColumn("normal_form", lit(udf_normal_fun("item")))
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
df_res= df_res.withColumn("normal_form_list", lit(udf_normal_fun_list("item")))
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
df_res= df_res.withColumn("часть речи", lit(udf_pos_fun("item")))
df_res.show()


def case_fun (x):
    result = [morph.parse(item)[0].tag.case for item in x.split()]
    result = [xstr(item) for item in result]
    result = ', '.join(result)
    result = word_list(result)
    return result

udf_case_fun = udf(case_fun , ArrayType(StringType())) 
df_res= df_res.withColumn("падеж", lit(udf_case_fun("item")))
df_res.show()













#
#
## функция для расчета косинусного расстояния
#
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.metrics import pairwise_kernels

# формируем список с группами из ОКП, с которым будем сравнивать каждую позицию из чека
target = df_OKP.select("Item").rdd.flatMap(lambda x: x).collect()


def cos_sim_fun(item):
    

    item_list = []
    item_list.append(item)
    
    #target = list(target)
    vec = CountVectorizer() # в скобках можно поставить параметр analyzer='char', если важно подобие с учетом букв
    vec.fit(target)
    
    vec.fit(item_list)

    result = pairwise_kernels(vec.transform(item_list),
                              vec.transform(target),
                              metric='cosine')
    result = list(result[0,:]) # матрицу с коэффицинтами преобразуем в list
   
    if max(result)>0 :
        elem_index = max(range(len(result)), key=result.__getitem__)
        elem = target[elem_index]
    else:
        elem = None
            
    return  elem








udf_cos_sim_fun = udf(cos_sim_fun , StringType()) 
df_res=df_res.withColumn("группа ОКП (по косин расстоянию)", lit(udf_cos_sim_fun("normal_form")))

#df_res= df_res.withColumn("косин расстояние", lit(udf_cos_sim_fun("Item", target_list)))
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

print(df_res.collect()[1]["падеж"][2])



def word_multiplicator(a, b):
    return [2 if a_val == 'NOUN' and b_val == 'accs' else 1 for a_val, b_val in zip(a, b)]

udf_word_multiplicator = udf(word_multiplicator , ArrayType(IntegerType())) 
df_res= df_res.withColumn("мультипликатор", lit(udf_word_multiplicator( "часть речи", "падеж")))
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


print(word_multiplicator(df_res.collect()[0]["часть речи"], df_res.collect()[0]["падеж"]))

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


