
OKP_list<-read.csv("C:/Users/msmirnov/Documents/проект УСН/Анализ данных/справочник ОКП (очищенный).csv", na.strings=c(" ", NA), stringsAsFactors=FALSE, header = TRUE, sep = ";")


OKP_list$id <- seq(length=nrow(OKP_list))
OKP_list<- as.data.frame(OKP_list [ , c(4,1,2,3)])



library(stringr)





# присваиваем признак "1" для позиции в подгруппе,т.е. названию начинающемуся с "-"
for (i in  1:nrow(OKP_list)) {
  
  OKP_list$sub [i]<-ifelse(str_locate(OKP_list [i,4], "-") == 2 , 1, 0) 
  
}

OKP_list$sub[is.na(OKP_list$sub)]<-""
OKP_list$sub<-ifelse(OKP_list$sub==0, "", OKP_list$sub)



for (i in  2:nrow(OKP_list)) {
  
  OKP_list$group [i]<-ifelse(OKP_list [i,5] == 1 && OKP_list [i-1,5] == "", OKP_list [i-1,4], "") 
  OKP_list$group [i]<-ifelse(OKP_list [i,5] == 1 && OKP_list [i-1,5] == 1, OKP_list$group [i-1], OKP_list$group [i]) 
  
}


OKP_list$fulllName <- paste(OKP_list[ ,6], OKP_list[ ,4])

write.csv(OKP_list, 'C:/Users/msmirnov/Documents/проект УСН/Анализ данных/ОКП парсинг.csv')









OKP_list[grep("шампанск",OKP_list[,7]), ]
OKP_list[grep("картоф",OKP_list[,7]), ]



# Библиотеки R
# text2vec - для стемминга (отбрасывания окончаний)
# quanteda - для поиска косинусного расстояния между вектрами слов (например, всеми позициями в чеке и названиями группы товаров в ОКП)

# Библиотеки Python
# pymorphy2 - для определения частей речи POS (возможно, понадобится для назаначения весовых коэффициентов)          
# gensim, word2vec - для тематического моделирования, т.е. поиска слов имеющих семантическую близость (вкл. синонимы)







# стемминг с помощью SnowballC (алгоритм Портера), который входит в библиотеку text2vec
library (text2vec)

# пишем функцию для стемминга
stem_tokenizer1 =function(x) {
  tokens = word_tokenizer(x)
  lapply(tokens, SnowballC::wordStem, language="ru")
}



# добавляем колонку в српавочник ОКП с названием после проведения стемминга
OKP_list$stem<-sapply(OKP_list[7],  stem_tokenizer1)


# в массив с содержанием 1-го конкретного чека добавляем колонку с названиями позиции после стемминга
checkStem<-as.data.frame(dat2[3,28]) # один чек

library (plyr)
# собираем все позиции из 10 тыс чеков в один массив
all_items <- rbind.fill(sapply(fullChecks[,28], function (x) {as.data.frame (x)}) )

checkStem<-all_items [, c(1:7)]

checkStem$stem <-sapply(checkStem[3],  stem_tokenizer1)


FUN <- function (x) paste(unlist(x), collapse = " ")

checkStem$stemmedNames <-apply(checkStem[8], 1, FUN) 


# анализ текста  для поиска косинусного расстояния используется библиотека Quanteda
library(quanteda)

myCorpus <- corpus(c(check = paste(unlist(OKP_list[39145,8]), collapse = " "), # название группы товаров из ОКП 
                     
                     target1 = checkStem$stemmedNames )) # содержание наименований конкретного чека из общего массива чеков 


myDfmNoStop <- dfm(myCorpus, remove = stopwords("english"), stem = TRUE, remove_punct = TRUE)

sim <- textstat_simil(myDfmNoStop , 'check', method = "cosine", margin = "documents") # нашли косинусное расстояние между вектором со словами в чеке и в ОКП
# первые 2 позиции в чеке "сахар", поэтому у обоих косинусное рассотяние отлично от "0" при сравнении с позицией ОКП " Сахар - песок /  - из сахарной свеклы"



# внесение данных по косинусному расстоянию в чек 
checkStemFinal<-cbind(checkStem [,c(1:7)],  sim[c(2:length(sim))])

# получаем чек с указанием полей из ОКП и косинусного расстояния для каждой позиции
# далее можно будет выбрать с какой категорией коэф. больше и классифицирвать позицию из чека в эту категорию
colnames(checkStemFinal)[8] <- paste (OKP_list[39145,7], OKP_list[39145,2], "/", OKP_list[39145,3])















##############################################


library(reticulate) # для работы с Python


# дистрибутивная семантика
# запускаем python скрипт, который загружает натренированную модель в память
setwd("C:/Users/msmirnov/")
#source_python("C:/Users/msmirnov/Documents/R scripts/gensim_distr_semantics.py")

# загрузка модели без использования python скрипта
gensim <- import("gensim")

model<-gensim$models$KeyedVectors$load_word2vec_format("C:/Users/msmirnov/model.bin", binary=TRUE)

model$wv$most_similar("газовый_ADJ") # выводим перечень слов из модели, которые семантически близких к заданному слову


model$wv$most_similar("смесь_NOUN")


t<- paste("смесь", "_NOUN", sep="")

model$wv$most_similar(t)








# POS токенизация - определение частей речи (возможно пригодиться для выставления весовых коэффициентов, т.к. семантически совпадения по существительным 
# дает бОльшую смысловую нагрузку, чем по прилагательным)

lib <- import("pymorphy2")


phrase = r_to_py(phrase) #передача объекта в python



morph = lib$MorphAnalyzer()


library(stringr)



# POS токенизация после стемминга
for (i in 1:length(unlist(checkStem[3,8]))) { 
  
  
  
  print(str_sub(morph$tag(unlist(checkStem[3,8])[i])[[1]], start=1, end=4))
  
  
}




#word_tokenizer(checkStem[11,4]) # токенизация из библиотеки text2vec


# токенизация до стемминга (учитывает окончания и правильно определяет часть речи)

checkStem<- subset (checkStem, checkStem[,3]!="" & checkStem[,3]!=" " & checkStem[,3]!="*" &  checkStem[ ,3] != "-")

# формируем колонку с длиной вектора со словами из названия позиции
checkStem$v_length <- sapply( checkStem [,8], function (x){ length(unlist(x)) })

# фильтруем массив, убирая значения с длиной вектора 0
checkStem<- subset (checkStem, checkStem$v_length >0)

#checkStem<- subset (checkStem, grepl("/^[\s\n\r]*$/", checkStem[,3]))
# цикл - очень медленное решение, остановил команду после 40 мин работы. Решение через sapply/ lapply работает не менее, чем в 10 раз быстрее
# system.time({
# for (k in 1:nrow(checkStem)) { #nrow(checkStem)
#  POS<- list()
#   
# for (i in 1:length(unlist(checkStem[k,8]))) { 
#   
#   
#   
#   #print(str_sub(morph$tag(unlist(word_tokenizer(checkStem[3,4]))[i])[[1]], start=1, end=4))
#   
#  #POS <- append(POS,str_sub(morph$tag(unlist(word_tokenizer(checkStem[k,4]))[i])[[1]], start=1, end=4))
#  POS <-append(POS,morph$tag(unlist(word_tokenizer(checkStem[k,3]))[i])[[1]])
#  
#  checkStem$POS[k]<-list(POS)
# }
# 
# }
# 
# })

# funct_token<- function (x) {unlist(word_tokenizer(x))}
# 
# funct_token<- function (x) {word_tokenizer(x)}
# 
# funct_token(checkStem[1,3])
# 
# funct_POS<- function (x) {morph$tag(x)} 
# 
# checkStem$POS<- sapply(checkStem [,3], unlist(lapply(checkStem[,3], funct_POS)) [[1]] ) 


# тестируем

#checkStem_test<- checkStem [c(1:10),]

# функция для токенизации всех слов в названии 
funct_token<- function (x) {unlist(word_tokenizer(x))}

# checkStem_test$token<- sapply(checkStem_test[,3], funct_token)

# функция для определения POS конкретного слова
funct_POS<- function (x) {morph$tag(x)[[1]]} 

POS<- list()

#lapply(unlist(checkStem_test[5,8]), funct_POS) # работает

#checkStem_test$POS<-sapply(checkStem_test[,8], function(x) {append(POS,lapply(unlist(x), funct_POS))}) 

system.time({
  
  checkStem$POS<-sapply(checkStem[,8], function(x) {append(POS,lapply(unlist(x), funct_POS))}) 
  
})

# POS[...] - ссылка на элемент, содержащий масссив граммем для каждого слова
# POS[...] [[...]] ссылка на единственный элемент массива, который содержит все граммемы для последующего парсинга
unlist(checkStem$POS[9])[[1]] 


# для доступа к переменной List, содержащей граммемы каждого слова в позиции чека 
# документация по граммемам https://pymorphy2.readthedocs.io/en/latest/user/grammemes.html

unlist(checkStem$POS[3])[1]   [[1]]

unlist(checkStem$POS[3])[2]   [[1]]

aa<- unlist(checkStem$POS[3])[3][[1]] # POS[...] - ссылка на элемент, содержащий масссив граммем
# POS[...] [...] - ссылка на конкртеный List с граммемами к конкретному слову (по порядку, стоящему в навзании позиции), т.е. [3] - это ссылка на список граммем к 3-му слову
# POS[...] [...] [[...]] ссылка на единственный элемент массива, который содержит все граммемы для последующего парсинга


as.list(strsplit(as.character(aa), ',')[[1]])  # получение всех граммем по одному слову в List, с вовожностью обратиться к каждой граммеме по индексу, напр [3]

as.list(strsplit(as.character(aa), ',')[[1]]) [3] == "nomn"



# вывод нормализованной формы слова
repl_python()
import pymorphy2
morph = pymorphy2.MorphAnalyzer()
word = "думающий"
p = morph.parse(word) [1] # Делаем полный разбор, и берем первый вариант разбора (условно "самый вероятный", но не факт что правильный)
task=p.normal_form  # стать
exit

py$task # передача объекта из REPL python в R












#########################################################

















str_sub(aa, 1, 12) # парсинг строки с граммемами
str_sub(r_to_py(checkStem$POS[37]), 13, 15) # парсинг всего массива с граммемами дл\ всех слов в позиции работает

str_sub (unlist(checkStem$POS[37])[2], 1, 5)



# as.list(strsplit(as.character(r_to_py(checkStem$POS[37])), "'")[[1]])[4]  другой вариант работы парсинга

# найти косинусное расстояние между нормализованным вектром слов в чеке и ОКП (стемминг)
# в случае, если группа в ОКП не найдена, то определив часть речи для слов в исходном чеке и группе ОКП сделать поиск по семантически близким существительным (прилагательным)



morph$tag("мытый")
morph$tag("-----")

morph$parse("сельдерея")
unlist(morph$parse("сельдерея"))

unlist(morph$tag("сельдерея"))[[3]]
length(morph$tag("сельдерея"))

as.list(strsplit(as.character(morph$tag("сельдерея")), ","))


strsplit(morph$tag("сельдерея"), ",")


py_to_r(morph$tag("сельдерея"))

strsplit(r_to_py(morph$tag("сельдерея")), ",")

r_to_py(morph$tag("сельдерея"))


str_sub(r_to_py(morph$tag("сельдерея")), -7, -4)

str_sub(r_to_py(morph$tag("свежим")), -7, -4)

append(POS,r_to_py(morph$tag(unlist(word_tokenizer(checkStem[k,4]))[i])))[[1]]


r_to_py(morph$tag(unlist(word_tokenizer(checkStem[k,4]))[i])[1])

append(POS,morph$tag(unlist(word_tokenizer(checkStem[k,4]))[i])[[1]])



str_sub(r_to_py(morph$tag(unlist(word_tokenizer(checkStem[k,4]))[i])[[1]]), -7, -4)

k<-12


#####################################################################################################################

# 
# stream<- readBin("C:/Users/msmirnov/180.zip", raw())
# model<-gensim$models$KeyedVectors$load_word2vec_format(stream, binary=True)





# import gensim, word2vec
# from gensim.test.utils import common_texts, get_tmpfile
# from gensim.models import Word2Vec
# 
# 
# import wget
# import zipfile

stream <- archive$open('model.bin')


with zipfile.ZipFile('180.zip', 'r') as archive:
  stream = archive.open('model.bin')
model = gensim.models.KeyedVectors.load_word2vec_format(stream, binary=True)

model$most_similar(positive=['датчик_NOUN'], topn=10)










gensim <- import("gensim")

model<-gensim$models$Word2Vec$load("word2vec_gensim")

matrix(unlist(model$wv$most_similar("queen")),ncol=2,byrow=T)


model<-gensim$models$Word2Vec$load("C:/Users/msmirnov/model.bin")
model<-gensim$models$KeyedVectors$load_word2vec_format("C:/Users/msmirnov/model.bin", binary=TRUE)


morph = lib$MorphAnalyzer()

word<- "стали"
p <- morph$parse(word)[1]  # Делаем полный разбор, и берем первый вариант разбора (условно "самый вероятный", но не факт что правильный)
p$normal_form  # стать


# вывод нормализованной формы слова
repl_python()
import pymorphy2
morph = pymorphy2.MorphAnalyzer()
word = "думающий"
p = morph.parse(word) [1] # Делаем полный разбор, и берем первый вариант разбора (условно "самый вероятный", но не факт что правильный)
task=p.normal_form  # стать
exit

py$task # передача объекта из REPL python в R


