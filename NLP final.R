library (jsonlite)
library (reshape2)

json_file <- "C:/Users/msmirnov/usn/src/test/resources/export_test.json"


dat2<- fromJSON( sprintf("[%s]",
                         paste(readLines(json_file, encoding = "UTF-8"),
                               collapse=",")), flatten = TRUE)







##################################################################################
# счетчик пустых массивов в поле content.item
countEmptyArrays <- 0

# вектор признаков массив пустой =1 или непустой =0
vec <- c(1:nrow(dat2))

for (i in  1:nrow(dat2)) {
  
  countEmptyArrays<- countEmptyArrays + ifelse (is.null(unlist(dat2[i,28])), 1, 0)
  vec [i]<- is.null(unlist(dat2[i,28]))
  
}

# количество чеков с пустым массивом в поле items
countEmptyArrays

# добавляем колонку с признаками "пустого массива" в поле "items" в чеке 
dat3<- cbind(dat2, vec)


# остортированная таблица с пустыми чеками в поле items
emptyItemsChecks <- subset(dat3, dat3[,76]==1) 

# отсортированная таблица с чеками с непустым массивом в содержании (может не быть нвзания позиций. но есть сумма проч.)
fullChecks <- subset(dat3, dat3[,76]==0)


















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
                     
                     target1 = checkStem$stemmedNames [2] )) # содержание наименований конкретного чека из общего массива чеков 


myDfmNoStop <- dfm(myCorpus, tolower = TRUE, remove = stopwords("english"), stem = TRUE, remove_punct = TRUE)

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



OKP_list$format<-sapply (OKP_list [,8], function (x) {paste(unlist(x), collapse = " ")} )

# POS[...] - ссылка на элемент, содержащий масссив граммем для каждого слова
# POS[...] [[...]] ссылка на единственный элемент массива, который содержит все граммемы для последующего парсинга
unlist(checkStem$POS[9])[[1]] 








####################################
####################################
# Формируем корпус из всех групп ОКП и всех позиций конретного чека

OKP_list$format<-sapply (OKP_list [,8], function (x) {paste(unlist(x), collapse = " ")} )

system.time({
  myCorpus <- corpus(c(group = OKP_list [ c(30000:30002),9] , # название группы товаров из ОКП 
                       
                       target1 =  checkStem$stemmedNames[c(1:2)]))
  
  
  
  myDfmNoStop <- dfm(myCorpus, remove = stopwords("english"), stem = TRUE, remove_punct = TRUE)
  
  tt<- textstat_simil(myDfmNoStop,  method = "cosine", margin = "documents") #selection = c("group30756", "group30773", "group30774")
  
  
})

tt<-as.matrix(tt)
tt<- as.data.frame(tt, optional = TRUE)
tt$items<- rownames(tt)


# оставляем только позиции чека
library(stringr)
library(dplyr)

tt1<-tt %>%
  filter(str_detect(tt$items, "target"))



# определили  в каких группах косинусное расстояние больше 0, т.е. хоть для какой-нибудь позиции найдена группа из ОКП
# для каждой строки из чека "target" определено ненулевое косинусное расстояние с каждой из групп ОКП

k<-ncol(tt1)-1 # 
tt2<- tt1[, colSums(tt1[ , c(1:k)]) != 0] # получаем массив ненулевых косинусных расстояний для каждой позиции чека и групп ОКП

# структура массива tt2 следующая:
# колонки "group1", "group2" и т.д. по количеству групп из ОКП, потом идут "target1", "target2"  т.д. по количеству переданных в корпус словп позиций из чека, 
# потом вставлена колонка "items", где перечислены номера переданных позиций из чека




# для анализа для кжадой позиции из чека собрать все группы из ОКп, с косинусным расстоянием больше 0, 
# т.к. необязательно, что макс. значение косинусного расстояния будет лучшим параметром

which.max(tt2[1,c(1:130)]) # определяет индекс колонки с макс значением 

checkStem[1,3]

col_name<-colnames(tt2)[which.max(tt2[1, c(1:(ncol(tt2)-nrow(tt2)-1) )])]

col_num<-as.numeric(str_sub ( col_name, start = nchar(col_name)-3, end = nchar(col_name)))

OKP_list[30000+col_num-1,7]



