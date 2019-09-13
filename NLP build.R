library (stringi)
library (stringr)
library (text2vec)
library (jsonlite)
library (reshape2)
library (plyr)
library (quanteda)
library (reticulate)

# кроме справочника ОКП еще должен быть использован справочник услуг (Общероссифский классификатор услуг населению -ОКУН)

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

# записываем файл с данными ОбщероссиЙского Классификатора Продукции (ОКП)
write.csv(OKP_list, 'C:/Users/msmirnov/Documents/проект УСН/Анализ данных/ОКП парсинг.csv', row.names = FALSE)







# OKP_list[grep("шампанск",OKP_list[,7]), ]
# OKP_list[grep("картоф",OKP_list[,7]), ]




# Библиотеки R
# text2vec - для стемминга (отбрасывания окончаний)
# quanteda - для поиска косинусного расстояния между вектрами слов (например, всеми позициями в чеке и названиями группы товаров в ОКП)

# Библиотеки Python
# pymorphy2 - для определения частей речи POS (возможно, понадобится для назаначения весовых коэффициентов)          
# gensim, word2vec - для тематического моделирования, т.е. поиска слов имеющих семантическую близость (вкл. синонимы)


# загружаем данные из справочника ОКП

OKP_list <- read.csv('C:/Users/msmirnov/Documents/проект УСН/Анализ данных/ОКП парсинг.csv')



# стемминг с помощью SnowballC (алгоритм Портера), который входит в библиотеку text2vec
library (text2vec)
library (stringr)

# пишем функцию для стемминга
stem_tokenizer1 =function(x) {
  tokens = word_tokenizer(x)
  lapply(tokens, SnowballC::wordStem, language="ru")
}



# добавляем колонку в српавочник ОКП с названием после проведения стемминга
OKP_list$stem<-sapply(OKP_list[7],  stem_tokenizer1)

########################
# загружаем данные по тестовой выборке чеков
library (jsonlite)
library (reshape2)

json_file <- "C:/Users/msmirnov/usn/src/test/resources/export_test.json"


dat2<- fromJSON( sprintf("[%s]",
                         paste(readLines(json_file, encoding = "UTF-8"),
                               collapse=",")), flatten = TRUE)



# сортируем чеки с непустым массивом в поле items, чтобы обрабатывать позиции с конкретными  наименованиями
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

# добавл¤ем колонку с признаками "пустого массива" в поле "items" в чеке 
dat3<- cbind(dat2, vec)


# остортированна¤ таблица с пустыми чеками в поле items
emptyItemsChecks <- subset(dat3, dat3[,76]==1) 

# отсортированна¤ таблица с чеками с непустым массивом в содержании (может не быть нвзани¤ позиций. но есть сумма проч.)
fullChecks <- subset(dat3, dat3[,76]==0)

########################


# в массив с содержанием 1-го конкретного чека добавляем колонку с названиями позиции после стемминга
#checkStem<-as.data.frame(dat2[3,28]) # один чек из загруженной выборки чеков

library (plyr)
# собираем все позиции из 10 тыс чеков в один массив
all_items <- rbind.fill(sapply(fullChecks[,28], function (x) {as.data.frame (x)}) )

checkStem<-all_items [, c(1:7)] 

#добавляем колонку с названиями после стемминга (выделения "корневой основы", без окончаний)
checkStem$stem <-sapply(checkStem[3],  stem_tokenizer1)

# пишем функцию и добавляем колонку выделяя из списка со словами каждого слово (может не понадобится)
FUN <- function (x) paste(unlist(x), collapse = " ")

checkStem$stemmedNames <-apply(checkStem[8], 1, FUN) 

OKP_list$stemmedNames <-apply(OKP_list[8], 1, FUN) 


# анализ текста  для поиска косинусного расстояния используется библиотека Quanteda
library(quanteda)




library(reticulate) # для работы с Python

lib <- import("pymorphy2")


#phrase = r_to_py(phrase) #передача объекта в python


morph = lib$MorphAnalyzer()



# делаем функцию POS теггинг для каждого токена. [[1]] - это выбор первого элемента из списка граммем для кжадого слова, как наиболее вероятного

funct_POS<- function (x) {morph$tag(x)[[1]]} 

# POS для всех позиций в чеке

system.time({ 
  
  checkStem$POS <- sapply(checkStem[,3], function (x) { list(lapply( unlist(tokens(as.character(x))), funct_POS)) } )
  
})

# вернуть конретный элемент из списка граммем
#str_split( as.character(unlist(checkStem$POS[3])[[1]]), ",")[[1]][3]


# приведение слова к нормальной форме
#word<- "общего"
#p <- morph$parse(word)[1]  # Делаем полный разбор, и берем первый вариант разбора (условно "самый вероятный", но не факт что правильный)
#p$normal_form  # стать

morph_function<- function (x) {morph$parse(x)[1] }

end_normal_sign_function<- function (x) {str_locate(as.character(unlist(x)[[1]]), "normal_form")[2]}

end_comma_function<-function (x) {str_locate_all(as.character(unlist(x)[[1]]), "'")[[1]][6]}


normal_form_function<- function (x) {str_sub(as.character(unlist(morph_function(x))[[1]]), 
                                             start= end_normal_sign_function(morph_function(x))+3, 
                                             end = end_comma_function (morph_function(x))-1 )}

normal_form_function_paste<- function(x) {paste(unlist(lapply( unlist(strsplit((x), " ")),normal_form_function)), collapse = " ")}






# оичщаем от цифр
no_digits_function<- function(x) {stringi::stri_replace_all_regex(x, "\\d", "")}
# убрать стоп-слова из текста
russianStopWords<- c("с", "в", "на", "перед", "из", "г", "кг", "килограмм" ,"мл", "л", "литр", "вес")
# оичщаем от стоп-слов
no_stop_words_function<- function (x) {  paste(   unlist(strsplit(x, " "))[!(unlist(strsplit(x, " "))) %in% russianStopWords], collapse =" " ) }

# уобрать пунктуацию из текста
no_punctuation_function<- function (x) {paste(unlist(tokens(as.character(x), remove_punct = TRUE)), collapse= " ")}


# POS для ОКП выполняется 20 мин

system.time({ 
  
  OKP_list$POS <- sapply(OKP_list[,7], function (x) { list(lapply( unlist(tokens(as.character(x))), funct_POS)) } )
  
})


# нормализация и очистка текста для ОКП и чеков

# выполняется 45 мин
system.time({ 
  OKP_list$normilizedForm<- sapply(as.character(OKP_list [,7]), function (x) {no_stop_words_function(no_punctuation_function(no_digits_function(normal_form_function_paste(x))))} )
  
})


# выполняется 25 мин
system.time({ 
  checkStem$normilizedForm<- sapply(checkStem [,3], function (x) {no_stop_words_function(no_punctuation_function(no_digits_function(normal_form_function_paste(x))))} )
  
})









# # название конкретной группы товаров из ОКП ("Сахарный песок из сахарной свеклы") записываем в OKP_group
# myCorpus <- corpus(c(OKP_group = paste(unlist(OKP_list[39145,8]), collapse = " "), 
#                      
#                      target1 = checkStem$normilizedForm )) # target1 - наименования всех позиций из общего массива чеков 
# 

# название позиции из конкретного чека сравниваем со всеми группами из ОКП
myCorpus <- corpus(c(check_item = checkStem$normilizedForm[99] , 
                     
                     target1 = OKP_list[,11] )) # target1 - наименования всех позиций из общего массива чеков 





myDfmNoStop <- dfm(myCorpus, remove = stopwords("english"), stem = TRUE, remove_punct = TRUE)

# нашли косинусное расстояние между вектором с наименованиями в чеках и конкретной  группе ОКП 
sim <- textstat_simil(myDfmNoStop , 'check_item', method = "cosine", margin = "documents") 
# первые 2 позиции в чеке "сахар", поэтому у обоих косинусное рассотяние отлично от "0" при сравнении с позицией ОКП " Сахар - песок /  - из сахарной свеклы"



# внесение данных по косинусному расстоянию в чек 
# checkStemFinal<-cbind(checkStem [,c(1:7)],  sim[c(2:length(sim))])

# внесение данных по косинусному расстоянию между каждой группой из ОКП и названием в чеке
OKP_fit<-cbind(OKP_list [,c(1:11)],  sim[c(2:length(sim))])

# сортируем массив с названиями по величине коиснусного  расстояния для каждой позиции и выбранной группы ОКП

# subset_cosine_check<-subset(checkStemFinal, checkStemFinal[,8]>0.2)


subset_cosine_OKP<-subset(OKP_fit, OKP_fit[,12]>0.2)

colnames(subset_cosine_OKP)[12] <- "sim_coef"

subset_cosine_OKP<-subset_cosine_OKP[order(subset_cosine_OKP$sim_coef, decreasing= TRUE),]






# расчет косинусного расстояния между результатми стемиминга

myCorpus <- corpus(c(check_item = checkStem$stemmedNames[99] , 
                     
                     target1 = OKP_list$stemmedNames )) # target1 - наименования всех позиций из общего массива чеков 





myDfmNoStop <- dfm(myCorpus, remove = stopwords("english"), stem = TRUE, remove_punct = TRUE)

# нашли косинусное расстояние между вектором с наименованиями в чеках и конкретной  группе ОКП 
sim <- textstat_simil(myDfmNoStop , 'check_item', method = "cosine", margin = "documents") 
# первые 2 позиции в чеке "сахар", поэтому у обоих косинусное рассотяние отлично от "0" при сравнении с позицией ОКП " Сахар - песок /  - из сахарной свеклы"



# внесение данных по косинусному расстоянию в чек 
# checkStemFinal<-cbind(checkStem [,c(1:7)],  sim[c(2:length(sim))])

# внесение данных по косинусному расстоянию между каждой группой из ОКП и названием в чеке
OKP_fit<-cbind(OKP_list [,c(1:11)],  sim[c(2:length(sim))])

# сортируем массив с названиями по величине коиснусного  расстояния для каждой позиции и выбранной группы ОКП

# subset_cosine_check<-subset(checkStemFinal, checkStemFinal[,8]>0.2)


subset_cosine_OKP_stemmed<-subset(OKP_fit, OKP_fit[,12]>0.2)
colnames(subset_cosine_OKP_stemmed)[12] <- "sim_coef"

subset_cosine_OKP_stemmed<-subset_cosine_OKP_stemmed[order(subset_cosine_OKP_stemmed$sim_coef, decreasing= TRUE),]
















# дистрибутивная семантика
# запускаем python скрипт, который загружает натренированную модель в память
setwd("C:/Users/msmirnov/")
#source_python("C:/Users/msmirnov/Documents/R scripts/gensim_distr_semantics.py")

# загрузка модели без использования python скрипта
gensim <- import("gensim")

model<-gensim$models$KeyedVectors$load_word2vec_format("C:/Users/msmirnov/model.bin", binary=TRUE)

model$wv$most_similar("газовый_ADJ") # выводим перечень слов из модели, которые семантически близких к заданному слову


model$wv$most_similar("смесь_NOUN")


t<- paste("аспирин", "_NOUN", sep="")

model$wv$most_similar(t)


t<- paste("пересылка", "_NOUN", sep="")

model$wv$most_similar(t)




# конец основного скрипта









##############################
# обработка для ГИР БО
all_atr_names<- read.csv('C:/Users/msmirnov/Documents/ГИР БО/all_atr_names.csv', row.names = NULL, header = TRUE, sep = ";")

all_atr_names<- read.csv('C:/Users/msmirnov/Documents/ГИР БО/all_atr_names_TEST.csv', row.names = NULL, header = TRUE, sep = ";")


#all_atr_names$stemmedNames<- sapply(all_atr_names [,1],  stem_tokenizer1)
all_atr_names$normalizedForm<- sapply(as.character(all_atr_names [,1]), function (x) {no_stop_words_function(no_punctuation_function(no_digits_function(normal_form_function_paste(x))))} )

library(RYandexTranslate)



# сгенерированный ключ API Яндекс переводчик
api_key<- 'trnsl.1.1.20190904T193649Z.c9d1c19d6a48e61c.cabe507a19291f0040dca170371df858f1a8c4e3'
translate = function (api_key, text = "", lang = "ru-en") 
{
  url = "https://translate.yandex.net/api/v1.5/tr.json/translate?"
  url = paste(url, "key=", api_key, sep = "")
  if (text != "") {
    url = paste(url, "&text=", text, sep = "")
  }
  if (lang != "") {
    url = paste(url, "&lang=", lang, sep = "")
  }
  url = gsub(pattern = " ", replacement = "%20", x = url)
  d = RCurl::getURL(url, ssl.verifyhost = 0L, ssl.verifypeer = 0L)
  d = jsonlite::fromJSON(d)
  d$code = NULL
  d
}

#translate(api_key,text=enc2utf8('граница'))





translate_yandex<-function (x) {translate (api_key,text=enc2utf8(x))}

# перевод на английский через API Яндекс
system.time({ 
all_atr_names$translation<- sapply( all_atr_names[,2], function (x) {unlist(lapply(x, translate_yandex))[[2]]})

})


# очистка от стоп-слов
system.time({ 
  
all_atr_names$noStopWords<-sapply( all_atr_names[,3], function (x) {paste(as.character(tokens_remove(tokens(x, remove_punct = TRUE), 

                                                    stopwords("english"))), collapse= " ")})
})   



abbreviation_function<- function (x) paste(unlist(lapply( as.character(tokens(x)), function (x) { abbreviate(x, 4, strict = TRUE) })), collapse = " ")


# функция для сокращения слов в зависимости от длины слова
abbreviation_function<-  function (x) paste(unlist(lapply( as.character(tokens(x)), function (x) { ifelse(nchar(x)>10, abbreviate(x, 6, strict = FALSE), abbreviate(x, 5, strict = TRUE)) })), collapse = " ")




library (snakecase)
system.time({ 
all_atr_names$snakeCase <- sapply(all_atr_names[,4], function (x) {to_snake_case(abbreviation_function(x), abbreviations = NULL, sep_in = "[^[:alnum:]]", parsing_option = 1, transliterations = NULL, numerals = "middle", sep_out = NULL, unique_sep = NULL, empty_fill = NULL, prefix = "", postfix = "")})
})    





write.table(all_atr_names, 'C:/Users/msmirnov/Documents/ГИР БО/all_atr_names_translated.csv',  sep = ";")






























library(reticulate) # для работы с Python

lib <- import("pymorphy2")


#phrase = r_to_py(phrase) #передача объекта в python


morph = lib$MorphAnalyzer()



# делаем функцию POS теггинг для каждого токена. [[1]] - это выбор первого элемента из списка граммем для кжадого слова, как наиболее вероятного

funct_POS<- function (x) {morph$tag(x)[[1]]} 


# определяем часть речи для каждого слова. В morph$tag можно передать только 1 слово
# morph$tag(unlist(tokens(subset_cosine_check[3,3]))[[1]]) 





# получаем для каждого токена из наименования в чеке список граммем 

for (i in (1:nrow(subset_cosine_check))){
  
  subset_cosine_check$POS[i]<- list(lapply( unlist(tokens(subset_cosine_check[i,3])), funct_POS))
  
}




# этот вариант для получения списка граммем работает быстрее !
#subset_cosine_check$POS <- sapply(subset_cosine_check[,3], function (x) { list(lapply( unlist(tokens(x)), funct_POS)) } )




# определение количества элементов в списке с граммемами, т.е. для какого количества слов были получены граммемы
length(unlist(subset_cosine_check$POS[i]))




# POS для ОКП

system.time({ 

OKP_list$POS <- sapply(OKP_list[,7], function (x) { list(lapply( unlist(tokens(as.character(x))), funct_POS)) } )

})


# уобрать пунктуацию из текста
unlist(tokens(as.character(OKP_list[5,7]), remove_punct = TRUE))

unlist(tokens(as.character("конфеты -? из , сахара перед; едой 200 г"), remove_punct = TRUE))


# убрать стоп-слова из текста
russianStopWords<- c("с", "в", "на", "у", "перед", "из", "г", "кг", "мл", "л", "литр")
x <- unlist(strsplit("конфеты из сахара перед едой 200 г", " "))

x <- x[!x %in% russianStopWords]

paste(x, collapse = " ")

# убрать все цифры из текста
library (stringi)

stringi::stri_replace_all_regex("конфеты сахара, : едой 200 г", "\\d", "")








 tokens_select(tokens(as.character(OKP_list[5,7]), remove_punct = TRUE), stopwords("english"), selection="remove")



# POS[...] - ссылка на элемент, содержащий масссив граммем для каждого слова
# POS[...] [[...]] ссылка на единственный элемент массива, который содержит все граммемы для последующего парсинга
unlist(subset_cosine_check$POS[9])[[1]] 




# приведение слова к нормальной форме
word<- "общего"
p <- morph$parse(word)[1]  # Делаем полный разбор, и берем первый вариант разбора (условно "самый вероятный", но не факт что правильный)
#p$normal_form  # стать

end_normal_sign<-str_locate(as.character(unlist(p)[[1]]), "normal_form")[2]

# индекс в строке для закрывающей "кавычки" номральной формы
end_comma<-str_locate_all(as.character(unlist(p)[[1]]), "'")[[1]][6]

str_sub(as.character(unlist(p)[[1]]), start= end_normal_sign+3, end = end_comma-1 )



morph_function<- function (x) {morph$parse(x)[1] }

end_normal_sign_function<- function (x) {str_locate(as.character(unlist(x)[[1]]), "normal_form")[2]}

end_comma_function<-function (x) {str_locate_all(as.character(unlist(x)[[1]]), "'")[[1]][6]}


normal_form_function<- function (x) {str_sub(as.character(unlist(morph_function(x))[[1]]), 
                                             start= end_normal_sign_function(morph_function(x))+3, 
                                             end = end_comma_function (morph_function(x))-1 )}

# пользовательская функция на R для обределения нормальной формы слова
normal_form_function("свежей")


# функция, приводящая каждое слово в названии к нормальной форме
paste(unlist(lapply( unlist(strsplit("Электроэнергия, произведенная / электростанциями ", " ")),normal_form_function)), collapse = " ")

normal_form_function_paste<- function(x) {paste(unlist(lapply( unlist(strsplit((x), " ")),normal_form_function)), collapse = " ")}

# План исследования
# 
# Посчитать косинусное расстояние после очистки от пунктуации и стоп-слов (предлогов, цифр, ед. изм и проч.)
# - при сравнении корневой основы (без окончаний) в чеке и ОКП
# - при сравнении нормальных форм слов (именительный падеж, ед.ч., инфинитив)
# 
# После POS анализа присвоить весовые коэффициенты, например, для существительных, находящихся в имен. падеже 



# приводим чек к нормальной форме
subset_cosine_check$normalForm<- sapply(subset_cosine_check[,3], normal_form_function_paste)

# оичщаем от цифр
no_digits_function<- function(x) {stringi::stri_replace_all_regex(x, "\\d", "")}

# очищаем от цифр


subset_cosine_check$noDigits<-sapply(subset_cosine_check$normalForm, function(x) {stringi::stri_replace_all_regex(x, "\\d", "")})

# убрать стоп-слова из текста
russianStopWords<- c("с", "в", "на", "перед", "из", "г", "кг", "килограмм" ,"мл", "л", "литр")
x <- unlist(strsplit("конфеты из сахара перед едой 200 г", " "))

x <- x[!x %in% russianStopWords]

subset_cosine_check$noStopWords<- sapply (subset_cosine_check$noDigits, function (x) {  paste(   unlist(strsplit(x, " "))[!(unlist(strsplit(x, " "))) %in% russianStopWords], collapse =" " ) } )

no_stop_words_function<- function (x) {  paste(   unlist(strsplit(x, " "))[!(unlist(strsplit(x, " "))) %in% russianStopWords], collapse =" " ) }


# уобрать пунктуацию из текста
subset_cosine_check$noPunct<-sapply(subset_cosine_check$noStopWords, function (x) {paste(unlist(tokens(as.character(x), remove_punct = TRUE)), collapse= " ")})

no_punctuation_function<- function (x) {paste(unlist(tokens(as.character(x), remove_punct = TRUE)), collapse= " ")}



# нормализации и очистки текста
subset_cosine_check$normilizedForm <- sapply(subset_cosine_check[,3], function (x) {no_stop_words_function(no_punctuation_function(no_digits_function(normal_form_function_paste(x))))} )





# нормализация и очистка текста для ОКП

system.time({ 
OKP_list$normilizedForm<- sapply(as.character(OKP_list [,7]), function (x) {no_stop_words_function(no_punctuation_function(no_digits_function(normal_form_function_paste(x))))} )

})



system.time({ 
  checkStem$normilizedForm<- sapply(checkStem [,3], function (x) {no_stop_words_function(no_punctuation_function(no_digits_function(normal_form_function_paste(x))))} )
  
})







































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


t<- paste("оный", "_ADJ", sep="")

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



# вывод нормализованной формы слова. Запускать вест блок кода целиком (не по-строчно, т.к. это REPL Python),  Команда py$task выдасть нормальную форму слова, т.е. преобразует "думающий" в "думать"
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

unlist(morph$tag("сельдерея"))[[1]]
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


# приведение слова к нормальной форме
word<- "произведенная"
p <- morph$parse(word)[1]  # Делаем полный разбор, и берем первый вариант разбора (условно "самый вероятный", но не факт что правильный)
#p$normal_form  # стать

end_normal_sign<-str_locate(as.character(unlist(p)[[1]]), "normal_form")[2]

# индекс в строке для закрывающей "кавычки" номральной формы
end_comma<-str_locate_all(as.character(unlist(p)[[1]]), "'")[[1]][6]

str_sub(as.character(unlist(p)[[1]]), start= end_normal_sign+3, end = end_comma-1 )









x <- unlist(strsplit("Электроэнергия, произведенная ГЭС (общего", " "))

x <- x[!x %in% stop_punctuation]

paste(x, collapse = " ")


gsub('[[:punct:] ]+',' ',"Электроэнергия, произведенная ГЭС (общего")

unlist(strsplit( gsub('[[:punct:] ]+',' ',"Электроэнергия, произведенная ГЭС (общего") , " "))






# вывод нормализованной формы слова
repl_python()
import pymorphy2
morph = pymorphy2.MorphAnalyzer()
word = "думающий"
p = morph.parse(word) [1] # Делаем полный разбор, и берем первый вариант разбора (условно "самый вероятный", но не факт что правильный)
task=p.normal_form  # стать
exit

py$task # передача объекта из REPL python в R


