library (stringi)
library (stringr)
library (text2vec)
library (jsonlite)
library (reshape2)
library (plyr)
library (quanteda)
library (reticulate) # загружаем библиотеку для работы с Python
library (snakecase)


# загружаем библиотеку Python для приведения слова к нормальной форме. В некторых случаях качество перевода для наименования поля улучшается.
# сейчас в скрипте релазован перевод не нормальных форм, а исходных фраз в строке, где вводится колонка all_atr_names$translation
lib <- import("pymorphy2")

morph = lib$MorphAnalyzer() # определеям функцию из билилотеки Python pymorphy2


# делаем функцию POS теггинг для каждого токена. [[1]] - это выбор первого элемента из списка граммем для кжадого слова, как наиболее вероятного

funct_POS<- function (x) {morph$tag(x)[[1]]} # функция для определения граммем (род, время, число)


morph_function<- function (x) {morph$parse(x)[1] } # функция для определения нормальной формы, вкл. список граммем 

# определеяем 2 вспомогательные функции для парсинга информации, которую выдает morph_function для получения нормальной формы 
end_normal_sign_function<- function (x) {str_locate(as.character(unlist(x)[[1]]), "normal_form")[2]}

end_comma_function<-function (x) {str_locate_all(as.character(unlist(x)[[1]]), "'")[[1]][6]}

# функция для получения номральной формы
normal_form_function<- function (x) {str_sub(as.character(unlist(morph_function(x))[[1]]), 
                                             start= end_normal_sign_function(morph_function(x))+3, 
                                             end = end_comma_function (morph_function(x))-1 )}

normal_form_function_paste<- function(x) {paste(unlist(lapply( unlist(strsplit((x), " ")),normal_form_function)), collapse = " ")}




# функция для очистки текста от цифр
no_digits_function<- function(x) {stringi::stri_replace_all_regex(x, "\\d", "")}

# перечень  стоп-слов на русском языке (можно расширять)
russianStopWords<- c("с", "в", "на", "перед", "из", "г", "кг", "килограмм" ,"мл", "л", "литр", "вес")

# функция для очистки текста от стоп-слов
no_stop_words_function<- function (x) {  paste(   unlist(strsplit(x, " "))[!(unlist(strsplit(x, " "))) %in% russianStopWords], collapse =" " ) }

# функция для очистки текста от пунктуации
no_punctuation_function<- function (x) {paste(unlist(tokens(as.character(x), remove_punct = TRUE)), collapse= " ")}

# загружаем данные с названиями полей на русском для перевода на английский 
all_atr_names<- read.csv('C:/Users/msmirnov/Documents/ГИР БО/all_atr_names_TEST.csv', row.names = NULL, header = TRUE, sep = ";")


# создаем колонку со словами, приведенными к нормальной форме
all_atr_names$normalizedForm<- sapply(as.character(all_atr_names [,1]), function (x) {no_stop_words_function(no_punctuation_function(no_digits_function(normal_form_function_paste(x))))} )








# сгенерированный ключ API Яндекс переводчик, который генерируется на сайте Яндекса
api_key<- 'trnsl.1.1.20190904T193649Z.c9d1c19d6a48e61c.cabe507a19291f0040dca170371df858f1a8c4e3'

# определяем функцию для передачи в Яндекс переводчик текста
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






# переопределяем функция для перевода, чтобы отправлять в кодировке enc2utf8(x)
translate_yandex<-function (x) {translate (api_key,text=enc2utf8(x))}

# перевод на английский через API Яндекс
# перевод нормализованной формы слова
#all_atr_names$translation<- sapply( all_atr_names[,3], function (x) {unlist(lapply(x, translate_yandex))[[2]]})
 
# перевод исходных фраз
  all_atr_names$translation<- sapply( as.character(all_atr_names[,1]), function (x) {unlist(lapply(x, translate_yandex))[[2]]})
  all_atr_names$translation<- tolower(all_atr_names$translation)


# функция для сокращения слов в зависимости от длины слова
abbreviation_function<-  function (x) paste(unlist(lapply( as.character(tokens(x)), function (x) { ifelse(nchar(x)>10, abbreviate(x, 6, strict = FALSE), abbreviate(x, 5, strict = TRUE)) })), collapse = " ")








# замена слов из словаря сокращений

# скачиваем глоссарий сокращений Naming Convention
vocab<- read.csv('C:/Users/msmirnov/Documents/ГИР БО/abbr_vocab.csv', row.names = NULL, header = TRUE, sep = ";", stringsAsFactors = FALSE)

# меняем весь текст на прописные буквы
vocab [,1]<- tolower(vocab [,1] )
vocab [,2]<- tolower(vocab [,2] )


# разбиваем справочник на 2 категории: фразы и слова

vocab_phrase<- subset (vocab , str_detect (vocab [,1], ' '))
vocab_word<- subset (vocab , !str_detect (vocab [,1], ' '))



# замена фраз


for (i in 1: nrow(all_atr_names) ) { 
  
  all_atr_names$vocab[i]<-  all_atr_names[i ,4]
  for (j in 1:nrow(vocab_phrase)) { 
    
    
    all_atr_names$vocab[i]<-ifelse( !is.na(str_locate (all_atr_names[i ,4] ,  vocab_phrase [j,1]) )[1],  #|| !is.na(str_locate (all_atr_names$vocab[i] ,  tolower(vocab [j,1])) )[1]
                                    gsub(vocab_phrase [j,1], vocab_phrase[j,2], all_atr_names$vocab[i]), all_atr_names$vocab[i] )
   
  }
}


# замена слов

for (i in 1: nrow(all_atr_names) ) { 
  
  #all_atr_names$vocab[i]<-  all_atr_names$vocab[i] 
  for (j in 1:nrow(vocab_word)) { 
    
    all_atr_names$vocab[i]<-ifelse( !is.na(str_locate (all_atr_names$vocab[i] ,  vocab_word [j,1]) )[1],  #|| !is.na(str_locate (all_atr_names$vocab[i] ,  tolower(vocab [j,1])) )[1]
                                    paste(unlist(lapply (strsplit(all_atr_names$vocab[i], split  =" ")[[1]], 
                                    function (x) {str_replace( x, paste0("\\b",vocab_word [j,1],"\\b"), vocab_word[j,2] )})), collapse = ' '), 
      all_atr_names$vocab[i] )
    
    
  }
}

# 
# # тест цикла
# for (i in 1: nrow(all_atr_names) ) {
# 
#   #all_atr_names$vocab[i]<-  all_atr_names$vocab[i]
#   for (j in 1:nrow(vocab)) {
# 
#     all_atr_names$vocab2[i]<-ifelse( !is.na(str_locate (all_atr_names$vocab[i] ,  vocab [j,1]) )[1],  #|| !is.na(str_locate (all_atr_names$vocab[i] ,  tolower(vocab [j,1])) )[1]
#                                     paste(unlist(lapply (strsplit(all_atr_names$vocab[i], split  =" ")[[1]],
#                                                          function (x) {str_replace( x, paste0("\\b",vocab [j,1],"\\b"), vocab[j,2] )})), collapse = ' '),
#                                     all_atr_names$vocab[i] )
# 
# 
#   }
# }



# очистка от стоп-слов на английском языке

all_atr_names$noStopWords<-sapply( all_atr_names$vocab, function (x) {paste(as.character(tokens_remove(tokens(x, remove_punct = TRUE), 
                                                                                                     
                                                                                                     stopwords("english"))), collapse= " ")})


# составление названия поля  в формате snake_case из полученных сокращений
all_atr_names$snakeCase <- sapply(all_atr_names$noStopWords, function (x) {to_snake_case(abbreviation_function(x), abbreviations = NULL, sep_in = "[^[:alnum:]]", parsing_option = 1, transliterations = NULL, numerals = "middle", sep_out = NULL, unique_sep = NULL, empty_fill = NULL, prefix = "", postfix = "")})



# сохраняем результат работы скрипта, выбрав инетерсующие колонки
write.table (all_atr_names[, c(1,4, 7)],'C:/Users/msmirnov/Documents/ГИР БО/all_atr_names_RESULT.csv',  row.names = FALSE, sep = ";")







# # берем слова из загруженного глоссария
# syn_1<- vocab[,1]
# 
# # создаем "словарь"
# names(syn_1)<-vocab[,2]
# 
# syn_replace_table <- syn_1
# 
# 
# 
# library(fastmatch)
# 
# syn_1 = c("nice", "like")
# names(syn_1) = rep('happy_emotion', length(syn_1))
# syn_2 = c("automobile")
# names(syn_2) = rep('car', length(syn_2))

# syn_replace_table = c(syn_1, syn_2)

# custom_tokenizer = function(text) {
#   word_tokenizer(text) %>% lapply(function(x) {
#     i = fmatch(x, syn_replace_table)
#     ind = !is.na(i)
#     i = na.omit(i)
#     x[ind] = names(syn_replace_table)[i]
#     x
#   })
# }
# 
# 
# all_atr_names$vocabFin<- sapply( all_atr_names[ ,4], function (x){
#   paste(lapply (as.character(tokens (toupper(x))), function (x) {paste(unlist(custom_tokenizer (x)), collapse = " ")}), collapse = " ")} )
# 

