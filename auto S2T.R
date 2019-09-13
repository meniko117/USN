library (stringi)
library (stringr)
library (text2vec)
library (jsonlite)
library (reshape2)
library (plyr)
library (quanteda)
library (reticulate)
library (snakecase)



lib <- import("pymorphy2")


#phrase = r_to_py(phrase) #передача объекта в python


morph = lib$MorphAnalyzer()



# делаем функцию POS теггинг для каждого токена. [[1]] - это выбор первого элемента из списка граммем для кжадого слова, как наиболее вероятного

funct_POS<- function (x) {morph$tag(x)[[1]]} 


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

all_atr_names<- read.csv('C:/Users/msmirnov/Documents/ГИР БО/all_atr_names_TEST.csv', row.names = NULL, header = TRUE, sep = ";")



#all_atr_names$stemmedNames<- sapply(all_atr_names [,1],  stem_tokenizer1)
all_atr_names$normalizedForm<- sapply(as.character(all_atr_names [,1]), function (x) {no_stop_words_function(no_punctuation_function(no_digits_function(normal_form_function_paste(x))))} )



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

vocab<- read.csv('C:/Users/msmirnov/Documents/ГИР БО/abbr_vocab.csv', row.names = NULL, header = TRUE, sep = ";", stringsAsFactors = FALSE)

vocab [,1]<- tolower(vocab [,1] )
vocab [,2]<- tolower(vocab [,2] )

# 
# 
# for (i in 1: nrow(all_atr_names) ) { 
#   for (j in 1: nrow(vocab) ) { 
#     
#     
#     all_atr_names$vocab[i]<-if( !is.na(str_locate (all_atr_names[i ,4] ,  ""(vocab [j,1])) )[1]) { #|| !is.na(str_locate (all_atr_names$vocab[i] ,  (vocab [j,1])) )[1]
#       gsub(tolower(vocab [j,1]), tolower(vocab[j,2]), all_atr_names[i ,4])
#     } else if ( is.na(str_locate (all_atr_names[i ,4] ,  tolower(vocab [j,1])) )[1] & is.null(all_atr_names$vocab[i])){
#       
#       all_atr_names[i ,4]        } 
#       else if (!is.null(all_atr_names$vocab[i])){
#       all_atr_names$vocab[i]
#     }
# 
#     
#   }
# }




# оптимизация
for (i in 1: nrow(all_atr_names) ) { 
  
  all_atr_names$vocab[i]<-  all_atr_names[i ,4]
  for (j in 1:nrow(vocab)) { 
    

    
    
    all_atr_names$vocab[i]<-ifelse( !is.na(str_locate (all_atr_names[i ,4] ,  vocab [j,1]) )[1],  #|| !is.na(str_locate (all_atr_names$vocab[i] ,  tolower(vocab [j,1])) )[1]
      gsub(vocab [j,1], vocab[j,2], all_atr_names$vocab[i]), all_atr_names$vocab[i] )
# 
#     all_atr_names$vocab[i]<-ifelse( is.na(str_locate (all_atr_names$vocab[i] ,  tolower(vocab [j,1])) )[1],  #|| !is.na(str_locate (all_atr_names$vocab[i] ,  tolower(vocab [j,1])) )[1]
#                                     all_atr_names$vocab[i] , gsub(tolower(vocab [j,1]), tolower(vocab[j,2]), all_atr_names[i ,4]) )
    
    
  }
}

i<-1
j<-18
# очистка от стоп-слов

all_atr_names$noStopWords<-sapply( all_atr_names$vocab, function (x) {paste(as.character(tokens_remove(tokens(x, remove_punct = TRUE), 
                                                                                                     
                                                                                                     stopwords("english"))), collapse= " ")})



all_atr_names$snakeCase <- sapply(all_atr_names$noStopWords, function (x) {to_snake_case(abbreviation_function(x), abbreviations = NULL, sep_in = "[^[:alnum:]]", parsing_option = 1, transliterations = NULL, numerals = "middle", sep_out = NULL, unique_sep = NULL, empty_fill = NULL, prefix = "", postfix = "")})






# i<-18
# j<-110


# # берем слова из глоссария
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



write.table (all_atr_names[, c(1,7)],'C:/Users/msmirnov/Documents/ГИР БО/all_atr_names_RESULT.csv',  row.names = FALSE, sep = ";")







all_atr_names[1,4] <- "great russian federal tax service in good shape"













# функция для замены слов из глассария сокращений  
paste(unlist(custom_tokenizer ("SYMBOL OF TERMINAL")), collapse = " ")

# пройти циклом полям в исходной таблицы для замены терминов 
for (i in 1: 19 ) { 
  for (j in 1: nrow(vocab) ) { 
  
    all_atr_names$vocab[i]<-ifelse( !is.na(str_locate (all_atr_names[i ,5] ,  tolower(vocab [j,1])) )[1] , #|| !is.na(str_locate (all_atr_names$vocab[i] ,  tolower(vocab [j,1])) )[1]
                                    gsub(tolower(vocab [j,1]), tolower(vocab[j,2]), all_atr_names[i ,5]), 
                                    all_atr_names$vocab[i] )
    
}
}


j<-77

gsub("INDIVIDUAL TAX PAYER", "itp", "profitable INDIVIDUAL TAX PAYER in bank")

gsub("value", "VAL", "the value of capital December")


!is.na(str_locate ("profitable INDIVIDUAL TAX PAYER in bank",  "INDIVIDUAL TAX PAYER") )


 "profitable INDIVIDUAL TAX PAYER in bank" %in% "INDIVIDUAL TAX PAYER"
 

 
 
 
 lapply (as.character(tokens (all_atr_names[19,4])), function (x) {paste(unlist(custom_tokenizer (x)), collapse = " ")})
 


 

 
 all_atr_names[1,4] <- "great russian federal tax service in good shape"

       