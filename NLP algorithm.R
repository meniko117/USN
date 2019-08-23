# Взять текстовую строку из чека
# произвести POS -токенизацию, т.е. определить части речи
# произвести лемматизацию - привести к нормальной форме (без окончаний)
# посчитать косинусное расстояние для строки их чека и строки из классификатора ОКП
# 
# присвоить веса для частей речи (возможно, посчитать относительные веса для каждого слова, исходя из кол-ва слов в полном названии)
# найти семантически близкие слова на основе обученной модели НКРЯ (Национального Корпуса Русского языка)
# настроить параметры классификатора для увеличения точности
# - весовые коэффициенты для частей речи
# - перебор семантически близких слов из чека и ОКП для нахождения лучшего соответствия
#   
# для увеличения скорости работы классфицировать ОКП (продукты питания, напитки, электрооборудование и проч.) для последующего перебора внутри групп, а не по все перечню ОКП
# 
# https://github.com/akutuzov/webvectors/blob/master/preprocessing/rusvectores_tutorial.ipynb - инструкция по семантическим моделям 
# http://vectors.nlpl.eu/repository/11/180.zip - модель НКРЯ
# C:\Users\msmirnov\gensim дистр семантика НКРЯ.ipynb - скрипт IPython notebook для скичвания модели
# https://webdevblog.ru/gensim-rukovodstvo-dlya-nachinajushhih/ - руководство по обучению моделей gensim
# 



# анализ текста  для поиска косинусного расстояния используется библиотека Quanteda
library(quanteda)

myCorpus <- corpus(c(check = paste(unlist(OKP_list[39145,8]), collapse = " "), # название группы товаров из ОКП 
                     
                     target1 = checkStem$stemmedNames[1])) # содержание наименований конкретного чека из общего массива чеков 


myDfmNoStop <- dfm(myCorpus, remove = stopwords("english"), stem = TRUE, remove_punct = TRUE)

sim <- textstat_simil(myDfmNoStop , 'check', method = "cosine", margin = "documents") # нашли косинусное расстояние между вектором со словами в чеке и в ОКП
# первые 2 позиции в чеке "сахар", поэтому у обоих косинусное рассотяние отлично от "0" при сравнении с позицией ОКП " Сахар - песок /  - из сахарной свеклы"



# для каждой позиции чека перебрать все группы из ОКП, посчитать косинусное расстояние и отфильтровать >0



sim_data_frame <- data.frame(OKP_full_name = "char",
                             sim_coeff = 11,
                             stringsAsFactors=FALSE)

OKP_list$format<-sapply (OKP_list [,8], function (x) {paste(unlist(x), collapse = " ")} )

system.time({
  
  for (i in  30700:30800) {
    
    # myCorpus <- corpus(c(check = OKP_list[i,9], # название группы товаров из ОКП 
    #                      
    #                      target1 = checkStem$stemmedNames[21])) # содержание наименований конкретного чека из общего массива чеков 
    
    myDfmNoStop <- dfm(myCorpus, remove = stopwords("english"), stem = TRUE, remove_punct = TRUE)
    
    sim_coef<- textstat_simil(myDfmNoStop ,  method = "cosine", margin = "documents")
    
    sim_data_frame[i, ]<-c(OKP_list[i,7], sim_coef[2]) 
    
  }
  
  
})

tt<-subset(sim_data_frame, sim_data_frame[,2]>0)




# вставляем колонку с корпусом слов 

for (i in 1: nrow(OKP_list)) {
  OKP_list$corpus [i]<-
    
    corpus(c(OKP_list [i,9], # название группы товаров из ОКП 
             
             target1 = checkStem$stemmedNames[21]))
  
} 




# весь корпус слов

myCorpus <- corpus(c(group = OKP_list [ c(30753:30757) ,9]  , # название группы товаров из ОКП 
                     
                     target1 =  checkStem$stemmedNames))



myDfmNoStop <- dfm(myCorpus, remove = stopwords("english"), stem = TRUE, remove_punct = TRUE)

textstat_simil(myDfmNoStop,   selection = c("group4","group5"), method = "cosine", margin = "documents")  # корпус составлен из 5 групп ОКП (строки 30753:30757)
# 21-я позиция "шампанск" совпадает с 4-й группой и коэф отличен от "0", это 21 +5 = 26 элемент




####################################
####################################
# Формируем корпус из всех групп ОКП и всех позиций конретного чека

OKP_list$format<-sapply (OKP_list [,8], function (x) {paste(unlist(x), collapse = " ")} )

system.time({
  myCorpus <- corpus(c(group = OKP_list [ c(30000:40000),9] , # название группы товаров из ОКП 
                       
                       target1 =  checkStem$stemmedNames[c(1:100)]))
  
  
  
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


max_col_index<- vector()
max_col_value<- vector()



for (i in 1:ncol(tt2)) {
  
  
  max_col_value<- append(max_col_index, max(tt2[i,]))
  
  
}


# для анализа для кжадой позиции из чека собрать все группы из ОКп, с косинусным расстоянием больше 0, 
# т.к. необязательно, что макс. значение косинусного расстояния будет лучшим параметром

which.max(tt2[1,c(1:130)]) # определяет индекс колонки с макс значением 

checkStem[68,3]

col_name<-colnames(tt2)[which.max(tt2[68, c(1:(ncol(tt2)-nrow(tt2)-1) )])]

col_num<-as.numeric(str_sub ( col_name, start = nchar(col_name)-3, end = nchar(col_name)))

OKP_list[30000+col_num-1,7]




colnames(tt2)[which.max(tt2[3,])]


list(which.max(tt2[1,]))






# text2vec
d1_d2_cos_sim = sim2(checkStem$stemmedNames[21], checkStem$stemmedNames[21], method = "cosine", norm = "none")

d1_d2_cos_sim = sim2(itoken(paste(unlist(OKP_list[1,8]), collapse = " "), progressbar = FALSE), 
                     itoken(checkStem$stemmedNames[21], progressbar = FALSE), method = "cosine", norm = "none")

itoken(paste(unlist(OKP_list[1,8]), collapse = " "), progressbar = FALSE)
