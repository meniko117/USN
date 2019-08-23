library (jsonlite)



library(plyr) # библиотека необходима для использования rbind.fill 
# склеивания таблиц "одна под другой" при условии разного количества колонок и названий колонок

BD <-  fromJSON( sprintf("[%s]",
                         paste(readLines("C:/Users/msmirnov/Documents/проект УСН/Анализ данных/big_chunk_1_new.json", encoding = "UTF-8"),
                               collapse=",")), flatten = TRUE)


system.time({
  
  for (i in  2:10) {
    
    file_name<-paste("C:/Users/msmirnov/Documents/проект УСН/Анализ данных/big_chunk_", i, "_new.json", sep="")
    
    BD_i<- fromJSON( sprintf("[%s]",
                             paste(readLines(file_name, encoding = "UTF-8"),
                                   collapse=",")), flatten = TRUE)
    
    BD<- rbind.fill(BD, BD_i)
    
  }
  
})


# перечень полей и типов 

library(dplyr)


coltypes<-BD[1,] %>% 
  summarise_all(typeof)



coltypes_sample<-rbind(coltypes, BD[835137,])



tt<- as.data.frame(t(coltypes_sample))











# счетчик пустых массивов в поле content.item
countEmptyArrays <- 0

# вектор признаков массив пустой =1 или непустой =0
vec <- c(1:nrow(BD))

for (i in  1:nrow(BD)) {
  
  countEmptyArrays<- countEmptyArrays + ifelse (is.null(unlist(BD[i,24])), 1, 0)
  vec [i]<- is.null(unlist(BD[i,24]))
  
}

# количество чеков с пустым массивом в поле items
countEmptyArrays

# добавляем колонку с признаками "пустого массива" в поле "items" в чеке 
dat3<- cbind(BD, vec)


# остортированная таблица с пустыми чеками в поле items
emptyItemsChecks <- subset(dat3, dat3[,ncol(dat3)]==1) 

# отсортированная таблица с чеками с непустым массивом в содержании Items (может не быть нвзания позиций. но есть сумма проч.)
fullChecks <- subset(dat3, dat3[,ncol(dat3)]==0)






tt5_31<-subset(emptyItemsChecks, emptyItemsChecks[,13]==31)



#собираем массив со всеми полями внутри чеков
#инициализируем массив с навзаниями полей в чеке
colNamesArray<-as.data.frame(colnames(as.data.frame(fullChecks[1,24])))
colnames(colNamesArray)<- "colNamesEl"

# fullChecks - весь массив с чеками, у который ittems != NULL
# собираем в таблицу "colNamesArray" содержимое названий всех полей всех чеков
# с помощью union из библиотеки dplyr (см. dplyr cheat sheet)
for (i in  2:nrow(fullChecks)) {
  
  
  colNamesEl<-colnames(as.data.frame(fullChecks[i,24]))
  
  colNamesEl <- as.data.frame(as.data.frame(colNamesEl))
  
  colNamesArray<-union(colNamesArray, colNamesEl) # функция из dplyr, получаем все навзания строк при join 
  
}


write.csv(colNamesArray, "C:/Users/msmirnov/Documents/проект УСН/Анализ данных/colnames_checks.csv", row.names=FALSE)

# поиск поля с названием в базе
grep("fiscalDocumentFormatVer", colnames(BD))
grep("protocolVer", colnames(BD))
grep("receiptCode", colnames(BD))
grep("receiptCode", colNamesArray)

tt1<- subset(BD,  !is.na(BD[,15])) # чеки с заполненным "fiscalDocumentFormatVer"



# виды протоколов и их количество в поле "content.fiscalDocumentFormatVer"
head(BD%>%
       select(one_of(c("content.fiscalDocumentFormatVer")))%>%
       group_by(content.fiscalDocumentFormatVer)%>%
       count(content.fiscalDocumentFormatVer)%>%
       arrange(desc(n)),20)
















# поле CreditSum не NA и в чеке более 1-й позиции


Subs3<- subset( BD, !is.na(BD[,32]))  # сортировка исходной выборки на отличные от NA значения в поле creditSum



#признак, если хотя бы одна позиция в чеке имеет paymentType 5 или 6
for (i in  1:nrow(Subs3)) {
  
  Subs3$PaymentType5[i]<-ifelse( 5%in% as.data.frame(Subs3[i,24])$paymentType || 
                                   6%in% as.data.frame(Subs3[i,24])$paymentType, 1, 0) 
  
}

# сортируем 
Subs4<- subset( Subs3,   Subs3$PaymentType5==1 )  # получили все чеки, где paymentType 5 или 6




# сортировка чеков creditSum >0 и кол-во Items >1

fun2<- function(x) { nrow(as.data.frame(x))}

Subs3$posNum<-sapply(Subs3[,24], fun2)

BD$posNum<-sapply(BD[,24], fun2) # количество позиций в чеке
#а вот так не работает: BD$posNum<-nrow(as.data.frame(BD[,24]))

#кол-во позиций в чеке
Subs4$posNum<-sapply(Subs4[,24], fun2)

Subs4_pos<-subset(Subs4, Subs4$posNum>1)

Subs5<-subset(Subs3, Subs3$posNum>1)


Subs6<- subset( BD,   BD[,23]<0) 




colnames_BD<- as.data.frame(colnames(BD))

# уникальные поля в Subtype
uniq_subtypes<-as.data.frame(unique(BD$subtype))







##############################


BD_checks_inside<- BD




# функция, возвращающая TRUE если выполняется условие, когда все значения вектора со строковыми переменными присутствуют в названиях колонок чека
fun_ag<-function(x) { 
  all(sapply(as.data.frame(
    
    sapply(c("providerName"), grepl, colnames(as.data.frame(x)), ignore.case=TRUE) # можно вставлять в вектор искомых значений, например, , "pho", "nam"
    
  ), any)) } # функция grepl 


# сортируем массив, применяя функцию ко всем рядам исходдно data.frame и получаем массив чеков, где содержатся нужные поля, после применения 
# функции fun_ag для определния полей
BD_AgentData<- subset(BD_checks_inside, sapply(BD_checks_inside[,24],  fun_ag))


# собираем в вектор значений все содержимое непустых полей paymentType
paymentTypes<-vector()
providerInn_vec<- vector()

for (i in  1:nrow(BD_AgentData)) {
  
  
  #paymentTypes<-append(paymentTypes, as.data.frame(BD_AgentData[i,24])$paymentType)
  providerInn_vec<-append(providerInn_vec, nchar(as.data.frame(BD_AgentData[i,24])$providerInn) )
  
  
}

table(paymentTypes) # как распределяются занчения в векторе
table(providerInn_vec)

providerInn_vec_space<- vector()

# функция для определения имюеются ли пробелы в строковой переменной
fun_space<-function(x){grep(" ", x)}

providerInn_vec_space<-sapply( as.data.frame(BD_AgentData[400,24])$providerInn,   fun_space )

# инициализируем пустой вектор 
providerInn_vec_types<-vector() 

# собираем в массив все значения, например, из providerInn
for (i in 1:nrow(BD_AgentData)){
  
  #paymentTypes<-append(paymentTypes, as.data.frame(BD_AgentData[i,24])$paymentType)
  #providerInn_vec<-append(providerInn_vec, nchar(as.data.frame(BD_AgentData[i,24])$providerInn) )
  #providerInn_vec<-append(providerInn_vec, as.data.frame(BD_AgentData[400,24])$providerInn )
  
  providerInn_vec_types<-append(providerInn_vec_types, as.data.frame(BD_AgentData[i,24])$providerInn)
  
}


providerInn_vec<- as.data.frame(providerInn_vec)

providerInn_vec_types<- as.data.frame(providerInn_vec_types)
fun_space<- function(x) {length(grep(" ", x)) }
providerInn_vec_types$symb<-sapply(providerInn_vec_types[,1], fun_space) # добавляем колонку имются ли пробелы 





table(providerInn_vec_types[,2]==1) # количество полей с ИНН, куда входит пробел (все ИНН 12-значные, но у некторых послдений два знака - пробелы)
table(is.na(providerInn_vec_types[,1])) # количество NA = TRUE (т.е. где не заполен providerInn)

tt5<-subset(providerInn_vec_types, providerInn_vec_types[,2]==1)

kktREG<-nchar(BD$kktRegId)

unique(kktREG)
length(kktREG)





library(readr)
library(jsonlite)
dat2 %>% 
  toJSON() %>%
  write_lines("C:/Users/msmirnov/Documents/проект УСН/Анализ данных/dat2.json")

