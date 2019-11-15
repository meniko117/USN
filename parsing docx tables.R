library(docxtractr)

real_world <- read_docx("C:/Users/msmirnov/Documents/проект УСН/Документация/УОК_30.04.2019_проект_протокола информационного обмена_ОФД-ФНС_ФФД 1.05.docx")

#real_world <- read_docx("C:/Users/msmirnov/Documents/проект УСН/Документация/Проект_протокола информационного обмена_ОФД-ФНС_ФФД 1.05_1.3.1 (11 ноября 2019).docx")


number_of_tables<-docx_tbl_count(real_world)

tbls <- docx_extract_all_tbls(real_world)



protocol_items<-as.data.frame(tbls[1])

for (i in  2:number_of_tables) {
  
  
  protocol_items<- rbind.fill(protocol_items, as.data.frame(tbls[i]))
  
}

all_reqs<-subset( protocol_items$Имя.реквизита.в.формате.JSON, !is.na(protocol_items$Имя.реквизита.в.формате.JSON))

# описание всех реквизитов
all_reqs_description<-subset(protocol_items[, c(26:35)], !is.na(protocol_items$Имя.реквизита.в.формате.JSON))
all_reqs_description<-subset( all_reqs_description, !duplicated(all_reqs_description$Имя.реквизита.в.формате.JSON ))


# описание всех реквизитов в новой версии Протокола
all_reqs_description_new <- subset( protocol_items[, c(26:35)], !is.na(protocol_items$Имя.реквизита.в.формате.JSON))
all_reqs_description_new<-subset( all_reqs_description_new, !duplicated(all_reqs_description_new))

reqs_comparison <- merge(all_reqs_description, all_reqs_description_new,  by = "Имя.реквизита.в.формате.JSON", all= TRUE )
reqs_comparison <- reqs_comparison [!duplicated(reqs_comparison$Имя.реквизита.в.формате.JSON), ]

compare_fun<- function (x,y) {identical(x,y)}
reqs_comparison$Описание.реквизита_comp <- mapply(compare_fun, reqs_comparison$Описание.реквизита.x, reqs_comparison$Описание.реквизита.y)
reqs_comparison$Тип.данных.JSON_comp <- mapply(compare_fun, reqs_comparison$Тип.данных.JSON.x, reqs_comparison$Тип.данных.JSON.y)
reqs_comparison$Кардинальность_comp <- mapply(compare_fun, reqs_comparison$Кардинальность, reqs_comparison$КардинальностьОбязательность)
reqs_comparison$Тег_comp <- mapply(compare_fun, reqs_comparison$Тег.x, reqs_comparison$Тег.y)
reqs_comparison$Ограничения_comp <- mapply(compare_fun, reqs_comparison$Ограничения.x, reqs_comparison$Ограничения.y)
reqs_comparison$Обязательно.вверсии.1.05_comp <- mapply(compare_fun, reqs_comparison$Обязательно.вверсии.1.05.x, reqs_comparison$Обязательно.вверсии.1.05.y)
reqs_comparison$Примечания_comp <- mapply(compare_fun, reqs_comparison$Примечания.x, reqs_comparison$Примечания.y)

reqs_comparison [reqs_comparison$Описание.реквизита_comp== FALSE, ][1]
reqs_comparison [reqs_comparison$Тип.данных.JSON_comp== FALSE, ][1]


# вводим колонку  c флагом, с указанием имеются ли изменения 
reqs_comparison$changes <- apply(reqs_comparison[, c(20:26)], 1, function(x) {any(x== FALSE)})
reqs_comparison_subset <- reqs_comparison [reqs_comparison$changes == TRUE, ]

write.table(reqs_comparison_subset, "C:/Users/msmirnov/Documents/проект УСН/Документация/сравнение Протоколов июль-нояб.csv", sep =";")

df[!(duplicated(df[c("c","d")]) | duplicated(df[c("c","d")], fromLast = TRUE)), ]











# перечень всех упоминаемых реквизитов чека из Протокола
unique(all_reqs)

# загружаем данные с полями внутир чека
colNamesArray<-read.csv("C:/Users/msmirnov/Documents/проект УСН/Анализ данных/colnames_checks.csv")

# соединили поля из выборки и Протокола
tt10<-as.data.frame(union(colnames(BD), unique(all_reqs)))
colnames(tt10)<- "colNamesEl"

# соединили с полями внутри чека
tt11<- union(tt10, colNamesArray)


# фильтруем таблицу по ункальным реквизитам
library (dplyr)
reqs_types<-subset(protocol_items [, c(26,27,29,30,31)],  !is.na(protocol_items$Имя.реквизита.в.формате.JSON))%>%
  
  distinct(.[[4]], .keep_all = TRUE)

write.csv(reqs_types, "C:/Users/msmirnov/Documents/проект УСН/Документация/реквизиты_типы_переменных.csv")  


#####################
# собрка навзания из всех документов



# таблица с навзаниями полей "верхнего уровня" из чеков
col_names_upper_layer<- as.data.frame(factor(colnames(BD)))
colnames(col_names_upper_layer)<-"names"

# таблица с навзаниями полей из Протокола
col_names_protocol<- as.data.frame(factor(unique(all_reqs)))
colnames(col_names_protocol)<-"names"

col_names_upper_layer$num<-1
col_names_protocol$num<-1

# соединили названия полей верхнего уровня" из чеков с навзаниями полей из Протокола
tt12<- merge(col_names_upper_layer, col_names_protocol, by = "names", all= TRUE)


# загрузили навзания полей из Items внутри чеков
colNamesArray<-read.csv("C:/Users/msmirnov/Documents/проект УСН/Анализ данных/colnames_checks.csv")
colnames(colNamesArray)<-"names"
colNamesArray$num<-1

# получаем массив с названиями полей "верхнего" уровня, из Протокола и Items 
tt13<- merge(tt12, colNamesArray, by = "names", all= TRUE)

colnames(tt13)[c(2:4)]<- c( "верхний слой полей чека", "названия из протокола", "названия из полей Items")  

tt13[,1]<- as.character(tt13[,1])

tt13$dot_sign<- sapply(tt13[,1], function (x) { gregexpr("\\.", x) } )

# количество уровней в названиии (кол-во ".")
tt13$levels<-sapply(tt13$dot_sign, function (x) { length(unlist(x)) } )



for  (i in 1:nrow(tt13)) {
  
  tt13$First_level [i]<- ifelse(as.vector(unlist(tt13$dot_sign[i]) ) !=-1, substr(tt13[i,1], start = 1, stop = as.vector(unlist(tt13$dot_sign[i]) )-1 ) , tt13[i,1] )
  
  
  tt13$Second_level [i] <- if (tt13$levels[i] == 1) { 
    
    substr(tt13[i,1], start = as.vector(unlist(tt13$dot_sign[i]) )[1]+1, stop = nchar(tt13[i,1])) 
    
  } 
  else if (tt13$levels[i] == 2) {
    
    substr(tt13[i,1], start = as.vector(unlist(tt13$dot_sign[i]) )+1, stop = as.vector(unlist(tt13$dot_sign[i]) )[2]-1 )
    
  } 
  
  tt13$Second_level [i] <- ifelse(unlist(tt13$dot_sign[i])==-1, "", tt13$Second_level [i]) # если в навзании нет ".", то второго уровня не существует
  
  tt13$Third_level [i] <- ifelse(tt13$levels[i]==2, substr(tt13[i,1], start = as.vector(unlist(tt13$dot_sign[i]) )[2]+1, stop = nchar(tt13[i,1]) ), "" )
  
}






# уникальные поля 1-го уровня

unique_first_level<- as.data.frame(unique(tt13$First_level))
colnames(unique_first_level)<- "First_level"

unique_first_level$num<-1

# уникальные поля 2-го уровня
unique_second_level<- as.data.frame(unique(tt13$Second_level))
colnames(unique_second_level)<- "Second_level"
unique_second_level$num<-1


# уникальные поля 3-го уровня


unique_third_level<- as.data.frame(unique(tt13$Third_level))
colnames(unique_third_level)<- "Third_level"
unique_third_level$num<-1




all_attrs <- unique_first_level %>%
  full_join(unique_second_level, by= c("First_level" = "Second_level")) %>%
  full_join(unique_third_level, by= c("First_level" = "Third_level")) 


colnames(all_attrs) <- c("название атрибута", "первый уровень", "второй уровень", "третий уровень")








# атрибут может встречаться на нескольких уровнях в каждом документе (скорее всего, это будет ошибкой документа)
# проверить на каком уровне встерчается атрибут в каждом документе

all_attrs_all_docs<- all_attrs%>%
  # проверяем в какие документы входит атрибут на первом уровне "First_level"
  left_join(tt13[, c(2, grep("First_level", colnames(tt13)))], by= c("название атрибута" = "First_level")) %>%
  distinct(.[[1]], .keep_all = TRUE)%>% 
  select(-".[[1]]")%>%
  left_join(tt13[, c(3, grep("First_level", colnames(tt13)))], by= c("название атрибута" = "First_level")) %>%
  distinct(.[[1]], .keep_all = TRUE)%>% 
  select(-".[[1]]")%>%
  left_join(tt13[, c(4, grep("First_level", colnames(tt13)))], by= c("название атрибута" = "First_level"))%>%
  distinct(.[[1]], .keep_all = TRUE)%>% 
  select(-".[[1]]") %>%
  
  # проверяем в какие документы входит атрибут на втором уровне "Second_level"
  left_join(tt13[, c(2, grep("Second_level", colnames(tt13)))], by= c("название атрибута" = "Second_level"))%>%
  distinct(.[[1]], .keep_all = TRUE)%>% 
  select(-".[[1]]")%>%
  left_join(tt13[, c(3, grep("Second_level", colnames(tt13)))], by= c("название атрибута" = "Second_level")) %>%
  distinct(.[[1]], .keep_all = TRUE)%>% 
  select(-".[[1]]")%>%
  left_join(tt13[, c(4, grep("Second_level", colnames(tt13)))], by= c("название атрибута" = "Second_level"))%>%
  distinct(.[[1]], .keep_all = TRUE)%>% 
  select(-".[[1]]")%>%
  
  # проверяем в какие документы входит атрибут на третьем уровне "Third_level"
  left_join(tt13[, c(2, grep("Third_level", colnames(tt13)))], by= c("название атрибута" = "Third_level"))%>%
  distinct(.[[1]], .keep_all = TRUE)%>% 
  select(-".[[1]]")%>%
  left_join(tt13[, c(3, grep("Third_level", colnames(tt13)))], by= c("название атрибута" = "Third_level")) %>%
  distinct(.[[1]], .keep_all = TRUE)%>% 
  select(-".[[1]]")%>%
  left_join(tt13[, c(4, grep("Third_level", colnames(tt13)))], by= c("название атрибута" = "Third_level"))%>%
  distinct(.[[1]], .keep_all = TRUE)%>% 
  select(-".[[1]]")






# добавляем названия
# итоговая таблица содержит колонки c 2-й по 4-ю, где указан признак, на каком уровне вложенности встречается атрибут
# например, у атрибута "content.reason.documentDateTime" - есть 3 уровня вложенности, а у "subtype" только 1

all_attrs_all_docs_Fin<- rbind( c(rep("",4), rep("Первый уровень", 3), rep("Второй уровень", 3), rep("Третий уровень", 3)),
                                all_attrs_all_docs )


write.csv(all_attrs_all_docs_Fin, "C:/Users/msmirnov/Documents/проект УСН/Документация/сравнение_атрибутов_документов.csv")  



write.csv(tt13[,c(1:4)], "C:/Users/msmirnov/Documents/проект УСН/Документация/все_атрибуты_сырой_вид.csv") 





