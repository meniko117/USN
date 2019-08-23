library (plyr)
library (data.table)



#инициализируем массив с навзаниями полей в чеке
colNamesArray<-transpose(as.data.frame(colnames(as.data.frame(fullChecks[1,28]))))

# fullChecks - весь массив с чеками, у который ittems != NULL
# собираем в таблицу "colNamesArray" содержимое названий всех полей всех чеков
for (i in  2:nrow(fullChecks)) {
  
  
  colNamesEl<-colnames(as.data.frame(fullChecks[i,28]))
  
  colNamesEl <- transpose(as.data.frame(colNamesEl))
  
  colNamesArray<-rbind.fill(colNamesArray, colNamesEl)
  
}


# конкантинируем все поля для каждого чека, чтобы выявить количество уникальных названий полей и их порядок

colNamesArray$concat <- do.call(paste, c(colNamesArray[c(1:9)], sep = ",")) 

#проверяем как выглядит
#paste(colNamesArray[1,], collapse =",")


# вводим дополнительную колонку, чтобы аггрегировать по кол-ву уникальных названий
colNamesArray$no <-1

#rколичество уникальных названий, вкл. их порядок
# т.е. навзавния полей в чеке могут быть одинаковыми, но их порядок разный - это формирует уникальный список полей
tt<-aggregate(colNamesArray$no, by=list(colNamesArray$concat), FUN= sum )


# поиск уникальных названий в полях чека
rowsAllNames<-as.vector(colNamesArray [,1])

#собираем все названия полей в один вектор
for (i in  2:9) {
  rowsAllNames<-c(rowsAllNames, as.vector(colNamesArray [,i]))
}

# таблица с уникальными полями
uniqueChecknames<-as.data.frame(unique(rowsAllNames))







library(dplyr)
colNamesArray %>%
  arrange()
