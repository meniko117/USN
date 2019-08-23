library (plyr)
library (data.table)




library (dplyr)

#инициализируем массив с навзаниями полей в чеке
colNamesArray<-as.data.frame(colnames(as.data.frame(fullChecks[1,28])))
colnames(colNamesArray)<- "colNamesEl"

# fullChecks - весь массив с чеками, у который ittems != NULL
# собираем в таблицу "colNamesArray" содержимое названий всех полей всех чеков
# с помощью union из библиотеки dplyr (см. dplyr cheat sheet)
for (i in  2:nrow(fullChecks)) {
  
  
  colNamesEl<-colnames(as.data.frame(fullChecks[i,28]))
  
  colNamesEl <- as.data.frame(as.data.frame(colNamesEl))
  
  colNamesArray<-union(colNamesArray, colNamesEl) # функция из dplyr, получаем все навзания строк при join 
  
}

<- subset(dat2, dat3[,68]==1) 