# номер колонки с наименованиям внутри чека
colNumberName<-grep("name", colnames(as.data.frame(dat2[1,28])))
# инициализируем вектор с данными из первого чека
checkContent<-c(as.data.frame(dat2[1,28])[,colNumberName])

# собираем в вектор "checkContent" содержимое всех чеков
for (i in  2:nrow(dat2)) {
  colNumberName<-grep("name", colnames(as.data.frame(dat2[i,28])))
  checkContent<-c(checkContent, as.data.frame(dat2[i,28])[,colNumberName])
  
}

# для BD - выборки на 1 млн чеков

# номер колонки с наименованиям внутри чека
colNumberName<-grep("name", colnames(as.data.frame(BD[1,24])))
# инициализируем вектор с данными из первого чека
checkContent<-c(as.data.frame(BD[1,24])[,colNumberName])

# собираем в вектор "checkContent" содержимое всех чеков
for (i in  2:nrow(BD)) {
  colNumberName<-grep("name", colnames(as.data.frame(BD[i,24])))
  checkContent<-c(checkContent, as.data.frame(BD[i,24])[,colNumberName])
  
}


# пишем функцию для сбора содержимого всех чеков

all_items <- rbind.fill(sapply(BD[,24], function (x) {as.data.frame (x)}) )

all_items_names<- all_items[,1]

# дубликаты id
tt<-subset(as.data.frame(table(BD[,10])), as.data.frame(table(BD[,10]))[,2]>1)


library(data.table)



# transpose
checks <- transpose(as.data.frame(checkContent))



colnames(checks)<- "checks"



# библиотека dplyr


checksGroup<-checks %>% 
  group_by(checks) %>% 
  count(Unique_Elements = n_distinct(checks))



tokenCheckd<-subset(checksGroup, checksGroup[,3]> 1)


df5000<-subset(NDSequalSum, NDSequalSum[ ,15] == 5000)


