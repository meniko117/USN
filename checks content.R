# номер колонки с наименованиям внутри чека
colNumberName<-grep("name", colnames(as.data.frame(dat2[1,28])))
# инициализируем вектор с данными из первого чека
checkContent<-c(as.data.frame(dat2[1,28])[,colNumberName])

# собираем в вектор "checkContent" содержимое всех чеков
for (i in  2:nrow(dat2)) {
  colNumberName<-grep("name", colnames(as.data.frame(dat2[i,28])))
  checkContent<-c(checkContent, as.data.frame(dat2[i,28])[,colNumberName])
  
}





library(data.table)



# transpose
checks <- transpose(as.data.frame(checkContent))



colnames(checks)<- "checks"



# библиотека dplyr


checksGroup<-checks %>% 
  group_by(checks) %>% 
  count(Unique_Elements = n_distinct(checks))



tokenCheckd<-subset(checksGroup, checksGroup[,3]> 1)


df5000<-subset(NDSequlaSum, NDSequlaSum[ ,15] == 5000)


