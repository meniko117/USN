library (jsonlite)
library (reshape2)

json_file <- "C:/Users/msmirnov/usn/src/test/resources/export_test.json"


# dat<- fromJSON(sprintf("[%s]",
#                        paste(readLines(json_file, encoding = "UTF-8"),
#                              collapse=",")))

dat2<- fromJSON( sprintf("[%s]",
                         paste(readLines(json_file, encoding = "UTF-8"),
                               collapse=",")), flatten = TRUE)







##################################################################################
# счетчик пустых массивов в поле content.item
countEmptyArrays <- 0

# вектор признаков массив пустой =1 или непустой =0
vec <- c(1:nrow(dat2))

for (i in  1:nrow(dat2)) {
  
  countEmptyArrays<- countEmptyArrays + ifelse (is.null(unlist(dat2[i,28])), 1, 0)
  vec [i]<- is.null(unlist(dat2[i,28]))
  
}


countEmptyArrays

# добавляем колонку с признаками "пустого массива" в поле "items" в чеке 
dat3<- cbind(dat2, vec)


# остортированная таблица с пустыми чеками в поле items
emptyItemsChecks <- subset(dat3, dat3[,76]==1) 
#####################################################################


#агреггируем данные номер docId ~ Ofd
resTable<-dcast([,c(2,3)], dat2[,3] ~ dat2[,2])

# пример сортировки по порядковому номеру 39
ofdSubset <- subset(dat3, dat3[,3]==39) 

#сортировка по УСН taxationtype 1 или 4
sortTaxationType <- subset(dat3, dat3[,14]==c(1,4))


########################################################################
## выбор чеков, где пустое поле name

vec2 <- c(1:nrow(dat2))

for (i in  1:nrow(dat2)) {

  colNumber<-grep("name", colnames(as.data.frame(dat2[i,28]))) # номер колонки в чеке с именем "name"

  # признак колонки 1, если полен name незаполнено    

       vec2 [i]<- ifelse(!is.null(unlist(dat2[i,28])) && as.data.frame((dat2[i,28]))[1, colNumber]==""
                         ||as.data.frame((dat2[i,28]))[1, colNumber]==" "
                         ,1,0)
  
}






dat3<- cbind(dat3, vec2)


# остортированная таблица с чеками, где неуказано поле name (по крайней мере в первой строке)
emptyNamesChecks <- subset(dat3, dat3[,77]==1) 

#########################################################################




grep("id|Id", colnames(dat))
grep("item", colnames(dat2))
grep("taxation", colnames(dat2))
grep("Sign", colnames(dat2))

colnames(dat2) [c(24,32,46,56,60,62,63,73)]

grep("nds", colnames(dat2))
allNDScolumns<-colnames(dat2) [c(33,35,36,39,40,54)]

#присваиваем 0, где NDS = NA

emptyNamesChecks[,allNDScolumns][is.na(emptyNamesChecks[,allNDScolumns])]<-0

# документы с полем sum= NA 
NAsum<-subset(dat2, is.na(dat2[,21])==TRUE)

#чеки, где не заполнено name и НДС не нечислен
noNdsAtAll<- subset(emptyNamesChecks, rowSums(emptyNamesChecks[,allNDScolumns])==0)


dat2[,allNDScolumns][is.na(dat2[,allNDScolumns])]<-0
noNdsTotal<- subset(dat2, rowSums(dat2[,allNDScolumns])==0)


# количество чеков, где SUM= НДС
dat2[, 15][is.na(dat2[,15])]<-0
dat2[,allNDScolumns][is.na(dat2[,allNDScolumns])]<-0

 NDSequalSum<- subset(dat2, dat2[,15]== rowSums(dat2[,allNDScolumns])) 


# количество чеков по TaxationType|, где не указан НДС вообще нигде
library(dplyr)
count(noNdsTotal, noNdsTotal[,14])


grep("protocolVersion", colnames(dat2))


grep("items|Items", colnames(dat2))






