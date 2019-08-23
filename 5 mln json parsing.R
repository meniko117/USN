library(jsonlite)

library(purrr)

library(data.table)

k<-5

file_name<-paste("C:/Users/msmirnov/Documents/проект УСН/Анализ данных/5 mln checks/", k, ".json", sep="")
BD_i<- readLines(file_name, encoding = "UTF-8")

# ????????? ?? json ???????
words <- lapply(BD_i,function(x) {strsplit(x, split="}/n")})


# баг в библиотеке data.table
# Subsetting does a better job of catching a malformed data.table with error https://rdrr.io/cran/data.table/f/NEWS.md



# обработка списка с объектами data.frame

possibly_some_function <- possibly(function (x) { as.data.frame(fromJSON( unlist(x)))}, otherwise = NA)  

# собираем в список все элементы после парсинга json
tt1<-map(words, possibly_some_function )
tt1 <-tt1[!is.na(tt1)] # удаляем все, что прошло парсинг

# присваиваем NA элепентам списка, которые при конвертации в data.table дают malformed data.table
fun_malform<-possibly(function(x) {subset(x, x[, .(type)==0])}, otherwise=NA)




tt2<-list()
system.time ({
  for (i in  1:length(tt1)) {
    
    elem<-map(list(as.data.table(tt1[i])), fun_malform)
    
    tt2<- append(tt2, elem)
    
  }
})

tt2 <-tt2[!is.na(tt2)] 

tt1<-tt2

dt<-rbindlist(tt1[1:length(tt1)], use.names=TRUE, fill = TRUE, idcol=NULL)

# ????? ?????????? purrr ??? ????????? ??????


# possibly_some_function <- possibly(function (x) { fromJSON( unlist(x))}, otherwise = NA)
# 
# # possibly_some_function <- possibly(function (x) { as.data.frame(fromJSON( unlist(x)))}, otherwise = NA) # преобразуем все эелементы из json строк в data.frame
# 
# tt1<-map(words, possibly_some_function )
# 
# tt1 <-tt1[!is.na(tt1)] # убираем NA-элементы листа 

# оптимизация
# system.time ({
# possibly_some_function <- possibly(function (x) { as.data.frame(fromJSON( unlist(x)))}, otherwise = NA)
# tt1<-map(words[1:10000], possibly_some_function )
# 
# tt1 <-tt1[!is.na(tt1)] # убираем NA-элементы листа 
# 
# 
# })


library(plyr)
system.time ({
  
  possibly_some_function <- possibly(function (x) { as.data.frame(fromJSON( unlist(x)))}, otherwise = NA)  
  
  all_data<-list()
  
  for (i in  1:50) {
    
    tt1<-map(words[1+(i*1000-1000):(i*1000)], possibly_some_function )
    
    tt1 <-tt1[!is.na(tt1)] # убираем NA-элементы листа 
    
    #assign(paste("dt", i, sep=""), rbindlist(tt1[1:length(tt1)], use.names=TRUE, fill = TRUE, idcol=NULL))
    
    dt<-rbindlist(tt1[1:length(tt1)], use.names=TRUE, fill = TRUE, idcol=NULL)
    
    all_data<- rbind.fill(all_data, dt)
  }
})




library(plyr)
system.time ({
  
  possibly_some_function <- possibly(function (x) { as.data.frame(fromJSON( unlist(x)))}, otherwise = NA)  
  
  all_data2<-list()
  
  for (i in  51:100) {
    
    tt1<-map(words[1+(i*1000-1000):(i*1000)], possibly_some_function )
    
    tt1 <-tt1[!is.na(tt1)] # убираем NA-элементы листа 
    
    #assign(paste("dt", i, sep=""), rbindlist(tt1[1:length(tt1)], use.names=TRUE, fill = TRUE, idcol=NULL))
    
    dt<-rbindlist(tt1[1:length(tt1)], use.names=TRUE, fill = TRUE, idcol=NULL)
    
    all_data2<- rbind.fill(all_data2, dt)
  }
})


all_data_total<-rbind.fill(all_data, all_data2)

write.csv(all_data_total, paste0("C:/Users/msmirnov/Documents/проект УСН/Анализ данных/5 mln checks/", k, ".csv"))

DT<- fread("C:/Users/msmirnov/Documents/3.csv")



dt1<- rbindlist(tt1[1:length(tt1)], use.names=TRUE, fill = TRUE, idcol=NULL) 
dt2<- rbindlist(tt1[1:length(tt1)], use.names=TRUE, fill = TRUE, idcol=NULL) 
dt3<- rbindlist(tt1[1:length(tt1)], use.names=TRUE, fill = TRUE, idcol=NULL)  
dt4<- rbindlist(tt1[1:length(tt1)], use.names=TRUE, fill = TRUE, idcol=NULL)  
dt5<- rbindlist(tt1[1:length(tt1)], use.names=TRUE, fill = TRUE, idcol=NULL) 
dt6<- rbindlist(tt1[1:length(tt1)], use.names=TRUE, fill = TRUE, idcol=NULL)  
dt7<- rbindlist(tt1[1:length(tt1)], use.names=TRUE, fill = TRUE, idcol=NULL) 
dt8<- rbindlist(tt1[1:length(tt1)], use.names=TRUE, fill = TRUE, idcol=NULL) 
dt9<- rbindlist(tt1[1:length(tt1)], use.names=TRUE, fill = TRUE, idcol=NULL) 




system.time ({
  possibly_frame <- possibly(function (x) { as.data.frame(x)}, otherwise = NA)
  dt_list <- map(tt1, possibly_frame)
  #dt_list <- map(tt1[50:1000], as.data.frame)
  dt_list <-dt_list[!is.na(dt_list)]
  
})






# Harness the power of rbind list
system.time ({
  possibly_frame <- possibly(function (x) { as.data.frame(x)}, otherwise = NA)
  dt_list <- map(tt1[1:50000], possibly_frame)
  #dt_list <- map(tt1[50:1000], as.data.frame)
  
  possibly_list<- possibly(function (x) { rbindlist((x), fill = TRUE)}, otherwise = NA)
  
  dt <- map(dt_list, possibly_list) # убрать строку?
  dt_list <-dt_list[!is.na(dt_list)]
  
  #dt_list <- map(dt_list, as.data.table) #
  
  dt<- rbindlist(dt_list, fill = TRUE) #выдает ошибку на строке 70712
  
})



system.time ({
  possibly_frame <- possibly(function (x) { as.data.frame(x)}, otherwise = NA)
  dt_list <- map(tt1, possibly_frame)
  #dt_list <- map(tt1[50:1000], as.data.frame)
  dt_list <-dt_list[!is.na(dt_list)]
  
})

#possibly_list<- possibly(rbindlist((x), fill = TRUE), otherwise = NA)

#dt1 <- map(dt_list, possibly_list) # убрать строку?


#dt_list <- map(dt_list, as.data.table) #

dt1<- rbindlist(tt1[1:10000], use.names=TRUE, fill = TRUE, idcol=NULL) #выдает ошибку на строке 70712



dt2<- rbindlist(tt1[10001:20000], use.names=TRUE, fill = TRUE, idcol=NULL) 
dt3<- rbindlist(tt1[20001:30000], use.names=TRUE, fill = TRUE, idcol=NULL) 
dt4<- rbindlist(tt1[30001:40000], use.names=TRUE, fill = TRUE, idcol=NULL) 
dt5<- rbindlist(tt1[40001:50000], use.names=TRUE, fill = TRUE, idcol=NULL) 
dt6<- rbindlist(tt1[50001:60000], use.names=TRUE, fill = TRUE, idcol=NULL) 
dt7<- rbindlist(tt1[60001:70000], use.names=TRUE, fill = TRUE, idcol=NULL) 
dt8<- rbindlist(tt1[70001:80000], use.names=TRUE, fill = TRUE, idcol=NULL) 
dt9<- rbindlist(tt1[80001:length(dt_list)], use.names=TRUE, fill = TRUE, idcol=NULL) 


library(plyr)

#
all_data<- rbind.fill(dt1, dt2, dt3, dt4, dt5, dt6, dt7, dt8, dt9)

write.csv(all_data, paste0("C:/Users/msmirnov/Documents/проект УСН/Анализ данных/5 mln checks/", i, ".csv"))

# все работает с файлом 3.json

length(dt_list)

tt1 <-tt1[!is.na(tt1)]
dt1<- rbindlist(tt1[1:50000], use.names=TRUE, fill = TRUE, idcol=NULL) 



BD_i<-n.readLines("C:/Users/msmirnov/Documents/проект УСН/Анализ данных/5 mln checks/5 mln clickhouse/data.csv", n=100, skip = 0)
all_data<- as.data.frame(BD_i)

as.data.frame(BD_i[1])

strsplit(x, split="}/n")


test1<-as.data.frame(read.table(textConnection(BD_i[1])))


BD_i<- readLines("C:/Users/msmirnov/Documents/проект УСН/Анализ данных/5 mln checks/5 mln clickhouse/data.csv", encoding = "UTF-8")

all_data<- fread("C:/Users/msmirnov/Documents/проект УСН/Анализ данных/5 mln checks/5 mln clickhouse/data.csv")




library(reticulate) # для работы с Python





lib <- import("jsonlines")

rat<-lib$open(file_name, mode='w')

repl_python()

rat = with jsonlines.open(file_name) as reader:
  