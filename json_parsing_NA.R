
library (jsonlite)



library(plyr) # библиотека необходима для использования rbind.fill 
# склеивания таблиц "одна под другой" при условии разного количества колонок и названий колонок

library (reader) # для получения доустпа к функции n.readLines

BD <-  fromJSON( sprintf("[%s]",
                         paste(n.readLines("C:/Users/msmirnov/Documents/проект УСН/Анализ данных/5 mln checks/2.json", n=2, skip = 25185),
                               collapse=",")), flatten = TRUE)



system.time({
  
  for (i in  2:10) {
    
    file_name<-paste("C:/Users/msmirnov/Documents/проект УСН/Анализ данных/5 mln checks/", i, ".json", sep="")
    
    BD_i<- fromJSON( sprintf("[%s]",
                             paste(readLines(file_name, encoding = "UTF-8"),
                                   collapse=",")), flatten = TRUE)
    
    BD<- rbind.fill(BD, BD_i)
    
  }
  
})






library (purrr)

file_name<-paste("C:/Users/msmirnov/Documents/проект УСН/Анализ данных/5 mln checks/", 2, ".json", sep="")

paste(n.readLines("C:/Users/msmirnov/Documents/проект УСН/Анализ данных/5 mln checks/2.json", n=50000, skip = 1000), collapse=",")

fromJSON( sprintf("[%s]",
                  paste(n.readLines(file_name, n=1, skip = 1000),
                        collapse=",")), flatten = TRUE)


library (plyr)


BD<- fromJSON( sprintf("[%s]",
                       paste(n.readLines("C:/Users/msmirnov/Documents/проект УСН/Анализ данных/5 mln checks/2.json", n=1, skip = 0),
                             
                             collapse=",")), flatten = TRUE)
system.time({  
  for (i in  1:100) {
    
    dat_i<-fromJSON( sprintf("[%s]",
                             paste(n.readLines(file_name, n=1, skip =i),
                                   collapse=",")), flatten = TRUE)
    
    BD<- rbind.fill(BD, dat_i)
    
  }
  
})



dat<-fromJSON( sprintf("[%s]",
                       paste(n.readLines(file_name, n=50000, skip = 1),
                             collapse=",")), flatten = TRUE)




dat<- sapply(file_name, possibly_some_function )






# загружаеим все как текст
library (jsonlite)
file_name<-paste("C:/Users/msmirnov/Documents/проект УСН/Анализ данных/5 mln checks/", 2, ".json", sep="")
BD_i<- readLines(file_name, encoding = "UTF-8")

# разбиваем на json объекты
words <- lapply(BD_i,function(x) {strsplit(x, split="}/n")})




# нужна библиотека purrr для обработки ошибок

library(purrr)
possibly_some_function <- possibly(function (x) { fromJSON( unlist(x))}, otherwise = NA_real_)

#сохраняем структуру List
possibly_some_function <- possibly(function (x) { as.list(fromJSON( unlist(x)))}, otherwise = NA_real_)

dataFrame<- ldply (tt1[3], data.frame)
dataFrame<- as.data.frame(tt1[3])


dataFrame<-as.data.frame(lapply(tt1[3], unlist))



# полный массив после парсинга json объектов, вкл. NA, если пасинг не прошел
tt1<-map(words, possibly_some_function )




test_line<-as.data.frame(tt1[1])
test_line2<-as.data.frame(tt1[5])

library(plyr)



uu_total<- rbind.fill(test_line)


system.time({
  
  for (i in  2:500) {
    
    uu_total<- rbind.fill(uu_total,as.data.frame(t(as.data.frame(unlist(tt1[i])))) )
  }
  
})



colnames(as.data.frame(t(as.data.frame(unlist(tt1[10000])))))


system.time({
  
  uu_total22<- lapply ( tt1[1:1000],  function (x) { as.data.frame(t(as.data.frame(unlist(x))) )} )
})


system.time({
  for (i in  2:1000) {
    
    uu_total23<- rbind.fill(uu_total23, as.data.frame(uu_total22[i])) 
    
  }
})



rat<- as.data.frame(uu_total22)

# read_err<- function (x) { 
#   
#   fromJSON( sprintf("[%s]",
#                     paste(n.readLines(x, n=1, skip =0),
#                           collapse=",")), flatten = TRUE)
#   
#   
# }


# лист, каждый элекмент которого является парсингом json строки
tt<- lapply (words[1:100], function (x) { fromJSON( unlist(x))})

# 
test_line<-as.data.frame(tt1[1])
test_line2<-as.data.frame(tt1[2])



uu_total<- rbind.fill(test_line, test_line2)



tt<- lapply (words[1:100], function (x) { fromJSON( unlist(x))})





tt2<-as.data.frame(tt1)


uu<-as.data.frame(unlist(tt1[50000]))
uu1<-as.data.frame(unlist(tt1[1]))
uu_total<- rbind.fill(uu, uu1)



json_test<- lapply( tt1[1:10],  function (x) {rbind.fill(as.data.frame(unlist(x)), as.data.frame(unlist(tt1[1])))}   )


as.data.frame(json_test)







possibly_some_function <- possibly(read_err, otherwise = NA_real_)

possibly_some_function <- safely(read_err)

tt<-map(words, possibly_some_function )

tt<- as.data.frame(tt)

words <- lapply(words, read_err)









possible_sqrt <- possibly(sqrt, otherwise = NA_real_)

numbers_with_error <- list(1, 2, 3, "spam", 4)

map(numbers_with_error, possible_sqrt)












col_code<-c(13,38,93)

tt5<- subset(BD, is.na (BD[,13]) &
               
               is.element(BD [,col_code], c(3,4,31,41)) == TRUE)

FUN<- function (x) { TRUE%in%is.element(x, c(3,4,31,41)) }


BD$cond2<- FUN(BD[,col_code])

tt6<- subset(BD, BD$cond2==FALSE)

tt4[1,col_code]


tt7<-  subset(BD, sapply(BD[, col_code],  function(x){is.na (x)}  ))

is.na(BD[1, col_code])

TRUE%in%is.element(BD [1,col_code], c(3,4,31,41)) 

# всегда одна из колонок, содержащих "code" содержит одно из значений c(3,4,31,41)          
tt8<- subset(BD, TRUE%in%is.element(BD [,col_code], c(3,4,31,41)) )

