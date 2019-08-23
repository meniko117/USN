
library(readr)
library(jsonlite)




# собираем все значения из исходных файлов в массив
all_el<- vector()

for (i in  1:10) {
  
  file_name<-paste("C:/Users/msmirnov/Documents/проект УСН/Анализ данных/big_chunk_", i, "_new.json", sep="")
  
  BD_i<- readLines(file_name, encoding = "UTF-8")
  
  all_el<- append(all_el, BD_i)
  
  
}

# разбиваеим весь вектор строковых эелементов на строки по "закрывающему" тэгу json строки из сходных файлов
words <- sapply(all_el,function(x) strsplit(x, split="}/n"))

# записываем в файл
write_lines(words, "C:/Users/msmirnov/Documents/проект УСН/Анализ данных/dat2.json")


json_file <- "C:/Users/msmirnov/Documents/проект УСН/Анализ данных/dat2.json"


# время на загрузку такое же, как при загрузке из 10 файлов json - ок. 6 мин
system.time({
  
  dat2<- fromJSON( sprintf("[%s]",
                           paste(readLines(json_file, encoding = "UTF-8"),
                                 collapse=",")), flatten = TRUE)
})







##############################################################################################

dat2 %>% 
  toJSON() %>%
  write_lines("C:/Users/msmirnov/Documents/проект УСН/Анализ данных/dat2.json")



# 1-й вариант сохранения data.frame в json файл c с помощью функции
sink("C:/Users/msmirnov/Documents/проект УСН/Анализ данных/dat2.json")

lines_toJson<-function(x) { 
  
  
  cat(x%>% 
        toJSON() )
  cat("\n")
  
}

apply(dat2, 1, lines_toJson)

sink()


# 2-й вариант сохранения data.frame в json файл c с цикла

sink("C:/Users/msmirnov/Documents/проект УСН/Анализ данных/dat2.json")

for (i in 1:10000) {
  
  cat(dat2 [i,]%>% 
        toJSON() )
  cat("\n")
  
}

sink()




for (i in  1:10) {
  
  file_name<-paste("C:/Users/msmirnov/Documents/проект УСН/Анализ данных/big_chunk_", i, "_new.json", sep="")
  
  BD_i<- paste(readLines(file_name, encoding = "UTF-8"), collapse=",")
  
  
}





# считываем файл по-строчно

file_name<-"C:/Users/msmirnov/usn/src/test/resources/export_test.json"

BD_i<- readLines(file_name, encoding = "UTF-8")

# разбиваем весь строковый вектор на элементы, получаем объект List, с кол-вом элементов, равным кол-ву строк
#words <- sapply(BD_i,function(x) strsplit(x, split="/n"))

words <- sapply(BD_i,function(x) strsplit(x, split="}/n"))


# записывваем по-строчно файл  
library(readr)
write_lines(words, "C:/Users/msmirnov/Documents/проект УСН/Анализ данных/dat2.json")



for (i in  1:10) {
  cat(words[i])
  
  cat("\n")
  
}

cat(words, sep = "/n")



lapply (words, function(x) (write_lines(x, "C:/Users/msmirnov/Documents/проект УСН/Анализ данных/dat2.json")))




json_file <- "C:/Users/msmirnov/Documents/проект УСН/Анализ данных/dat2.json"


dat2<- fromJSON( sprintf("[%s]",
                         paste(readLines(json_file, encoding = "UTF-8"),
                               collapse=",")), flatten = TRUE)

dat2<- fromJSON( "C:/Users/msmirnov/usn/src/test/resources/export_test.json")


##################################################

