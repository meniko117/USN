library(sqldf)

# sqldf не работает с колонками, в которых содержится тип List
# определить тип колонок
coltypes<-dat2[1,] %>% 
  summarise_all(typeof)

# преобразовать ряды в колонки
coltypes<-tidyr::gather(coltypes)


# оставляем в таблице только колонки без переменных List
datSQL<- dat2[, c(1:23, 25:27, 29:36, 39:42, 46:54, 56:75)]


# убираем "content" из названий колонок, т.к. sqldf не идентифицирует эти названия
library(stringr)

colnames(datSQL)<-colnames(datSQL) %>% str_replace("content.", "")

#убираем дубликаты в названиях
colnames(datSQL)[27]<-"kktRegId2"
colnames(datSQL)[43]<-"protocolVersion2" 


# ПИШЕМ SQL запросы

# группируем чеки по ofd
View(sqldf("SELECT ofdId, COUNT (ofdId) FROM datSQL GROUP BY ofdId " ))

# cумма чека равна сумме некторых полей с НДС
View(sqldf("SELECT * FROM datSQL WHERE totalSum = nds0 + nds18+ ndsNo " ))

# больше всего пробитых чеков
View(sqldf("SELECT userInn, user, COUNT (userInn) AS cnt FROM datSQL GROUP BY userInn  ORDER BY cnt DESC LIMIT 10" ))


# https://jasminedaly.com/tech-short-papers/sqldf_tutorial.html





 

