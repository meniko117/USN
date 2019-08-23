library(sqldf)
library(dplyr)
library(stringr)

# sqldf не работает с колонками, в которых содержится тип List
# определить тип колонок
coltypes<-dat2[1,] %>% 
  summarise_all(typeof)

# преобразовать ряды в колонки
coltypes<-tidyr::gather(coltypes)


# оставляем в таблице только колонки без переменных List. datSQL<- dat2
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

# по какому ИНН больше всего пробитых чеков 
View(sqldf("SELECT userInn, user, COUNT (userInn) AS cnt FROM datSQL GROUP BY userInn  ORDER BY cnt DESC LIMIT 10" ))



# количество заполненных полей адреса
View(sqldf("SELECT  COUNT(retailPlaceAddress)  from datSQL" ))





################################################################################

head(dat2%>%
       select(one_of(c("content.userInn", "content.user")))%>%
       group_by(content.userInn)%>%
       count(content.userInn)%>%
       arrange(desc(n)),20)





View(sqldf("select userInn, user from
( 
select userInn, user, SUM(totalSum) over (partition BY userInn order by totalSum DESC) as Rank
from datSQL 
) temp " )   )

View(sqldf("select userInn, user, rank() over (partition by userInn order by totalSum DESC)  as Rank
from datSQL" ))


sqldf('select *, rank() over  (partition by "Group_A", "Group_B" order by "Value") 
       from "DF" 
       order by "Group_A", "Group_B", "Group_C" ')





#убираем дубликаты в названиях
colnames(datSQL)[29]<-"kktRegId2"
colnames(datSQL)[50]<-"protocolVersion2" 


datSQL<- ss.data.frame(datSQL)
# больше всего пробитых чеков
View(sqldf("SELECT subtype FROM datSQL LIMIT 10" ))


options(sqldf.driver = "SQLite")
