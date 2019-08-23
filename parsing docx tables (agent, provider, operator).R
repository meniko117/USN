library(docxtractr)
library (plyr)
library(fst)


real_world <- read_docx("C:/Users/msmirnov/Documents/проект УСН/Документация/УОК_30.04.2019_проект_протокола информационного обмена_ОФД-ФНС_ФФД 1.05.docx")
real_world <- read_docx("C:/Users/msmirnov/Documents/проект УСН/Документация/Проект_протокола информационного обмена_ОФД-ФНС_ФФД 1.05_1.1.3 (8 июля 2019).docx")

real_world <- read_docx("C:/Users/msmirnov/Documents/проект УСН/Документация/Проект_протокола информационного обмена_ОФД-ФНС_ФФД 1.05_1.1.3_согласовано УОК (16 июля).docx")


number_of_tables<-docx_tbl_count(real_world)

tbls <- docx_extract_all_tbls(real_world)



protocol_items<-as.data.frame(tbls[1])

for (i in  2:number_of_tables) {
  
  
  protocol_items<- rbind.fill(protocol_items, as.data.frame(tbls[i]))
  
}

all_reqs<-subset( protocol_items$Имя.реквизита.в.формате.JSON, !is.na(protocol_items$Имя.реквизита.в.формате.JSON))


protocol_items_subset<-protocol_items[, c("Имя.реквизита.в.формате.JSON", "Кардинальность", "Обязательно.вверсии.1.05", "Тег", "Тэг")]

protocol_items_subset<- subset( protocol_items_subset, !is.na(protocol_items$Имя.реквизита.в.формате.JSON))

library (reshape2)


dcast(protocol_items_subset, protocol_items_subset[,1] ~ protocol_items_subset[,3])


res<-reshape(protocol_items_subset, direction = "wide", idvar = "Имя.реквизита.в.формате.JSON", 
             timevar = "Кардинальность", times = seq_along(varying[[1]]))

library (data.table)
# оставляем уникальные реквизиты из Протокола
protocol_items_subset<-unique(as.data.table(protocol_items_subset), by = "Имя.реквизита.в.формате.JSON")

protocol_items_subset [c(148:153),4]<- protocol_items_subset [c(148:153),5]


# все реквизиты из Протокола, содержащие agent, operator, provider
uu<-protocol_items_subset[c(grep("gent",unlist(protocol_items_subset[,1])), grep("erator",unlist(protocol_items_subset[,1])), grep("ovider",unlist(protocol_items_subset[,1]))) , ]




# реквизиты из всех 5 млн  чеков

# собираем все реквизиты всех 50-ти исходных файлов

rat<-list()

for (i in (1:50)){
  
  cols<-list(colnames(fst(paste0( i, ".fst"))))
  
  
  rat<-append(rat, cols)
  
}

tt<-as.data.frame(unique(unlist(rat)), stringsAsFactors = FALSE)

tt<- as.data.frame(cbind(tt[,1], rep(1, nrow(tt))))

colnames(tt)<-c("reqs", "5mln_flag")

colnames(protocol_items_subset)[1]<- "reqs"



# ищем "." в строке
library(stringr)
tt$endReq<-sapply( as.character(tt[,1]), function (x) as.data.frame(str_locate_all (pattern ="\\.", x))[nrow (as.data.frame(str_locate_all (pattern ="\\.", x))),1])

number_char<- function(x) {nchar(as.character(x))}


tt$finishReq<-sapply(tt [,1],  function(x) {str_sub(x , start = as.data.frame(str_locate_all (pattern ="\\.", x))[nrow (as.data.frame(str_locate_all (pattern ="\\.", x))),1]+1, 
                                                    end =  number_char(x) )} )


for (i in (1:nrow(tt))){
  
  tt[i,4]<- ifelse( identical(unlist(tt[i,3 ]), integer(0)), as.character(tt[i,1]), unlist(tt[i,4]))
} 


tt[,1]<-as.character(tt[,4])

tt_subset<- tt[c(grep("gent",unlist(tt[,1])), grep("erator",unlist(tt[,1])), grep("ovider",unlist(tt[,1]))) , ]

tt_subset<-droplevels(tt_subset)

# берем только "концевой" реквизит
# tt_subset[,1] 
# 
# which (as.character(tt_subset[11,1]) %in% "item")




all<- merge(protocol_items_subset, tt, by = "reqs", all =TRUE)

all<- all[,c(1:6,7)]

all<- unique(all, by = "reqs")

library(readxl)

s2t_reqs<- read_xlsx("C:/Users/msmirnov/Documents/проект ККТ/S2T DL_RAW (countType, countCorrectionSubtype для sumReports)_2 версия.xlsx")
s2t_reqs<-s2t_reqs[,6]

s2t_reqs<-as.data.frame(cbind(s2t_reqs[,1], rep(1, nrow(s2t_reqs))))
colnames(s2t_reqs)<- c("reqs", "s2t_flag")

all<- merge(all, s2t_reqs, by = "reqs", all =TRUE)

all<- unique(all, by = "reqs")

all_reqs_sources<- all[, c(-5,-7)]

write.csv(all_reqs_sources, "C:/Users/msmirnov/Documents/проект ККТ/all_reqs_sources.csv")

all_subset<- all[c(grep("gent",unlist(all[,1])), grep("erator",unlist(all[,1])), grep("ovider",unlist(all[,1]))) , ] [, c(1,3,4,5)]

library(snakecase)

all$snake_case<- apply(all[,1], 1, function (x) { to_snake_case(as.character(x))})

all_snake<- all[,c(1,6)]

write.csv(all_snake, "C:/Users/msmirnov/Documents/проект ККТ/all_snake.csv")

# поиск по реквизитам
grep("taxRegi", as.data.frame(all[,1]) [,1])

grep("proper", as.data.frame(protocol_items_subset[,1]) [,1])

all[c(grep("proper", as.data.frame(all[,1]) [,1])),]
