
# 
library(jsonlite)
library (fst)

library(purrr)

#library(data.table)

library (plyr)



file_name<-paste("C:/Users/msmirnov/Documents/Парсинг_жалоб/claims.json")
BD_i<- readLines(file_name, encoding = "UTF-8")

json_parse_raw<-fromJSON( BD_i)


library(docxtractr)

real_format <- read_docx("C:/Users/msmirnov/Documents/Парсинг_жалоб/Сравнение новых и старых жалоб.docx")
number_of_tables<-docx_tbl_count(real_format)

tbls <- docx_extract_all_tbls(real_format)  

all_tbl<-rbind.fill(as.data.frame(tbls[1]), as.data.frame(tbls[2]), as.data.frame(tbls[3]))
colnames(all_tbl)[1]<- "req_name"

headings<- as.data.frame(colnames(json_parse))
headings<- cbind(headings, rep(1, nrow(headings)))

colnames(headings) [1] <- "req_name"

all_tbl_real_headings<- merge(all_tbl, headings, by = "req_name", all = TRUE)

all_tbl_real_headings<-all_tbl_real_headings[order(all_tbl_real_headings$Описание), ]


json_parse$unlist_id <- unlist(json_parse$`_id`)

unlist_fun<- function (x) {unlist(x, use.names = FALSE)}
unlist_fun(json_parse$unlist_sum[13])

json_parse$unlist_sum[13]<-unlist_fun(json_parse$unlist_sum[13])

json_parse$unlist_sum<- vapply(json_parse$unlist_sum, paste, collapse = ", ", character(1L))

json_parse$unlist_sum<- unlist_fun(json_parse$sum)

json_parse<-json_parse[, -32]

json_parse$unlist_sum <- sapply (json_parse [, 16], unlist_fun)

colnames(json_parse)[31:32] <-  colnames(json_parse_raw)[c(1,16)]

json_parse$sum1<- gsub('', NA, json_parse$sum1)

write.table(json_parse[ ,c(31,2:15, 32, 17:30)], "C:/Users/msmirnov/Documents/Парсинг_жалоб/json_parse.csv", row.names = FALSE,   sep = ';')





json_parse$phone[is.list(json_parse[,2])][1:5]
nrow(subset(json_parse, sapply(json_parse[,1], function (x) {is.list(x)})) [1:5,]

     is.list(json_parse[,3])
     