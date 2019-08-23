library(jsonlite)
library (fst)

library(purrr)

#library(data.table)

library (plyr)


for  (k in 24:25){
  
  
  file_name<-paste("C:/Users/msmirnov/Documents/проект УСН/Анализ данных/5 mln checks/", k, sep="")
  BD_i<- readLines(file_name, encoding = "UTF-8")
  
  system.time({
    
    # ????????? ?? json ???????
    words <- lapply(BD_i,function(x) {strsplit(x, split="}/n")})
    
    
    
    
    # ????? ?????????? purrr ??? ????????? ??????
    
    library(purrr)
    possibly_some_function <- possibly(function (x) { as.data.frame(fromJSON( unlist(x)))}, otherwise = NA_real_)
    
    
    tt1<-map(words, possibly_some_function )
    
    tt1<-tt1[!is.na(tt1)]
    
    
    
    
    
    
    
    
    
    bind2<-possibly(function (x) {rbind.fill(x , x)}, otherwise = NA)
    tt1_valid<-map(tt1, bind2) # работает
    tt1_valid<-tt1_valid[!is.na(tt1_valid)]
    
    
    
    #total<- as.data.frame(lapply(tt1[10:25], function (x) { rbind.fill(as.data.frame(tt1[1]), as.data.frame(x))}))
    
    # system.time({
    # total<- rbind.fill(map(tt1[64300:96548], function (x){as.data.frame(x)} ))
    # })
    
    # 
    # system.time({
    #   total<- rbind.fill(map(tt1[1:1000], function (x){as.data.frame(x)} ))
    # })
    
    tt1<-tt1_valid  
    
    rm(tt1_valid)
    
    total<- rbind.fill(map(tt1[1:1], function (x){as.data.frame(x)} ))
    
    
    
    for  (i in 1:(floor(length(tt1)/20000))){
      
      total_i<- rbind.fill(map(tt1[(1+(i*20000-20000)):(i*20000)], function (x){as.data.frame(x)} ))
      total<-rbind.fill(total, total_i)
    }
    
    total_reminder<-rbind.fill(map(tt1[(floor(length(tt1)/20000)*20000+1):length(tt1)], function (x){as.data.frame(x)} ))
    
    total_all<- rbind.fill(total,total_reminder)
    
    # убрать дубликаты
    total_all <- total_all[-which(duplicated(total_all)), ]
    
    # определить в каких колонках содержится list
    # col_del<-subset(lapply(total_all[10,], function(x) {typeof(x)}), lapply(total_all[10,], function(x) {typeof(x)})== "list")
    # col_del<-match("list", lapply(total_all[1,], function(x) {typeof(x)}))
    # 
    # 
    # types_col<-list(unlist(lapply(total_all[10,], function(x) {typeof(x)}[[1]])))
    # 
    # which("list" %in% lapply(total_all[10,], function(x) {typeof(x)}))
    # 
    # total_all <- total_all [, - col_del ]
    # 
  })
  
  # write.csv( total_all [,c(-119, -121)], paste0("C:/Users/msmirnov/Documents/проект УСН/Анализ данных/5 mln checks/", k, ".csv"))
  
  #fwrite(total_all[c(2:nrow(total_all)), ], file =paste0("C:/Users/msmirnov/Documents/проект УСН/Анализ данных/5 mln checks/", k, ".csv"))
  
  # колконка "content.items.modifiers" не встречается в названиях колонок
  ifelse(identical(grep("content.items.modifiers", colnames(total_all)), integer(0)),
         write_fst(total_all, paste0( k, ".fst"), 50, uniform_encoding = TRUE),
         
         write_fst(total_all[, -grep("content.items.modifiers", colnames(total_all))], paste0( k, ".fst"), 50, uniform_encoding = TRUE) )
  
}





library(data.table)
system.time({
  DT<- fread(paste0("C:/Users/msmirnov/Documents/проект УСН/Анализ данных/5 mln checks/", k, ".csv"))
})

DT1<-fread(paste0("C:/Users/msmirnov/Documents/проект УСН/Анализ данных/5 mln checks/",1, ".csv"))
DT2<-fread(paste0("C:/Users/msmirnov/Documents/проект УСН/Анализ данных/5 mln checks/",2, ".csv"))
DT3<-fread(paste0("C:/Users/msmirnov/Documents/проект УСН/Анализ данных/5 mln checks/",3, ".csv"))
DT4<-fread(paste0("C:/Users/msmirnov/Documents/проект УСН/Анализ данных/5 mln checks/",4, ".csv"))
DT5<-fread(paste0("C:/Users/msmirnov/Documents/проект УСН/Анализ данных/5 mln checks/",5, ".csv"))



total_all <- total_all[-which(duplicated(total_all)), ]


# 
library (plyr)
system.time({
  DT_ALL<- rbind.fill(DT1,DT2,DT3,DT4,DT5)
})


is.null()
total_all<-total_all[!is.null(total_all)]

total_all[is.null(total_all)]<-0


write_fst(DT1, "17.fst", 50, uniform_encoding = TRUE)


# Default compression
write_fst( total_all[ , -119], "17.fst", 100, uniform_encoding = TRUE)

# пример загрузки через fst
UP<-read_fst("18.fst", as.data.table = TRUE, columns = "content.items.name", from = 1, to = 15)
UP<-read_fst("19.fst", as.data.table = TRUE)

library (reader)
DT1<-n.readLines("C:/Users/msmirnov/Documents/проект УСН/Анализ данных/5 mln checks/5 mln clickhouse/data.csv", n=10000, skip = 0)
write.csv(DT1, paste0("C:/Users/msmirnov/Documents/проект УСН/Анализ данных/5 mln checks/", "test", ".csv"))

DT2<- fread("C:/Users/msmirnov/Documents/проект УСН/Анализ данных/5 mln checks/test.csv")



# проврека, находится ли тип list  в какой либо колонке
subset(total_all, sapply(total_all[,119],  function(x) {!is.null(unlist(x))}))

total_all[225314 ,119] <- unlist(total_all[225313 ,119])[[1]]

total_all[ , -119]

total_all[ ,119] <- as.data.frame(total_all[ ,119])

total_all[ ,119]<- unnest(tibble(total_all[, 119]), drop = NA, .id = NULL, .sep = NULL, .preserve = NULL)


total_all$unlist<-sapply(total_all[, 119], function (x) {unlist(x)[[1]]})

total_all$unlist<-do.call(unlist(total_all[, 119], recursive=FALSE))

do.call(total_all[, 119], unlist(total_all[, 119], recursive=FALSE))


total_unlist<- list()

for (i in 1: nrow(total_all)) {
  
  total_unlist[i]<- append(total_unlist, unlist(total_all[i, 119]))
  
}


total_unlist<- list()
system.time ({ 
  total_unlist <- lapply(total_all[, 119], function (x) {append(total_unlist, unlist(x)[[1]]) })
})

total_all$unlist<- total_unlist

colnames(total_all)[ncol(total_all)]<-colnames(total_all)[119]


tets_json<-toJSON(total_all)

write(tets_json, "C:/Users/msmirnov/Documents/проект УСН/Анализ данных/5 mln checks/test.json")

uu<- fromJSON("C:/Users/msmirnov/Documents/проект УСН/Анализ данных/5 mln checks/test.json")






















flattenlist <- function(x){  
  morelists <- sapply(x, function(xprime) class(xprime)[1]=="list")
  out <- c(x[!morelists], unlist(x[morelists], recursive=FALSE))
  if(sum(morelists)){ 
    Recall(out)
  }else{
    return(out)
  }
}


total_all$unlist<-lapply (list(total_all[, 119]), flattenlist)


total_all$unlist[225313]

fun_unlist<- function (x) {unlist(x)[[1]]}

unlist_vector<-fun_unlist (total_all[, 119])

fun_unlist( total_all[225313 ,119])

total_all$unlist[225313]<-unlist(total_all$unlist[225313])

total_all$unlist2[225313]
total_all[225313, 119]

total_all[ ,119]<-sapply (total_all[ ,119], function (x) {as.data.frame((x))})



###############################################
install.packages("RClickhouse")

con <- DBI::dbConnect(RClickhouse::clickhouse(), host="192.168.34.189:8123")

jdbc:clickhouse://192.168.34.189:8123/default

default db
port 8123


library (RClickhouse)
library (DBI)
con <- DBI::dbConnect(RClickhouse::clickhouse(), host="192.168.34.189", db="default")

#(host="localhost", port = 9000, db = "default", user = "default", password = "", compression = "lz4")

res <- DBI::dbGetQuery(con, "SELECT (*)
                             FROM receipts
                             WHERE kktRegId = 0000453043022071 ")

res <- DBI::dbGetQuery(con, "SELECT (*)
                             FROM receipts
                              WHERE ofdId = "ofd5" ")
library(dplyr)

test_dt<-as.data.frame(tbl(con, "receipts") %>% filter(ofdId == "ofd5"))

system.time({ 
  test_dt<-as.data.frame(tbl(con, "receipts") )
})



