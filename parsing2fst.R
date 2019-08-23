
library(jsonlite)
library (fst)
library(purrr)
#library(data.table)
library (plyr)

for  (k in 27:50){
  
  #file_name<-paste("/Users/apple/Desktop/[C] Windows 7/мои документы/FLS/", k,  sep="")
  file_name<-paste("/Users/apple/Documents/FLS_app/", k,  sep="")
  
  BD_i<- readLines(file_name, encoding = "UTF-8")
  
    
    # ????????? ?? json ???????
    words <- lapply(BD_i,function(x) {strsplit(x, split="}/n")})
    
    
    
    
    # ????? ?????????? purrr ??? ????????? ??????
    
    library(purrr)
    possibly_some_function <- possibly(function (x) { as.data.frame(fromJSON( unlist(x)))}, otherwise = NA_real_)
    
    
    tt1<-map(words, possibly_some_function )
    
    tt1<-tt1[!is.na(tt1)]
    
    
    
    
    
    
    
    system.time({
    bind2<-possibly(function (x) {rbind.fill(x , x)}, otherwise = NA)
    tt1_valid<-map(tt1, bind2) # работает
    tt1_valid<-tt1_valid[!is.na(tt1_valid)]
    })
    
    Sys.sleep(30)
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
    
    Sys.sleep(30)
    
    system.time({ 
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
  
  
  # write.csv( total_all [,c(-119, -121)], paste0("C:/Users/msmirnov/Documents/проект УСН/Анализ данных/5 mln checks/", k, ".csv"))
  
  #fwrite(total_all[c(2:nrow(total_all)), ], file =paste0("C:/Users/msmirnov/Documents/проект УСН/Анализ данных/5 mln checks/", k, ".csv"))
  
  # колконка "content.items.modifiers" не встречается в названиях колонок
  ifelse(identical(grep("content.items.modifiers", colnames(total_all)), integer(0)),
         write_fst(total_all, paste0( "/Users/apple/Documents/FLS_app/", k, ".fst"), 50, uniform_encoding = TRUE),
         
         write_fst(total_all[, -grep("content.items.modifiers", colnames(total_all))], paste0( "/Users/apple/Documents/FLS_app/", k, ".fst"), 50, uniform_encoding = TRUE) )
})

  
    Sys.sleep(30)
    rm(total)
    rm(total_all)
    rm(total_i)
    rm(total_reminder)
    rm(BD_i)
    rm(tt1)
    rm(words)
    gc()
    #.rs.restartR()
    Sys.sleep(10)
}


k<-25
dt<-read_fst(paste0( "/Users/apple/Documents/FLS_app/", k, ".fst"), as.data.table = TRUE)
#colnames(dt)

# ft<- fst(paste0( "/Users/apple/Documents/FLS_app/", k, ".fst"))
# colnames(ft)
head
rm(DT_total)
gc()

