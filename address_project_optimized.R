library(quanteda)
library (dplyr)
library (tidyr)

system.time({ 
test  <- read.csv("C:/Users/msmirnov/Documents/Проект_адреса/300K_addresses.csv",sep = ";",stringsAsFactors=FALSE, row.names = NULL)


# конкатенируем все колонки, которые содержат информацию, относящуюся к адресу
test$aggregated_address<- apply( test[, c(5,6, 8, 9, c(11:14))], 1, paste,  collapse = " ")

# убираем дублированные адреса (проверяем по ключу, куда входит также VAT номер, 
# чтобы определить нет одинаковых адресов с разным VAT номером или одинаковых VAT номеров с разными адресами). Поиск дубликатов по составному ключу работает 7 мин
# простая сортирвока по уник. адресам 0.02 сек
system.time({
test<- test [!duplicated(test$aggregated_address), ]
})


# делаем попарный подсчет всех косинусных расстояний

system.time({ 
  
  # формируем корпус словарных элементов для расчета попарного косинусного расстояния
  myCorpus_pairwise <- corpus(c(original_address = test$aggregated_address[1:nrow(test)])) # через "," можно подать еще вектор для сравнения, наприммер, target1 = test$aggregated_address [1:10000]
  
  
  myDfmNoStop <- dfm(myCorpus_pairwise, remove = stopwords("english"), remove_punct = TRUE)
  
  # нашли косинусное расстояние 
  sim <- textstat_simil(myDfmNoStop , method = "cosine", margin = "documents" , upper = TRUE)  #margin="features"
  
})


# формируем квадратную матрицу с колонками и рядами, представляющими собой сравниваемые словарные элементы , в ячейках которых записано значение косинусного расстояния 
tt<-as.matrix(sim)

# освобождаем память
rm(sim)
rm(myDfmNoStop)
gc() # работает garbage collector

tt<-as.data.frame(tt)
rownames(tt)<-test$aggregated_address[1:nrow(test)]
colnames(tt)<-test$aggregated_address[1:nrow(test)]








# tt_df_total<- as.data.frame(matrix("", ncol = 3, nrow = 1), stringsAsFactors = FALSE) 
# colnames(tt_df_total)<- c("address_cluster",	"address",	"sim_coef")

# неоптимальное составление результирующей таблицы
# system.time({ 
#   
#   for (i in 1:nrow(tt)) {
#     
#     tt_filter<- subset( tt, tt [ , i]  > 0.8) 
#     
#     tt_df <-as.data.frame(cbind(row.names(tt_filter), tt_filter[,i]))
#     tt_df <-tt_df[order(tt_df[,2], decreasing = TRUE),]
#     
#     tt_df <- cbind(rep(as.character(tt_df[1,1]), nrow(tt_df)), tt_df)
#     
#     
#     colnames(tt_df)<- c("address_cluster",	"address",	"sim_coef")
#     
#     tt_df_total<- rbind(tt_df_total, tt_df)
#     
#     
#     
#     
#   } 
#   
# })

# подставляем колонку с названиями к имеющейся матрице значений косинусных расстояний



tt_row_names<-cbind(row.names(tt),tt)

# преобразовываем таблицу из "широкого" формата в "длинный" и фильтруем для получения адресов с выским коэффициентом подобия
system.time({ 
tt_df_total_optim<- tt_row_names %>% 
                      gather(address, sim_coef, 2:ncol(tt_row_names)) %>% 
                      filter (sim_coef> 0.8)

})

colnames(tt_df_total_optim)[1:2]<-c( "address", "address_cluster" )

# 
# 
# обработчик ошибок, в случае, если rbind не будте работать (при попытке соединить таблицы, с количеством строк 0)
# 
# 
# tt_df_total<- as.data.frame(matrix("", ncol = 3, nrow = 1), stringsAsFactors = FALSE) 
# colnames(tt_df_total)<- c("address_cluster",	"address",	"sim_coef")
# 
# system.time({ 
#   
#   for (i in 1:10000) {
#     
#     tt_filter<- subset( tt , tt [ , i]  > 0.8) 
#     
#     tt_df <-as.data.frame(cbind(row.names(tt_filter), tt_filter[,i]))
#     
#     
#     
#     #обработка ошибки, если результат соритровки возвращает пустой массив
#     
    # possibleError <- tryCatch(
    # 
    #   tt_df <- cbind(rep(tt_df[1,1], nrow(tt_df)), (tt_df[order(tt_df[,2], decreasing = TRUE),])),
    # 
    #   error=function(e) e
    # 
    # 
    # )
    # 
    # 
    # 
    # if(inherits(possibleError, "error")) next
    # 
    # colnames(tt_df)<- c("address_cluster",	"address",	"sim_coef")
    # 
    # tt_df_total<- rbind(tt_df_total, tt_df)

#     ;
#     
#     
#     
#   } 
#   
# })
# 
# 
# 
# 
# 
# 
# 









all_address  <- read.csv("C:/Users/msmirnov/Documents/Проект_адреса/300K_addresses.csv",sep = ";",stringsAsFactors=FALSE, row.names = NULL)

# конкатенируем все колонки, которые содержат информацию, относящуюся к адресу
all_address$aggregated_address<- apply( all_address[, c(5,6, 8, 9, c(11:14))], 1, paste,  collapse = " ")

# вводим переменную all_address - все адреса, но не загружаем ее из файла, а присваиваем уже имеющийся в памяи массив
# TODO  требует улучшения названия и исаравления кода для оптимизации




# для каждого адреса из имеющейся выборки указываем сколько раз пара по составному ключу "адрес-номер VAT" встретился в полной выборке
# фактически не было обнаружено ни одного адреса
addr_vat_composite_key<-all_address %>% 
  group_by(aggregated_address, VAT_NUMBER)  %>% 
  summarise(n = n())


library(plyr)

# в уже имеющеийся массива со схожими адресами подставляем номера VAT

address_VAT_result<-left_join(tt_df_total_optim, addr_vat_composite_key, by = c("address" = "aggregated_address"))



#address_VAT_result <- address_VAT_result [ , c(2,1,3:5)]
# обязательно для корректной работы dplyr выгрузить из памяти библиотеку plyr
detach (package:plyr)


# количество разлинчых vat номеров в группе подобных адресов
library(dplyr)
fin_summary<- address_VAT_result %>% 
  group_by(address_cluster) %>% 
  summarize(n_unique = length(unique(VAT_NUMBER)))

# получаем группы адресов, внутри которых более 2 уник vAT номеров
fin_summary_sorted<-subset(fin_summary, fin_summary$n_unique > 1)

# сортируем общий массив с адресами, остваляя только те групппы, где адреса схожи, но номера VAT внутри группы разные
vAT_problem<-address_VAT_result[address_VAT_result$address_cluster %in% fin_summary_sorted$address_cluster, ]

# vAT_problem<- vAT_problem [order(vAT_problem$address_cluster),]
# 
# 
# vAT_problem_del_duplicates<-vAT_problem[!duplicated(vAT_problem),]
# 
# 
# vAT_problem_del_duplicates <-vAT_problem_del_duplicates[ vAT_problem_del_duplicates$sim_coef !=1, ]

# сортируем массив в порядке убывания косин. расстояния
vAT_problem <- vAT_problem [order(vAT_problem$address_cluster, vAT_problem$sim_coef, decreasing = TRUE), ]

})


vAT_problem <- vAT_problem [ ,c(2,1, 3:5)]

write.table(vAT_problem, "C:/Users/msmirnov/Documents/Проект_адреса/vAT_problem_optimized_fin.csv", sep = ";",row.names = FALSE)






# keep only words occurring >= 5 times and in at least 0.95 of the documents
# dfmTrim <- dfm_trim(myDfmNoStop, min_termfreq = 5, min_docfreq = 0.95)
# sim <- textstat_simil(dfmTrim , method = "cosine", mmargin="documents", upper = TRUE)  #margin = "documents"

