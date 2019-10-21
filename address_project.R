library(quanteda)

test  <- read.csv("C:/Users/msmirnov/Documents/Проект_адреса/300K_addresses.csv",sep = ";",stringsAsFactors=FALSE, row.names = NULL)


# конкатенируем все колонки, которые содержат информацию, относящуюся к адресу
test$aggregated_address<- apply( test[, c(5,6, 8, 9, c(11:14))], 1, paste,  collapse = " ")

# убираем дублированные адреса
test<- test [!duplicated(test$aggregated_address), ]

# создаем пустую таблицу- заготовку для наполнения 10 наиболее подходящими адресами (по косинусному рассотоянию)
empty_df <- as.data.frame(matrix("", ncol = 10, nrow = nrow(test)), stringsAsFactors = FALSE) 

test<- cbind(test, empty_df)


system.time({ 
  
  for (i in  1:nrow(test)) {

    # название позиции из конкретного чека сравниваем со всеми группами из ОКП
    myCorpus <- corpus(c(original_address = test$aggregated_address [i] , 
                         
                         target1 = test$aggregated_address )) # target1 - наименования всех позиций из общего массива чеков 
    
    
    
    
    
    myDfmNoStop <- dfm(myCorpus, remove = stopwords("english"), stem = TRUE, remove_punct = TRUE)
    
    # нашли косинусное расстояние между позицией в чеке и всеми группами ОКП 
    sim <- textstat_simil(myDfmNoStop , 'original_address', method = "cosine", margin = "documents") 
    # первые 2 позиции в чеке "сахар", поэтому у обоих косинусное рассотяние отлично от "0" при сравнении с позицией ОКП " Сахар - песок /  - из сахарной свеклы"
    
    
    

    
    # первый элемент вектора sim со коэф. косинусного расстояния - это расстояние с самим элементом, игнорируем
    address_fit<-cbind(test$aggregated_address,  sim[c(2:length(sim))])
   
    # сортируем массив с названиями по величине коиснусного  расстояния для каждой позиции и выбранной группы ОКП
    address_fit<- address_fit[order( address_fit[,2], decreasing = TRUE),]
  
     test[i, c((ncol(test)-9) :ncol(test))] <- as.character(address_fit[c(1:10),1])


    
    
    
  }   })


write.table(test, "C:/Users/msmirnov/Documents/Проект_адреса/test.csv", sep = ";")

system.time({ 

for (i in  1:nrow(test)) {

myCorpus <- corpus(c(original_address = test$aggregated_address [i] , 
                     
                     target1 = test[i,17] )) # target1 - наименования всех позиций из общего массива чеков 





myDfmNoStop <- dfm(myCorpus, remove = stopwords("english"), stem = TRUE, remove_punct = TRUE)

# нашли косинусное расстояние между позицией в чеке и всеми группами ОКП 
sim <- textstat_simil(myDfmNoStop , 'original_address', method = "cosine", margin = "documents") 
# первые 2 позиции в чеке "сахар", поэтому у обоих косинусное рассотяние отлично от "0" при сравнении с позицией ОКП " Сахар - песок /  - из сахарной свеклы"
test$sim_coef[i] <- as.numeric(sim)[2]
}

  })













all_address  <- read.csv("C:/Users/msmirnov/Documents/Проект_адреса/300K_addresses.csv",sep = ";",stringsAsFactors=FALSE, row.names = NULL)


# конкатенируем все колонки, которые содержат информацию, относящуюся к адресу
all_address$aggregated_address<- apply( all_address[, c(5,6, 8, 9, c(11:14))], 1, paste,  collapse = " ")

library(reshape2)
vAT_address <- dcast(all_address,  VAT_NUMBER ~ aggregated_address)

# сколько раз встречается vat  на всем списке ункильных адресов
length(subset( as.numeric(vAT_address[2,c(2:13811)]),  as.numeric(vAT_address[2,c(2:13811)])   >0))


vAT_address$address_count <- apply(vAT_address[ ,c(2:13811)],  1,
                                    function (x) {length(subset( x, x>0))})

#"GB720113890" встречается на 245 адресах
subset(vAT_address, vAT_address$address_count>245)[,1]



#сколько колонок с адресами для каждого vat
subset( as.numeric(vAT_address[100,c(2:13811)]),  as.numeric(vAT_address[100,c(2:13811)])   >0)

#перечень адресов
vAT_address[100, vAT_address[100, ]>0]

# убираем 
vAT_address_clean<-vAT_address [vAT_address$VAT_NUMBER!= c(' ', 'GB'), ]


# адрес
names(vAT_address[100, vAT_address[100, ]>0][4])

library(dplyr)

#piping
tt<-all_address %>% count(VAT_NUMBER, aggregated_address)

subset(tt, tt$VAT_NUMBER== 'GB720113890')


tt1<-all_address %>% count(aggregated_address, VAT_NUMBER)

addr_vat_composite_key<-all_address %>% 
  group_by(aggregated_address, VAT_NUMBER)  %>% 
  count(aggregated_address, VAT_NUMBER)

# в рамках каждого кластера с адресами по кос. расстоянию
# подставить данные по vat и кол-ву раз, которое встретилось в выброке из  tt1
# будет 3 колонки адрес/vat/ кол-во раз
# таким образом, в каждом кластере схожих адресов может быть рекомендован 1 номер vat чаще встречающийся




test<- read.table("C:/Users/msmirnov/Documents/Проект_адреса/test.csv", sep = ";", stringsAsFactors = FALSE)


test [1, c(15:25)]

cbind(rep(test [1, 15], 10), test [1, c(16:25)])


all_classified_address<-cbind(as.data.frame(rep(test [1, 15], 10)), t(test [1, c(16:25)]))
colnames(all_classified_address)<-c('address_cluster', 'address')

system.time({ 
  
  for (i in  1:nrow(test)) {

df_aggr<-cbind(as.data.frame(rep(test [i, 15], 10)), t(test [i, c(16:25)]))

colnames(df_aggr)<-c('address_cluster', 'address')
all_classified_address<-rbind (all_classified_address, df_aggr)

}
})

all_classified_address<-all_classified_address[11:nrow(all_classified_address), ]


# пишем функцию для расчета косинусного расстояния

cosine_sim_fun<- function (x, y){ 
  
myCorpus <- corpus(c(original_address = x , 
                     
                     target1 = y )) # target1 - наименования всех позиций из общего массива чеков 



myDfmNoStop <- dfm(myCorpus, remove = stopwords("english"), stem = TRUE, remove_punct = TRUE)

# нашли косинусное расстояние между позицией в чеке и всеми группами ОКП 
textstat_simil(myDfmNoStop , 'original_address', method = "cosine", margin = "documents") [2] } # выбираем второй элемент, т.к. первый элемент возвращает коэф. с самим собой


cosine_sim_fun (as.character(all_classified_address[3,1]), as.character(all_classified_address[3,2]))


# вставляем колонку с косинусным расстоянием

system.time({ 
  
  for (i in  1:nrow(all_classified_address)) {
    
all_classified_address$sim_coef[i]<- cosine_sim_fun (as.character(all_classified_address[i,1]), as.character(all_classified_address[i,2]))

  
  }
})
  

# mapply должна отработать быстрее для расчета косинусных расстояний

system.time({ 
df_coef<-as.data.frame(mapply(cosine_sim_fun, as.character(all_classified_address[,1]), as.character(all_classified_address[,2]) ))
})


all_classified_address<- cbind(all_classified_address, df_coef)

all_classified_address<-all_classified_address [ , c(1,2, ncol(all_classified_address))]
colnames(all_classified_address)[3]<- "sim_coef"


# address_VAT_result<- merge(all_classified_address, addr_vat_composite_key, by.x=c('address'),
#          by.y=c('aggregated_address'), all.x = TRUE)


library(plyr)

address_VAT_result<-left_join(all_classified_address, addr_vat_composite_key, by = c("address" = "aggregated_address"))

# сортируем по коэф. >0 .8, чтобы выбрать действительно схожие адреса
address_VAT_result_sorted<- subset(address_VAT_result, address_VAT_result$sim_coef>0.8)

write.table(address_VAT_result, "C:/Users/msmirnov/Documents/Проект_адреса/address_VAT_result.csv", sep = ";",row.names = FALSE)

detach(plyr)


# количество vat номеров в группе подобных адресов
library(dplyr)
fin_summary<- address_VAT_result_sorted %>% 
              group_by(address_cluster) %>% 
              summarize(n_unique = length(unique(VAT_NUMBER)))

# сортируем группы внутри которых более 2 уник vAT номеров
fin_summary_sorted<-subset(fin_summary, fin_summary$n_unique>1)

vAT_problem<-address_VAT_result_sorted[address_VAT_result_sorted$address_cluster %in% fin_summary_sorted$address_cluster, ]

write.table(vAT_problem, "C:/Users/msmirnov/Documents/Проект_адреса/vAT_problem.csv", sep = ";",row.names = FALSE)



















subset(fin_summary, fin_summary$n_unique==6)

# проверка есть ли у адреса несколько VAT номеров
addr_VAT_check<- all_address%>% 
  # select(address_cluster, VAT_NUMBER) %>%
  group_by(aggregated_address) %>% 
  summarize(n_unique = length(unique(VAT_NUMBER)))


subset(fin_summary, fin_summary$address == "AGRA-HAN Agravis Raiffeisen AG NL-GVW Nederland PlathnerstraРЇe 4-A 30175 Hannover DE")


tt3<-address_VAT_result %>%  select(address, VAT_NUMBER) %>%group_by( address) %>% count (VAT_NUMBER)



# выводит кластер, адреса в кластере, количество раз в общей выборке, когда встретился VAT номер
subset(all_address, all_address$aggregated_address == "AVEN-IZE Vanden Avenne Commodities N.V. OSS Oss Engelse Wandeling 2-F3 8500 Kortrijk BE")

subset(address_VAT_result, address_VAT_result$address_cluster == "AGRI-HER Agri NР№goce SAS LOIRET Loiret 49, rue de Touraine 41190 Herbault FR")



subset(address_VAT_result, address_VAT_result$VAT_NUMBER == " ")

length(unique(subset(all_address[c('aggregated_address', "VAT_NUMBER")], all_address$VAT_NUMBER == "DE115657267")[,1]))

length(unique(subset(address_VAT_result, address_VAT_result$VAT_NUMBER == "DE115657267")[,1]))

# вывели адреса, относящиеся к конкретному кластеру
subset(address_VAT_result, address_VAT_result$address_cluster == "AB-GBP ABN AMRO Clearing Chic. (USD) LIFFE Liffe 175 West Jackson Boulevard IL 60604 Chicago US")

















tmp = vAT_address_clean[, 2:ncol(vAT_address_clean)]
tmp = apply(tmp, 1, sort, decreasing = TRUE)
colnames(tmp)<-colnames(vAT_address_clean)[2:ncol(vAT_address_clean)]

df3 = cbind.data.frame(vAT_address_clean[,1],tmp)
#colnames(df3) = c("VAT_NUMBER",paste("Col",1:length(unique(df$Item)), sep = ""))

colSums(df3 [ 3, c(2:8) ])

rowSums(df3 [ , c(2:13810) ])
















vAT_address <- dcast(all_address,  aggregated_address ~  VAT_NUMBER)

# сколько раз встречается vat  на всем списке ункильных адресов
length(subset( as.numeric(vAT_address[2,c(2:3717)]),  as.numeric(vAT_address[2,c(2:3717)])   >0))


vAT_address$address_count <- apply(vAT_address[ ,c(2:3717)],  1,
                                   function (x) {length(subset( x, x>0))})

#"GB720113890" встречается на 245 адресах

subset(vAT_address, vAT_address$address_count>0)[,1]



# проверка веремени работы при попарном подсчете всех косинусных расстояний, на пример для матрицы 7 000 * 7 0000 = 49 млн коэф. время работы 20 сек.

system.time({ 
myCorpus_pairwise <- corpus(c(original_address = test$aggregated_address[1:10000], 
                     
                     target1 = test$aggregated_address [1:10000]))



myDfmNoStop <- dfm(myCorpus_pairwise, remove = stopwords("english"), remove_punct = TRUE)

# нашли косинусное расстояние между позицией в чеке и всеми группами ОКП 
sim <- textstat_simil(myDfmNoStop , method = "cosine", margin = "documents" , upper = TRUE)  #margin="features"

})

tt<-as.matrix(sim)

tt_dfm<- as.matrix(dfmTrim)

# меняет название рядов в матрице dfm
rownames(tt_dfm)<-append(test$aggregated_address[1:10000], test$aggregated_address[1:10000])

# keep only words occurring >= 10 times and in at least 0.4 of the documents
dfmTrim <- dfm_trim(myDfmNoStop, min_termfreq = 5, min_docfreq = 0.95)
sim <- textstat_simil(dfmTrim , method = "cosine", mmargin="documents", upper = TRUE)  #margin = "documents"
tt<-as.matrix(sim)








system.time({ 
  myCorpus_pairwise <- corpus(c(original_address = c("da va", "bla", "blabla"), 
                                
                                target1 = c("rat", "da", "brown") ))
  
  
  
  myDfmNoStop <- dfm(myCorpus_pairwise, remove = stopwords("english"), remove_punct = TRUE)
  
  # нашли косинусное расстояние между позицией в чеке и всеми группами ОКП 
  sim <- textstat_simil(myDfmNoStop , method = "cosine", margin = "documents") 
  
})

tt<-as.matrix(sim)
dim(tt)
