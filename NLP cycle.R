
library(stringr)
library(dplyr)

batchSize_items <- 100 # кол-во позиций из чеков для анализа

similiarity_analysis<- matrix( nrow = batchSize_items, ncol = 0)


funColNames<- function (x) (as.numeric(str_sub(x, start =6, end = nchar(x)))) # функция для парсинга навзаний итоговой таблицы с косинусными расстояниями

batchSize_Group <- 10588 # кол-во групп из ОКП для анализа при проходе циклом

system.time({
  
  for (i in  1:5) {
    # 
    
    myCorpus <- corpus(c(group = OKP_list [ c((1+(batchSize_Group*i)-batchSize_Group) : (batchSize_Group*i)),9] , # название группы товаров из ОКП кол-во за итерацию 10588 групп
                         
                         target =  checkStem$stemmedNames[c(1:batchSize_items)]))
    
    
    
    myDfmNoStop <- dfm(myCorpus, remove = stopwords("english"), tolower = TRUE, stem = TRUE, remove_punct = TRUE)
    
    tt<- textstat_simil(myDfmNoStop,  method = "cosine", margin = "documents") #selection = c("group30756", "group30773", "group30774")
    
    
    
    
    
    tt<-as.matrix(tt)
    tt<- as.data.frame(tt, optional = TRUE)
    tt$items<- rownames(tt)
    
    
    # оставляем только позиции чека
    
    # 
    tt1<-tt %>%
      filter(str_detect(tt$items, "target"))%>%
      select(-(grep( "target", colnames(tt))))
    
    
    
    
    
    colnames(tt1) [c(1:batchSize_Group)]<- paste0("group", unlist(lapply(colnames(tt1)[c(1:batchSize_Group)], funColNames ))+i*batchSize_Group-batchSize_Group)
    
    # определили  в каких группах косинусное расстояние больше 0, т.е. хоть для какой-нибудь позиции найдена группа из ОКП
    # для каждой строки из чека "target" определено ненулевое косинусное расстояние с каждой из групп ОКП
    # 
    # k<-ncol(tt1)-1 # 
    # tt2<- tt1[, colSums(tt1[ , c(1:k)]) != 0] # получаем массив ненулевых косинусных расстояний для каждой позиции чека и групп ОКП
    
    similiarity_analysis<- cbind(similiarity_analysis, tt1) #tt2[, c(1:(ncol(tt2)-nrow(tt2)-1))]
  }
  
})

nc<-grep( "items", colnames(similiarity_analysis)[c(1:(ncol(similiarity_analysis)-1))])

similiarity_analysis<- similiarity_analysis [, c(1:(ncol(similiarity_analysis)-1))] %>%
  select(-(nc))%>%
  cbind(similiarity_analysis[,ncol(similiarity_analysis)])

# переставляем колонку с items в начало
similiarity_analysis<- similiarity_analysis [ , c (ncol(similiarity_analysis), c(1:(ncol(similiarity_analysis)-1)))]


# убираем NA
similiarity_analysis[is.na(similiarity_analysis)]<-0

# оставляем только группы с ненулевыми значениями косинусного расстояния для какой либо позиции чека
#simTotal<- similiarity_analysis[ ,colSums(similiarity_analysis[ , c(2:ncol(similiarity_analysis))])!= 0]



item_n <- 12

checkStem[item_n,3] # позиция из чека

#which.max(simTotal[9 , simTotal[ 9, c(2:ncol(simTotal))] != 0]) # номер группы с макс. значением косинусного расстояния

#simVec<- as.data.frame(t(simTotal[item_n , simTotal[ item_n, c(2:ncol(simTotal))] != 0]))
library (tibble)

simVec <- as.data.frame(t(similiarity_analysis))

simVec <- as.data.frame(unlist(simVec, recursive = TRUE, use.names = TRUE))

simVec <-drop.levels(simVec)
simVec <-factor(simVec)
colnames(simVec) <- simVec [1,] 

simVec <- simVec[c(2:nrow(simVec)), ]
#simVec <- as.data.frame(t(as_tibble(rownames_to_column(similiarity_analysis))))

simVec [ , item_n]<- as.double ( simVec [ , item_n])

simVec_Item<-subset(simVec [, item_n],  simVec [, item_n]>0 )

simVec_Item<-arrange(simVec, desc(simVec[,2])) # сортируем в порядке убывания коэффициента

fun_OKP <- function (x) { OKP_list[str_sub(x,  start=6, end= nchar(x)), 7] }


simVec$OKP_group<- sapply( simVec [,1], fun_OKP) 

simVec

grep("Редис", simVec[,3])

OKP_list[50081, 7]

# структура массива tt2 следующая:
# колонки "group1", "group2" и т.д. по количеству групп из ОКП, потом идут "target1", "target2"  т.д. по количеству переданных в корпус словп позиций из чека, 
# потом вставлена колонка "items", где перечислены номера переданных позиций из чека




checkStem[59,3]

col_name<-colnames(similiarity_analysis)[which.max(similiarity_analysis[59, c(1:(ncol(similiarity_analysis)-nrow(similiarity_analysis)-1) )])]

col_num<-as.numeric(str_sub ( col_name, start = nchar(col_name)-3, end = nchar(col_name)))

OKP_list[30000+col_num-1,7]

