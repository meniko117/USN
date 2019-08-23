library(ggplot2)
library(ggpubr)



# гистограмма 
# чек на 3,5 млрд руб
hist1<-ggplot(BD, aes(x=BD$content.totalSum)) + geom_histogram(bins=200) + scale_y_continuous( breaks = c(0:7), limits=c(0, 7)) + labs(x = "Сумма чека, руб", y = "Кол-во чеков")+ scale_x_continuous(labels = scales::comma, limits=c(0, 5000000000))
hist2<-ggplot(BD, aes(x=BD$content.totalSum)) + geom_histogram(bins=200) + scale_y_continuous( limits=c(0, 3000)) + labs(x = "Сумма чека, руб", y = "Кол-во чеков")+ scale_x_continuous(labels = scales::comma, limits=c(0, 10000000))
hist3<-ggplot(BD, aes(x=BD$content.totalSum)) + geom_histogram(bins=200) + xlim(c(50000,1000000)) + ylim(c(0, 5000)) + labs(x = "Сумма чека, руб", y = "Кол-во чеков")
hist4<-ggplot(BD, aes(x=BD$content.totalSum)) + geom_histogram(bins=200) + xlim(c(0,250000)) + ylim(c(0, 15000)) + labs(x = "Сумма чека, руб", y = "Кол-во чеков")


figure<- ggarrange(hist1, hist2, hist3, hist4, 
                   labels = c("A", "B", "C", "D"),
                   ncol = 2, nrow = 2)

# количество чеков с кол-вом позиций 1,2,3
hist7<-ggplot(BD, aes(x=BD$posNum)) + geom_histogram(bins=10) + scale_x_continuous(name = "Кол-во позиций в чеке", breaks = c(1:3), limits=c(1, 3)) + scale_y_continuous(name = "Кол-во чеков", labels = scales::comma, limits=c(0, 500000))+ geom_bar( width = 0.1)


# количество чеков с кол-вом позиций 0, 4-10
hist8<-ggplot(BD, aes(x=BD$posNum)) + geom_histogram(bins=50)    + ylim(c(0, 70000))+  
  scale_x_continuous(name = "Кол-во позиций в чеке", breaks = c(0:10), limits=c(-1, 11)) + geom_bar( width = 0.5)



figure2<- ggarrange( hist7, hist8, 
                     labels = c("A", "B", "C", "D"),
                     ncol = 1, nrow = 2)






###################
tt<- as.data.frame(table(subset(BD$content.totalSum, !is.na(BD$content.totalSum))))
tt1<- subset(tt, tt$Freq>2500 && tt$Freq<7500 )
tt1<- subset(BD, BD$content.totalSum>90000 & BD$content.totalSum<110000 )
max(table(tt1$content.totalSum))

tt<-subset(BD, !is.na(BD$content.totalSum))

tt1<- subset(BD, BD$content.totalSum>99999 & BD$content.totalSum<100010 )
tt1<- subset(BD, BD$posNum==0 )

tt2<-subset(tt1, tt1$subtype=="receipt")

tt3<-as.data.frame(table(tt1$subtype))

tt6_zero_Sum<- subset(tt2, tt2$content.totalSum==0)

tt4<-as.data.frame( t(tt2[1,]))
tt5<-as.data.frame( t(tt2[37,]))

hist(tt1$content.totalSum)

table(tt1$content.totalSum)


subset(table(tt1$content.totalSum), tt1$content.totalSum==10000)

max(table(tt1$content.totalSum))


max(subset(BD$content.totalSum, !is.na(BD$content.totalSum)))

max()# по количеству позиций в чеках статистика
summary(BD$posNum)
order(as.data.frame(table(BD$posNum))$Freq, decreasing = TRUE) # отфильтровать !
names(sort(-table(BD$posNum)))[1] #мода




#
s + geom_bar(position = "stack") +  
  theme(axis.text.x = element_text(angle=90, vjust=1)) + 
  geom_text(stat='count', aes(label=..count..), position = position_stack(vjust = 0.5),size=4)



###########################################################################



hist(head(subset(BD$content.totalSum, !is.na(BD$content.totalSum))))


hist(subset(BD$content.totalSum, !is.na(BD$content.totalSum))[ c(1:1000000)], ylim=c(0,5))

# гистограмма чеков без макс значения (сумма 3 572 355 870 руб - техническая нисправность)
hist1<-hist(subset(BD$content.totalSum, !is.na(BD$content.totalSum) & BD$content.totalSum <3572355870), ylim=c(0,20))
hist2<-hist(subset(BD$content.totalSum, !is.na(BD$content.totalSum) & BD$content.totalSum <100000), breaks =10, xlim=c(0,120000), ylim=c(0,400000))
hist3<-hist(subset(BD$content.totalSum, !is.na(BD$content.totalSum) & BD$content.totalSum >=100000 & BD$content.totalSum <120000000), 
            breaks =5000, xlim=c(0,1000000), ylim=c(0,30000))
hist4<-hist(subset(BD$content.totalSum, !is.na(BD$content.totalSum) & BD$content.totalSum >=120000000 ), 
            breaks =150, xlim=c(150000000,400000000), ylim=c(0,10))

boxplot(subset(BD$content.totalSum, !is.na(BD$content.totalSum) & BD$content.totalSum <100000))




max(subset(BD$content.totalSum, !is.na(BD$content.totalSum)))

big_sum<-subset(BD, BD$content.totalSum>50000000)

subset(BD, BD$content.totalSum==3572355870)$subtype












sample_check <-fromJSON("C:/Users/msmirnov/Documents/проект УСН/Анализ данных/sample_check_STM.json", flatten = TRUE)


sample_check <-  fromJSON( sprintf("[%s]",
                                   paste(readLines("C:/Users/msmirnov/Documents/проект УСН/Анализ данных/sample_check_STM.json", encoding = "UTF-8"),
                                         collapse=",")), flatten = TRUE)



t1<-as.data.frame(colnames(BD))
t2<-as.data.frame(colnames(sample_check))
colnames(t1)[1]<-"req"
colnames(t2)[1]<-"req"

t1$num<-1
t2$num<-1

req_compare<- merge(t1, t2, by = "req", all=TRUE)

colnames(req_compare)[c(2,3)]<- c("выборка 1 млн чеков", "чек STM")

req_compare[,2][,req_compare[,2] == 1]  <- "присутствует"

req_compare[,3] [req_compare[,3] == 1] <- "присутствует"

req_compare<- as.data.frame(req_compare)

is.na(req_compare[,3]) <- "нет"


df1$Sp2[df1$Sp2 == 8] <- 800
req_compare$num.x [req_compare$num.x == 1] <- "присутствует"

