library(sqldf)

# sqldf íå ðàáîòàåò ñ êîëîíêàìè, â êîòîðûõ ñîäåðæèòñÿ òèï List
# îïðåäåëèòü òèï êîëîíîê
coltypes<-dat2[1,] %>% 
  summarise_all(typeof)

# ïðåîáðàçîâàòü ðÿäû â êîëîíêè
coltypes<-tidyr::gather(coltypes)


# îñòàâëÿåì â òàáëèöå òîëüêî êîëîíêè áåç ïåðåìåííûõ List
datSQL<- dat2[, c(1:23, 25:27, 29:36, 39:42, 46:54, 56:75)]


# óáèðàåì "content" èç íàçâàíèé êîëîíîê, ò.ê. sqldf íå èäåíòèôèöèðóåò ýòè íàçâàíèÿ
library(stringr)

colnames(datSQL)<-colnames(datSQL) %>% str_replace("content.", "")

#óáèðàåì äóáëèêàòû â íàçâàíèÿõ
colnames(datSQL)[27]<-"kktRegId2"
colnames(datSQL)[43]<-"protocolVersion2" 


# ÏÈØÅÌ SQL çàïðîñû

# ãðóïïèðóåì ÷åêè ïî ofd
View(sqldf("SELECT ofdId, COUNT (ofdId) FROM datSQL GROUP BY ofdId " ))

# cóììà ÷åêà ðàâíà ñóììå íåêòîðûõ ïîëåé ñ ÍÄÑ
View(sqldf("SELECT * FROM datSQL WHERE totalSum = nds0 + nds18+ ndsNo " ))

# áîëüøå âñåãî ïðîáèòûõ ÷åêîâ
View(sqldf("SELECT userInn, user, COUNT (userInn) AS cnt FROM datSQL GROUP BY userInn  ORDER BY cnt DESC LIMIT 10" ))


# https://jasminedaly.com/tech-short-papers/sqldf_tutorial.html





 

