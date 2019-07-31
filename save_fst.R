library (data.table)
library(plyr)





# ??????????? csv ? fst

library(fst)
library(plyr)

for (i in(19:24)){
  
  
  DT_i<-fread(paste0("C:/Users/msmirnov/Documents/?????? ???/?????? ??????/5 mln checks/",i, ".csv"))
  
  ifelse(identical(grep("content.items.modifiers", colnames(DT_i)), integer(0)),
         write_fst(DT_i, paste0( i, ".fst"), 50, uniform_encoding = TRUE),
         
         write_fst(DT_i[, -grep("content.items.modifiers", colnames(DT_i))], paste0( i, ".fst"), 50, uniform_encoding = TRUE) )
  
}




# ???????, ??????? ?????????? ??? ???????
cols<-which(colnames(fst(paste0( 26, ".fst"))) %in%  c("id", "content.items.name", "content.receiptCode" , "content.code", "content.bsoCode", 
                                                      
                                                      "content.fiscalDocumentFormatVer", "content.userInn", "content.dateTime",
                                                      "content.operationType", "content.taxationType", "content.items.paymentType", "content.creditSum", "content.items.paymentAgentByProductType",
                                                      "content.items.providerInn", "content.items.paymentType", "content.kktRegId"
))


dt<-read_fst(paste0( 26, ".fst"), columns= colnames(fst(paste0( 26, ".fst")))[cols])

DT<-dt

system.time({ 
for (i in(26:50)){
  
  cols<-which(colnames(fst(paste0( i, ".fst"))) %in%  c("id", "content.items.name", "content.receiptCode" , "content.code", "content.bsoCode", 
                                                         
                                                         "content.fiscalDocumentFormatVer", "content.userInn", "content.dateTime",
                                                         "content.operationType", "content.taxationType", "content.items.paymentType", "content.creditSum", "content.items.paymentAgentByProductType",
                                                         "content.items.providerInn", "content.items.paymentType", "content.kktRegId"
  ))
  
dt_i<-read_fst(paste0( i, ".fst"), columns= colnames(fst(paste0( i, ".fst")))[cols])

DT<-rbind.fill(DT, dt_i)

#??????? ?????????

#DT<- DT[-which(duplicated(DT)), ]

}
})

# ?????? ?????????
DT<- DT[-which(duplicated(DT)), ]

write_fst(DT,"1_25.fst", 50, uniform_encoding = TRUE)
write_fst(DT,"26_50.fst", 50, uniform_encoding = TRUE)


receiptCode(code)
bsoCode
fiscalDocumentFormatVer
userInn
dateTime
operationType
appliedTaxationType
paymentType
items

#nds 

paymentAgentByProductType
items.providerInn
items.paymentType
#creditSum 
kktRegId 


library(fst)
DT1<- read_fst("1_25.fst", as.data.table = TRUE)
DT1<- read_fst("26_50.fst", as.data.table = TRUE)

setkey(DT1)

system.time({ 
DT1<- unique(DT1)
})


system.time({ 
  DT1<- DT1[-which(duplicated(DT1)), ]
})


# ?? ???????? "receiptCode"
tt<-subset(DT1[,1], is.na(DT1$content.receiptCode))
tt<- tt[-which(duplicated(tt)), ]

# ?? ???????? code
tt2<-subset(DT1[,1], is.na(DT1$content.code))
tt2<- tt2[-which(duplicated(tt2)), ]


# ?? ???????? code ??? receiptCode
tt3<-subset(DT1[,c(1,2,9,13)], is.na(DT1$content.code) | is.na(DT1$content.receiptCode))
tt3<- tt3[-which(duplicated(tt3)), ]

# ?? ???????? ?? bsoCode ??? receiptCode
tt4<-subset(DT1[,c(1,2,9,13,15)], is.na(DT1$content.receiptCode) & is.na(DT1$content.bsoCode) )
tt4<- tt4[-which(duplicated(tt4)), ]

# ??? ?? ???????? ??????
tt5<-subset(DT1[,c(1,2,9,13)], is.na(DT1$content.fiscalDocumentFormatVer))
tt5<- tt5[-which(duplicated(tt5)), ]

length(unique(tt5$id))

# ??? ?? ???????? ??????
tt5<-subset(DT1, DT1$content.fiscalDocumentFormatVer!= 2  & DT1$content.fiscalDocumentFormatVer!= 3)
tt5<- tt5[-which(duplicated(tt5)), ]

system.time({
DT1<- DT1[-which(duplicated(DT1)), ]})

uu<-as.data.frame(table(DT1$content.fiscalDocumentFormatVer))

nrow(subset(DT1, is.na(DT1$content.fiscalDocumentFormatVer) ))

# ??????????? userInn

tt5<-subset(DT1, is.na(DT1$content.userInn))
tt5<- tt5[-which(duplicated(tt5)), ]

# ???-?? ???? ? userInn

#DT1$userInn_digits_count<- sapply(as.data.frame(DT1$content.userInn), function (x) {nchar(as.character(x))})

# ???? userInn ???????? ???????????? ? ???? "1.390311e-312" ???????? ???????? csv ????????, ????????, DT1_1<- read_fst("2.fst", as.data.table = TRUE, columns= "content.userInn")

library(stringr)


# ??????? ????????? ???? ?? ??????
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
}

DT1$userInn_digits_count<-nchar(numextract(DT1$content.userInn))
# 
# table(DT1$userInn_digits_count)
# length(is.na(DT1$userInn_digits_count))

# ?????????? ???????? ?????????? ?????
# library(bit64)
# DT1$content.userInn<- as.integer64(DT1$content.userInn)
tt5<-DT1 [, c(1, 4,16)]
tt5<- tt5[-which(duplicated(tt5)), ]
table(tt5$userInn_digits_count)


# ???-?? NA ? dateTime

tt5<- subset(DT1, is.na(DT1$content.dateTime))

# if(code=3 || code=4 || code=31 ||code=41)                                ! Exist operationType


tt5<- subset(DT1,  is.na(DT1$content.operationType )) 

length(unique(tt5$id))

# operationType != 1,2,3,4 ??? ???????? ?? ????????????? ???????????


uu<-as.data.frame(table(tt5$content.code))

length(subset(DT1$content.operationType, is.na(DT1$content.operationType)))

#

uu<-as.data.frame(table(DT1$content.taxationType))

uu_1<- subset(DT1, DT1$content.taxationType == -128)

#if(code=3 || code=4) &       ! Exist paymentType



tt5<- subset(DT1,  DT1$content.receiptCode ==3 || DT1$content.receiptCode ==4) 

nrow(subset(tt5, is.na(tt5$content.items.paymentType)))



#
uu<-as.data.frame(table(DT1$content.items.paymentType))



# 

tt5<- subset(DT1,  DT1$content.receiptCode ==3 || DT1$content.bsoCode ==4)


tt5<-subset(tt5, is.na(tt5$content.items.name))



# 

tt5<- subset(DT1,  !is.na(DT1$content.items.paymentAgentByProductType) &  DT1$content.items.providerInn !=""  )



#


library(stringr)


# ??????? ????????? ???? ?? ??????
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
}


DT1$providerInn_digits_count<-nchar(numextract(DT1$content.userInn))

# ????????? ?? ???????????? ???
tt5<- subset(DT1, !is.na(DT1$content.items.providerInn) ) 


table(DT1$providerInn_digits_count)


tt5<-DT1 [, c(1, 3,16)]
tt5<- tt5[-which(duplicated(tt5)), ]
table(tt5$providerInn_digits_count)



# 
#if exist items.paymentType = 5 ???? ?? ? ????? items ? creditSum <= 0 || ! Exist creditSum 

tt5<- subset(DT1, (DT1$content.items.paymentType==5) & (DT1$content.creditSum<=0 | is.na(DT1$content.creditSum) ))

length(unique(tt5$id))


#
tt5<- subset(DT1, is.na(DT1$content.kktRegId))

DT1$kktReg_digits<- nchar(numextract(DT1$content.kktRegId))


table(DT1$kktReg_digits)




# ???? ?????????? ?? ???? 5 ??? ?????
rat<-list()

for (i in (1:50)){
 
  cols<-which(colnames(fst(paste0( i, ".fst"))) %in%  c("content.serviceSign", "id")) # ???????? ????? ???????? ? ??????
  
  ifelse(identical(cols, integer(0)),
         rat_i<-rat_i,
   
  rat_i<-read_fst(paste0( i, ".fst"), columns= colnames(fst(paste0( i, ".fst")))[cols]) )
  rat<-rbind.fill(rat, rat_i)
  
}
     
table(rat)

rat<-rat[-which(duplicated(rat)), ]



rat<-list()

for (i in (1:50)){
  
  cols<-which(colnames(fst(paste0( i, ".fst"))) %in%  c("content.totalSum", "id", "content.receiptCode",  
                                                        "content.code", "content.bsoCode")) # ???????? ????? ???????? ? ??????
  
  ifelse(identical(cols, integer(0)),
         rat_i<-rat_i,
         
         rat_i<-read_fst(paste0( i, ".fst"), columns= colnames(fst(paste0( i, ".fst")))[cols]) )
  rat<-rbind.fill(rat, rat_i)
  
}



rat<-rat[-which(duplicated(rat)), ]

uu<-subset(rat, rat$content.receiptCode ==3 || rat$content.bsoCode ==4 )

items_NUll<-subset(uu, is.na(uu$content.totalSum))

items_NUll<-subset(items_NUll, items_NUll$content.receiptCode==3) # ??? ? ??????? ??????

length(unique(items_NUll$id))

table(rat)


DT1_1<- read_fst("2.fst", as.data.table = TRUE, columns= "content.userInn")




# собираем все реквизиты из 5 млн чеков

rat<-list()

for (i in (1:50)){
  
  cols<-list(colnames(fst(paste0( "C:/Users/msmirnov/Documents/", i, ".fst"))))
  
  
  rat<-append(rat, cols)
  
}

tt<-as.data.frame(unique(unlist(rat)), stringsAsFactors = FALSE)




rat<-list()

# ?????????????? 1-? ??c???, ?????????? phone 
cols<-which(colnames(fst(paste0( 1, ".fst"))) %in%  tt[c(11, grep("hone", tt[,1])),]) # ???????? ?????, ?????????? "phone" ???????? ? ??????
rat_i<-read_fst(paste0( 1, ".fst"), columns= colnames(fst(paste0( 1, ".fst")))[cols]) 
rat_i<- unique(rat_i, by = "id")
rat<-rbind.fill(rat, rat_i)


for (i in (2:50)){
  
  cols<-which(colnames(fst(paste0( i, ".fst"))) %in%  tt[c(11, grep("hone", tt[,1])),]) # ???????? ?????, ?????????? "phone" ???????? ? ??????
  
  ifelse(identical(cols, integer(0)),
         rat_i<-rat_i,
         
         rat_i<-read_fst(paste0( i, ".fst"), columns= colnames(fst(paste0( i, ".fst")))[cols]) )
  rat_i<- unique(rat_i, by = "id")
  
  rat<-rbind.fill(rat, rat_i)
  
}

colnames(rat)
   
list_of_phones<- list()

for (i in (2:length(colnames(rat)))){   # ???????? ?? 2-?? ????????, ?.?. 1-? "id" ??????????? ?????

  requistis_extist<-nrow(subset(rat, !is.na(rat[,i])))
  
  list_of_phones<- append(list_of_phones, requistis_extist)
 
  
}  
  
cbind(tt[c(grep("hone", tt[,1])),], list_of_phones)




rat<-list()

# ???????? ?????? ?? ???? ?????? ????? ?? ???? ??????
for (i in (1:50)){
  
  cols<-which(colnames(fst(paste0( i, ".fst"))) %in%  tt[c(11, grep("paymentAgentType", tt[,1]), grep("operationType", tt[,1]), grep("paymentAgentByProductType", tt[,1])  ),]) #11 - ??? ??????? ? id
  
  ifelse(identical(cols, integer(0)),
         rat_i<-rat_i,
         
         rat_i<-read_fst(paste0( i, ".fst"), columns= colnames(fst(paste0( i, ".fst")))[cols]) )
  #rat_i<- unique(rat_i, by = "id")
  
  rat<-rbind.fill(rat, rat_i)
  
}

test<-subset(rat, (rat$content.paymentAgentType==5 | rat$content.paymentAgentType==6) &
             (rat$content.operationType ==1 | rat$content.operationType ==2) &
               (rat$content.items.paymentAgentByProductType>0 | rat$content.paymentAgentType>0) )

test<-subset(rat, (rat$content.paymentAgentType==5 | rat$content.paymentAgentType==6)  )


credit<- subset(rat, rat$content.items.paymentType==5)
unique_credit<- unique(credit, by = "id")




rat2<- list()
for (i in (1:50)){
  
  cols<-which(colnames(fst(paste0( i, ".fst"))) %in%  tt[c(11, grep("subtype", tt[,1]), grep("azart", tt[,1])),] ) #11 - ??? ??????? ? id
  
  ifelse(identical(cols, integer(0)),
         rat_i<-rat_i,
         
         rat_i<-read_fst(paste0( i, ".fst"), columns= colnames(fst(paste0( i, ".fst")))[cols]) )
  #rat_i<- unique(rat_i, by = "id")
  
  rat2<-rbind.fill(rat2, rat_i)
  
}



# ??????????? ?? ???, ? ??????? ??????? ????? 2-? ????? paymentType ? items
library (reshape2)


paymentTypesTable<-dcast(rat, id ~ content.items.paymentType)
paymentTypesTable$Sum<- rowSums(paymentTypesTable[2:9])


difPaymentTypes<- subset(paymentTypesTable, paymentTypesTable$Sum>1)

funSum <- function (x) {sum(x>0)} 
funSum(difPaymentTypes [1, c(2:8)])

difPaymentTypes$countTypesNumber<-  apply(difPaymentTypes [, c(2:8)], 1, funSum )

subset_difPaymentTypes <- subset(difPaymentTypes, difPaymentTypes$countTypesNumber > 1)

# ????? 

rat<-list()

cols<-which(colnames(fst(paste0( 1, ".fst"))) %in%  tt[c(grep(c("id") , tt[,1]), grep(c("items") , tt[,1]),  grep(c("Code"), tt[,1]), grep(c("code"), tt[,1]), tt[,1]),])  # ???????? ?????, ?????????? "phone" ???????? ? ??????
rat_i<-read_fst(paste0( 1, ".fst"), columns= colnames(fst(paste0( 1, ".fst")))[cols]) 
#rat_i<- unique(rat_i, by = "id")
rat<-rbind.fill(rat, rat_i)



# ????? ?????????, ??????? ??????????? ?????? ? ????? (???), ????? ????????? (??? ?????????) ? ?? ??????????? ? ?????????




req_belong_to_check<-vector("list", length = nrow(tt))

system.time ({

for (k in (175: nrow(tt) )){     #nrow(tt)
 rat2<- list()
for (i in (1:50)){ # ???-?? ??????
  
  
  
  cols<-which(colnames(fst(paste0( i, ".fst"))) %in% tt[c(11, 12, grep(tt[k,1], tt[,1])  ),] ) #11 - ??? ??????? ? id
  
  ifelse((identical(cols, integer(0)) || length(cols)<3),
         rat_i<-rat_i,
         
         rat_i<-read_fst(paste0( i, ".fst"), columns= colnames(fst(paste0( i, ".fst")))[cols]) )
  
  
  rat2<-rbind.fill( rat2, rat_i)
  

  
}
 na_reqs_test<-subset(rat2, (rat2$subtype== "receipt" |  rat2$subtype== "receiptCorrection" | rat2$subtype== "bso" | rat2$subtype== "bsoCorrection") & !is.na(rat2[,3]) )
 
 req_belong_to_check [k]<- ifelse(   nrow(na_reqs_test) < 1  , "no", tt[k,1])
}

})



subset(as.data.frame(unlist(req_belong_to_check)), as.data.frame(unlist(req_belong_to_check)) [,1]=="no")



requisites_present_in_checks<- as.data.frame(tt[which( req_belong_to_check != "no"), 1])
