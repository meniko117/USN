library(jsonlite)

# формируем json

string<- toJSON(list(as.list(dd)[1:3],
                     content.items.unitNds = list(studentid = 15, name = "Kevin")) )

#убираем "1" в строке
string_correct<- substr(string, start =6, stop = nchar(string)-1)
# убираем квадратные скобки (если требуется)
#string <-gsub('\"', "'", string_correct, fixed=TRUE)
string <-gsub("\\[|\\]", "", string_correct)




toJSON(list(list(as.list(dd)[1:3],
            content.items.unitNds = list(studentid = 15, name = "Kevin"))[[1]] 
           ))


unlist(as.list(dd)[1:3])



rat_i<-read_fst(paste0( i, ".fst"))

colnames(rat_i)[11:15]

list(status , code ) <- c (10,12)




#составляем словарь из тэгов чека и значений. Но можно сделать и data.frame ключи-значения через cbind

h <- hash( keys=colnames(rat_i), values=1:139 ) 

toJSON(as.list(h), auto_unbox = TRUE)

keys(h)




dd <- hash()
.set( dd, keys=colnames(rat_i), values=1:139 )




library (data.table)
dd<- as.data.frame(as.list(dd))
dd_t<-transpose(as.data.frame(as.list(dd))  )

colnames(dd_t)<- rownames (dd)
rownames(dd_t)<-colnames(dd)

dd_t<-as.data.frame(dd_t, rownames=FALSE)

dd_t$tags<-rownames(dd_t)
dd_t[order( dd_t[,1]   ),]

toJSON(as.list(dd_t[,2]))
toJSON(as.list(dd_t))

