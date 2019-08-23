

# Encoding(test_dt2[,134]) <- "UTF-8"


# Ðàáîòàåò


library (RClickhouse)
library (DBI)
con <- DBI::dbConnect(RClickhouse::clickhouse(), host="192.168.34.34", db="default", encoding= "window-1251")

# êîë-âî ïî òèïàì äîêóìåíòîâ
extr_test <- dbGetQuery(con, "SELECT subtype, COUNT(*)  
                      FROM receipts 
                      --WHERE content_code = 3 
                      GROUP BY subtype ") 

# êîë-âî ïî òèïàì taxationType
extr_test <- dbGetQuery(con, "SELECT content_taxationType, COUNT(*)  
                      FROM receipts 
                      --WHERE content_code = 3 
                      GROUP BY content_taxationType ") 


extr_test <- dbGetQuery(con, "SELECT (*)
                      FROM receipts 
                      WHERE content_taxationType = -128 
                      --
                       ") 


set_utf8 <- function(x) {
  # Declare UTF-8 encoding on all character columns:
  chr <- sapply(x, is.character)
  x[, chr] <- lapply(x[, chr, drop = FALSE], `Encoding<-`, "utf-8")
  # Same on column names:
  Encoding(names(x)) <- "utf-8"
  x
}

Sys.setlocale(category = "LC_ALL" )

extr_test <-set_utf8(dbGetQuery(con, "SELECT *
                      FROM receipts 
                      WHERE item_name =   'ÐÀÏÈÄÎ'"))

Sys.setlocale(category = "LC_ALL","Russian")

extr_test <-dbGetQuery(con, "SELECT *
                      FROM receipts 
                      WHERE item_name =   'ÐÀÏÈÄÎ'")

Sys.setlocale("LC_ALL","English") # ðàáîòàåò
extr_test <- dbGetQuery(con, "SELECT *
                      FROM receipts 
                      WHERE item_name =   'ÆË'") 





extr_test <- dbGetQuery(con, "SELECT *
                      FROM receipts 
                      WHERE item_name =  '"+test+"'   ") 




test<- "ÐÀÏÈÄÎ"





# Ð—Ð¾Ð»Ð¾Ñ‚Ð°Ñ Ð¿Ð¾Ð´ÐºÐ¾Ð²Ð°   Ð‘Ð¸Ð½Ð³Ð¾ 75       Ð ÐÐŸÐ\u0098Ð”Ðž
"select * from trafficdata where message_time between '" + val1 + "' and '" + val2 + "'"

Encoding(extr_test[,134]) <- "UTF-8"


extr_test <- dbGetQuery(con, "SELECT content_taxationType, COUNT (*)
                        
                      FROM receipts 
                      
                      
                      ") #ORDER BY ts




# êîëîíêè c "code"
# colnames(extr_test) [c(grep("Code", colnames(extr_test)), grep("code", colnames(extr_test)))]

extr_test <- dbGetQuery(con, "SELECT *
                        
                      FROM receipts
                      --WHERE subtype = 'openShift' 
                      LIMIT 100
                     
                      ") #ORDER BY ts


# îïðåäåëÿåò òîëüêî 2 òèïà subtype: receipt è bso
extr_test <- dbGetQuery(con, "SELECT DISTINCT
                        subtype
                      FROM receipts 
                      ") #ORDER BY t

extr_test <- dbGetQuery(con, "SELECT *
                        
                      FROM receipts
                      --WHERE subtype = 'openShift' 
                    LIMIT 100
                     
                      ") #ORDER BY ts

extr_test <- dbGetQuery(con, "SELECT *
                        
                      FROM receipts
                      WHERE id  = '20190331227a7001f4219c4b495eb1be92d4f4ff7c5cdbcb0f6c4473f70b3c86532da740' 
                    
                     
                      ") #ORDER BY ts

