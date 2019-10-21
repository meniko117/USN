library (stringr)

# пулучаем список скриптов для замены названий "полей"
script_list<- list.files("C:/Users/msmirnov/Documents/ГИр БО/Скрипты на замену/", pattern=NULL, all.files=FALSE,
           full.names=FALSE)



for (k in 1: length(script_list) ) {

# загружаем скрипт из полученного списка для замены названий 
sample_script<-readLines(paste0("C:/Users/msmirnov/Documents/ГИр БО/Скрипты на замену/", script_list[k]))

# загружаем списко необходимых замен
vocab_phrase <- read.csv ("C:/Users/msmirnov/Documents/ГИР БО/механизм нейминга 1500 полей/список_замен.csv", header = FALSE,
                          sep =";", stringsAsFactors = FALSE)

# 
# 
# test_script[10]<-gsub("taxpayer_inn", "yahoo", test_script[10])
# 
# write(test_script, "C:/Users/msmirnov/Downloads/test script")

new_script <- sample_script

for (i in 1: length(sample_script) ) {
  
 
  
  for (j in 1:nrow(vocab_phrase)) {
    
    
    new_script [i]<- gsub(vocab_phrase [j,1], vocab_phrase[j,2], new_script [i])
                                    
    
  }
}

write(new_script, 
                         
                         ifelse(is.na(str_locate (script_list [k], "\\." ) [1]), 
                                
                                paste0("C:/Users/msmirnov/Documents/ГИр БО/Скрипты на замену/", script_list [k], "_new"),
                                paste0("C:/Users/msmirnov/Documents/ГИр БО/Скрипты на замену/",  str_sub(script_list [k], 1, str_locate ( script_list [k], "\\." ) [1]-1), 
                                     "_new.",
                                     str_sub(script_list [k], str_locate ( script_list [k], "\\." ) [1]+1)   ))
)

}






# замена слов
# по исходным данным проходимся 2-й раз циклом т.к. сначала сравниваем данные для замены со списком фраз vocab_phrase, а потом со списком слов vocab_word


for (i in 1: nrow(all_atr_names) ) {
  
  
  for (j in 1:nrow(vocab_word)) {
    
    all_atr_names$vocab[i]<-ifelse( !is.na(str_locate (all_atr_names$vocab[i] ,  vocab_word [j,1]) )[1],  #|| !is.na(str_locate (all_atr_names$vocab[i] ,  tolower(vocab [j,1])) )[1]
                                    paste(unlist(lapply (strsplit(all_atr_names$vocab[i], split  =" ")[[1]],
                                                         function (x) {str_replace( x, paste0("\\b",vocab_word [j,1],"\\b"), vocab_word[j,2] )})), collapse = ' '),
                                    all_atr_names$vocab[i] )
    
    
  }
}