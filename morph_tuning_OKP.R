

system.time({ 
  
  for (w in  1: nrow(OKP_list)) {
    
    
    
    
    #OKP_list$clean_name[i]<-paste(as.character(tokens (OKP_list$normilizedForm[i]))[ lapply (test1_symb , function (x) {nchar(as.character(tokens(x ))) })[[1]]>3], collapse = " ")
    
    
    
    
    
    add_nominative <- tokens (as.character(OKP_list[w,7]))[[1]][ 
      
      
      lapply (OKP_list$POS[w][[1]], 
              function (x) {strsplit(as.character(x), ",") [[1]][1]} ) == 'NOUN' &
        
        lapply (OKP_list$POS[w][[1]], 
                function (x) {strsplit(as.character(x), ",") [[1]][2]} ) == 'inan' &
        
        (lapply (OKP_list$POS[w][[1]], 
                 function (x) {strsplit(as.character(x), ",") [[1]][4]} ) == 'nomn' |
           
           lapply (OKP_list$POS[w][[1]], 
                   function (x) {strsplit(as.character(x), ",") [[1]][5]} ) == 'nomn' ) # иногда данные по падежу могут находится в 5-ом элемента массива
      
      ] 
    
    
    # gent - родительный падеж
    add_genitive <-tokens (as.character(OKP_list[w,7]))[[1]][ 
      
      
      lapply (OKP_list$POS[w][[1]], 
              function (x) {strsplit(as.character(x), ",") [[1]][1]} ) == 'NOUN' &
        
        lapply (OKP_list$POS[w][[1]], 
                function (x) {strsplit(as.character(x), ",") [[1]][2]} ) == 'inan' &
        
        (lapply (OKP_list$POS[w][[1]], 
                 function (x) {strsplit(as.character(x), ",") [[1]][4]} ) == 'gent' |
           
           lapply (OKP_list$POS[w][[1]], 
                   function (x) {strsplit(as.character(x), ",") [[1]][5]} ) == 'gent' )
      
      ] 
    
    
    
    add_acc <- tokens (as.character(OKP_list[w,7]))[[1]][ 
      
      
      lapply (OKP_list$POS[w][[1]], 
              function (x) {strsplit(as.character(x), ",") [[1]][1]} ) == 'NOUN' &
        
        lapply (OKP_list$POS[w][[1]], 
                function (x) {strsplit(as.character(x), ",") [[1]][2]} ) == 'inan' &
        
        (lapply (OKP_list$POS[w][[1]], 
                 function (x) {strsplit(as.character(x), ",") [[1]][4]} ) == 'accs' |
           
           lapply (OKP_list$POS[w][[1]], 
                   function (x) {strsplit(as.character(x), ",") [[1]][5]} ) == 'accs' )
      
      ] 
    
    # выбираем не NA, первое слово в списке и придаем ему вес (повторяем слово в иходной строке несколько раз)
    # в данном случае для нарицательного существительного в им. падеже, 
    add_nominative <- normal_form_function_paste (ifelse(paste(rep (add_nominative[!is.na(add_nominative)], 5), collapse =" ") == "", 
                             paste(rep (add_nominative[!is.na(add_nominative)], 5), collapse =" "), 
                             paste(rep (add_nominative[!is.na(add_nominative)][1], 5), collapse =" ")))
    
    # в данном случае для нарицательного существительного в род. падеже
    add_genitive <-normal_form_function_paste (ifelse(paste(rep (add_genitive[!is.na(add_genitive)], 3), collapse =" ") == "", 
                          paste(rep (add_genitive[!is.na(add_genitive)], 3), collapse =" "), 
                          paste(rep (add_genitive[!is.na(add_genitive)][1], 3), collapse =" ")))
    
    # в данном случае для нарицательного существительного в винит. падеже
    add_acc <- normal_form_function_paste (ifelse(paste(rep (add_acc[!is.na(add_acc)], 3), collapse =" ") == "", 
                      paste(rep (add_acc[!is.na(add_acc)], 3), collapse =" "), 
                      paste(rep (add_acc[!is.na(add_acc)][1], 3), collapse =" ")))
    
    # "обогатить" исходную строку словами, которым нужно придать больший вес при расчете косинусного расстояния
    #paste( OKP_list$normilizedForm [w], add_nominative, add_genitive, add_acc, collapse = '')
    
    
    
    
    # существительные в им. падеже, находящиеся на первой позиции в строке, как правило имеют определеюящее значение и им нужно придать больший вес
    # например, "пакет майка ЛЕНТА"
    
    OKP_list$weighted_normilizedForm [w] <-paste( OKP_list$normilizedForm [w], add_nominative, add_genitive, add_acc, collapse = '')
    
    
    
    
  }
  
})



# убрать дефис из названия
affix_removel_function<- function (x) {gsub ("-", " ", x) }

# убрать точки и слэши из названия
slash_removal_function<- function (x) { gsub('\\.', ' ', x)}

point_removal_function <- function (x) {gsub('/', ' ', x)}


# убираем из строки слова длиной менее 3-х символов

clean_name_function <- function (x) {paste(as.character(tokens (x))[ lapply (x , function (x) {nchar(as.character(tokens(x ))) })[[1]]>3], collapse = " ")}

system.time({ 
  
  OKP_list$cleanName <- sapply (OKP_list$weighted_normilizedForm , function (x) { clean_name_function(affix_removel_function(slash_removal_function(point_removal_function(x) ) )) })
  
})

