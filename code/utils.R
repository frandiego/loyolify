## utils.R
## author: Francisco Jose Diego Acosta


get_package <- function(x){
  if(grepl('RUN R -e ', x)){
    pname = unlist(strsplit(unlist(strsplit(x ,', '))[1], '\\('))[2]
    gsub("[^a-zA-Z0-9.]", '', pname)
  }
}

library_dockerfile <- function(path){
  pnames = unlist(lapply(readLines(path), get_package))
  for (i in pnames[!is.null(pnames)]){
    library(i, character.only = T)
  }
}


filter_vector <- function(df, filter){
  filter %>%
    map2(.x=names(.), .y=.,
         .f = function(x, y) as.integer(df[[x]] %in% y)) %>% 
    reduce(`*`) %>% as.logical()
}


filter_diff <- function(df, filter, element){
  diff= setdiff(unique(df[[element]]), unique(filter[[element]]))
  filter[[element]] <- diff[!is.na(diff)]
  return(filter)
} 


filter_vector_diff_element <- function(df, filter, element){
  filter_vector(df, filter_diff(df, filter, element))
}




reset_database <- function(user, password, passphrase, path='credentials.sqlite',comment='school:'){
  file.remove(path)
  credentials <- data.frame(
    user = c(user),
    password = c(password),
    group = c(NA),
    comment = c(comment),
    admin = c(TRUE),
    stringsAsFactors = FALSE
  )
  
  # Create credentials DB (only once)
  create_db(
    credentials_data = credentials, 
    sqlite_path = path, 
    passphrase = passphrase
  )
  
}


filter_comment <- function(df, comment){
  comment %>% as.character() %>% 
    strsplit(';') %>% 
    unlist %>% 
    .[.!=''] %>% 
    .[grepl(':|=', .)]-> filters
  if(length(filters)>0){
    filters %>% 
      strsplit(':|=') %>%  
      map(trimws) %>% 
      map(~list(name=head(.,1), value=tail(.,1))) %>% 
      map(~list(name=.[['name']], 
                value=strsplit(.[['value']], '-|,') %>% unlist() %>% trimws() %>% 
                  paste0(collapse = ',') %>% paste0(' %in% c(', ., ')'))) %>% 
      map(~paste0('(', .[['name']], .[['value']], ')')) %>% 
      unlist() %>% paste0(collapse = ' & ') -> text_filter
    df[eval(parse(text = text_filter))] %>% .[]
  }else{
    df %>% .[]
  }
}





