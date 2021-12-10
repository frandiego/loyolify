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

lslatr <- function(path){
  path %>% 
    list.files(full.names = T) %>%
    file.info() %>% 
    as.data.table(keep.rownames = T) %>% 
    .[order(-mtime)] 
}


get_filepath <- function(cnf){
  cnf$preprocess$output_path %>% 
    lslatr() %>% 
    .[grepl('Dataset', rn)] %>% 
    .[, selected := ifelse(grepl('Selected', rn), 1, 0)] %>% 
    .[order(-selected, -mtime)] %>% 
    head(1) %>% 
    .[['rn']]
}

get_data <- function(cnf){
  get_filepath(cnf) %>% 
    readRDS() %>% 
    as.data.table() %>% 
    .[!is.na(school)] %>% 
    unique()
}


update_selected <- function(filename, cnf){
  cnf$preprocess$output_path %>% 
    list.files(full.names = T) %>% 
    .[grepl('Selected', .)] -> prev_selected
  
  filename %>%
    gsub('.RDS', '', .) %>% 
    paste0('.RDS') %>% 
    file.path(cnf$preprocess$output_path, .) -> new_dataset
  
  filename %>% 
    gsub('.RDS', '', .) %>% 
    paste0('.RDS') %>% 
    paste0('Selected - ', .) %>% 
    file.path(cnf$preprocess$output_path, .) -> new_selected
  
  file.copy(new_dataset, new_selected, overwrite = T)
  
  if(length(prev_selected)>0){
    for(file in c(prev_selected)){
      file.remove(file)
    }
  }
}




