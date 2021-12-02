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


filter_vector_diff_element <- function(df, filter, element){
  diff= setdiff(unique(df[[element]]), unique(filter[[element]]))
  filter[[element]] <- diff[!is.na(diff)]
  filter_vector(df, filter)
}
