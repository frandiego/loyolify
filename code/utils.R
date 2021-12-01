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
