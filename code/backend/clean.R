## clean.R
## author: Francisco Jose Diego Acosta


take_integer <- function(x){
  x %>% gsub('[^0-9]', '', .) %>% as.integer()
}


take_words <- function(x, words = c('mujer', 'hombre'), fill='otro'){
  for (w in words){
    x[grepl(tolower(w), tolower(x))] <- tolower(w)
  }
  x[!grepl(paste0(tolower(words), collapse = '|'), tolower(x))] <- fill
  x
}

