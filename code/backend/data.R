## data.R
## author: Francisco Jose Diego Acosta

read_data <- function(cnf){
  ls= list.files(cnf$preprocess$output_path)
  ls_num = map_dbl(ls, ~strsplit(., '_') %>% unlist %>% head(1) %>% as.numeric)
  file = ls[which(ls_num == max(ls_num))]
  readRDS(file.path(cnf$preprocess$output_path, file))
}

