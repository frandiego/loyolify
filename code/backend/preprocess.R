## preprocess.R
## author: Francisco Jose Diego Acosta


get_filenames <- function(path){
  ls = list.files(gsub('/$', '', path))
  ls_split = strsplit(ls, '_')
  time = map_chr(ls_split, ~.[1])
  base = map_chr(ls_split, ~paste0(.[-1], collapse = '_'))
  data.table::data.table(filename=ls, time=time, base=base) -> dfile
  dfile[, max_time := max(time), by = base]
  unique(dfile[max_time == time][['filename']])
}


preprocess_read <- function(path){
  path %>% readRDS() %>% data.table::as.data.table() %>% 
    setnames(names(.), map_chr(names(.), clean_colname))
}


preprocess <- function(cnf){
  columns = cnf$preprocess$columns
  path = cnf$tidy$output_path
  path %>% get_filenames %>% file.path(path, .) %>% 
    map(preprocess_read) %>% 
    data.table::rbindlist(fill = T) %>% 
    .[, columns, with=F] %>% 
    .[]
}