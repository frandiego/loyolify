## tidy.R
## author: Francisco Jose Diego Acosta


clean_string <- function(x) {
  gsub("[^a-z0-9| ]", "", trimws(tolower(x)))
}


clean_column <- function(x) {
  x %>% map_chr(~rev(unlist(strsplit(tolower(.), '->')))[1]) %>% trimws
}


clean_colname <- function(colname){
  colname %>% tolower() %>%  strsplit('\\..') %>%
    unlist()  %>% gsub('[^a-z_]', '', .) %>% .[.!=''] %>%
    head(1)
}


guess_column <- function(x,
                         funs = c(as.integer, as.numeric, as.character),
                         out = c("", NA),
                         time_fraction=0.5) {
  x_ = clean_column(x)
  for (f in funs) {
    f_ = get(f)
    if (!any(is.na(f_(x_[!x_ %in% out])))) {
      return(f_(x_))
    }
  }
}


tidy_file <- function(file, cnf, out=c('', NA)){
  df = readxl::read_excel(file, skip = 1)
  df = data.table::as.data.table(df)
  columns = names(df)[which(gsub("[^a-z]", "", tolower(names(df))) != "")]
  clean_cols = map_chr(columns, clean_colname)
  df = df[, c(columns), with = F]
  setnames(df, columns, clean_cols)
  df <- df[, .SD, .SDcols = unique(names(df))]
  clean_cols = unique(clean_cols)
  
  df = df[, `:=`(c(clean_cols),
                 map(.SD, ~guess_column(.,
                                        funs = cnf$tidy$guess_functions,
                                        out=out))),
          .SDcols = c(clean_cols)]
  base = unlist(strsplit(basename(file), '\\.'))[1]
  filename = paste0(format(Sys.time(), "%Y%m%d%H%M%S"), '_' ,base ,".RDS")
  
  saveRDS(df, file.path(cnf$tidy$output_path, filename))
  Sys.sleep(cnf$tidy$sleep_seconds)
}

tidy <- function(input_files, cnf) {
  input_files %>% walk(~tidy_file(., cnf))
}



tidy_unique_file <- function(intput_path, output_name,  cnf, out=c('', NA)){
  msg = "Es necesario un nombre de archivo"
  if(!is.null(output_name)){
    if(output_name!=''){
      df = readxl::read_excel(intput_path, skip = 1)
      df = data.table::as.data.table(df)
      columns = names(df)[which(gsub("[^a-z]", "", tolower(names(df))) != "")]
      clean_cols = map_chr(columns, clean_colname)
      df = df[, c(columns), with = F]
      setnames(df, columns, clean_cols)
      df <- df[, .SD, .SDcols = unique(names(df))]
      clean_cols = unique(clean_cols)
      
      df = df[, `:=`(c(clean_cols),
                     map(.SD, ~guess_column(.,
                                            funs = cnf$tidy$guess_functions,
                                            out=out))),
              .SDcols = c(clean_cols)]
      df <- df[, c(cnf$preprocess$columns), with=F]
      saveRDS(df, file.path(cnf$tidy$output_path, paste0(output_name, '.RDS')))
    }else{
      stop(msg)
    }
  }else{
    stop(msg)
  }
}

tidy_unique_file_safe <- purrr::safely(tidy_unique_file)
