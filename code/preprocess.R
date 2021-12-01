## preprocess.R
## author: Francisco Jose Diego Acosta

library_requirements <- function(path){
  path %>% readLines() %>% 
    map_chr(~strsplit(., '==') %>% unlist %>% head(1)) %>% 
    walk(~library(., character.only = T))
}


get_filenames <- function(path){
  ls = list.files(gsub('/$', '', path))
  ls_split = strsplit(ls, '_')
  time = map_chr(ls_split, ~.[1])
  base = map_chr(ls_split, ~paste0(.[-1], collapse = '_'))
  data.table::data.table(filename=ls, time=time, base=base) -> dfile
  dfile[, max_time := max(time), by = base]
  unique(dfile[max_time == time][['filename']])
}


preprocess_read_tidy <- function(path){
  path %>% readRDS() %>% data.table::as.data.table() %>% 
    setnames(names(.), map_chr(names(.), clean_colname))
}


preprocess_filter <- function(cnf){
  columns = cnf$preprocess$columns
  path = cnf$tidy$output_path
  path %>% get_filenames %>% file.path(path, .) %>% 
    map(preprocess_read_tidy) %>% 
    data.table::rbindlist(fill = T) %>% 
    .[, columns, with=F] %>% 
    .[]
}

preprocess_variables <- function(df){
  l = list()
  l[['actitudes']] <- create_section_actitudes(df)
  l[['cognitivas']] <- create_section_cognitivas(df)
  l[['social']] <- create_section_social(df)
  l[['salud']] <- create_section_salud(df)
  l %>% rbindlist() %>% unique() %>% setnames('alumno_id', 'uid') %>% .[]
}


preprocess_group <- function(df){
  vars= c('alumno_id','estudio', 'curso', 'grupo', 'genero', 'zurdo')
  dt = df[, vars, with=F]
  dt[, uid := as.integer(alumno_id)]
  dt[, school := as.integer(estudio)]
  dt[, course := as.integer(gsub('[^0-9]', '', curso))]
  dt[, group := as.integer(factor(grupo, levels = letters))]
  dt[, gender := ifelse(genero == 'mujer', 1, 0)]
  dt[, is_left_handed := ifelse(zurdo %in% c('si', 'izquierda'), 1, 
                                ifelse(zurdo %in% c('no', 'derecha'), 0, NA))]
  dt[, -c(vars), with=F] %>% unique()
}


stats_mode <- function(x){
  names(head(sort(table(x),T),1))
}


find_repetidor <- function(df){
  dft = df[, c('alumno_id', 'curso', 'grupo', 'estudio', 'ano_nacimiento'), with=F]
  dft[, curso := as.integer(gsub('[^0-9]', '', curso))]
  dft[, grupo := tolower(trimws(grupo))]
  dft[, estudio := as.integer(estudio)]
  dft = unique(dft)
  dft[, .(ano_nacimiento_mode = as.numeric(stats_mode(ano_nacimiento))), 
      by = .(grupo, curso, estudio)] -> df_ano_nacimiento_mode
  
  dft = merge(dft, df_ano_nacimiento_mode, by=c('grupo', 'curso', 'estudio'), all.x = T)
  dft[, repetidor := as.integer(ano_nacimiento>2000 & ano_nacimiento<ano_nacimiento_mode)]
  dft[is.na(repetidor), repetidor := 0]
  dft[, c('alumno_id', 'repetidor'), with=F] %>% unique() %>% 
    setnames(names(.), c('uid', 'is_repeater')) %>% .[]
  
}


preprocess <- function(cnf){
  df = preprocess_filter(cnf)
  dfvars = preprocess_variables(df)
  dfgroups = preprocess_group(df)
  dfpopu = dfvars[variable == 'relacional', .(is_popular = as.integer(y!=0)), by = .(uid)]
  dfgroups = dfgroups %>% merge(dfpopu, by = 'uid', all.x = T)
  df_rep = find_repetidor(df)
  dfgroups = dfgroups %>% merge(df_rep, by = 'uid', all.x = T)
  
  cols = unique(c(colnames(dfgroups), colnames(dfvars)))
  dfvars %>% merge(dfgroups, by = c('uid'), all.x = T) %>% 
    .[, c(cols), with=F] %>% unique() -> dfprep
  
  filename = paste0(format(Sys.time(), "%Y%m%d%H%M%S"), '_prep'  ,".RDS")
  saveRDS(dfprep, file.path(cnf$preprocess$output_path, filename))
}


read_data <- function(cnf){
  ls= list.files(cnf$preprocess$output_path)
  ls_num = map_dbl(ls, ~strsplit(., '_') %>% unlist %>% head(1) %>% as.numeric)
  file = ls[which(ls_num == max(ls_num))]
  readRDS(file.path(cnf$preprocess$output_path, file))
}

