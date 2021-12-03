## actitudes.R
## author: Francisco Jose Diego Acosta


create_paciencia  <- function(df){
  key = c('alumno_id')
  variables = paste0('dinero__', head(letters, 6))
  df[, c(key, variables), with=F] %>% 
    melt(key) %>% 
    .[, value := tolower(value)] %>% 
    .[, n := as.integer(grepl('b', value))] %>% 
    .[, .(n = sum(n, na.rm = T)), by = c(key)] %>% 
    .[, y := ifelse(n<=3, 0, n-3)] %>% 
    .[, variable := 'paciencia'] %>% 
    .[, c(key, 'variable', 'y'), with=F] %>% .[]
}


create_emprendimiento  <- function(df){
  key = c('alumno_id')
  variables = paste0('probabilidad__', head(letters, 6))
  df[, c(key, variables), with=F] %>% 
    melt(key) %>% 
    .[, value := tolower(value)] %>% 
    .[, n := as.integer(grepl('b', value))] %>% 
    .[, .(n = sum(n, na.rm = T)), by = c(key)] %>% 
    .[, y:= fcase(n==0, 0, 
                   n>0 & n<=2, 1, 
                   n>2 & n<=4, 2, 
                   n>4 & n<=6, 3)] %>% 
    .[, variable := 'emprendimiento'] %>% 
    .[, c(key, 'variable', 'y'), with=F] %>% .[]
}


create_estrategia <- function(df){
  key = c('alumno_id')
  variables = paste0('pensamiento_estrategico__', head(letters, 4))
  df_ok = data.table::data.table(variable = variables, ok = c(5,4,5,4))
  df[, c(key, variables), with=F] %>% 
    melt(key) %>% 
    .[, variable := as.character(variable)] %>% 
    .[, value := as.integer(value)] %>% 
    merge(df_ok, by = 'variable', all.x = T) -> dt
  dttrick <- dt[variable == 'pensamiento_estrategico__d', -('ok')]
  dttrick[, variable := 'pensamiento_estrategico__c']
  setnames(dttrick, 'value', 'ok_trick')
  
  dttrick =  dttrick[,.(ok_trick =max(ok_trick, na.rm = T)), by = c("variable", key)]
  
  merge(dt, dttrick, by = c('variable', key), all.x = T) -> dt
  dt <- dt[variable != 'pensamiento_estrategico__d']
  dt[!is.na(ok_trick), ok := ok_trick]
  by = setdiff(names(dt), c('variable', 'value', 'correct', 'correct_trick'))
  dt[, value := as.integer(value)]
  dt[, ok := as.integer(ok)]
  dt[, n := as.integer(value == ok)]
  dt[is.na(n), n:= 0]
  dt[, .(variable = 'estrategia', y=sum(n, na.rm = T)), by =c(key)]
}


create_section_actitudes <- function(df){
  data.table::rbindlist(list(
    create_emprendimiento(df), 
    create_paciencia(df), 
    create_estrategia(df)
  )) -> dt
  dt[, section := 'actitudes']
  dt[, c('alumno_id', 'section','variable', 'y'), with=F] %>% .[]
}

