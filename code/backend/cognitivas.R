## cognitivas.R
## author: Francisco Jose Diego Acosta

create_autocontrol  <- function(df){
  key = c('alumno_id')
  variables = c('adivinanzas__carrera', 
                'adivinanzas__biblioteca', 
                'adivinanzas__padre')
  dt_ok <- data.table::data.table(variable = variables, 
                                  ok = c('1','24', 'junio'))
  df[, c(key, variables), with=F] %>% 
    melt(key) %>% 
    .[, value := tolower(as.character(value))] %>%
    merge(dt_ok, by = 'variable', all.x = T) %>% 
    .[, n:= as.integer(value == ok)] %>% 
    .[is.na(n), n := 0] %>% 
    .[, .(variable = 'autocontrol', n = sum(n)), by=key] %>% 
    .[, y := 3-n] %>% 
    .[, -c('n')] %>% .[]
}


create_consistencia <- function(df){
  key = c('alumno_id')
  variables = c(paste0(c('dinero__'), head(letters, 6)), 
                paste0('probabilidad__', head(letters, 6)))
  df[, c(key, variables), with=F] %>% 
    melt(key) %>% 
    .[, type := ifelse(grepl('dinero', tolower(variable)), 
                       'dinero', 'probabilidad')] %>% 
    .[, n := as.integer(grepl('b', tolower(value)))] %>% 
    .[, n_prev := shift(n, 1), by = c(key, 'type')] %>% 
    .[, n_check := n - n_prev] -> dt
  dt[!is.na(n_check), 
     .(consistencia = 1-as.integer(any(n_check == -1))), 
     by = c(key, 'type')] %>% 
    .[, consistencia := sum(consistencia, na.rm = T), 
      by = c(key)] -> dtcon
  
  dt[variable == 'probabilidad__f' & n==1, c(key), with=F] %>% 
    merge(dt[variable == 'probabilidad__a' & n == 0, c(key), with=F]) %>% 
    .[, check := 1] -> dtcheck
  
  
  dtcon %>% merge(dtcheck, by =c(key), all = T) %>% 
    .[is.na(check), check := 0] %>% 
    .[, y := consistencia + check] %>% 
    .[, variable := 'consitencia'] %>% 
    .[, c(key, 'variable', 'y'), with=F] %>% 
    unique() 
}


is_string_complex <- function(s, nwords=3){
  x = unlist(strsplit(as.character(s), ' - '))
  map(x, ~unlist(strsplit(., ' '))) %>% 
    map_int( ~length(.[nchar(.)>nwords])) %>% max()
  
}


clean_text <- function(x){
  x %>% strsplit('->') %>% unlist() %>% rev %>% head(1) %>% trimws() %>% 
    gsub('[[:punct:]]$', '', .) %>% 
    gsub(' o | y ', '.', .) %>% 
    gsub('[[:punct:]]', '.', .) %>% 
    strsplit('\\.') %>% unlist %>% .[.!=''] %>% 
    tolower() %>% trimws()
}


create_creatividad <- function(df){
  
  dt = df[, c('alumno_id', 'creatividad__clip'), with=F]
  
  dt[['creatividad__clip']] %>%
    map_chr(~clean_text(.) %>% paste0(collapse = ' - ')) -> clean
  
  dt[, text := clean]
  dt[, creatividad__clip := NULL]
  dt[['n_token']] = dt[['text']] %>% map_int(~length(unlist(strsplit(., ' - '))))
  dt[['complexity']]  = dt[['text']] %>% map_dbl(is_string_complex) 
  dt[is.infinite(complexity) | is.na(complexity), complexity := 0]
  
  dt[['text']] %>% 
    map(~unlist(strsplit(paste0(unlist(strsplit(., ' - ')), collapse = ' '), ' '))) %>% 
    map_dbl(~uniqueN(.[nchar(.)>3])) -> flex
  dt[['flexibility']] = flex
  dt[is.infinite(flexibility) | is.na(flexibility), flexibility := 0]
  
  dt[['text']] %>% map(~unlist(strsplit(., ' - '))) %>% 
    unlist() %>% paste0(collapse = ' ') %>% strsplit(' ') %>% unlist() %>% 
    .[nchar(.)>3] %>% table() %>% .[.==1] %>% names() -> uniques
  
  dt[['text']] %>% map(~paste0(unlist(strsplit(., ' - ')), collapse = ' ')) %>% 
    map(~unique(unlist(strsplit(.,  ' ')))) %>% 
    map_dbl(~length(intersect(., uniques))) -> original
  dt[['original']]  = original
  
  dt[, text := NULL]
  dt %>% melt('alumno_id') %>% 
    .[, c('min', 'max') := list(min(value), max(value)), by = .(alumno_id)] %>% 
    .[, std := (value - min) / (max- min)] %>% 
    .[, .(n = sum(std)), by = .(alumno_id)] %>% 
    .[['n']] -> x
  x[is.na(x)] = 0
  y = x
  xm = mean(x)
  maxx = max(x)
  y[x<=1] = 0 
  y[x<=xm & x>1] = 1
  y[y==maxx] = 3
  y[y>=xm & y<3] = 2
  
  dt[['y']] = y
  dt[['variable']] = 'creatividad'
  dt[, c('alumno_id', 'variable', 'y')] %>% .[]
  
}


create_section_cognitivas <- function(df){
  data.table::rbindlist(list(
    create_autocontrol(df), 
    create_consistencia(df), 
    create_creatividad(df)
  )) -> dt
  dt[, section := 'cognitivas']
  dt[, c('alumno_id', 'section','variable', 'y'), with=F] %>% .[]
}
