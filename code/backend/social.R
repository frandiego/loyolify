## social.R
## author: Francisco Jose Diego Acosta


create_prosocial  <- function(df){
  key = c('alumno_id')
  variables = paste0('pensamiento_estrategico_dinero__', head(letters, 3))
  df_ok = data.table::data.table(variable = variables, correct = c('b','a','b'))
  df[, c(key, variables), with=F] %>% 
    melt(key) %>% 
    .[, value := gsub('[^a-z]', '', tolower(value))] %>% 
    .[is.na(value), value := ''] %>% 
    merge(df_ok, by=c('variable'), all.x = T) %>% 
    .[, n := as.integer(value == correct)] %>% 
    .[, variable := 'prosocial'] %>% 
    .[, .(y=sum(n, na.rm = T)), by = c(key, 'variable')] %>% 
    .[]
}


expand_row <- function(x){
  node_from = as.numeric(x[, 1])
  node_to = unlist(strsplit(as.character(x[, 2]), ' ')) %>% .[.!=''] %>% as.numeric()
  expand.grid(from= node_from, to=node_to) %>% as.data.table()
}


data_graph <- function(df, key, var){
  dt  = unique(df[, c(key, var), with=F])
  1:nrow(dt) %>% 
    map(~dt[.]) %>% 
    map(expand_row) %>% 
    rbindlist() %>% 
    .[, var := as.character(var)] %>% 
    .[]
}


create_conflicto_cohesion <- function(df){
  c('social__amigos', 'social__no_amigos') %>% 
    map(~data_graph(df, 'usuario_id', .)) %>% 
    rbindlist() -> dfsoc
  
  dfsoc[['edge']] <- map2_chr(.x=dfsoc[['from']], .y=dfsoc[['to']], 
                              .f = function(x,y) paste(sort(c(x,y)), collapse = '-'))
  dfsoc[, reciprocity := .N-1, by = .(edge, var)]
  dfsoc[, .(n=sum(reciprocity)), by = .(from, var)] -> dfsoc
  setnames(dfsoc, c('from', 'var'), c('usuario_id', 'variable'))
  dfsoc[variable == 'social__amigos', variable := 'cohesion']
  dfsoc[variable == 'social__no_amigos', variable := 'conflicto']
  dfsoc[variable == 'cohesion' &  n ==0, y:= 0]
  dfsoc[variable == 'cohesion' &  n >0 & n<6, y:= 1]
  dfsoc[variable == 'cohesion' &  n >=6 & n<11, y:= 2]
  dfsoc[variable == 'cohesion' &  n >=11 , y:= 3]
  
  dfsoc[variable == 'conflicto' &  n ==0, y:= 0]
  dfsoc[variable == 'conflicto' &  n==1, y:= 1]
  dfsoc[variable == 'conflicto' &  n >1 & n<4, y:= 2]
  dfsoc[variable == 'conflicto' &  n >4 , y:= 3]
  df[, c('usuario_id', 'alumno_id'), with=F]
  unique(df[, c('alumno_id', 'usuario_id'), with=F]) %>% 
    merge(dfsoc, by = c('usuario_id'), all.y = T) %>% 
    .[, c('alumno_id', 'variable', 'y'), with=F] %>% 
    unique()
}


create_section_social <- function(df){
  data.table::rbindlist(list(
    create_prosocial(df), 
    create_conflicto_cohesion(df) 
  )) -> dt
  dt[, section := 'social']
  dt[, c('alumno_id', 'section','variable', 'y'), with=F] %>% .[]
}

