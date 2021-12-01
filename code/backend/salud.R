## salud.R
## author: Francisco Jose Diego Acosta


create_animo <- function(df){
  key = c('alumno_id')
  variables = c('estado_animo__solo', 
                'estado_animo__diversion', 
                'estado_animo__general')
  animo_levels = c('nunca', 'casi nunca', 'algunas veces', 'casi siempre', 'siempre')

  df[, c(key, variables), with=F] %>% 
    melt(key) %>% 
    .[, variable := as.character(variable)] %>%
    .[, value:= factor(value, levels = animo_levels, ordered = T)] %>% 
    .[, n:= as.integer(value) - 1] %>% 
    .[variable == 'estado_animo__solo', n:= 4-n] %>% 
    .[, .(n=sum(n, na.rm = T)/12), by=key] %>% 
    .[, y := fcase(n<0.6, 0, 
                   n>=0.6 & n<0.85, 1, 
                   n>0.85 & n<0.99, 2, 
                   n>=0.99, 3)] %>% 
    .[, variable := 'anímica'] %>% 
    .[, c(key, 'variable', 'y'), with=F] %>% 
    unique()
}


create_alimenticia <- function(df){
  key = c('alumno_id')
  variables = c('comida__fruta', 
                'comida__verduras', 
                'comida__soda', 
                'comida__pescado', 
                'comida__rapida')
  unique_day = c("más de una vez al día", "menos de una vez al día", "una vez al día" )
  unique_week = c("más de 2 veces a la semana", "menos de 2 veces a la semana", "2 veces a la semana" )
  unique = c(unique_day, unique_week)

  df[, c(key, variables), with=F] %>% 
    melt(key) %>% 
    .[, value := factor(value, levels = unique, ordered = T)] %>% 
    .[, value_int := as.integer(value)] -> dt
  
  dt[variable == 'comida__fruta', n := fcase(value_int == 1, 2, value_int==2, 0, value_int==3, 1)]
  dt[variable == 'comida__verduras', n := fcase(value_int == 1, 2, value_int==2, 0, value_int==3, 1)]
  dt[variable == 'comida__soda', n := fcase(value_int == 1, 0, value_int==2, 0, value_int==3, 2)]
  dt[variable == 'comida__pescado', n := fcase(value_int == 4, 2, value_int==5, 0, value_int==6, 1)]
  dt[variable == 'comida__rapida', n := fcase(value_int == 4, 0, value_int==5, 2, value_int==6, 1)]
  by = setdiff(names(dt), c('variable', 'value', 'value_int','correct', 'n'))
  dt = dt[, .(n = sum(n, na.rm = T)/10), by = by] 
  dt[n<0.45, y:= 0]
  dt[n>=0.45 & n<=0.65, y:= 1]
  dt[n>=0.65 & n<0.85, y:= 2]
  dt[n>=0.85, y:= 3]
  dt[, variable := 'alimenticia']
  dt[, c(key, 'variable', 'y'), with=F] %>% unique
}


flat_vector <- function(vector){
  vector %>% 
    map(., ~strsplit(.,'\\|') %>% 
          unlist %>% trimws %>% as.integer) %>% 
    unlist() %>% 
    .[!is.na(.)]
}


create_relacional <- function(df){
  key = c('usuario_id')
  variables = c('social__amigos', 
                'social__no_amigos', 
                'social__mejores_amigos', 
                'social__peores_amigos')
  dt = df[, c(key, variables), with=F] %>% unique()
  
  
  variables %>% map(., ~flat_vector(dt[[.]])) -> flats
  names(flats) <- variables
  dt[, amigos := map_int(dt$usuario_id, ~sum(flats$social__amigos == .))]
  dt[, mejores_amigos := map_int(dt$usuario_id, ~sum(flats$social__mejores_amigos == .))]
  dt[, enemigos := map_int(dt$usuario_id, ~sum(flats$social__no_amigos == .))]
  dt[, peores_enemigos := map_int(dt$usuario_id, ~sum(flats$social__peores_amigos == .))]
  dt[, n := (amigos + 2* mejores_amigos) - (enemigos + 2* peores_enemigos)]
  nmax = max(dt$n, na.rm = T)
  nmin = min(dt$n, na.rm = T)
  dt[, npct := (n-nmin) / (nmax - nmin)]
  dt[, y:= fcase(npct<=0.25, 0, npct>0.25 & npct<=0.5, 1, npct>0.5 & npct<0.75, 2, npct>0.75, 3)]
  unique(dt[, c(key, 'y'), with=F]) %>% 
    merge( df[, c(key, 'alumno_id'), with=F], by = key, all.x = T) %>% 
    .[, variable := 'relacional'] %>% 
    .[, c('alumno_id', 'variable', 'y'), with=F] %>% .[]
}


create_section_salud <- function(df){
  data.table::rbindlist(list(
    create_animo(df), 
    create_alimenticia(df), 
    create_relacional(df)
  )) -> dt
  dt[, section := 'salud']
  dt[, c('alumno_id', 'section','variable', 'y'), with=F] %>% .[]
}
