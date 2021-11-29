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
    .[, n:= as.integer(value) - 1] %>% .[]
  
  
    merge(df_ok, by = 'variable', all.x = T) -> dt
  dttrick <- dt[variable == 'pensamiento_estrategico__d', -('ok')]
  dttrick[, variable := 'pensamiento_estrategico__c']
  setnames(dttrick, 'value', 'ok_trick')
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

