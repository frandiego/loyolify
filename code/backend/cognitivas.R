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