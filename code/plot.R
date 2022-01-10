## plot.R
## author: Francisco Jose Diego Acosta

fill_na <- function(x, fill=0){
  x[is.na(x)] = fill
  return(x)
}


to_binary <- function(x, positive=1, negative=0){
  x[x!=positive] = negative
  return(x)
}


create_comparison <- function(df, filter, comparison){
  if(comparison %in% names(filter)){
    comp = as.integer(filter_vector_diff_element(df, filter, comparison))
  }else{
    if(comparison %in% names(df)){
      comp = df[[comparison]] %>% fill_na(0) %>% as.integer() %>% to_binary()
    }else{
      # comp =  as.integer(filter_vector(df, filter))
      comp = rep(1, nrow(df))
    }
  }
  return(comp)
}





plot_tidy_data <- function(df){
  columns = c('grupo', 'posicion', 'nivel', 'porcentaje')
  grupo_labels = c('Seleccionado', 'Comparación')
  posicion_labels = c( 'Izquierda', 'Derecha')
  nivel_labels = cnf$plot$x_labels
  df %>%
    .[all>0] %>%
    .[!is.na(y)] %>%
    .[, .N, by = .(y, facet, compare)] %>%
    .[, TOTAL := sum(N), by = .(compare,  facet)] %>%
    .[, porcentaje := round(N/TOTAL, 3) * 100] %>%
    .[, grupo := ifelse(compare==1, head(grupo_labels, 1), tail(grupo_labels, 1))] %>%
    .[, grupo := factor(grupo, grupo_labels, ordered = F)] %>%
    .[, nivel := factor(y, levels=c(0,1,2,3), labels = nivel_labels, ordered = T) ] %>%
    .[, posicion := factor(facet, levels = c(1, 0), labels = posicion_labels, ordered = T)] %>%
    .[, c(columns), with=F] %>%
    setkeyv(columns) %>%
    .[]
}


plot_options <- function(hc){
  hc %>%
    hc_xAxis(title=list(text=''),
             categories = cnf$plot$x_labels)  %>%
    hc_yAxis(title=list(text=''),
             max =100,
             labels = list(
               formatter = JS("function () {return Math.abs(this.value) + '%';}"))) %>%
    hc_plotOptions(
      column = list(
        dataLabels=list(enabled=T, format='{y} %'),
        borderRadius = 10
      )) %>%
    hc_colors(c("#5e72e4",  "#2b2d2f"))
}


plural <- function(string, vector){
  paste0(string, ifelse(length(vector)>1, 's', ' '))
}


set_sentence <- function(string,vector){
  paste0(plural(string, vector), ' : ', paste0(vector, collapse = ' - '))
}


set_title <- function(main=NULL, schools, courses, groups){
  title_list = list(
    set_sentence('Centro', schools),
    set_sentence('Curso', courses),
    set_sentence('Grupo', groups)
  )
  title_str = paste0(paste0(title_list, collapse = '<br/>'), '<br/>')
  if(!is.null(main)){
    title_str = paste0('<b>', main, '</b> <br/>', title_str)
  }
  return(title_str)
}


title_comp <- function(main, x, selected = 'Seleccionado', notselected='Comparación'){
  title_str = paste0('<b>', main, '</b> <br/>')
  if(x=='gender'){
    return(paste0(title_str, selected, ': Mujer <br/>', notselected, ': Hombre'))
  }
  if(x=='is_repeater'){
    return(paste0(title_str, selected, ': Repetidor <br/>', notselected, ': No Repetidor'))
  }
  if(x == 'is_popular'){
    return(paste0(title_str, selected, ': Popular <br/>', notselected, ': No Popular'))
  }
  
}


title_side <- function(division, is_left=T){
  if(division %in% c('gender', 'is_repeater', 'is_popular')){
    if(division == 'gender'){
      return(ifelse(is_left, 'Mujer', 'Hombre'))
    }
    if(division=='is_repeater'){
      return(ifelse(is_left, 'Repetidor', 'No Repetidor'))
    }
    if(division=='is_popular'){
      return(ifelse(is_left, 'Popular', 'No Popular'))
    }
  }
}



set_main_title <- function(variable){
  variable = ifelse(variable == 'anímica', 'ánimo', variable)
  variable = ifelse(variable == 'alimenticia', 'alimentación', variable)
  variable = ifelse(variable == 'relacional', 'relaciones sociales', variable)
  return(stringr::str_to_title(variable))
}


plot_chart = function(df){
  df %>%
    hchart(hcaes(x=nivel, y=porcentaje,group=factor(grupo)), type='column',
           name=.[, sort(unique(grupo))]) %>%
    plot_options()
  
}


data_expand <- function(df, cnf){
  grupos = df[, as.character(unique(grupo))]
  posiciones = df[, as.character(unique(posicion))]
  niveles = unique(cnf$plot$x_labels)
  dfex = expand.grid(grupo=grupos, posicion=posiciones, nivel=niveles)
  merge(dfex, df, by=c('grupo', 'posicion', 'nivel'), all.x = T) %>%
    as.data.table() %>% .[]
}



double_check <- function(df){
  df %>%
    .[, total := sum(porcentaje, na.rm = T), by = .(grupo, posicion)] %>%
    .[, pct := round((porcentaje / total)*100, 2)] %>%
    .[, c('grupo', 'posicion', 'nivel', 'pct'), with=F] %>%
    setnames(., c('pct'), c('porcentaje')) %>%
    .[]
}



plot <- function(df, cnf, metric, filter=list(), compare='', facet='', display=T){
  df %>%
    plot_data(metric=metric,cnf=cnf, filter=filter, compare=compare, facet=facet) %>%
    data_expand(cnf) %>%
    double_check() -> dfp
  
  if(display==T){
    has_facets = max(dfp[, .(n=uniqueN(posicion)), by = .(grupo)][['n']])>1
    if(has_facets==T){
      dfp[['posicion']] %>% unique() %>% as.character() %>%
        map(~dfp[as.character(posicion) == .]) %>%
        map(plot_chart) %>%
        hw_grid(ncol = 2,browsable = T)
    }else{
      dfp %>% plot_chart()
    }
  }else{
    return(dfp)
  }
  
}


plot_data <- function(df, cnf, metric, filter=list(), compare='' , facet=''){
  columns = c( 'division','comparacion', 'nivel', 'porcentaje')
  default_levels = c('Seleccionado', 'Comparacion')
  facet_levels = str_to_title(cnf[['booleans']][[facet]])
  compare_levels = str_to_title(cnf[['booleans']][[compare]])
  nivel_labels = cnf$plot$x_labels
  if(length(facet_levels)==0){facet_levels = default_levels}
  if(length(compare_levels)==0){compare_levels = default_levels}
  
  df = df[variable == metric] %>% .[!is.na(y)]
  
  filter_int = as.integer(filter_vector(df, filter))
  if(compare %in% c('school', 'group')){
    compare_int = as.integer(df[[compare]] != 0)
  }else{
    compare_int = as.integer(create_comparison(df, filter, compare))
  }
  
  
  
  facet_int = as.integer(create_comparison(df, filter, facet))
  
  df[, c('filter', 'compare', 'facet') :=
       list(filter_int, compare_int, facet_int)]
  df[school==0, filter := 1]
  
  # if(facet %in% names(df)){
  #   df[['facet']] = df[[facet]] %>% fill_na() %>% as.integer()
  # }else{
  #   df[, facet := 0]
  # }
  if((compare==facet) | (sum(compare_int) == 0) |  sum(filter_int - compare_int) == 0){
    df[, all := filter]
  }else{
    df[, all := filter+facet+compare]
  }
  
  dfex = expand.grid(comparacion=compare_levels, division=facet_levels, nivel=nivel_labels)
  df = df[all>0]
  
  if(df[, uniqueN(compare)] == 1){
    df[, compare := 1]
  }
  if(df[, uniqueN(facet)] == 1){
    df[, facet := 1]
  }
  df %>%
    .[, .N, by = .(y,facet, compare)] %>%
    .[, TOTAL := sum(N), by = .(compare,  facet)] %>%
    .[, porcentaje := round(N/TOTAL, 3) * 100] %>%
    .[, comparacion := factor(compare, levels = c(1, 0), labels = compare_levels, ordered = T)] %>%
    .[, division := factor(facet, levels = c(1, 0), labels = facet_levels, ordered = T)] %>%
    .[, nivel := factor(y, levels=c(0,1,2,3), labels = nivel_labels, ordered = T) ] %>%
    .[, c(columns), with=F] %>%
    setkeyv(columns) %>%
    merge(dfex, ., by=c('division', 'comparacion', 'nivel'), all.x = T) %>%
    as.data.table() %>%
    .[, total := sum(porcentaje, na.rm = T), by = .(division, comparacion)] %>%
    .[, pct := round((porcentaje / total)*100, 2)] %>%
    .[, c('division', 'comparacion', 'nivel', 'pct'), with=F] %>%
    setnames(., c('pct'), c('porcentaje')) %>%
    .[] -> df
  
  if(all(compare_levels == default_levels)){
    if(df[comparacion == tail(default_levels, 1), all(is.na(porcentaje))]){
      df = df[comparacion == head(default_levels, 1)]
    }
  }
  if(all(facet_levels == default_levels)){
    if(df[division == tail(default_levels, 1), all(is.na(porcentaje))]){
      df = df[division == head(default_levels, 1)]
    }
  }
  
  df %>% .[]
}


plot_chart_no_facet = function(df){
  hchart(df,hcaes(x=nivel,
                  y=porcentaje,
                  group=factor(comparacion)),
         type='column',
         name=df[, sort(unique(comparacion))]) %>%
    plot_options()
}

plot_chart_facet <- function(df){
  df[['division']] %>% unique() %>% as.character() %>%
    map(~df[as.character(division) == .]) %>%
    map(plot_chart_no_facet) %>%
    hw_grid(ncol = 2,browsable = T)
}

plot_chart <- function(df){
  has_facets = max(df[, .(n=uniqueN(division)), by = .(comparacion)][['n']])>1
  if(has_facets){
    df %>% plot_chart_facet()
  }else{
    df %>% plot_chart_no_facet()
  }
}



plot <- function(df, cnf, metric, filter, compare, facet){
  plot_data(df, cnf, metric, filter, compare ,facet)
  plot_chart()
}