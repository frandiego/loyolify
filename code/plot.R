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
      comp =  as.integer(filter_vector(df, filter))
    }
  }
  return(comp)
}





plot_data <- function(df, filter=list(), compare='' , facet=''){
  f_int = as.integer(filter_vector(df, filter))
  c_int = as.integer(create_comparison(df, filter, compare))
  facet_int = as.integer(create_comparison(df, filter, facet))
  df[, c('filter', 'compare', 'facet') := list(f_int, c_int, facet_int)]
  df[, all := as.integer(filter+compare+facet>0)]
  df %>% .[]
}


plot_tidy_data <- function(df){
  columns = c('grupo', 'nivel', 'porcentaje')
  grupo_labels = c('Seleccionado', 'Comparación')
  nivel_labels = cnf$plot$x_labels
  df %>% 
    .[all==1] %>% 
    .[!is.na(y)] %>% 
    .[, .N, by = .(y, filter, facet)] %>% 
    .[, TOTAL := sum(N), by = .(filter, facet)] %>% 
    .[, porcentaje := round(N/TOTAL, 3) * 100] %>% 
    .[, grupo:= ifelse(filter==1, head(grupo_labels, 1), tail(grupo_labels, 1))] %>% 
    .[, grupo := factor(grupo, grupo_labels, ordered = T)] %>% 
    .[, nivel := factor(y, labels = nivel_labels, ordered = T) ] %>% 
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


set_title <- function(schools, courses, groups, variables){
  title_list = list(
    set_sentence('Colegio', schools), 
    set_sentence('Curso', courses), 
    set_sentence('Grupo', groups),
    set_sentence('Variable', variables)
  )
  paste0(paste0(title_list, collapse = '<br/>'), '<br/>')
}


plot <- function(df, cnf, filter=list(), compare='', facet='', display=T){
  df %>% 
    plot_data(filter=filter, compare=compare, facet=facet) %>% 
    plot_tidy_data() -> dfp
  if(display==T){
    dfp %>% 
    hchart(hcaes(x=nivel, y=porcentaje,group=grupo), type='column', name=.[, unique(grupo)]) %>% 
      plot_options()
  }else{
    return(dfp)
  }

}


