## plot.R
## author: Francisco Jose Diego Acosta


plot_data <- function(df, filter=list(), compare='' , facet=''){
  f_ = rep(1, nrow(df))
  c_ = rep(1, nrow(df))
  ft_ = rep(1, nrow(df))
  for (n in names(filter)){
    if(!all(filter[[n]] %in% c('', NA, NULL))){
      b_ = as.integer(df[[n]] %in% filter[[n]])
      f_ = f_ * b_
      if (n != compare){
        c_ = c_ * b_
      if (n != facet){
        ft_ = ft_ * b_
      }
      }
    }
  }
  df[, c('filter', 'compare', 'facet') := list(f_, c_, ft_)]
  df[, all := as.integer(filter+compare+facet>0)]
  df %>% .[]
}


plot_tidy_data <- function(df, cnf){
  df %>% 
    .[filter==1] %>% 
    .[!is.na(y)] %>% 
    .[, .N, by = .(y, filter, compare, facet)] %>% 
    .[, TOTAL := sum(N)] %>% 
    .[, PCT := round(N/TOTAL, 3) * 100] %>% 
    .[]
}


plot_options <- function(hc){
  hc %>% 
  hc_plotOptions(
    column = list(
      dataLabels=list(enabled=T, format='{y} %'), 
      stacking = "stream", 
      borderRadius=10
    )) %>% 
    hc_xAxis(title=list(text=''), 
             categories = cnf$plot$x_labels) %>% 
    hc_yAxis(title=list(text=''), 
             max =100, 
             labels = list(
               formatter = JS("function () {return Math.abs(this.value) + '%';}"))) %>% 
    hc_colors(cnf$plot$main_color)
}


plot <- function(df, cnf, filter=list(), compare='', facet=''){
  df %>% 
    plot_data(filter=filter, compare=compare, facet=facet) %>%
    plot_tidy_data(., cnf) %>% 
    hchart('column', 
           hcaes(x=y, y=PCT), name='Selecinado') %>%  
    plot_options()
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

