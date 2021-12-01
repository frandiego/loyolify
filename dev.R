rm(list = ls())
cnf = yaml::read_yaml('config.yml')
sapply(list.files('code', recursive = T, full.names = T), source)
# tidy(list.files('data/raw', full.names = T), cnf)
# preprocess(cnf)
df = read_data(cnf)

filter = list(variable='autocontrol', group=c(1,2))




filter_bool <- function(df, filter, variables = ''){
  variables = ifelse(variables=='', names(filter), variables)
  filter[intersect(names(filter), variables)] %>% 
    map2(.x=names(.), .y=., .f=function(x,y) df[[x]] == y) %>% 
    reduce(`*`) %>% as.integer()
}

filter_bool(df,filter, c('variable', 'school')) %>% sum
