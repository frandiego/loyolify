rm(list = ls())

deploy <- function(config_path='config.yml', run=T){
  cnf <<- yaml::read_yaml(config_path)
  sapply(list.files(cnf$deploy$codepath, recursive = T, full.names = T), source)
  library_dockerfile(cnf$deploy$dockerfile)
  if(run==T){
    shinyApp(ui = ui(cnf), 
             server = server, 
             options = list(port=cnf$deploy$port))
  }
}


deploy(run = T)





df = read_data(cnf)
varnames = c('variable', 'school', 'course', 'group')
varnames %>%  map(~df[[.]] %>% unique) -> filter
names(filter) = varnames
filter[['course']] = c(1,2)
filter[['variable']] = c('relacional')

compare = 'centro'
facet = 'gender'

dfp = plot(df, cnf, filter, compare = compare, facet=facet, display = F)




