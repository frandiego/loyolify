rm(list = ls())

deploy <- function(config_path='config.yml'){
  cnf <<- yaml::read_yaml(config_path)
  sapply(list.files(cnf$deploy$codepath, recursive = T, full.names = T), source)
  library_dockerfile(cnf$deploy$dockerfile)
  shinyApp(ui = ui(cnf), 
           server = server, 
           options = list(port=cnf$deploy$port))
}


deploy()

# filter
filter <- reactive({ list(variable = input$variable, 
                          school = input$school, 
                          course = input$course, 
                          group = input$group
)
})
df = read_data(cnf)
varnames = c('variable', 'school', 'course', 'group')
varnames %>%  map(~df[[.]] %>% unique) -> filter
names(filter) = varnames


filter[['school']] = 337

filter %>% 
  map2(.x=names(.), .y=., 
       .f = function(x, y) as.integer(df[[x]] == y)) %>% 
  reduce('*')

