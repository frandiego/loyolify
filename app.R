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
