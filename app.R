rm(list = ls())

cnf <<- yaml::read_yaml('config.yml')
sapply(list.files(cnf$deploy$codepath, recursive = T, full.names = T), source)
library_dockerfile(cnf$deploy$dockerfile)
shinyApp(ui = shinymanager::secure_app(ui=ui(cnf), enable_admin = T), 
         server = server, 
         options = list(port=cnf$deploy$port))

