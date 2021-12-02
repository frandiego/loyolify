## ui.R
## author: Francisco Jose Diego Acosta


header <- function() {
  argonDashHeader(gradient = TRUE, color = "primary", separator = F)
}


navbar <- function() {
  return(argonDashNavbar())
}


footer <- function() {
  return(argonDashFooter())
}


sidebar <- function(cnf) {
  argonDashSidebar(
    vertical = TRUE,
    skin = "dark",
    background = "white",
    size = "md",
    side = "left",
    id = "sidebar", 
    
    argonSidebarHeader(),
    
    # schools
    shiny::selectizeInput(inputId = 'school', 
                          label = 'Colegios', 
                          choices = c(''), 
                          multiple = T), 
    
    # course
    shiny::selectizeInput(inputId = 'course', 
                          label = 'Cursos', 
                          choices = c(''), 
                          multiple = T), 
    
    # group
    shiny::selectizeInput(inputId = 'group', 
                          label = 'Grupos', 
                          choices = c(''), 
                          multiple = T), 
    
    # sections
    shiny::selectizeInput(inputId = 'section', 
                          label = 'Sección', 
                          choices = c(''), 
                          multiple = F), 
    
    # variables
    shiny::selectizeInput(inputId = 'variable', 
                         label = 'Variables', 
                         choices = c(''), 
                         multiple = F)
    
    
  )
  


  
}


body <- function() {
  argonDashBody(
    argonTabItems(
      argonTabItem(
        tabName = 'plot', 
        
        shiny::selectizeInput(inputId = 'compare', 
                              label = 'Comparar', 
                              choices = list(No='No', 
                                             Centro='school', 
                                             Curso = 'course', 
                                             Grupo = 'group', 
                                             Género = 'gender', 
                                             Repetidor = 'is_repeater', 
                                             Popular = 'is_popular'),  
                              multiple = F), 
        argonRow(uiOutput('title')),
        shiny::br(),
        argonRow(),
        highchartOutput('plot')
      )
    )
  )
}


ui <- function(cnf) {
  return(argonDashPage(
    title = cnf$ui$title, 
    description = cnf$ui$description, 
    author = cnf$ui$author, 
    
    sidebar = sidebar(),
    
    navbar = navbar(),
    
    header = header(),
    
    body = body(),
    
    footer = footer()
    
    
  ))
}