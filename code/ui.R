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
                          label = 'Centros', 
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
    , uiOutput("upload_data_admin")
    , uiOutput("update_data_admin")

    
    
  )
  


  
}


body <- function() {
  argonDashBody(
    argonTabItems(
      argonTabItem(
        tabName = 'plot', 
        argonRow(argonColumn(uiOutput('main_title'), width = 12, center = T)),
        argonRow(argonColumn(uiOutput('title'), width = 6), 
                 argonColumn(uiOutput('title_comp'), width = 6)),
        shiny::br(),
        argonRow(argonColumn(uiOutput('subtitle_left'), width = 6, center = T), 
                 argonColumn(uiOutput('subtitle_right'), width = 6, center = T)),
        htmlOutput('plot'), 
        argonRow(
          argonColumn(with=6, 
                      shiny::selectizeInput(inputId = 'compare', 
                                            label = 'Comparar', 
                                            choices = list(No='No', 
                                                           Centro='school', 
                                                           Curso = 'course', 
                                                           Grupo = 'group', 
                                                           Género = 'gender', 
                                                           Repetidor = 'is_repeater', 
                                                           Popular = 'is_popular'),  
                                            multiple = F)
          ), 
          
          argonColumn(with=6, 
                      shiny::selectizeInput(inputId = 'facet', 
                                            label = 'Dividir', 
                                            choices = list(No='No', 
                                                           Género = 'gender', 
                                                           Repetidor = 'is_repeater', 
                                                           Popular = 'is_popular'),  
                                            multiple = F)
          ))
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