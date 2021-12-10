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

  )
  


  
}


body <- function() {
  argonDashBody(
    tags$head(
      tags$style(
        HTML(".shiny-notification {
             position:fixed;
             top: calc(70%);
             left: calc(20%);
             }
             "
        )
      )
    ),
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
                      
                      pickerInput(
                        inputId = "compare",
                        label = "Comparar", 
                        choices = list(' '=' ', 
                                       Centro='school', 
                                       Curso = 'course', 
                                       Grupo = 'group', 
                                       Género = 'gender', 
                                       Repetidor = 'is_repeater', 
                                       Popular = 'is_popular'),
                        multiple = TRUE,
                        selected = ' ', 
                        options = pickerOptions(maxOptions = 1, noneSelectedText=' ' ), 
                        choicesOpt = list(
                          style = "color: steelblue")
                      )
          ), 
          
          argonColumn(with=6, 
                      
                      pickerInput(
                        inputId = "facet",
                        label = "Dividir", 
                        choices = list(' '=' ', 
                                       Género = 'gender', 
                                       Repetidor = 'is_repeater', 
                                       Popular = 'is_popular'),
                        selected = ' ',
                        multiple = T,
                        options = pickerOptions(maxOptions = 1, noneSelectedText=' ' ), 
                        choicesOpt = list(
                          style = "color: steelblue")
                      )
          )
          ), 
        shiny::br(),
        argonDash::argonSidebarDivider(),
        
        argonRow(
          argonColumn(width = 3, 
                      uiOutput("admin_upload_data_title")
          ), 
          argonColumn(width = 3, 
                      uiOutput("admin_files_title")
          ), 
          argonColumn(width = 3, 
                      uiOutput('admin_dataset_title'))
        ), 
        
        argonRow(
          argonColumn(width = 3, 
            uiOutput("admin_upload_data_select")
            ), 
          
          argonColumn(width = 3, 
                      uiOutput("admin_files_list")
          ),
          argonColumn(width = 3, 
                      uiOutput("admin_dataset_list")
          ), 
          argonColumn(width = 1,  
                      shiny::br(), 
                      uiOutput("admin_dataset_delete")
          ), 
          
          argonColumn(width = 1,  
                      shiny::br(), 
                      uiOutput("admin_dataset_update")
          )
          ), 
        argonRow(
          argonColumn(width = 3, 
                      uiOutput("admin_upload_data_process")
          ), 
          argonColumn(width = 1, 
                      uiOutput("admin_files_delete")
          ), 
          argonColumn(width = 1, 
                      uiOutput("admin_dataset_create")
          )
          
        ), 
        argonRow(
          argonColumn(width = 3, 
          uiOutput("admin_upload_data_name")
          ), 
          argonColumn(width = 3, 
                      uiOutput("admin_files_delete_check"), 
                      
          ), 
          argonColumn(width = 3, 
                      tableOutput("admin_dataset_content"), 
                      
          )
        )
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