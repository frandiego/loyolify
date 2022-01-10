## ui.R
## author: Francisco Jose Diego Acosta

# HEADER ------------------------------------------------------------------


header <- function() {
  argonDashHeader(gradient = TRUE, color = "primary", separator = F)
}



# NAVBAR ------------------------------------------------------------------


navbar <- function() {
  argonDashNavbar(
  )
}


# FOOTER ------------------------------------------------------------------



footer <- function() {
  argonDashFooter(
    copyrights = "Mapeo De Competencias",
    src = "https://www.mapeodecompetencias.es/"
  )
}


# SIDEBAR -----------------------------------------------------------------


sidebar <- function(cnf) {
  argonDashSidebar(
    vertical = TRUE,
    skin = "dark",
    background = "white",
    side = "left",
    id = "sidebar",
    brand_url = "https://www.mapeodecompetencias.es/",
    brand_logo = "https://pbs.twimg.com/profile_images/992101578365693952/FjSMREIy_400x400.jpg",
    size = 'lg',
    
    argonSidebarHeader(),
    shiny::uiOutput('selector_school'),
    
    
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


# BODY --------------------------------------------------------------------


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
        
        # DTOutput('plot_data'),
        
        argonRow(
          argonColumn(with=6,
                      
                      pickerInput(
                        inputId = "compare",
                        label = "Comparar",
                        choices = list('No'='no',
                                       Centro='school',
                                       Curso = 'course',
                                       Grupo = 'group',
                                       Género = 'gender',
                                       Repetidor = 'is_repeater',
                                       Popular = 'is_popular'),
                        multiple = F,
                        choicesOpt = list(
                          style = "color: steelblue")
                      )
          ),
          
          argonColumn(with=6,
                      
                      pickerInput(
                        inputId = "facet",
                        label = "Dividir",
                        choices = list('No'='no',
                                       Género = 'gender',
                                       Repetidor = 'is_repeater',
                                       Popular = 'is_popular'),
                        multiple = F,
                        choicesOpt = list(
                          style = "color: steelblue")
                      )
          )
        ),
        shiny::br(),
        argonDash::argonSidebarDivider(),
        
        argonRow(
          argonColumn(width = 4,
                      uiOutput("admin_upload_data_title")
          ),
          argonColumn(width = 4,
                      uiOutput("admin_files_title")
          ),
          argonColumn(width = 4,
                      uiOutput('admin_dataset_title'))
        ),
        
        argonRow(
          argonColumn(width = 4,
                      uiOutput("admin_upload_data_select")
          ),
          
          argonColumn(width = 4,
                      uiOutput("admin_files_list")
          ),
          argonColumn(width = 4,
                      uiOutput("admin_dataset_list")
          )
        ),
        argonRow(
          argonColumn(width = 4,
                      uiOutput("admin_upload_data_process")
          ),
          argonColumn(width = 1,
                      uiOutput("admin_files_delete")
          ),
          argonColumn(width = 3,
                      uiOutput("admin_dataset_create")
          ),
          argonColumn(width = 1,
                      uiOutput("admin_dataset_delete")
          ),
          argonColumn(width = 2,
                      uiOutput("admin_dataset_update")
          )
        ),
        argonRow(
          argonColumn(width = 4,
                      uiOutput("admin_upload_data_name")
          ),
          argonColumn(width = 4,
                      uiOutput("admin_files_delete_check"),
                      
          ),
          argonColumn(width = 4,
                      tableOutput("admin_dataset_delete_check"),
                      
          )
        )
        ,
        argonRow(uiOutput('selected_title'), center = F),
        argonRow(shiny::br(),uiOutput("selected_dataset"), center = F),
        argonRow(uiOutput('admin_content_title')),
        argonRow(uiOutput("admin_dataset_list_title")),
        argonRow(uiOutput("download_data_ui")),
        argonRow(DT::DTOutput("admin_dataset_content"))
        
        
      )
    )
  )
  
  
}


# UI ----------------------------------------------------------------------



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