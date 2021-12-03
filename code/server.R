server <- function(input, output, session) {
  
  # check_credentials directly on sqlite db
  auth_out <- secure_server(
    check_credentials = check_credentials(
      db = normalizePath(cnf$deploy$database),
      passphrase =  cnf$deploy$passphrase
    ), timeout = 0, 
    inputs_list = list(group = list(fun = "selectInput", 
                                    args = list(choices = c("all", "restricted"), 
                                                multiple = TRUE, 
                                                selected = c("all", "restricted")
                                    )
    )
    )
  )
  
  # upload data menu
  upload_data_sidebar <- reactive({
    auth_out_list = reactiveValuesToList(auth_out)
    if(as.logical(auth_out_list$admin)){
      shiny::fileInput(inputId = 'input_file', 
                       label = '', 
                       buttonLabel = 'Agregar Encuestas', 
                       placeholder = '', 
                       multiple = T, 
                       accept = ".xlsx|.xls|.odt")
      
    }else{
      div()
    }
  })
  
  # update data 
  update_data_admin <- reactive({
    auth_out_list = reactiveValuesToList(auth_out)
    if(as.logical(auth_out_list$admin)){
      shiny::actionButton('update_data', 'Procesar')
    }else{
      div()
    }
  })
  
  
  output$upload_data_admin <- renderUI({ upload_data_sidebar() })
  output$update_data_admin <- renderUI({ update_data_admin() })
  

  
  
  
  # read data
  there_is_data = F
  data <- reactive({
    users = reactiveValuesToList(auth_out)
    
    dt = read_data(cnf) %>% filter_comment(comment = users$comment)
    
    there_is_data<<-T
    
    dt %>% .[]
    
    })
  
  # sections
  sections <- reactive({ data() %>% .[['section']] %>% unique() })
  
  # filter
  filter <- reactive({ list(variable = input$variable, 
                            school = input$school, 
                            course = input$course, 
                            group = input$group 
                            )
    })
  
  # title
  
  reavtive_main_title <- reactive({
    shiny::h1(set_main_title(input$variable))
  })
  
  reactive_title <- reactive({
    HTML(set_title(main='Seleccionado', 
                   schools = input$school, 
                   courses = input$course, 
                   groups = LETTERS[as.integer(c(input$group))]
                   ))
  })
  
  reactive_title_comp <- reactive({
    filter_comp = filter_diff(data(), filter(), input$compare)
    comp_external <- as.character(input$compare) %in% c('gender', 'is_repeater', 'is_popular')
    if(comp_external){
      HTML(title_comp('Comparación', as.character(input$compare), 'Seleccionado', 'Comparación'))
    }else{
      zero_filter = any(map_dbl(filter_comp, length) == 0)
      same_filter = all(map2_lgl(.x=filter_comp, .y=filter(), 
                                 .f = function(x, y) all(x==y)))
      if(zero_filter | same_filter){
        div()
      }else{
        HTML(set_title(main='Comparación', 
                       schools = filter_comp$school, 
                       courses = filter_comp$course, 
                       groups = LETTERS[as.integer(c(filter_comp$group))]))
      }
    }
    
  })
  
  reactive_title_facet <- reactive({
    if(as.character(input$facet) %in% c('gender', 'is_repeater', 'is_popular')){
      HTML(title_comp('División', as.character(input$facet), 'Izquierda', 'Derecha'))
    }else{
      div()
    }
  })
  
  
  output$title <- renderUI({reactive_title()})
  output$title_comp <- renderUI({reactive_title_comp()})
  output$title_facet <- renderUI({reactive_title_facet()})
  output$main_title <- renderUI({reavtive_main_title()})
  
  
  
  
  # subtitles
  reactive_title_facet_left <- reactive({
    if(input$facet %in% c('gender', 'is_repeater', 'is_popular')){
      HTML(paste0('<b>', title_side(input$facet, is_left = T), '</b>'))
    }else{
      div()
    }
  })
  
  reactive_title_facet_rigth <- reactive({
    if(input$facet %in% c('gender', 'is_repeater', 'is_popular')){
      HTML(paste0('<b>', title_side(input$facet, is_left = F), '</b>'))
    }else{
      div()
    }
  })
  
  output$subtitle_left <- renderUI({ reactive_title_facet_left() })
  output$subtitle_right <- renderUI({ reactive_title_facet_rigth() })
 
  
  # plot
  reactive_plot <- reactive({ plot(df=data(), 
                                   cnf = cnf, 
                                   filter = filter(), 
                                   compare=input$compare, 
                                   facet = input$facet
                                   ) })
  

  output$plot <- renderUI({reactive_plot()})
  
  
  # update schools
  observeEvent(eventExpr = there_is_data, 
               handlerExpr = {
                 data() %>% .[['school']] %>% unique() %>% .[!is.na(.)] -> choices
                 label = ifelse(length(choices)>1, 'Centros', 'Centro')
                 updateSelectizeInput(session = session, 
                                      inputId = 'school', 
                                      label = label, 
                                      choices = choices, 
                                      selected = choices
                 )
               })
  
  # update course
  observeEvent(eventExpr = input$school, 
               handlerExpr = {
                 data() %>% .[school %in% input$school] %>% 
                   .[['course']] %>% unique() -> choices
                 updateSelectizeInput(session = session, 
                                      inputId = 'course', 
                                      choices = choices, 
                                      selected = head(choices, round(length(choices) / 2))
                 )
               })
  
  # update group
  observeEvent(eventExpr = input$course, 
               handlerExpr = {
                 data() %>% .[course %in% input$course] %>% 
                   .[['group']] %>% unique() %>% sort() %>% .[!is.na(.)] -> choices
                 names(choices) <- LETTERS[choices]
                 updateSelectizeInput(session = session, 
                                      inputId = 'group', 
                                      choices = choices, 
                                      selected = head(choices, round(length(choices) / 2))
                 )
               })
  
  # update sections
  observeEvent(eventExpr = there_is_data, 
               handlerExpr = {
                 data() %>% .[['section']] %>% unique() %>% .[!is.na(.)] -> choices
                 names(choices) <- stringr::str_to_title(choices)
                 updateSelectizeInput(session = session, 
                                      inputId = 'section', 
                                      choices = choices, 
                                      selected = choices
                 )
               })
  
  # update variables
  observeEvent(eventExpr = input$section, 
               handlerExpr = {
                 data() %>% .[section %in% input$section] %>% 
                   .[['variable']] %>% unique() %>% .[!is.na(.)] -> choices
                 
                 names(choices) <- map_chr(choices, set_main_title)
                 
                 updateSelectizeInput(session = session, 
                                      inputId = 'variable', 
                                      choices = choices, 
                                      selected = head(choices, 1)
                 )
               })
  
  # update data
  observeEvent(input$update_data, {
    if (is.null(input$input_file)){
      showNotification('No file Updated', duration = 10, type='error')
    }else{
      res <<- tidy_and_process_safe(input$input_file$datapath, cnf)
    }
  })
  
  res = F
  observeEvent(res, {
    showNotification("This is a notification.")
    if(is.list(res)){
      if(!is.null(res$error)){
        shinyalert::shinyalert("Oops!", "Something went wrong.", type = "error")
      }else{
        shinyalert::shinyalert("OK", type = "success")
      }
    }
  })
  
  
  

  

  
}