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
  
  # Survey
  admin_upload_data_title <- reactive({
    auth_out_list = reactiveValuesToList(auth_out)
    if(as.logical(auth_out_list$admin)){
      shiny::h1('Nueva Encuesta')
    }else{
      div()
    }
  })
  
  admin_upload_data_select <- reactive({
    auth_out_list = reactiveValuesToList(auth_out)
    if(as.logical(auth_out_list$admin)){
      shiny::fileInput(inputId = 'input_file', 
                       label = '', 
                       buttonLabel = 'Agregar', 
                       placeholder = '', 
                       multiple = F, 
                       accept = ".xlsx|.xls|.odt")
      
    }else{
      div()
    }
  })

  admin_upload_data_name <- reactive({
    auth_out_list = reactiveValuesToList(auth_out)
    if(as.logical(auth_out_list$admin)){
      shiny::textInput(inputId = 'update_data_name', label='', placeholder = 'Nombre')
    }else{
      div()
    }
  })
  
  admin_upload_data_process <- reactive({
    auth_out_list = reactiveValuesToList(auth_out)
    if(as.logical(auth_out_list$admin)){
      shiny::actionButton('update_data', 'Cargar')
    }else{
      div()
    }
  })
  
  output$admin_upload_data_title <- renderUI({ admin_upload_data_title() })
  output$admin_upload_data_select <- renderUI({ admin_upload_data_select() })
  output$admin_upload_data_name <- renderUI({ admin_upload_data_name() })
  output$admin_upload_data_process <- renderUI({ admin_upload_data_process() })
  
  # Files
  admin_files_title <- reactive({
    auth_out_list = reactiveValuesToList(auth_out)
    if(as.logical(auth_out_list$admin)){
      shiny::h1('Encuestas')
    }else{
      div()
    }
  })
  admin_files_list <- reactive({
    auth_out_list = reactiveValuesToList(auth_out)
    if(as.logical(auth_out_list$admin)){
      cnf$tidy$output_path %>% lslatr() %>% .[['rn']] %>% 
        basename() %>% gsub('.RDS$', '', .) -> choices
      shiny::selectInput(
        inputId = 'admin_list_tidy_files', 
        label = '', 
        choices = choices, 
        selected = choices, 
        multiple = T
      )
    }else{
      div()
    }
  })
  admin_files_delete <- reactive({
    auth_out_list = reactiveValuesToList(auth_out)
    if(as.logical(auth_out_list$admin)){
      shiny::actionButton(inputId = 'delete', label = 'Eliminar')
    }else{
      div()
    }
  })
  admin_files_delete_check <- reactive({
    auth_out_list = reactiveValuesToList(auth_out)
    if(as.logical(auth_out_list$admin)){
      shiny::textInput(inputId = 'check', label = '', value = '', placeholder = 'Seguro?')
      
    }else{
      div()
    }
  })
  output$admin_files_title <- renderUI({ admin_files_title() })
  output$admin_files_list <- renderUI({ admin_files_list() })
  output$admin_files_delete <- renderUI({ admin_files_delete() })
  output$admin_files_delete_check <- renderUI({ admin_files_delete_check() })
  
  
  
  # Dataset
  admin_dataset_title <- reactive({
    auth_out_list = reactiveValuesToList(auth_out)
    if(as.logical(auth_out_list$admin)){
      shiny::h1('Datasets')
    }else{
      div()
    }
  })
  
  admin_dataset_create <- reactive({
    auth_out_list = reactiveValuesToList(auth_out)
    if(as.logical(auth_out_list$admin)){
      shiny::actionButton(inputId = 'create', label = 'Procesar')
    }else{
      div()
    }
  })

  admin_dataset_list <- reactive({
    auth_out_list = reactiveValuesToList(auth_out)
    if(as.logical(auth_out_list$admin)){
      cnf$preprocess$output_path %>% 
        list.files() %>% 
        .[grepl('^Dataset', .)] %>% 
        gsub('.RDS$', '', .) %>% 
        unique() %>% 
        sort(T) -> choices
      shiny::selectInput(
        inputId = 'admin_dataset_list', 
        label = '', 
        choices = choices, 
        selected = head(choices, 1), 
        multiple = F
      )
    }else{
      div()
    }
  })
  
  
  admin_dataset_delete <- reactive({
    auth_out_list = reactiveValuesToList(auth_out)
    if(as.logical(auth_out_list$admin)){
      shiny::actionButton(inputId = 'delete_dataset', label = 'Eliminar')
    }else{
      div()
    }
  })
  
  admin_dataset_update <- reactive({
    auth_out_list = reactiveValuesToList(auth_out)
    if(as.logical(auth_out_list$admin)){
      shiny::actionButton(inputId = 'update_dataset', label = 'Actualizar')
    }else{
      div()
    }
  })
  
  admin_dataset_delete_check <- reactive({
    auth_out_list = reactiveValuesToList(auth_out)
    if(as.logical(auth_out_list$admin)){
      shiny::textInput(inputId = 'dataset_check', label = '', value = '', placeholder = 'Seguro?')
      
    }else{
      div()
    }
  })
  

  
  
  admin_dataset_content <- reactive({
    auth_out_list = reactiveValuesToList(auth_out)
    if(as.logical(auth_out_list$admin)){

      input$admin_dataset_list %>% 
        file.path(cnf$preprocess$output_path,. ) %>% 
        paste0(., '.RDS') %>% 
        readRDS() %>% 
        as.data.table() %>% 
        .[, c('school', 'course', 'group'), with=F] %>% 
        unique() %>% 
        .[!is.na(school)] %>% 
        .[, group := factor(group, levels=1:length(LETTERS), labels = LETTERS)] %>% 
        .[, file := as.character(input$admin_dataset_list)] %>% 
        as.data.frame()
    }else{
      data.frame()
    }
  })
    
  output$admin_dataset_title <- renderUI({ admin_dataset_title() })
  output$admin_dataset_create <- renderUI({ admin_dataset_create() })
  output$admin_dataset_list <- renderUI({ admin_dataset_list() })
  
  output$admin_dataset_delete <- renderUI({admin_dataset_delete() })
  output$admin_dataset_update <- renderUI({ admin_dataset_update() })

  output$admin_dataset_content <- renderTable({admin_dataset_content() })
  
  

  
  
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
                                      selected = choices
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
                                      selected = choices
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
      res <<- tidy_unique_file_safe(input$input_file$datapath, input$update_data_name, cnf)
      if (!is.null(res$error)){
        showNotification(as.character(res$error), duration = 10, type='error')
      }else{
        showNotification('Nueva encuesta añadida')
        cnf$tidy$output_path %>% lslatr() %>% .[['rn']] %>% 
          basename() %>% gsub('.RDS$', '', .) -> choices
        updateSelectInput(
          session = session, 
          inputId = 'admin_list_tidy_files',
          choices = choices, 
          selected = choices
        )
        
      }
    }
  })
  
  # eliminar
  observeEvent(input$delete, {
    if (input$check != 'eliminar'){
      showNotification("Estas seguro? Es irreversible.  Escribe: 'eliminar'", duration = 10, type='error')
    }else{
      input$admin_list_tidy_files %>% 
        paste0('.RDS') %>% 
        file.path(cnf$tidy$output_path, .) %>% 
        file.remove()
      showNotification('Encuestas eliminadas')
      cnf$tidy$output_path %>% lslatr() %>% .[['rn']] %>% 
        basename() %>% gsub('.RDS$', '', .) -> choices
      updateSelectInput(
        session = session, 
        inputId = 'admin_list_tidy_files',
        choices = choices, 
        selected = choices
      )
      updateTextInput(session = session, inputId = 'check', value = '')
    }
  })
  
  # eliminar
  observeEvent(input$delete_dataset, {
    if (input$dataset_check != 'eliminar'){
      showNotification("Estas seguro? Es irreversible.  Escribe: 'eliminar'", duration = 10, type='error')
    }else{
      input$admin_dataset_list %>% 
        paste0(., '.RDS') %>% 
        file.path(cnf$preprocess$output_path, .) %>% 
        file.remove()
      showNotification('Dataset Eliminado')
      cnf$preprocess$output_path %>% 
        list.files() %>% 
        .[grepl('^Dataset', .)] %>% 
        gsub('.RDS$', '', .) %>% 
        unique() %>% 
        sort(T) -> choices
      updateSelectInput(
        session = session, 
        inputId = 'admin_dataset_list',
        choices = choices, 
        selected = head(choices, 1)
      )
      updateTextInput(session = session, inputId = 'dataset_check', value = '')
    }
  })
  

  
  # eliminar
  observeEvent(input$create, {
    if (input$check != 'procesar'){
      showNotification("Estas seguro? Escribe: 'procesar'", duration = 10, type='error')
    }else{
      input$admin_list_tidy_files %>% 
        paste0('.RDS') %>% 
        file.path(cnf$tidy$output_path, .) %>% 
        preprocess_paths_safe(., cnf) -> res
      if(!is.null(res$error)){
        showNotification(as.character(res$error), duration = 10, type='error')
      }else{
        showNotification('Nuevo dataset añadido')
        cnf$preprocess$output_path %>% 
          list.files() %>% 
          .[grepl('^Dataset', .)] %>% 
          gsub('.RDS$', '', .) %>% 
          unique() %>% 
          sort(T) -> choices
        updateSelectInput(
          session = session, 
          inputId = 'admin_dataset_list',
          choices = choices, 
          selected = head(choices,1)
        )
        
        updateTextInput(session = session, inputId = 'check', value = '')
  
        
      }
      
    }
  }
)
    

  
  
 
  
}