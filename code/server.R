server <- function(input, output, session) {
  
  # read data
  there_is_data = F
  data <- reactive({dt = read_data(cnf); there_is_data=T; dt %>% .[]})
  
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
                 data() %>% .[['school']] %>% unique() -> choices
                 updateSelectizeInput(session = session, 
                                      inputId = 'school', 
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
                   .[['group']] %>% unique() %>% sort()-> choices
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
                 data() %>% .[['section']] %>% unique() -> choices
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
                   .[['variable']] %>% unique() -> choices
                 
                 names(choices) <- map_chr(choices, set_main_title)
                 
                 updateSelectizeInput(session = session, 
                                      inputId = 'variable', 
                                      choices = choices, 
                                      selected = head(choices, 1)
                 )
               })
  

  
}