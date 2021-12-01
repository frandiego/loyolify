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
  reactive_title <- reactive({
    HTML(set_title(schools = input$school, 
                   courses = input$course, 
                   groups = LETTERS[1:length(input$group)], 
                   variables = input$variable
                   ))
  })
  output$title <- renderUI({reactive_title()})
  
  # plot
  reactive_plot <- reactive({ plot(df=data(), cnf = cnf, filter = filter()) })
  output$plot <- renderHighchart({reactive_plot()})
  
  
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
                                      selected = choices
                 )
               })
  
  # update group
  observeEvent(eventExpr = input$course, 
               handlerExpr = {
                 data() %>% .[course %in% input$course] %>% 
                   .[['group']] %>% unique() -> choices
                 names(choices) <- LETTERS[1:length(choices)]
                 updateSelectizeInput(session = session, 
                                      inputId = 'group', 
                                      choices = choices, 
                                      selected = choices
                 )
               })
  
  # update sections
  observeEvent(eventExpr = there_is_data, 
               handlerExpr = {
                 data() %>% .[['section']] %>% unique() -> choices
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
                 updateSelectizeInput(session = session, 
                                      inputId = 'variable', 
                                      choices = choices, 
                                      selected = choices
                 )
               })
  

  
}