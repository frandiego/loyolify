server <- function(input, output, session) {
  
  # read data
  there_is_data = F
  data <- reactive({dt = read_data(cnf); there_is_data=T; dt %>% .[]})
  
  # sections
  sections <- reactive({ data() %>% .[['section']] %>% unique() })
  
  # filter
  filter <- reactive({ list(variable = input$variable, 
                            school = input$school)
    })
  
  # plot
  reactive_plot <- reactive({ plot(df=data(), cnf = cnf, filter = filter()) })
  output$plot <- renderHighchart({reactive_plot()})
  
  
  # update schools
  observeEvent(eventExpr = there_is_data, 
               handlerExpr = {
                 choices =data() %>% .[['school']] %>% unique() -> choices
                 updateSelectizeInput(session = session, 
                                      inputId = 'school', 
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
                 choices = data() %>% .[section %in% input$section] %>% 
                   .[['variable']] %>% unique() -> choices
                 updateSelectizeInput(session = session, 
                                      inputId = 'variable', 
                                      choices = choices, 
                                      selected = choices
                 )
               })
  

  
}