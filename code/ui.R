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
    id = "sidebar"
  )
}


body <- function() {
  argonDashBody(
    argonTabItems(
      argonTabItem(
        tabName = 'test'
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