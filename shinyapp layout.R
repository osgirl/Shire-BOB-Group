

library(shiny)

# Define UI for application that draws a histogram
ui <- navbarPage( 
  "Shire",
                  tabPanel("New_Patient",
                                      sidebarLayout(
                                        sidebarPanel(
                                          selectizeInput("group", label="Group:",
                                                         choices=c("6451  AARP MedicareComplete Plus"),
                                                         multiple=F,options = list(create = TRUE)),
                                          selectizeInput("bob",label = "BOB:",
                                                         choices = c("name of BOB"),
                                                         multiple=F, options = list(create = TRUE)),
                                          selectizeInput("graph",label = "Graph:",
                                                         choices = c("Date","Geographic"),
                                                         multiple = F, options = list(create = TRUE))
                                       ),
                           
                           mainPanel(
                             "mainPanel"
                           )
                           )),
                           

  
  
                  tabPanel("Switch_Patient",
                           sidebarLayout(
                             sidebarPanel(
                               selectizeInput("group", label="Group:",
                                              choices=c("6451  AARP MedicareComplete Plus"),
                                              multiple=F,options = list(create = TRUE)),
                               selectizeInput("bob",label = "BOB:",
                                              choices = c("name of BOB"),
                                              multiple=F, options = list(create = TRUE)),
                               selectizeInput("graph",label = "Graph:",
                                              choices = c("Date","Geographic"),
                                              multiple = F, options = list(create = TRUE))
                             ),
                             mainPanel(
                               "mainPanel"
                             )
                           )
))


  
server <- function(input, output) {
  
}

shinyApp(ui = ui, server = server)

