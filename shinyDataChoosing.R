
# make tables with only some data

# QB.condensed <- QB[, c(5, 6, 8, 11, 12, 16, 17, 21)]
# RB.condensed <- RB[, c(6, 7, 10, 11, 14:18)]
# WR.condensed <- WR[, c(5:9, 12, 14)]
# TE.condensed <- TE[, c(5:9, 12, 14)]
# 
# View(TE.condensed)


# Use shiny to make 

new.shiny.ex=function() {
  library(shiny)
  library(ggplot2)
  
  dataTables = c("QB", "RB", "WR", "TE")
  
  ui <- fluidPage(
    titlePanel("Select data and variables"),
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "getData",
                    label = "Choose a dataset:",
                    choices = dataTables
                    ),
        fluidRow(
          column(7, uiOutput("variable1"))
        )
      ),
      
      mainPanel(
        plotOutput("plots")
      )
    )
  )
  
  server <- function(input, output) {
    
    myData <- reactive({
      switch(input$getData,
             "QB" = QB,
             "RB" = RB,
             "WR" = WR,
             "TE" = TE)
    })
    
    myData.condensed <- reactive({
      switch(input$getData,
             "QB" = QB.condensed,
             "RB" = RB.condensed,
             "WR" = WR.condensed,
             "TE" = TE.condensed)
    })
    
    output$variable1 <- renderUI({
      myNames = names(myData.condensed())
      selectInput(inputId = "getVariable1",
                  label = "X Variable",
                  choices = myNames,
                  selected = NULL)
    })
    
    output$plots <- renderPlot({
      ggplot(myData(), aes_string(x=input$getVariable1, y='Fantasy_Points')) +
        geom_point() +
        facet_wrap(~Year)
    })
  }
  
  shinyApp(ui, server)
}

new.shiny.ex()







