
# Use shiny to make 

new.shiny.ex=function() {
  library(shiny)
  library(ggplot2)
  
  ##### UI #####
  
  ui <- fluidPage(
    titlePanel("Fantasy Football"),
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      # Sidebar panel for inputs ----
      sidebarPanel(
        # Input: Selector for choosing dataset ----
        selectInput(inputId = "getPlot",
                    label = "Choose a dataset:",
                    choices = c("Means", "QB", "RB", "WR", "TE"))
      ),
      
      # sidebarPanel(
      #   # Input: Selector for choosing dataset ----
      #   selectInput(inputId = "getColor",
      #               label = "Choose a color:",
      #               choices = c("Red", "Blue", "Green"))
      # ),
      
      # Main panel for displaying outputs ----
      mainPanel(
        # Output: HTML table with requested number of observations ----
        plotOutput("view")
      )
    )
  )
  
  server <- function(input, output) {
    
    myPlot <- reactive({
      switch(input$getPlot,
             "Means" = plotMean,
             "QB" = plotQB,
             "RB" = plotRB,
             "WR" = plotWR,
             "TE" = plotTE)
    })
    
    # myColors <- reactive({
    #   switch(input$getColor,
    #          "Red" = "red",
    #          "Blue" = "blue",
    #          "Green" = "green")
    # })

    output$view <- renderPlot({
      # datasetInput
      myPlot()
    })
  }
  
  # actually create a shiny app using the ui and server
  shinyApp(ui, server)
}

new.shiny.ex()







