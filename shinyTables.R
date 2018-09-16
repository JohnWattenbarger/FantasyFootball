

new.shiny.ex=function() {
  library(shiny)
  library(ggplot2)
  
  # name of the data tables you can choose from
  dataTables = c("QB", "RB", "WR", "TE")
  
  ui <- fluidPage(
    titlePanel("Select data and variables"),
    sidebarLayout(
      sidebarPanel(
        # create a panel to choose the data table
        selectInput(inputId = "getData",
                    label = "Choose a position:",
                    choices = dataTables
        ),
        # used to choose the x variable
        fluidRow(
          column(7, uiOutput("variable1"))
        )
      ),
      
      mainPanel(
        # random gibberish
        h1("Fantasy Football Data", align="center", 
           style="font-family: Arial; padding-bottom: 75px"),
        
        # the graph
        h2("Scatterplot", align="center"),
        plotOutput("plots"),
        
        # the table
        h2("Data table", align="center", style="padding-top: 25px"),
        dataTableOutput("myTable"),
        
        # Sources
        p("Sources", align="center", style="padding-top: 50px; font-weight: bold"),
        tagList(a("http://www.espn.com/nfl/statistics/player/_/stat/", href="http://www.espn.com/nfl/statistics/player/_/stat/", style="padding-bottom: 10px"))
      )
    )
  )
  
  server <- function(input, output) {
    
    # chooses a data table based on input (my data tables are called QB, RB, WR, and TE)
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
    
    # displays column names as X Variable choices
    output$variable1 <- renderUI({
      myNames = names(myData.condensed())
      selectInput(inputId = "getVariable1",
                  label = "X Variable",
                  choices = myNames,
                  selected = NULL)
    })
    
    output$plots <- renderPlot({
      ggplot(myData(), aes_string(x=input$getVariable1, y='Fantasy_Points')) +
        geom_point() + facet_wrap(~Year)
    })
    
    # displays a table (based on what was selected above)
    output$myTable <- renderDataTable(
      myData()
    )
  }
  
  shinyApp(ui, server)
}

# runs everything
new.shiny.ex()







