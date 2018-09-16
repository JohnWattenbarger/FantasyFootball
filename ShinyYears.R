
toNumeric <- function(data)
{
  data <- sapply(data, as.numeric)
  return(data)
}

new.shiny.ex=function() {
  library(shiny)
  library(ggplot2)
  library(stringr)
  
  # name of the data tables you can choose from
  dataTables = c("QB", "RB", "WR", "TE")
  yearChoices = c("All", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016","2017")
  
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
        ),
        # create a panel to choose the year
        selectInput(inputId = "getYear",
                    label = "Choose a year:",
                    choices = yearChoices
        )
      ),
      
      mainPanel(
        # title
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
  
  ######################################################################################
  ##################################    Server    ######################################
  ######################################################################################
  
  server <- function(input, output) {
    
    # chooses a data table based on input (my data tables are called QB, RB, WR, and TE)
    myData <- reactive({
      switch(input$getData,
             "QB" = QB,
             "RB" = RB,
             "WR" = WR,
             "TE" = TE)
    })
    
    # Use condensed data to only display a few desireable columns
    myData.condensed <- reactive({
      switch(input$getData,
             "QB" = QB.condensed,
             "RB" = RB.condensed,
             "WR" = WR.condensed,
             "TE" = TE.condensed)
    })
    
    # UNUSED! Creates a table with only the selected year included
    myTable <- reactive({
      switch(input$getYear,
             "All" = 
               myData(),
             switch(input$getData,
                    "QB" = QB[QB$Year %in% toNumeric(input$getYear),],
                    "RB" = RB[RB$Year %in% toNumeric(input$getYear),],
                    "WR" = WR[WR$Year %in% toNumeric(input$getYear),],
                    "TE" = TE[TE$Year %in% toNumeric(input$getYear),]
             )
      )
    })
    
    # Create the plot to display
    yearPlot <- reactive({
      switch(input$getYear,
             # If All is selected, create a facet graph with all years included
             "All" = 
               switch(input$getData,
                      "QB" = ggplot(QB, aes_string(x=input$getVariable1, y='Fantasy_Points')) +
                        geom_point() + facet_wrap(~Year),
                      "RB" = ggplot(RB, aes_string(x=input$getVariable1, y='Fantasy_Points')) +
                        geom_point() + facet_wrap(~Year),
                      "WR" = ggplot(WR, aes_string(x=input$getVariable1, y='Fantasy_Points')) +
                        geom_point() + facet_wrap(~Year),
                      "TE" = ggplot(TE, aes_string(x=input$getVariable1, y='Fantasy_Points')) +
                        geom_point() + facet_wrap(~Year)
               )
             ,
             # If All is not selected, only graph the selected year
             {switch(input$getData,
                     "QB" = ggplot(QB[QB$Year %in% toNumeric(input$getYear),], aes_string(x=input$getVariable1, y='Fantasy_Points')) +
                       geom_point(),
                     "RB" = ggplot(RB[RB$Year %in% toNumeric(input$getYear),], aes_string(x=input$getVariable1, y='Fantasy_Points')) +
                       geom_point(),
                     "WR" = ggplot(WR[WR$Year %in% toNumeric(input$getYear),], aes_string(x=input$getVariable1, y='Fantasy_Points')) +
                       geom_point(),
                     "TE" = ggplot(TE[TE$Year %in% toNumeric(input$getYear),], aes_string(x=input$getVariable1, y='Fantasy_Points')) +
                       geom_point()
             )}
             )
    })
    # displays column names as X Variable choices
    output$variable1 <- renderUI({
      myNames = names(myData.condensed())
      selectInput(inputId = "getVariable1",
                  label = "X Variable",
                  choices = myNames,
                  selected = NULL)
    })
    
    # create a graph based on user input
    output$plots <- renderPlot({
        yearPlot() 
      })
    
    # displays a table (based on what was selected above)
    output$myTable <- renderDataTable(
      myData(), options = list(order=list(ncol(myTable())-1, 'desc'), pageLength=10)
      # Note: switch from myData() to myTable() to make the table only include the current year
    )
  }
  
  shinyApp(ui, server)
}

# runs everything
new.shiny.ex()







