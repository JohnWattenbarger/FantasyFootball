

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
        # used to choose the x variable
        fluidRow(
          column(7, uiOutput("variableColor"))
        ),
        # create a panel to choose the year
        selectInput(inputId = "getYear",
                    label = "Choose a year:",
                    choices = yearChoices
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
    
    myData2 <- reactive({
      switch(input$getData,
             "QB" = QB[QB$Year %in% yearChoicesNumberic(),],
             "RB" = RB[RB$Year %in% yearChoicesNumberic(),],
             "WR" = WR[WR$Year %in% yearChoicesNumberic(),],
             "TE" = TE[TE$Year %in% yearChoicesNumberic(),]
      )
    })
    
    
    
    yearPlot <- reactive({
      switch(input$getYear,
             "All" = 
               switch(input$getData,
                      "QB" = ggplot(QB, aes_string(x=input$getVariable1, y='Fantasy_Points')) +
                        geom_point(aes(col=QB$P_TDs)) +
                        scale_color_gradientn(colors=c("blue", "light blue", "green", "gold", "orange", "red")) + 
                        facet_wrap(~Year),
                      "RB" = ggplot(RB, aes_string(x=input$getVariable1, y='Fantasy_Points')) +
                        geom_point(aes(col=RB$Ru_TDs)) +
                        scale_color_gradientn(colors=c("blue", "light blue", "green", "gold", "orange", "red")) + 
                        facet_wrap(~Year),
                      "WR" = ggplot(WR, aes_string(x=input$getVariable1, y='Fantasy_Points')) +
                        geom_point(aes(col=WR$TD)) +
                        scale_color_gradientn(colors=c("blue", "light blue", "green", "gold", "orange", "red")) + 
                        facet_wrap(~Year),
                      "TE" = ggplot(TE, aes_string(x=input$getVariable1, y='Fantasy_Points')) +
                        geom_point(aes(col=TE$TD)) +
                        scale_color_gradientn(colors=c("blue", "light blue", "green", "gold", "orange", "red")) + 
                        facet_wrap(~Year)
               )
             ,
             
             {switch(input$getData,
                     "QB" = ggplot(QB[QB$Year %in% toNumeric(input$getYear),], aes_string(x=input$getVariable1, y='Fantasy_Points')) +
                       geom_point(aes(col=QB[QB$Year %in% toNumeric(input$getYear),]$P_TDs)) +
                       scale_color_gradientn(colors=c("blue", "light blue", "green", "gold", "orange", "red")) +
                       guides(color=guide_legend(title="Passing TDs")),
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
    
    # displays column names as Variables for color choices
    output$variableColor <- renderUI({
      myNames = names(myData.condensed())
      selectInput(inputId = "getColor",
                  label = "Color points based on:",
                  choices = myNames,
                  selected = NULL)
    })
    
    # create a graph based on user input
    output$plots <- renderPlot({
      yearPlot() +
        theme(plot.title = element_text(size=30, hjust = 0.5), 
              panel.background = element_rect(fill = 'black', colour = 'white'), 
              plot.background = element_rect(fill=plotBackgroundColor), 
              strip.background = element_rect(colour = "white", fill = stripColor))
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







