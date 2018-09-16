
# Use shiny to make 

new.shiny.ex=function() {
  library(shiny)
  library(ggplot2)
  
  # import dataset from other project
  
  ######################################################
  ######################################################
  #############
  #############   UI
  #############
  ######################################################
  ######################################################
  
  ui <-
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "example",
          label="example",
          choices=c("Scatterplot", "Histogram")
        )
      ),
    
    mainPanel(plotOutput("plot1"),
              plotOutput("plot2"))
    )
  
  
  ######################################################
  ######################################################
  #############
  #############   SERVER
  #############
  ######################################################
  ######################################################  
  
  server <- function(input,output) { 
    dataset=data.frame(cbind(rnorm(100),rnorm(100)))
    output$plot1 <- renderPlot({
      
      yourPlot
    })
    
  }
  
  server2 <- function(input,output) { 
    dataset=data.frame(cbind(rnorm(100),rnorm(100)))
    output$plot1 <- renderPlot({
      
      ggplot(meanFP, aes(Year, `Fantasy Points`, group=Position)) +
        geom_line(aes(col=meanFP$Position), size=1.5) +
        geom_point(aes(col=meanFP$Position), size=3) +
        scale_x_continuous("Year", labels = as.character(meanFP$Year), breaks = meanFP$Year) +
        guides(color=guide_legend(title="Position")) +
        ggtitle("Average Fantasy Points of the Top 32 Players") +
        theme(plot.title = element_text(hjust = 0.3)) +
        scale_color_discrete(breaks=c("QB","RB","WR", "TE"))
    })
    
  }
  
  shinyApp(ui, server2)
  
  #shinyApp(ui,server)
}

new.shiny.ex()







