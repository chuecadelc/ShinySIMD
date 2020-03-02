### Shiny App ###

## Load the required libraries
library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)

## Load your data
data <- read.csv("data_simdGla.csv")
data$Population <- data$ï..Population
data$ï..Population <- NULL

## User Interface
ui <- 
  dashboardPage(skin="blue",
                dashboardHeader(title ="My first Shiny App",titleWidth = 550),
                
                dashboardSidebar(width = 400,
                                 sidebarMenu(style = "font-size:20px",
                                             menuItem("", tabName = "Info"))),
                dashboardBody(
                  
                       fluidRow( 
                         
                              box(width=5,
                             
                                sliderInput("num", label = "Choose a number", value = 20, min =1, max= 50 ),
                        
                                #plotting the values from the sloder input in the form of a histogram
                                plotOutput("hist",height = 200, width = 400)),
                       
            
                             box(width = 7,
                                 
                                #to select a variable from the imported dataset
                                selectInput("vars1", "X-Axis", choices = names(data), selected = "Population"), 
                                selectInput("vars2", "Y-Axis", choices = names(data)),
                       
                                #plotting the values from the variables into a scatterplot.
                                #specifying the size of the plot
                                plotOutput("scatt", height = 550, width = 800)
                            
                               ) 
                             )
    
                          )
            )
  
  
  
server <- function(input, output, session) {
  
  output$hist <- renderPlot({
    
    #Creates a histogram with the values from the slider
    
    tittle <- "Histogram of a random distribution"
    
    hist(rnorm(input$num), main= tittle)
    
})

  output$scatt <- renderPlot({
    
    #Connecting the inputs selected (variables from the dataset) to the outplot plot
      ggplot(data=data, aes_string(x=input$vars1, y=input$vars2)) +
      
        #specify which graph type, here a scatterplot
        geom_point(alpha=1/5, position = "jitter", color = "black") +
      
        #to add the best fit line
        geom_smooth(method= "lm", se= FALSE, color = "black") +
        ggtitle("Scatterplot")
    
  })
  
} 

shinyApp(ui=ui, server=server)
  
  
  