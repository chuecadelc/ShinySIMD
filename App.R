###LIBRARIES REQUIRED ####
library(haven)
library(Hmisc)
library(ggplot2)
library(gridExtra)
library(grid)
library(data.table)
library(shiny)
library(rsconnect)
library(shinythemes)
library(RColorBrewer)
library(ggpubr)
library(png)
library(shinydashboard)
library(leaflet)
library(scales)
library(lattice)
library(plyr)
library(dplyr)
library(magrittr)
library(rgdal)

#####FOR THE DATASET######
data <-read.csv("data_simd.csv") #the names are not the same as in the GEOJSON file

data2<-data[complete.cases(data),]

dataGlas <- read.csv("data_simdGla.csv")
dataGlas$Population <- dataGlas$ï..Population

ColAttr <- function(x, attrC, ifIsNull) {
  # Returns column attribute named in attrC, if present, else isNullC.
  atr <- attr(x, attrC, exact = TRUE)
  atr <- if (is.null(atr)) {ifIsNull} else {atr}
  atr
}
AtribLst <- function(df, attrC, isNullC){
  # Returns list of values of the col attribute attrC, if present, else isNullC
  lapply(df, ColAttr, attrC=attrC, ifIsNull=isNullC)
}
variables <- AtribLst(data, attrC="labels", isNullC=NA)

countriesGla <- readOGR("json/SG_SIMD_2016_3.geojson", "SG_SIMD_2016_2", stringsAsFactors = F)
na.omit(countriesGla)

#######SHINY APP CODE BELOW#####

# Define USER INTERFACE for application 
ui <- #website design  
  dashboardPage(skin= "purple",
                #),
                dashboardHeader(title ="Scottish Index of Multiple Deprivation",titleWidth = 550),
                # Application title
                #headerPanel("Scottish Index of Multiple Deprivation"),
                #br(),
                dashboardSidebar(width = 400,
                                 sidebarMenu(style = "font-size:20px",
                                   menuItem("Information", tabName = "Info"),
                                   menuItem("Summary Statistics and Distributions", tabName = "Stats"),
                                   menuItem("Comparison of Variables", tabName = "Comparison"),
                                   menuItem("Interactive Map", tabName = "map"))
                ),
                
                dashboardBody(
                  tags$head(
                    
                    tags$style(HTML("
                                    @import url('https://fonts.googleapis.com/css?family=Neucha|Cabin+Sketch');
                                    
                                    .main-header .logo {
                                    font-family: 'Neucha', cursive;
                                    font-size: 32px;
                                    color: #212539;
                                    }
                                    
                                    ")),
                    
                    tags$style(HTML("
                                    @import url('https://fonts.googleapis.com/css?family=Neucha|Cabin+Sketch');
                                    .selectize-input {
                                    font-family: 'Times New Roman', Times, serif;
                                    font-size: 32px;
                                    color: #212539;
                                    }
                                    
                                    "))
                    
                    
                    ),
                  
                  tabItems(
                    tabItem(tabName = "Info",
                            tags$img(src = "qstep2.png", height = 110, width = 220), 
                            tags$img(src = "university.png.png", height = 110, width = 250),
                            br(),
                            br(),
                            
                            p("This app will be using the Scottish Index of Multiple Deprivation to showcase measures of central tendency, statistical tests such as correlation.
                              In terms of visualisations it haves scatterplots, histograms, and an interactive map of Glasgow.",style = "font-size:32px"),
                            br(),
                            p("The aim of this app is to explain the different relationships between the variables in such index, obtained from the Scottish Government Data Center.",style = "font-size:32px"),
                            br(),
                            p("Developed by Cristina Chueca, Q-Step Graduate of the University of Glasgow 2018, 
                              in collaboration with Dr. Brian Fogarty and Dr. Niccole Pamphilis.",style = "font-size:32px"),
                            br()
                            ),
                    #tabsetPanel(
                    
                    tabItem(tabName = "Stats", 
                            fluidRow(
                              #column(6,
                              
                              box(width = 3,
                                  sidebarPanel(selectInput("cov3", "Variables:", choices = names(data2), selected = "Population"),width = 11)),
                              
                              box(width = 2, selectInput(inputId = "bincolor",
                                                         label = "Select a Color",
                                                         choices = colors(),
                                                         selected = "grey"),
                                  
                                  checkboxInput(inputId = "addmean",
                                                label = "Add the mean line?",
                                                value = FALSE)),
                              
                              box(width= 7, sliderInput(inputId = "bins",
                                                        label = "Number of bins:",
                                                        min = 1,
                                                        max = 50,
                                                        value = 30))),
                            
                            
                            fluidRow(
                              box(width= 5,
                                  verbatimTextOutput("table"),verbatimTextOutput("table1")), 
                              box(width=7,plotOutput("distPlot",height = 550, width = 800)))
                            
                            #)#)
                    ),
                    
                    tabItem(tabName = "Comparison", fluid = TRUE,
                            
                            fluidRow(
                              
                              box(width=5, selectInput("cov1", "X-Axis", choices = names(data2)), selectInput("cov2", "Y-Axis", choices = names(data2)), 
                                  verbatimTextOutput("Explanation1"), verbatimTextOutput("Explanation2")
                                  ,
                                  checkboxInput(inputId = "gla",
                                                label = "What about only in Glasgow?",
                                                value = FALSE)),
                              
                              box(selectInput(inputId = "bincolor1",
                                              label = "Color",
                                              choices = colors(),
                                              selected = "grey"),
                                  
                                  checkboxInput(inputId = "addcor",
                                                label = "Add Pearson's correlation to the graph?",
                                                value = FALSE)),
                              
                              
                              box(width= 7,
                                  mainPanel(title = "Scatterplot", status = "primary",solidHeader = TRUE, plotOutput("compPlot", height = 550, width = 800)))
                              
                              
                            )
                            
                            
                    ),
                    
                    tabItem(tabName = "map", 
                            
                            fluidRow( width = 6,
                                      
                                      tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                                      
                                      
                                      sidebarPanel(
                                        selectInput("cov4", "Variables:", choices = names(data2)), selected = "Population",
                                      
                                      
                                        selectInput("colors", "Color Scheme",
                                                   rownames(subset(brewer.pal.info, category %in% c("seq", "div"))))),          
                                
                                        leafletOutput("my_map", width = "100%", height = 600))
                            
                            )
                    ))
)

##### SERVER CODE FOR THE APP####

server <- function(input, output, session) {
  
  selectedData<-reactive({
    as.numeric(unlist(data2[,input$cov3]))
  })
  
  output$table <- renderPrint({
    summary(data2[,input$cov3])
  })
  
  output$table1 <- renderText({
    paste('Standard Deviation=', format(sd(data2[,input$cov3]), digits = 4, justify = "none"))
  })
  
  ###### HISTOGRAM OUTPUT CODE #####  
  output$distPlot <- renderPlot({
    bins <- seq(min(data2[,input$cov3]), max(data2[,input$cov3]), length.out = input$bins + 1)
    
    if (input$addmean == "TRUE") {
      ggplot(data2, aes_string(input$cov3))+
        geom_histogram(fill= input$bincolor, breaks= bins) +
        ggtitle("Histogram")+
        geom_density(adjust=1) +
        geom_vline(xintercept = mean(data2[,input$cov3]),   
                   lwd = 2, 
                   lty = 2)+
        theme(axis.text=element_text(size=12, face = "bold"),axis.title=element_text(size=14,face="bold")) +
        theme(plot.title = element_text(size=22))
      
      
    } else if (input$addmean == "FALSE") {
      ggplot(data2, aes_string(input$cov3))+
        geom_histogram(fill= input$bincolor, breaks= bins) +
        ggtitle("Histogram")+
        #geom_density(adjust=1) +
        theme(axis.text=element_text(size=12, face = "bold"),axis.title=element_text(size=14,face="bold")) +
        theme(plot.title = element_text(size=22))
      
    }
  })
  
  #MEASURES OF CENTRAL TENDENCY OUTPUT CODE ####
  output$Explanation<-renderPlot({
    id<-which(names(data2) == input$cov3)
    if(!is.na(variables[id])){
      grid.draw(tableGrob(data.frame(variables[id])))}
  })
  
  output$Explanation1<-renderPrint({
    variables <- AtribLst(data2, attrC="labels", isNullC=NA)
    id<-which(names(data2) == input$cov1)
    if(!is.na(variables[id])){
      data.frame(variables[id])}
  })
  
  output$Explanation2<-renderPrint({
    variables <- AtribLst(data2, attrC="labels", isNullC=NA)
    id<-which(names(data2) == input$cov2)
    if(!is.na(variables[id])){
      data.frame(variables[id])}
  })
  
  output$Explanation3<-renderPrint({
    variables <- AtribLst(data2, attrC="labels", isNullC=NA)
    id<-which(names(data2) == input$cov3)
    if(!is.na(variables[id])){
      data.frame(variables[id])}
  })
  
  #####SCATTERPLOT OUTPUT CODE###
  
  output$compPlot<-renderPlot({
    
    if (input$gla == "TRUE" && input$addcor == "TRUE") 
     
      { 
      ggplot(dataGlas, aes_string(x = input$cov1, y = input$cov2)) +
        geom_point(alpha=1/5,position="jitter",size=3,colour = input$bincolor1) +
        geom_smooth(method="lm",se=FALSE,color = "black") +
        ggtitle("Scatterplot") +
        stat_cor(method = "pearson", label.x.npc = 0.71, label.y.npc = "top", size=6) +
        scale_y_continuous(limits=c(0,max(data2[,input$cov2]))) +
        theme(axis.text=element_text(size=12, face = "bold"),axis.title=element_text(size=14,face="bold")) +
        theme(plot.title = element_text(size=22))
      }
   
    else if  (input$addcor == "TRUE" && input$gla == "FALSE") 
      
      { 
        ggplot(data2, aes_string(x = input$cov1, y = input$cov2)) +
        geom_point(alpha=1/5,position="jitter",size=3,colour = input$bincolor1) +
        geom_smooth(method="lm",se=FALSE,color = "black") +
        ggtitle("Scatterplot") +
        stat_cor(method = "pearson", label.x.npc = 0.71, label.y.npc = "top", size=6) +
        scale_y_continuous(limits=c(0,max(data2[,input$cov2]))) +
        theme(axis.text=element_text(size=12, face = "bold"),axis.title=element_text(size=14,face="bold")) +
        theme(plot.title = element_text(size=22)) 
      
    }
    
    else if (input$gla == "TRUE" && input$addcor == "FALSE") 
      
      { 
      
      ggplot(dataGlas, aes_string(x = input$cov1, y = input$cov2)) +
        geom_point(alpha=1/5,position="jitter",size=3, colour = input$bincolor1) +
        geom_smooth(method="lm",se=FALSE, color = "black") +
        ggtitle("Scatterplot") +
        scale_y_continuous(limits=c(0,max(data2[,input$cov2]))) +
        theme(axis.text=element_text(size=12, face = "bold"),axis.title=element_text(size=14,face="bold")) +
        theme(plot.title = element_text(size=22))
      }
      
    
 
    else if (input$gla == "FALSE" && input$addcor == "FALSE") 
    
    {
      ggplot(data2, aes_string(x = input$cov1, y = input$cov2)) +
      geom_point(alpha=1/5,position="jitter",size=3,colour = input$bincolor1) +
      geom_smooth(method="lm",se=FALSE,color = "black") +
      ggtitle("Scatterplot") +
      scale_y_continuous(limits=c(0,max(data2[,input$cov2]))) +
      theme(axis.text=element_text(size=12, face = "bold"),axis.title=element_text(size=14,face="bold")) +
      theme(plot.title = element_text(size=22))
    }
 
    
  })
 
  
  #


#One improvement would be to be able to translate the datazones into groups like Maryhill, Southside, City Centre 
#adding an extra layer instead all together it separates by location. I´ll see if that could be done. 
#first get a glasgow city council map. I could do it through Wards or districts. Perhaps I could have a button to choose?


output$my_map <- renderLeaflet({
  
  if(input$cov4=="Population"){x <- countriesGla$Population}
  if(input$cov4=="Working_Population"){x <- countriesGla$Working_Po}
  if(input$cov4=="School_Attendance"){x <- countriesGla$School_Att}
  if(input$cov4=="Alcohol"){x <- countriesGla$Alcohol}
  if(input$cov4=="Income_Deprived"){x <- countriesGla$Income_Dep}
  if(input$cov4=="Employment_Deprived"){x <- countriesGla$Employment}
  if(input$cov4=="Illness"){x <- countriesGla$Illness}
  if(input$cov4=="Mortality"){x <- countriesGla$Mortality}
  if(input$cov4=="Drugs"){x <- countriesGla$Drugs}
  if(input$cov4=="Depress"){x <- countriesGla$Depress}
  if(input$cov4=="lowBW"){x <- countriesGla$lowBW}
  if(input$cov4=="HospEmer"){x <- countriesGla$HospEmer}
  if(input$cov4=="NoQuals"){x <- countriesGla$NoQuals}
  if(input$cov4=="Crime"){x <- countriesGla$Crime}
  if(input$cov4=="No_heating"){x <- countriesGla$No_heating}
  
  colorpal <- reactive({
    colorNumeric(input$colors,x)
  })
  
  labels <- countriesGla$DataZone
  
  
  pal <- colorpal()
  # palette1 <- colorNumeric("Spectral",
  #                          domain = countriesGla,input$cov4)
  
  
  leaflet(countriesGla)%>%
    addProviderTiles("Esri.WorldGrayCanvas", 
                     options = tileOptions(minZoom=6, maxZoom=16)) %>% #"freeze" the mapwindow to max and min zoomlevel
    addProviderTiles("Stamen.Toner", group = "Toner") %>%
    addProviderTiles("Esri.WorldImagery", group = "Toner Lite") %>%
    addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
    
    addPolygons(smoothFactor = 0.2, 
                fillColor = ~pal(x),  
                fillOpacity = 0.8,  
                color = "lightblue",    
                weight = 1.5,
                highlight = highlightOptions(
                  weight = 5,
                  color = "#666",
                  fillOpacity = 0,
                  bringToFront = FALSE),
                
                label = labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textOnly = TRUE,
                  textsize = "15px",
                  direction = "auto")) %>%
    
    
    addLegend("bottomright", pal = pal, values = ~x,
              title = "Density Levels",
              labFormat = labelFormat(suffix = " "),
              opacity = 0.75)%>%
    
    addLayersControl(baseGroups = c("Toner Lite", "CartoDB","Toner", "hex"))
  
  
})


}

# Run the application and enjoy!
shinyApp(ui = ui, server = server)
