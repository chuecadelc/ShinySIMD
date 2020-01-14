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
library(mapview)
library(htmltools)
library(htmlwidgets)
library(maptools)


#####FOR THE DATASET######
data <-read.csv("data_simd.csv")

data$Population <- as.numeric(as.character(data$Population))
data$Working_Population <- as.numeric(as.character(data$Working_Population))
data$School_Attendance <- as.numeric(as.character(data$School_Attendance))
data$Alcohol <- as.numeric(as.character(data$Alcohol))
data$Income_Deprived <- as.numeric(as.character(data$Income_Deprived))
data$Employment_Deprived <- as.numeric(as.character(data$Employment_Deprived))
data$Illness <- as.numeric(as.character(data$Illness))
data$Mortality <- as.numeric(as.character(data$Mortality))
data$Drugs <- as.numeric(as.character(data$Drugs))
data$Depress <- as.numeric(as.character(data$Depress))
data$lowBW <- as.numeric(as.character(data$lowBW))
data$HospEmer <- as.numeric(as.character(data$HospEmer))
data$NoQuals <- as.numeric(as.character(data$NoQuals))
data$Crime <- as.numeric(as.character(data$Crime))
data$No_heating <- as.numeric(as.character(data$No_heating))
data$Glasgow<- as.numeric(data$Glasgow)

data2<-data[complete.cases(data),]

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

countriesGlaX <- readOGR("SG_SIMD_2016_1.geojson", "SG_SIMD_2016_1", stringsAsFactors = F)
countriesGla <- countriesGlaX[countriesGlaX$Council_ar=="Glasgow City", ]

countriesGla$Council_ar <- NULL
countriesGla$LAName <- NULL

countriesGla$Shape_Leng <- NULL
countriesGla$Shape_Area <-NULL

countriesGlaX1 <- countriesGla

countriesGlaX1$DataZone <- NULL



countriesGlaX1@data[complete.cases(countriesGlaX1@data),] 

countriesGlaX1$Population <- as.numeric(as.character(countriesGlaX1$Population))
countriesGlaX1$Working_Po <- as.numeric(as.character(countriesGlaX1$Working_Po))
countriesGlaX1$School_Att <- as.numeric(as.character(countriesGlaX1$School_Att))
countriesGlaX1$Alcohol <- as.numeric(as.character(countriesGlaX1$Alcohol))
countriesGlaX1$Income_Dep <- as.numeric(as.character(countriesGlaX1$Income_Dep))
countriesGlaX1$Employment <- as.numeric(as.character(countriesGlaX1$Employment))
countriesGlaX1$Illness <- as.numeric(as.character(countriesGlaX1$Illness))
countriesGlaX1$Mortality <- as.numeric(as.character(countriesGlaX1$Mortality))
countriesGlaX1$Drugs <- as.numeric(as.character(countriesGlaX1$Drugs))
countriesGlaX1$Depress <- as.numeric(as.character(countriesGlaX1$Depress))
countriesGlaX1$lowBW <- as.numeric(as.character(countriesGlaX1$lowBW))
countriesGlaX1$HospEmer <- as.numeric(as.character(countriesGlaX1$HospEmer))
countriesGlaX1$NoQuals <- as.numeric(as.character(countriesGlaX1$NoQuals))
countriesGlaX1$Crime <- as.numeric(as.character(countriesGlaX1$Crime))
countriesGlaX1$No_heating <- as.numeric(as.character(countriesGlaX1$No_heating))



#######SHINY APP CODE BELOW#####

# Define USER INTERFACE for application 
ui <- #website design  
  dashboardPage(skin= "purple",
                #),
                dashboardHeader(title ="Scottish Index of Multiple Deprivation",titleWidth = 550),
                # Application title
                #headerPanel("Scottish Index of Multiple Deprivation"),
                #br(),
                dashboardSidebar(width = 300,
                                 sidebarMenu(
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
                                    font-size: 22px;
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
                              In terms of visualisations it haves scatterplots, histograms, and an interactive map of Glasgow."),
                            br(),
                            p("The aim of this app is to explain the different relationships between the variables in such index, obtained from the Scottish Government Data Center."),
                            br(),
                            p("Developed by Cristina Chueca, Q-Step Graduate of the University of Glasgow 2018, 
                              in collaboration with Dr. Brian Fogarty and Dr. Niccole Pamphilis."),
                            br()
                    ),
                    #tabsetPanel(
                    
                    tabItem(tabName = "Stats", 
                            fluidRow(
                              #column(6,
                              
                              box(width = 3,
                                  sidebarPanel(selectInput("cov3", "Variables:", choices = names(data2), selected = "Population"),width = 11)),
                              
                              box(width = 2, selectInput(inputId = "bincolor",
                                                         label = "Select a Colour",
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
                              box(width=7,plotOutput("distPlot",height = 450, width = 600)))
                            
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
                                              label = "Colour of line",
                                              choices = colors(),
                                              selected = "grey"),
                                  
                                  checkboxInput(inputId = "addcor",
                                                label = "Add Pearson's correlation to the graph?",
                                                value = FALSE)),
                              
                              
                              box(width= 7,
                                  mainPanel(title = "Scatterplot", status = "primary",solidHeader = TRUE, plotOutput("compPlot", height = 500, width = 650)))
                              
                              
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
    data2$Glasgow2 <- mapvalues(data2$Glasgow, from = c("0", "1"),
                                to = c("0. Not Glasgow Council", "1. Glasgow Council"))
    
    
    if (input$gla == "TRUE" && input$addcor == "TRUE") 
      
    { 
      ggplot(data2, aes_string(x = input$cov1, y = input$cov2)) +
        geom_point(alpha=1/5,position="jitter",size=3, aes(colour=Glasgow2)) +
        geom_smooth(method="lm",se=FALSE, colour = input$bincolor1) +
        ggtitle("Scatterplot") +
        stat_cor(method = "pearson", label.x.npc = 0.51, label.y.npc = "top", size=6) +
        scale_y_continuous(limits=c(0,max(data2[,input$cov2]))) +
        theme(axis.text=element_text(size=12, face = "bold"),axis.title=element_text(size=14,face="bold")) +
        theme(plot.title = element_text(size=22))
    }
    
    else if  (input$addcor == "TRUE" && input$gla == "FALSE") 
      
    { 
      ggplot(data2, aes_string(x = input$cov1, y = input$cov2)) +
        geom_point(alpha=1/5,position="jitter",size=3) +
        geom_smooth(method="lm",se=FALSE, colour = input$bincolor1) +
        ggtitle("Scatterplot") +
        stat_cor(method = "pearson", label.x.npc = 0.51, label.y.npc = "top", size=6) +
        scale_y_continuous(limits=c(0,max(data2[,input$cov2]))) +
        theme(axis.text=element_text(size=12, face = "bold"),axis.title=element_text(size=14,face="bold")) +
        theme(plot.title = element_text(size=22)) 
      
    }
    
    else if (input$gla == "TRUE" && input$addcor == "FALSE") 
      
    { 
      ggplot(data2, aes_string(x = input$cov1, y = input$cov2)) +
        geom_point(alpha=1/5,position="jitter",size=4, aes(colour=Glasgow2)) +
        geom_smooth(method="lm",se=FALSE, colour = input$bincolor1) +
        ggtitle("Scatterplot") +
        #stat_cor(method = "pearson", label.x.npc = 0.81, label.y.npc = "top", size=6) +
        scale_y_continuous(limits=c(0,max(data2[,input$cov2]))) +
        theme(axis.text=element_text(size=12, face = "bold"),axis.title=element_text(size=14,face="bold")) +
        theme(plot.title = element_text(size=22)) 
    }
    
    
    
    else if (input$gla == "FALSE" && input$addcor == "FALSE") 
      
    {
      ggplot(data2, aes_string(x = input$cov1, y = input$cov2)) +
        geom_point(alpha=1/5,position="jitter",size=3) +
        geom_smooth(method="lm",se=FALSE, colour = input$bincolor1) +
        ggtitle("Scatterplot") +
        scale_y_continuous(limits=c(0,max(data2[,input$cov2]))) +
        theme(axis.text=element_text(size=12, face = "bold"),axis.title=element_text(size=14,face="bold")) +
        theme(plot.title = element_text(size=22))
    }
    
    
  })
  
  ######MAP CODE
  
  output$my_map <- renderLeaflet({
    
    if(input$cov4=="Population"){x <- countriesGlaX1$Population}
    if(input$cov4=="Working_Population"){x <- countriesGlaX1$Working_Po}
    if(input$cov4=="School_Attendance"){x <- countriesGlaX1$School_Att}
    if(input$cov4=="Alcohol"){x <- countriesGlaX1$Alcohol}
    if(input$cov4=="Income_Deprived"){x <- countriesGlaX1$Income_Dep}
    if(input$cov4=="Employment_Deprived"){x <- countriesGlaX1$Employment}
    if(input$cov4=="Illness"){x <- countriesGlaX1$Illness}
    if(input$cov4=="Mortality"){x <- countriesGlaX1$Mortality}
    if(input$cov4=="Drugs"){x <- countriesGlaX1$Drugs}
    if(input$cov4=="Depress"){x <- countriesGlaX1$Depress}
    if(input$cov4=="lowBW"){x <- countriesGlaX1$lowBW}
    if(input$cov4=="HospEmer"){x <- countriesGlaX1$HospEmer}
    if(input$cov4=="NoQuals"){x <- countriesGlaX1$NoQuals}
    if(input$cov4=="Crime"){x <- countriesGlaX1$Crime}
    if(input$cov4=="No_heating"){x <- countriesGlaX1$No_heating}
    
    colorpal <- reactive({
      colorNumeric(input$colors,x)
    })
    
    labels <- countriesGla$DataZone
    
    
    pal <- colorpal()
    # palette1 <- colorNumeric("Spectral",
    #                          domain = countriesGla,input$cov4)
    
    
    leaflet(countriesGlaX1)%>%
      addProviderTiles("Esri.WorldGrayCanvas", 
                       options = tileOptions(minZoom=6, maxZoom=16)) %>% #"freeze" the mapwindow to max and min zoomlevel
      addProviderTiles("Stamen.Toner", group = "Toner") %>%
      addProviderTiles("Esri.WorldImagery", group = "Toner Lite") %>%
      addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
    
      addPolygons(smoothFactor = 0.5, 
                  fillColor = ~pal(x),  
                  fillOpacity = 1.5,  
                  color = "lightblue",    
                  weight = 2,
                  #this is when you hover over with the mouse so you can see underneath the main color.
                   highlight = highlightOptions(
                                weight = 5,
                                color = "#666",
                                fillOpacity = 0.5,
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
  #
}

# Run the application and enjoy!
shinyApp(ui = ui, server = server)
