### SCOTTISH INDEX OF MULTIPLE DEPRIVATION 2016 SHINY APP ## 

###LIBRARIES REQUIRED ####

# general packages
library(tidyverse)
library(gridExtra)
library(grid)
library(data.table)
library(DT)
library(scales)
library(lattice)
library(magrittr)
library(stats)
library(ineq) # for Gini coeff

# data imports
library(readxl)
library(haven)
library(Hmisc)
# library(rgdal) # deprecated in 2023
library(sf)
library(sp)
library(raster)  


# shiny related
library(shiny)
library(shinyFeedback) # to show warnings
library(rsconnect)
library(shinythemes)
library(fresh) # improving appearance
library(bslib) # for nicer themes
library(RColorBrewer)
library(ggpubr)
library(png)
library(shinydashboard)
library(leaflet)



#####FOR THE DATASET######

data_2016 <-read_excel("SIMD16_Data.xlsx", sheet = "Data")
# removing unnecessary rank variables
data_2016 <- data_2016 %>% 
  dplyr::select(-c(Overal_SIMD16_Rank,
              SIMD_2016_Percentile,
              SIMD_2016_Vigintile,
              SIMD_2016_Decile,
              SIMD_2016_Quintile,             
              Income_rate,
              Employment_rate,
              Income_Domain_2016_Rank,
              Employment_Domain_2016_Rank,
              Health_Domain_2016_Rank,
              Education_Domain_2016_Rank,
              Geographic_Access_Domain_2016_Rank,
              Crime_Domain_2016_Rank,
              Housing_Domain_2016_Rank,
              overcrowded_rate,
              nocentralheat_rate)) %>% 
  rename(Working_Age_population = Working_age_population_Revised)


## using the 2016 one as the 2020 one is the same
data_vars <-read_excel("SIMD16_Data.xlsx", sheet = "Indicator descriptions",range = "B1:D37", 
                            col_types = c("text", "text", "text"),  
                            col_names = TRUE)

var_names <- c("Data Zone", "Intermediate zone", "Council Area", "Total population","Working Age population",
               "Count of Income deprived","Unemployment count","Comparative Illness Factor" ,"Alcohol-related hospitalisations",
               "Drug-related hospitalisations","Standardised mortality rate","Mental health prescription rates","Low birth weight rate",
               "Emergency hospitalisation rate","School pupil attendance","Attainment education of school leavers ","Working age of no qualification individuals",
               "NEET","HESA","Avg driving time to a petrol station","Avg driving time to a GP","Avg driving time to a post office",
               "Avg driving time to primary school","Avg driving time to retail store","Avg driving time to secondary school",
               "Avg public transport time to GP","Avg public transport time to post office","Avg public transport time to retail store",
               "Crime rate per 10,000 people","Overcrowded household count","Count of households w/ no central heating")

exclude_vars <- c("Income_rate","Employment_rate","crime_count","overcrowded_rate","nocentralheat_rate")

data_vars <- data_vars %>% 
  filter(!Column %in% exclude_vars) %>% 
  mutate(label = var_names)


data_2020 <- read_excel("SIMD20_Data.xlsx", sheet = "Data")

data_2020 <- data_2020 %>%
  dplyr::select(
    -c(SIMD2020v2_Rank,
      SIMD_2020v2_Percentile,
      SIMD2020v2_Vigintile,
      SIMD2020v2_Decile,
      SIMD2020v2_Quintile,
      income_rate,
      employment_rate,
      SIMD2020v2_Income_Domain_Rank,
      SIMD2020_Employment_Domain_Rank,
      SIMD2020_Health_Domain_Rank,
      SIMD2020_Education_Domain_Rank,
      SIMD2020_Access_Domain_Rank,
      SIMD2020_Crime_Domain_Rank,
      SIMD2020_Housing_Domain_Rank,
      overcrowded_rate,
      nocentralheating_rate)) %>% 
  ## changing var names here too so labels can later on match
  rename(Income_count = income_count, Employment_count = employment_count,
         Noquals = no_qualifications,HESA = University,
         NEET = not_participating, 
         nocentralheat_count = nocentralheating_count,
         drive_PO = drive_post,  
         PT_Post = PT_post )



## using the 2016 one as the 2020 one is the same
data_vars1 <-read_excel("SIMD20_Data.xlsx", sheet = "Indicator descriptions",range = "B1:D37", 
                       col_types = c("text", "text", "text"),  
                       col_names = TRUE)

# Using same labels as above plus two new ones for recorded crimes and broadband variables
var_names1 <- c("Data Zone", "Intermediate zone", "Council Area", "Total population","Working Age population",
               "Count of Income deprived","Unemployment count","Comparative Illness Factor" ,"Alcohol-related hospitalisations",
               "Drug-related hospitalisations","Standardised mortality rate","Mental health prescription rates","Low birth weight rate",
               "Emergency hospitalisation rate","School pupil attendance","Attainment education of school leavers ","Working age of no qualification individuals",
               "NEET","HESA","Avg driving time to a petrol station","Avg driving time to a GP","Avg driving time to a post office",
               "Avg driving time to primary school","Avg driving time to retail store","Avg driving time to secondary school",
               "Avg public transport time to GP","Avg public transport time to post office","Avg public transport time to retail store",
               "Percentage of households w/out fast broadband","Recorded crimes","Crime rate per 10,000 people",
               "Overcrowded household count","Count of households w/ no central heating")

exclude_vars1 <- c("Income_rate","Employment_rate","overcrowded_rate")

data_vars1 <- data_vars1 %>% 
  filter(!Column %in% exclude_vars1) %>% 
  mutate(
        Column = case_when(
            Column == "no_qualifications" ~ "Noquals",
            Column == "University" ~ "HESA",
            Column == "not_participating" ~ "NEET",
            Column == "nocentralheating_count" ~ "nocentralheat_count",
            Column == "drive_post" ~ "drive_PO",
            Column == "PT_post"~ "PT_Post",
            Column == "Working_age_population" ~ "Working_Age_population",
            TRUE ~ Column),
        label = var_names1)

# ensuring all vars have their correct corresponding label
var_names_combined <- bind_rows(data_vars, data_vars1) %>%
  distinct(Column, .keep_all = TRUE)



Scotland_local_auth2016 <- read_sf("SG_SIMD_2016_1.geojson", "SG_SIMD_2016_1", stringsAsFactors = F)
# na.omit(Glasgow_map)

# importing latest version of local authorities for 2016 & 2020 map
uk_local_authority2016 <- st_read("LAD/2016/Local_Authority_Districts_December_2016_FCB_in_the_UK.shp")
uk_local_authority2020 <- st_read("LAD/2020/LAD_DEC_2020_UK_BFE.shp")



# Define USER INTERFACE for application 

# Uncomment if you have your own CSS and you want more customisation
# my_theme <- bs_theme(
#   version = 5,
#   bootswatch = "flatly",  # You can pick any bootswatch theme or set colors manually
#   base_font = font_google("Neucha"),
#   heading_font = font_google("Cabin Sketch")
# )


ui <- navbarPage(
  title = "Scottish Index of Multiple Deprivation (SIMD)",
  theme = bs_theme(
    version = 5,
    bootswatch = "minty"
   # primary = "#48DAC6"
    ),
  id = "main_navbar",
  
  tabPanel(
    "Information",
    fluidPage(
      p(
        "This app uses the Scottish Index of Multiple Deprivation data. You can choose from either the 2016 or 2020 datasets to showcase measures of central tendency, statistical tests such as correlation.
               It includes histograms, scatterplots, and an interactive map of Glasgow."
      ),
      p(
        "The aim of this app is to explain the different relationships between the variables in such index, obtained from the Scottish Government Data Center.
               This app was used for promotional activities of the Q-Step programme across schools in the Greater Glasgow Area and University of Glasgow Open Days."
      ),
      p(
        "Developed by Dr. Cristina Chueca Del Cerro, under the supervision of Dr. Brian Fogarty and Dr. Niccole Pamphilis in 2017 for the Q-Step programme.
                              Latest update including new theme, visualisations and interactive map features Oct 2025."
      ),
      #, style = "font-size:26px"
      textInput("story", "Please leave your comments here"),
      br(),
      tags$img(
        src = "qstep2.png",
        height = 110,
        width = 220
      ),
      tags$img(
        src = "university.png.png",
        height = 110,
        width = 250
      )
    )
  ),
  
  tabPanel("Summary statistics and visualisation",
           fluidPage(
             shinyFeedback::useShinyFeedback(),
             fluidRow(
             column(
               width = 4,
               
               radioButtons("dataset", "Choose SIMD Dataset:",
                            choices = c("2016"="data_2016", "2020"="data_2020"),
                            selected = "data_2016",
                            inline = TRUE),
               
               selectInput(
                 "selected_var",
                 "Choose a Variable:",
                 choices = NULL # so when you select a df it shows the relevant var names
                 
               ),
               selectInput(
                 "bincolor",
                 "Select histogram bars color:",
                 choices = colors(),
                 selected = "grey"
               ),
               sliderInput(
                 "bins",
                 "Number of histogram Bins:",
                 min = 1,
                 max = 50,
                 value = 30
               ),
               checkboxInput("addmean", "Add Mean Line to plots?", value = FALSE),
               hr(),
               h4("Description"),
               uiOutput("varDescription"), # to show the var description
             ),
             column(
               width = 8,
               navset_card_underline(
                 nav_panel("Histogram", plotOutput("HistPlot", height = "300px")),
                 nav_panel("Density", plotOutput("DensityPlot", height = "300px")),
                 nav_panel("Boxplot", plotOutput("BoxPlot", height = "300px"))
               ),
               h4("Summary Statistics"),
               DTOutput("summaryStats") # all summary stats together, as a table
              )
           ))),
  
    tabPanel("Variable Relationship Exploration",
             fluidPage(fluidRow(
               column(
                 5,
                 selectInput("cov1", "X-Axis", choices = names(data_2016)),
                 selectInput("cov2", "Y-Axis", choices = names(data_2016)),
                 verbatimTextOutput("Explanation1"),
                 verbatimTextOutput("Explanation2"),
                 checkboxInput("gla", "What about only in Glasgow?", FALSE)
               ),
               column(
                 3,
                 selectInput(
                   "bincolor1",
                   "Color",
                   choices = colors(),
                   selected = "grey"
                 ),
                 checkboxInput("addcor", "Add Pearson's correlation to the graph?", FALSE)
               ),
               column(7,
                      plotOutput(
                        "compPlot", height = 550, width = 800
                      ))
             ))),
    
    tabPanel("Interactive Map of Deprivation",
             fluidPage(fluidRow(
               column(
                 4,
                 selectInput(
                   "cov4",
                   "Variables:",
                   choices = names(data_2016),
                   selected = "Population"
                 ),
                 selectInput("colors", "Color Scheme",
                             rownames(subset(
                               brewer.pal.info, category %in% c("seq", "div")
                             )))
               ),
               column(8,
                      leafletOutput(
                        "my_map", width = "100%", height = 600
                      ))
             )))
  )


##### SERVER CODE FOR THE APP####

server <- function(input, output, session) {

      datasetInput <- reactive({
        
        if(input$dataset == "data_2020"){
          
          data_2020 <- data_2020 %>% 
           dplyr::select(-c(Data_Zone, # make sure you include dplyr:: because otherwise it mixes up w/ a different package
                      Intermediate_Zone,
                      Council_area)) %>% 
            mutate_if(is.character,as.numeric) %>% 
            na.omit()
        
        } 
        else{
          
          data_2016 <- data_2016 %>% 
            dplyr::select(-c(Data_Zone,
                      Intermediate_Zone,
                      Council_area)) %>% 
            mutate_if(is.character,as.numeric) %>% 
            na.omit()
          
          }
        
      })
      
      observeEvent(input$dataset, {
        
        req(datasetInput())
        
        updateSelectInput(
          session,
          inputId = "selected_var",
          choices = names(datasetInput()),
          selected = names(datasetInput())[1]
        )
      })
      
      # Summary statistics
      output$summaryStats <- DT::renderDataTable({
        
        req(datasetInput(),input$selected_var)
     
        table_df <- datasetInput() %>%
          dplyr::select(input$selected_var) %>% # so it has the variable name
          dplyr::summarise(
            Mean = round(mean(.data[[input$selected_var]]), 3),
            Median = round(median(.data[[input$selected_var]]), 3),
            `Std. Dev` = round(sd(.data[[input$selected_var]]), 3),
            `Gini coefficient` = round(ineq::Gini(.data[[input$selected_var]]), 3)
          )
        
        
        DT::datatable(
          table_df,
          selection = 'none',rownames = FALSE,
          class = 'table table-primary',
          options = list(dom = 't', ordering = FALSE,
              initComplete = JS( # to manually change out table style
                "function(settings, json) {",
                "$(this.api().table().body()).addClass('table-light');",
                "}"
              )
            )
          )
      })
      

    # Variable description 
    output$varDescription <- renderUI({
      
      #subsetting the var names
      var_names <-  var_names_combined %>% 
        filter(Column == input$selected_var)
      
      # displaying the relevant variable properties
      tagList(
        tags$p(tags$strong("Variable:"),var_names$label),
        tags$p(tags$strong("Indicator Type:"), var_names$`Indicator type`),
        tags$p(tags$strong("Description:"), var_names$Description)
        )
      
    })
    
    variableLabel <- reactive({
      
      label <- var_names_combined$label[var_names_combined$Column == input$selected_var]
      
    })
  
    # Histogram
    output$HistPlot <- renderPlot({
      
      p <- ggplot(datasetInput(), aes_string(x =input$selected_var)) +
        geom_histogram(bins = input$bins, fill = input$bincolor, color = "black") +
        labs(x = variableLabel())+
        theme_classic(base_size = 14) +
        theme(axis.text = element_text(size = 12, face = "bold"))
      
      if (input$addmean) {
        p <- p + geom_vline(xintercept = mean(.data[[input$selected_var]]), lwd = 2, lty = 2)
      }
      
      p
    })
    
    # Density plot
    
    output$DensityPlot <- renderPlot({
      
      p <- ggplot(datasetInput(), aes(x = .data[[input$selected_var]])) +
        geom_density(alpha = 0.4) +
        labs(x = variableLabel())+
        theme_classic(base_size = 14) +
        theme(axis.text = element_text(size = 12, face = "bold"))
      
      
      if (input$addmean) {
        p <- p + geom_vline(xintercept = mean(.data[[input$selected_var]], na.rm = TRUE), lwd = 2, lty = 2)
      }
      
      p
    })
    
    # Boxplot
    output$BoxPlot <- renderPlot({
      
      # compare both datasets for the same variable (except for Broadband, only in 2020)
      
      if(!input$selected_var %in% names(data_2016)){
        
        shinyFeedback::feedbackDanger("selected_var", input$selected_var == "broadband", 
        "This variable was only present in the 2020 dataset. Boxplot comparison not possible")
        
        return(NULL)
      }
      
      else {
        hideFeedback("selected_var") # to remove the warning once they've chosen a diff var
        
        combined_data <- bind_rows(
        data_2016 %>% dplyr::select(input$selected_var) %>% mutate(Year="2016"),
        data_2020 %>%  dplyr::select(input$selected_var) %>%  mutate(Year="2020")
      )
      }
      
      ggplot(combined_data, aes(x = Year, y = .data[[input$selected_var]])) +
          geom_boxplot(alpha = 0.5) +
          coord_flip() +
          labs(x= "Year", y = variableLabel())+
          theme_classic(base_size = 14) +
          theme(axis.text = element_text(size = 12, face = "bold"))
      
    })
  #####SCATTERPLOT OUTPUT CODE###
  
  output$compPlot <-renderPlot({
    
    if (input$gla == "TRUE" && input$addcor == "TRUE") {
      ggplot(dataGlas, aes_string(x = input$cov1, y = input$cov2))
      + geom_point(alpha=1/5,position="jitter",size=3,colour = input$bincolor1)
      + geom_smooth(method="lm",se=FALSE,color = "black")
      + ggtitle("Scatterplot")
      + stat_cor(method = "pearson", label.x.npc = 0.51, label.y.npc = "top", size=6)
      + stat_cor(method = "pearson", label.x.npc = 0.71, label.y.npc = "top", size=6)
      + scale_y_continuous(limits=c(0,max(data2[,input$cov2]))) 
      + theme(axis.text=element_text(size=12, face = "bold"),axis.title=element_text(size=14,face="bold")) 
      + theme(plot.title = element_text(size=22))
    }
   
    else if  (input$addcor == "TRUE" && input$gla == "FALSE") {
      ggplot(data2, aes_string(x = input$cov1, y = input$cov2))
      + geom_point(alpha=1/5,position="jitter",size=3,colour = input$bincolor1)
      + geom_smooth(method="lm",se=FALSE,color = "black") 
      + ggtitle("Scatterplot")
      + stat_cor(method = "pearson", label.x.npc = 0.51, label.y.npc = "top", size=6) 
      + stat_cor(method = "pearson", label.x.npc = 0.71, label.y.npc = "top", size=6)
      + scale_y_continuous(limits=c(0,max(data2[,input$cov2]))) 
      + theme(axis.text=element_text(size=12, face = "bold"),axis.title=element_text(size=14,face="bold")) 
      + theme(plot.title = element_text(size=22)) 
      
    }
    
    else if (input$gla == "TRUE" && input$addcor == "FALSE") {
      ggplot(dataGlas, aes_string(x = input$cov1, y = input$cov2))
      + geom_point(alpha=1/5,position="jitter",size=3, colour = input$bincolor1)
      + eom_smooth(method="lm",se=FALSE, color = "black") 
      + ggtitle("Scatterplot") 
      + scale_y_continuous(limits=c(0,max(data2[,input$cov2]))) 
      + theme(axis.text=element_text(size=12, face = "bold"),axis.title=element_text(size=14,face="bold")) 
      + theme(plot.title = element_text(size=22))
      }
      
    else if (input$gla == "FALSE" && input$addcor == "FALSE") {
      ggplot(data2, aes_string(x = input$cov1, y = input$cov2))
      + geom_point(alpha=1/5,position="jitter",size=3,colour = input$bincolor1) 
      + geom_smooth(method="lm",se=FALSE,color = "black") 
      + ggtitle("Scatterplot") 
      + scale_y_continuous(limits=c(0,max(data2[,input$cov2]))) 
      + theme(axis.text=element_text(size=12, face = "bold"),axis.title=element_text(size=14,face="bold"))
      + theme(plot.title = element_text(size=22))
    }
  })
 
 #### Code for the Leaflet Map of Glasgow

output$my_map <- renderLeaflet({
  #linking csv data with geojson data
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
  
  #color set up
  colorpal <- reactive({
    colorNumeric(input$colors,x)
  })
  pal <- colorpal()
  # palette1 <- colorNumeric("Spectral",
  #                          domain = countriesGla,input$cov4)
  
  #using the labels from geojson file
  labels <- countriesGla$DataZone
  
  #plotting the map  
  leaflet(countriesGla)%>%
  # map options
  addProviderTiles("Esri.WorldGrayCanvas", 
                   options = tileOptions(minZoom=6, maxZoom=16)) %>% 
  addProviderTiles("Stamen.Toner", group = "Toner") %>%
  addProviderTiles("Esri.WorldImagery", group = "Toner Lite") %>%
  addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
  addLayersControl(baseGroups = c("Toner Lite", "CartoDB","Toner", "hex"))%>%
  
  #adding the shapes for the map and the labels
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
  
   # legent options
    addLegend("bottomright", pal = pal, values = ~x,
              title = "Density Levels",
              labFormat = labelFormat(suffix = " "),
              opacity = 0.75)
  })


}

# Run the application and enjoy!
shinyApp(ui = ui, server = server)
