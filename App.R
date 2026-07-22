### SCOTTISH INDEX OF MULTIPLE DEPRIVATION SHINY APP ###
### Author: Dr. Cristina Chueca Del Cerro
### Originally developed 2017 (Q-Step Programme, University of Glasgow)
### Updated 2026: new theme, better descriptions, SIMD 2020 dataset,
### improved visualisations, output downloads, Scotland-wide interactive map
### ============================================================

### LIBRARIES REQUIRED ###
library(tidyverse)
library(DT)
library(scales)
library(ineq)       # for Gini coefficient

# data imports
library(readxl)
library(sf)

# shiny related
library(shiny)
library(shinyFeedback)
library(shinythemes)
library(shinyWidgets) # warning message
library(bslib)
library(RColorBrewer)
library(ggpubr)
library(shinydashboard)
library(leaflet)
library(rmapshaper) # improve leaflet rendering performance


### ============================================================
### DATA PREPARATION
### ============================================================

## ---- SIMD 2016 ----

data_2016 <- read_excel("SIMD16_Data.xlsx", sheet = "Data")

data_2016 <- data_2016 %>%
  dplyr::select(-c(
    Overal_SIMD16_Rank, SIMD_2016_Percentile, SIMD_2016_Vigintile,
    SIMD_2016_Decile, SIMD_2016_Quintile, Income_rate, Employment_rate,
    Income_Domain_2016_Rank, Employment_Domain_2016_Rank,
    Health_Domain_2016_Rank, Education_Domain_2016_Rank,
    Geographic_Access_Domain_2016_Rank, Crime_Domain_2016_Rank,
    Housing_Domain_2016_Rank, overcrowded_rate, nocentralheat_rate
  )) %>%
  rename(Working_Age_population = Working_age_population_Revised) %>%
  na.omit() %>%
  mutate(
    Attainment = case_when(Attainment == "*" ~ NA, TRUE ~ Attainment),
    Attainment = as.numeric(Attainment),
    Council_area = str_replace_all(Council_area, "_", " ")
  ) %>%
  mutate_if(is.character, as.factor)

data_vars <- read_excel(
  "SIMD16_Data.xlsx", sheet = "Indicator descriptions",
  range = "B1:D37", col_types = c("text", "text", "text"), col_names = TRUE
)

var_names <- c(
  "Data Zone", "Intermediate zone", "Council Area", "Total population",
  "Working Age population", "Count of Income deprived", "Unemployment count",
  "Comparative Illness Factor", "Alcohol-related hospitalisations",
  "Drug-related hospitalisations", "Standardised mortality rate",
  "Mental health prescription rates", "Low birth weight rate",
  "Emergency hospitalisation rate", "School pupil attendance",
  "Attainment education of school leavers", "Working age of no qualification individuals",
  "NEET", "HESA", "Avg driving time to a petrol station", "Avg driving time to a GP",
  "Avg driving time to a post office", "Avg driving time to primary school",
  "Avg driving time to retail store", "Avg driving time to secondary school",
  "Avg public transport time to GP", "Avg public transport time to post office",
  "Avg public transport time to retail store", "Crime rate per 10,000 people",
  "Overcrowded household count", "Count of households w/ no central heating"
)

exclude_vars <- c("Income_rate", "Employment_rate", "crime_count",
                  "overcrowded_rate", "nocentralheat_rate")

data_vars <- data_vars %>%
  filter(!Column %in% exclude_vars) %>%
  mutate(label = var_names)


## ---- SIMD 2020 ----

data_2020 <- read_excel("SIMD20_Data.xlsx", sheet = "Data")

data_2020 <- data_2020 %>%
  dplyr::select(-c(
    SIMD2020v2_Rank, SIMD_2020v2_Percentile, SIMD2020v2_Vigintile,
    SIMD2020v2_Decile, SIMD2020v2_Quintile, income_rate, employment_rate,
    SIMD2020v2_Income_Domain_Rank, SIMD2020_Employment_Domain_Rank,
    SIMD2020_Health_Domain_Rank, SIMD2020_Education_Domain_Rank,
    SIMD2020_Access_Domain_Rank, SIMD2020_Crime_Domain_Rank,
    SIMD2020_Housing_Domain_Rank, overcrowded_rate, nocentralheating_rate
  )) %>%
  rename(
    Income_count = income_count, Employment_count = employment_count,
    Noquals = no_qualifications, HESA = University, NEET = not_participating,
    nocentralheat_count = nocentralheating_count, drive_PO = drive_post,
    PT_Post = PT_post
  ) %>%
  na.omit() %>%
  mutate(
    Attainment = case_when(Attainment == "*" ~ NA, TRUE ~ Attainment),
    Attainment = as.numeric(Attainment),
    Attendance = case_when(Attendance == "*" ~ NA, TRUE ~ Attendance),
    Attendance = as.numeric(Attendance),
    crime_count = case_when(crime_count == "*" ~ NA, TRUE ~ crime_count),
    crime_count = as.numeric(crime_count),
    crime_rate = case_when(crime_rate == "*" ~ NA, TRUE ~ crime_rate),
    crime_rate = as.numeric(crime_rate),
    Council_area = str_replace_all(Council_area, "_", " ")
  ) %>%
  mutate_if(is.character, as.factor)

data_vars1 <- read_excel(
  "SIMD20_Data.xlsx", sheet = "Indicator descriptions",
  range = "B1:D37", col_types = c("text", "text", "text"), col_names = TRUE
)

var_names1 <- c(
  "Data Zone", "Intermediate zone", "Council Area", "Total population",
  "Working Age population", "Count of Income deprived", "Unemployment count",
  "Comparative Illness Factor", "Alcohol-related hospitalisations",
  "Drug-related hospitalisations", "Standardised mortality rate",
  "Mental health prescription rates", "Low birth weight rate",
  "Emergency hospitalisation rate", "School pupil attendance",
  "Attainment education of school leavers", "Working age of no qualification individuals",
  "NEET", "HESA", "Avg driving time to a petrol station", "Avg driving time to a GP",
  "Avg driving time to a post office", "Avg driving time to primary school",
  "Avg driving time to retail store", "Avg driving time to secondary school",
  "Avg public transport time to GP", "Avg public transport time to post office",
  "Avg public transport time to retail store",
  "Percentage of households w/out fast broadband", "Recorded crimes",
  "Crime rate per 10,000 people", "Overcrowded household count",
  "Count of households w/ no central heating"
)


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
      Column == "PT_post" ~ "PT_Post",
      Column == "Working_age_population" ~ "Working_Age_population",
      TRUE ~ Column
    ),
    label = var_names1
  )

# Combined variable label lookup used across all tabs
var_names_combined <- bind_rows(data_vars, data_vars1) %>%
  distinct(Column, .keep_all = TRUE)


## ---- Geographic data (Scotland-wide, 2016 boundaries) ----

Scotland_local_auth2016 <- read_sf("SG_SIMD_2016_1.geojson")

Scotland_local_auth2016 <- st_transform(Scotland_local_auth2016, crs = 4326)

Scotland_local_auth2016 <- Scotland_local_auth2016 %>%
  dplyr::select(Data_Zone = DataZone, LAName, Shape_Leng, Shape_Area) %>%
  ms_simplify(keep = 0.1, keep_shapes = TRUE)

saveRDS(Scotland_local_auth2016,
        "Scotland_local_auth2016.rds")

Scotland_local_auth2016_revised <- readRDS(
  "Scotland_local_auth2016.rds"
)

## Random selection of 12 colors instead of the 600+ in colors()
curated_palettes <- c(
  "yellow","orange2", "thistle","plum" ,"turquoise" ,
  "royalblue","violet", "orchid4" ,"tan", "sienna",
  "grey", "black")



### ============================================================
### UI
### ============================================================

# Uncomment if you have your own CSS and you want more customisation
# my_theme <- bs_theme(
#   version = 5,
#   bootswatch = "flatly",  # You can pick any bootswatch theme or set colors manually
#   base_font = font_google("Neucha"),
#   heading_font = font_google("Cabin Sketch")
# )


ui <- page_fluid(
  theme = bs_theme(
    # bootswatch = "lux",
    # base_font = '"Georgia", Georgia, serif',
    bootswatch = "flatly",
    base_font = font_google("Source Sans 3"),
    code_font = font_google("JetBrains Mono"),
    bg = "#FFF",
    fg = "#101010",
    primary = "#1A1A1A",
    secondary = "#FFFFFF",
    success = "#009E73"
  ),

  ##
  tags$style(
    HTML(
      "
  .simd-header {
    text-align: center;
  }

  .nav-link {
    text-transform: none !important;
    letter-spacing: normal !important;
  }
  "
    )
  ),


  # ---- Header ----

  div(
    class = "simd-header px-4 py-3",
    style = "
      background:#FFFFFF;
      border-bottom:4px solid #009E73;
    ",

    h1("Scottish Index of Multiple Deprivation (SIMD)", class = "mb-1"),

    p(
      "An interactive exploration of deprivation indicators across Scotland",
      class = "lead mb-0"
    )
  ),


  # Navigation
  navset_bar(
    id = "main_navbar",

    # navset_tab(
    nav_panel(
      "Information",
      fluidPage(
        tags$div(style = "margin-bottom: 20px;"),

        h3("Overview"),

        p(
          "This interactive dashboard enables exploration of the Scottish Index of Multiple Deprivation (SIMD)
      across the 2016 and 2020 releases. Users can investigate patterns of deprivation across Scotland
      through summary statistics, interactive visualisations and geographical patterns using an interactive map."
        ),

        tags$div(style = "margin-bottom: 20px;"),


        h3("About the project"),

        p(
          "I developed RShiny app this part of a Q-Step internship in 2017 under the supervision of Dr. Brian Fogarty and
         Dr. Niccole Pamphilis. It was originally created to support outreach
         activities for the Q-Step Centre at the University of Glasgow, helping prospective students
         and schools explore deprivation data through an accessible and interactive interface."
        ),

        tags$div(style = "margin-bottom: 20px;"),


        layout_column_wrap(
          width = 1 / 2,

          card(card_header(h3("Features")), card_body(
            tags$ul(
              tags$li(
                tags$strong("Summary statistics and indicator visualisations: "),
                "Explore individual SIMD indicators through customisable histogram,
                density plot and boxplot (compares both datasets).
                View descriptive statistics (mean, median, standard deviation and
                Gini coefficient) access variable descriptions and customise plots."
              ),

              tags$li(
                tags$strong("Exploring relationships between indicators: "),
                "Compare two SIMD indicators using an interactive scatterplot and hexbin.
                Calculate variable correlations and simple linear regression.
                Filter observations by Council too."
              ),

              tags$li(
                tags$strong("Interactive deprivation mapping: "),
                "Explore deprivation patterns across Scottish data zones using an
               interactive map. Select between dataset, Council areas and indicators
               while viewing corresponding descriptions."
              ),

              tags$li(
                tags$strong("Data and visualisation exports: "),
                "Download the underlying SIMD datasets and summary outputs used within
                 the application. Export visualisations generated during analysis."
              )


            )
          )),


          card(card_header(h3(
            "What's new in this update"
          )), card_body(
            tags$ul(
              tags$li(
                tags$strong("Redesigned interface: "),
                "A refreshed interface with a more polished and accessible user
                experience, including revised information tab and improved descriptions."
              ),

              tags$li(
                tags$strong("SIMD 2020 integration: "),
                "The 2020 dataset has been added alongside the original 2016 release,
                 enabling direct comparison between the two most recent SIMD publications."
              ),

              tags$li(
                tags$strong("Data exports: "),
                "Dataset, summary outputs and visualisations available for download."
              ),

              tags$li(
                tags$strong("Expanded visualisations: "),
                "Additional functionality supports both single-variable exploration
                 and comparisons between multiple indicators"
              ),
              tags$li(
                tags$strong("Scotland-wide interactive mapping: "),
                "The map has been expanded from Glasgow City Council to cover Scotland
                 using Local Authority boundaries."
              ),

            )
          ))
        ),

        tags$div(style = "margin-bottom: 20px;"),

        h3("Data access"),

        p(
          "Additional information about the SIMD methodology, indicators and data releases is
        available from the Scottish Government. Either dataset used in this app can be downloaded below."
        ),

        div(
          class = "d-flex gap-3 mt-3",

          downloadButton("download_2016simd", "Download 2016 SIMD dataset", class = "btn-primary"),
          downloadButton("download_2020simd", "Download 2020 SIMD dataset", class = "btn-primary"),

          tags$a(
            href = "https://www.gov.scot/collections/scottish-index-of-multiple-deprivation-2020/",
            target = "_blank",
            class = "btn",
            style = "
           color:#009E73;
           border:1px solid #009E73;
           text-decoration:none;
           display:inline-block;
          ",
            "View SIMD documentation"
          )
        ),
        tags$div(style = "margin-bottom: 20px;"),

        div(
          style = "text-align:center;
        margin-top:50px;
        ",

          img(src = "QStep_logo", height = "90px", ),

          img(src = "UofGlasgow_logo", height = "90px")
        )
        ,

        tags$footer(
          style = "text-align: center; padding: 20px; color: #888; font-size: 13px;",
          "Built by Dr. Cristina Chueca Del Cerro | ",
          tags$a(href = "https://github.com/chuecadelc", "GitHub"),
          " | ",
          tags$a(href = "https://chuecadelc.github.io/", "Portfolio")
        )
      )
    ),

    nav_panel(
      "Summary statistics and visualisation",
      fluidPage(
        shinyFeedback::useShinyFeedback(),
        fluidRow(
          column(
            width = 4,
            radioButtons(
              "dataset",
              "Choose SIMD Dataset:",
              choices = c("2016" = "data_2016", "2020" = "data_2020"),
              selected = "data_2016",
              inline = TRUE
            ),
            selectInput("selected_var", "Choose a Variable:", choices = NULL),
            selectInput(
              "bincolor",
              "Select histogram bars color:",
              choices = curated_palettes,
              selected = "orange2"
            ),
            sliderInput(
              "bins",
              "Number of histogram Bins:",
              min = 1,
              max = 50,
              value = 30
            ),
            checkboxInput("addmean", "Add Mean line to histogram & desnity plots?", value = FALSE),
            hr(),
            h4("Description"),
            uiOutput("varDescription")
          ),
          column(
            width = 8,
            navset_card_underline(
              nav_panel("Histogram", plotOutput("HistPlot", height = "300px")),
              nav_panel("Density", plotOutput("DensityPlot", height = "300px")),
              nav_panel("Boxplot", plotOutput("BoxPlot", height = "300px"))
            ),
            h4("Summary Statistics"),
            DTOutput("summaryStats"),
            downloadButton("download_summary", "Download summary table (CSV)")
          )
        )
      )
    ),

    nav_panel("Variable Relationship Exploration", fluidPage(fluidRow(
      column(
        4,
        radioButtons(
          "dataset1",
          "Choose SIMD Dataset:",
          choices = c("2016" = "data_2016", "2020" = "data_2020"),
          selected = "data_2016",
          inline = TRUE
        ),
        selectInput("covariate1", "X-Axis", choices = NULL),
        selectInput("covariate2", "Y-Axis", choices = NULL),
        checkboxInput("subset", "Subset the data for a specific Council area only?", FALSE),
        conditionalPanel(
          condition = "input.subset == true",
          selectInput("subset_council", "Select Council area:", choices = NULL)
        ),
        selectInput(
          "bincolor1",
          "Color",
          choices = curated_palettes,
          selected = "orange2"
        ),
        checkboxInput("addcor", "Add Pearson's correlation to scatterplot?", FALSE)
      ),
      column(width = 8, navset_card_underline(
        nav_panel("Scatterplot", plotOutput("scatterPlot", height = "400px")),
        nav_panel("Hexbin", plotOutput("HexbinPlot", height = "400px"))
      ))
    ))),

    nav_panel("Interactive Map of Deprivation", fluidPage(fluidRow(
      column(
        4,
        radioButtons(
          "dataset2",
          "Choose SIMD Dataset:",
          choices = c("2016" = "data_2016", "2020" = "data_2020"),
          selected = "data_2016",
          inline = TRUE
        ),
        selectInput("covariate3", "Variable", choices = NULL),
        checkboxInput("show_all_scotland", "Show the full map of Scotland?", FALSE),
        conditionalPanel(
          condition = "!input.show_all_scotland",
          selectInput("council_map", "Select Council area:", choices = NULL)
        ),
        selectInput("colors", "Color Scheme", rownames(subset(
          brewer.pal.info, category %in% c("seq", "div")
        ))),
        h4("Variable Description"),
        uiOutput("varDescription")
      ),
      column(8, leafletOutput(
        "my_map", width = "100%", height = 600
      ))
    )))
  )
  #)
)



### ============================================================
### SERVER
### ============================================================

server <- function(input, output, session) {

  ## In progress
  # observe(session$setCurrentTheme(
  #   if (isTRUE(input$dark_mode)) dark else light
  # ))

  ## ---- Dataset selection helper ----
  # Replaces three near-identical if/else blocks with one shared function

  get_dataset <- function(choice) {
    if (choice == "data_2020") data_2020 else data_2016
  }

  datasetInput  <- reactive({ get_dataset(input$dataset) })
  datasetInput1 <- reactive({ get_dataset(input$dataset1) })
  datasetInput2 <- reactive({ get_dataset(input$dataset2) })

  ## ---- Download selected SIMD dataset ----


  output$download_data_2016 <- downloadHandler(
    filename = function() "SIMD_2016_data.csv",
    content = function(file) {
      write.csv(data_2016, file, row.names = FALSE)
    }
  )

  output$download_data_2020 <- downloadHandler(
    filename = function() "SIMD_2020_data.csv",
    content = function(file) {
      write.csv(data_2020, file, row.names = FALSE)
    }
  )

  ## ---- Variable choice update helper ----

  # Replaces three near-identical observeEvent blocks


  # Displays reader-friendly labels while selecting the equivalent col from df.

  get_labelled_choices <- function(dataset) {
    col_names <- names(dataset)[4:length(names(dataset))]
    labels <- var_names_combined$label[match(col_names, var_names_combined$Column)]
    setNames(col_names, labels)
  }


  update_var_choices <- function(session, dataset, input_id, selected_index = 1) {
    choices <- get_labelled_choices(dataset)
    updateSelectInput(session, inputId = input_id, choices = choices,
                      selected = choices[selected_index])
  }

  observeEvent(input$dataset, {
    req(datasetInput())
    update_var_choices(session, datasetInput(), "selected_var")
  })

  observeEvent(input$dataset1, {
    req(datasetInput1())
    update_var_choices(session, datasetInput1(), "covariate1", 1)
    update_var_choices(session, datasetInput1(), "covariate2", 2)
  })

  observeEvent(input$dataset2, {
    req(datasetInput2())
    update_var_choices(session, datasetInput2(), "covariate3")
  })


  ## ---- Council area choices ----

  observe({
    req(datasetInput1())
    council_names <- datasetInput1()$Council_area %>%
      trimws() %>%
      unique() %>%
      sort()
    updateSelectInput(session, inputId = "subset_council",
                      choices = council_names, selected = council_names[1])
  })

  observe({
    req(datasetInput2())
    council_names <- datasetInput2()$Council_area %>%
      trimws() %>%
      unique() %>%
      sort()
    updateSelectInput(session, inputId = "subset_council_map",
                      choices = council_names, selected = council_names[1])
  })


  subset_data <- reactive({
    req(datasetInput1())
    data <- datasetInput1()
    if (isTRUE(input$subset)) {
      req(input$subset_council)
      data <- dplyr::filter(data, Council_area == input$subset_council)
    }
    data
  })


  # Data actually used for scatter/hexbin plots -- avoids duplicating
  # the subset vs full-data branch inside every plot renderer
  plot_data <- reactive({
    if (isTRUE(input$subset)) subset_data() else datasetInput1()
  })


  ## ---- Summary statistics ----

  output$summaryStats <- DT::renderDataTable({
    req(datasetInput(), input$selected_var)

    table_df <- datasetInput() %>%
      dplyr::summarise(
        Mean = round(mean(.data[[input$selected_var]], na.rm = TRUE), 3),
        Median = round(median(.data[[input$selected_var]], na.rm = TRUE), 3),
        `Std. Dev` = round(sd(.data[[input$selected_var]], na.rm = TRUE), 3),
        `Gini coefficient` = round(ineq::Gini(.data[[input$selected_var]]), 3)
      )

    DT::datatable(
      table_df, selection = "none", rownames = FALSE,
      class = "table table-primary",
      options = list(
        dom = "t", ordering = FALSE,
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().body()).addClass('table-light');",
          "}"
        )
      )
    )
  })


  ## ---- Variable description ----

  output$varDescription <- renderUI({
    req(input$selected_var)
    var_info <- var_names_combined %>% filter(Column == input$selected_var)

    tagList(
      tags$p(tags$strong("Variable:"), var_info$label),
      tags$p(tags$strong("Indicator Type:"), var_info$`Indicator type`),
      tags$p(tags$strong("Description:"), var_info$Description)
    )
  })

  variableLabel <- reactive({
    var_names_combined$label[var_names_combined$Column == input$selected_var]
  })


  ## ---- Histogram ----

  output$HistPlot <- renderPlot({
    req(datasetInput(), input$selected_var)

    p <- ggplot(datasetInput(), aes(x = .data[[input$selected_var]])) +
      geom_histogram(bins = input$bins, fill = input$bincolor, color = "black") +
      labs(x = variableLabel()) +
      theme_classic(base_size = 14) +
      theme(axis.text = element_text(size = 12, face = "bold"))

    if (input$addmean) {
      mean_val <- mean(datasetInput()[[input$selected_var]], na.rm = TRUE)
      p <- p + geom_vline(xintercept = mean_val, lwd = 1, lty = 2)
    }

    p
  })


  ## ---- Density plot ----

  output$DensityPlot <- renderPlot({
    req(datasetInput(), input$selected_var)

    p <- ggplot(datasetInput(), aes(x = .data[[input$selected_var]])) +
      geom_density(alpha = 0.4) +
      labs(x = variableLabel()) +
      theme_classic(base_size = 14) +
      theme(axis.text = element_text(size = 12, face = "bold"))

    if (input$addmean) {
      mean_val <- mean(datasetInput()[[input$selected_var]], na.rm = TRUE)
      p <- p + geom_vline(xintercept = mean_val, lwd = 1, lty = 2)
    }

    p
  })


  ## ---- Boxplot (2016 vs 2020 comparison) ----

  output$BoxPlot <- renderPlot({
    req(input$selected_var)

    var_in_both <- input$selected_var %in% names(data_2016) &&
                   input$selected_var %in% names(data_2020)

    if (!var_in_both) {
      shinyFeedback::feedbackDanger(
        "selected_var", TRUE,
        "This variable is not available in both datasets. Boxplot comparison not possible."
      )
      return(NULL)
    }

    shinyFeedback::hideFeedback("selected_var")

    combined_data <- bind_rows(
      data_2016 %>% dplyr::select(all_of(input$selected_var)) %>% mutate(Year = "2016"),
      data_2020 %>% dplyr::select(all_of(input$selected_var)) %>% mutate(Year = "2020")
    )

    ggplot(combined_data, aes(x = Year, y = .data[[input$selected_var]])) +
      geom_boxplot(alpha = 0.5) +
      coord_flip() +
      labs(x = "Year", y = variableLabel()) +
      theme_classic(base_size = 14) +
      theme(axis.text = element_text(size = 12, face = "bold"))
  })


  ## ---- Scatterplot / Hexbin labels ----

  x_variableLabel <- reactive({
    var_names_combined$label[var_names_combined$Column == input$covariate1]
  })

  y_variableLabel <- reactive({
    var_names_combined$label[var_names_combined$Column == input$covariate2]
  })


  ## ---- Scatterplot ----

  output$scatterPlot <- renderPlot({
    req(plot_data(), input$covariate1, input$covariate2)

    p <- ggplot(plot_data(), aes(x = .data[[input$covariate1]], y = .data[[input$covariate2]])) +
      geom_point(alpha = 1 / 5, position = "jitter", size = 3, colour = input$bincolor1) +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      labs(x = x_variableLabel(), y = y_variableLabel()) +
      theme_classic(base_size = 14) +
      theme(axis.text = element_text(size = 12, face = "bold"))

    if (input$addcor) {
      p <- p + stat_cor(method = "pearson", label.x.npc = 0.71,
                         label.y.npc = "top", size = 6)
    }

    p
  })


  ## ---- Hexbin ----

  output$HexbinPlot <- renderPlot({
    req(plot_data(), input$covariate1, input$covariate2)

    ggplot(plot_data(), aes(x = .data[[input$covariate1]], y = .data[[input$covariate2]])) +
      stat_density2d(geom = "tile", aes(fill = after_stat(density)), contour = FALSE) +
      geom_point(input$bincolor1) +
      labs(x = x_variableLabel(), y = y_variableLabel()) +
      theme_classic(base_size = 14) +
      theme(axis.text = element_text(size = 12, face = "bold"))
  })


  ## ---- Interactive Map (Scotland-wide) ----


  observe({
    req(datasetInput2(),input$covariate3 )
    council_names <- unique(datasetInput2()$Council_area)
    updateSelectInput(session, "council_map", choices = council_names,
                      selected = "Glasgow City")

  })

  variableLabel1 <- reactive({
    var_names_combined$label[var_names_combined$Column == input$covariate3]
  })

  observeEvent(input$show_all_scotland, {

    if (input$show_all_scotland) {# when selecting ALL Scotland, issue warning

      sendSweetAlert(
        session = session,
        title = "Loading full Scotland",
        text = "Rendering the full map may take around a minute.",
        type = "warning"
      )

    }

  })

  map_data <- reactive({

    req(datasetInput2(), input$covariate3)

    map_dataset <- Scotland_local_auth2016 %>%
      left_join(datasetInput2(), by = "Data_Zone")

    if (!input$show_all_scotland) {

      req(input$council_map)

      map_dataset <- map_dataset %>%
        filter(Council_area == input$council_map)

    }

    map_dataset

  })


  output$my_map <- renderLeaflet({

    req(map_data(), input$covariate3, input$colors)

    pal <- colorNumeric(input$colors, map_data()[[input$covariate3]])

    leaflet(map_data()) %>%
      addProviderTiles("Esri.WorldGrayCanvas",options = tileOptions(minZoom = 6, maxZoom = 16)) %>%
      addProviderTiles("Esri.WorldImagery", group = "Toner Lite") %>%
      addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
      addLayersControl(baseGroups = c("Grey Canvas","Toner","Toner lite", "CartoDB")) %>%
      addPolygons(
        smoothFactor = 0.2,
        fillColor = ~pal(get(input$covariate3)),
        fillOpacity = 0.8,
        color = "lightblue",
        weight = 1.5,
        highlight = highlightOptions(weight = 5, color = "#666", fillOpacity = 0, bringToFront = FALSE),
        label = ~Data_Zone,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textOnly = TRUE, textsize = "15px", direction = "auto"
        )
      ) %>%
      addLegend("bottomright", pal = pal, values = ~get(input$covariate3),
                title = variableLabel1(), opacity = 0.75)
  })
}

# Run the application and enjoy!
shinyApp(ui = ui, server = server)
