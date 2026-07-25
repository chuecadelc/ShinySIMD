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
library(broom)      # t-test
library(plotly)     # enbles text hovering feature
library(cowplot)    # extract plot legend as object
library(hexbin)    # MUST be installed even if you have tidyverse loaded - otherwise no geom_hex()

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
library(mapview)    # to export map as jpeg
library(webshot)    # to export as html


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

# Scotland_local_auth2016 <- read_sf("SG_SIMD_2016_1.geojson")
#
# Scotland_local_auth2016 <- st_transform(Scotland_local_auth2016, crs = 4326)
#
# Scotland_local_auth2016 <- Scotland_local_auth2016 %>%
#   dplyr::select(Data_Zone = DataZone, LAName, Shape_Leng, Shape_Area) %>%
#   ms_simplify(keep = 0.1, keep_shapes = TRUE)
#
# saveRDS(Scotland_local_auth2016,
#         "Scotland_local_auth2016.rds")

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

  # ---- Google Analytics 4 ----

  tags$head(
    tags$script(async = NA,
                src = "https://www.googletagmanager.com/gtag/js?id=G-XXXXXXXXXX"),

    tags$script(
      HTML(
        "window.dataLayer = window.dataLayer || [];
         function gtag(){dataLayer.push(arguments);}
         gtag('js', new Date());
         gtag('config', 'G-XXXXXXXXXX', { debug_mode: true });"
        )
      ),

    tags$script(HTML("
        Shiny.addCustomMessageHandler('trackPageview', function(tabName) {
          gtag('event', 'page_view', { page_title: tabName });
        });
           Shiny.addCustomMessageHandler('ga_event', function(eventData) {
          gtag('event', eventData.event, {
          dataset: eventData.dataset,
          output_type: eventData.output_type || null
          });
        });
      "))
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


        layout_column_wrap(width = 1 / 2,

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
                tags$strong("Ranked data zones: "),
                "Identify the highest- or lowest-scoring data zones for any indicator,
                  with an adjustable number of areas shown and downloadable results."
              ),
              tags$li(
                tags$strong("Exploring relationships between indicators: "),
                "Compare two SIMD indicators using an interactive scatterplot and hexbin.
                   Hover over individual points to see the corresponding council area, data
                   zone, and population. Calculate variable correlations and simple linear
                   regression. Filter observations by Council too."
              ),
              tags$li(
                tags$strong("Interactive deprivation mapping: "),
                "Explore deprivation patterns across Scottish data zones using an
                   interactive map. Select between datasets, Council areas and indicators
                   while viewing corresponding descriptions.
                   Also, compare a single indicator across releases and a statistical test to
                   assess whether the change is significant."
              ),

              tags$li(
                tags$strong("Data and visualisation exports: "),
                "Download the underlying SIMD datasets, summary outputs, and ranked
                  tables used within the application. Export visualisations generated
                  during analysis."
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
                tags$strong("SIMD 2020 integration & data exports: "),
                "The 2020 dataset has been added, enabling comparison between the two releases.
                 Dataset, summary outputs and visualisations available for download.
                 New dropdown table identifying the highest- or lowest-scoring areas
                 for any indicator."
              ),

              tags$li(
                tags$strong("Expanded visualisations: "),
                "Additional functionality supports both single-variable exploration
                 and comparisons between multiple indicators."
              ),

              tags$li(
                tags$strong("Interactive hover tooltips: "),
                "Scatterplot and hexbin comparisons now show area and population
                  details on hover."
              ),
              tags$li(
                tags$strong("Scotland-wide interactive mapping: "),
                "The map has been expanded from Glasgow City Council to cover Scotland
                 using Local Authority boundaries. Direct 2016 vs 2020 comparison for a chosen council
                 area and indicator, including a statistical significance test."
              ),
              tags$li(
                tags$strong("Usage analytics: "),
                "Anonymous, privacy-conscious usage analytics have been added to help
                 guide future development -- see the Analytics & Privacy section below
                 for full details."
              )

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

        br(), br(),
        hr(),
        h4("Data Analytics & Privacy"),
        p("This app uses Google Analytics (GA4) to understand usage patterns
          and guide future development. For full detail on how Google collects and processes analytics
          data, see ",
          a("Google's Privacy Policy",
            href = "https://policies.google.com/privacy", target = "_blank"),
          "."),
        tags$ul(
          tags$li(strong("What is collected: "), "anonymous page/tab views,
                feature interaction events (e.g. which datasets or tables
                are downloaded), and approximate country-level location
                derived from network data -- not your precise location
                or IP address."),
          tags$li(strong("What is NOT collected: "), "any personally
                identifiable information (name, email, etc.), precise
                location, or IP addresses. Cross-device and
                advertising-related tracking (Google Signals) is disabled
                for this property."),
          tags$li(strong("Applicability: "), "These practices apply equally to
                all visitors and are in line with UK GDPR and EU GDPR
                principles."),
          tags$li(strong("Data retention: "), "The above analytics data is retained
                for a maximum of 2 months before automatic deletion.")
        ),
        p(),
        p(em("This statement reflects good-faith transparency about data
        practices for a personal academic project and does not constitute
        formal legal advice."), style = "font-size: 13px; color: #666;"),
        br(),

        div(
          style = "text-align:center;
        margin-top:50px;
        ",

          img(src = "Q_Step_logo.png", height = "90px", ),

          img(src = "UofG_logo.jpg", height = "90px")
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
            hr(),
            h4("Description"),
            uiOutput("varDescription")
          ),
          column(
            width = 8,
            navset_card_underline(
              nav_panel(
                "Histogram",
                plotOutput("HistPlot", height = "300px"),
                checkboxInput("addmean_hist", "Add mean line?", value = FALSE),
                downloadButton("download_hist", "Download plot")
              ),

              nav_panel(
                "Density",
                plotOutput("DensityPlot", height = "300px"),
                checkboxInput("addmean_density", "Add mean line?", value = FALSE),
                downloadButton("download_density", "Download plot")
              ),

              nav_panel(
                "Boxplot",
                plotOutput("BoxPlot", height = "300px"),
                downloadButton("download_boxplot", "Download plot")
              )
            ),
            h4("Summary Statistics"),
            DTOutput("summaryStats"),
            downloadButton("download_summary", "Download summary table (CSV)"),

            br(),

            fluidRow(
              column(
                width = 12,
                accordion(
                  open = FALSE,
                  accordion_panel(
                    title = "Ranked Data Zones (click to expand)",
                    fluidRow(
                      column(3, radioButtons("rank_order", "Show:",
                                             choices = c("Highest values" = "Highest",
                                                         "Lowest values" = "Lowest"),
                                             inline = TRUE)),
                      column(6, sliderInput("rank_n", "Number of areas to show:",
                                            min = 5, max = 50, value = 10, width = "100%")),
                      column(3, downloadButton("download_ranked", "Download ranked table (CSV)"),
                             style = "padding-top: 25px; text-align: right;")
                    ),
                    DTOutput("rankedTable", width = "100%")
                  )
                )
              )
            )
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
        h4("Variable Description"),
        uiOutput("varDescription2")
      ),
      column(width = 8, navset_card_underline(
        nav_panel("Scatterplot",
                  fluidRow(
                    column(9, plotlyOutput("scatterPlot", height = "400px")),
                    column(3, plotOutput("sizeLegend", height = "400px"))
                  ),
                  #plotlyOutput("scatterPlot", height = "400px"),
                  checkboxInput("addcor", "Add Pearson's correlation?", FALSE),
                  conditionalPanel(
                    condition = "input.addcor == true",
                    uiOutput("corText")
                  ),
                  downloadButton("download_scatter", "Download plot")),

        nav_panel("Hexbin",
                  fluidRow(
                     column(width = 6,
                            selectInput("covariate3","Color by:",choices = NULL)),
                     column(width = 6,
                            selectInput("hex_fun", "Summary statistic",
                            choices = c(
                              "Mean" = "mean",
                              "Median" = "median",
                              "Maximum" = "max",
                              "Minimum" = "min"),
                            selected = "mean"))
                    ),
                  helpText(
                    "Note: The hexagons represent clusters of Data Zones.
                    If you subset the data by Council area only those Data Zone clusters will appear."
                  ),
                  fluidRow(
                    column(9, plotlyOutput("HexbinPlot", height = "400px")),
                    column(3, plotOutput("HexsizeLegend", height = "400px"))
                  ),
                  downloadButton("download_hexbin", "Download plot"))
                )
            )
          )
        )
    ),

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
        selectInput("covariate4", "Variable", choices = NULL),
        checkboxInput("show_all_scotland", "Show the full map of Scotland?", FALSE),
        conditionalPanel(
          condition = "!input.show_all_scotland",
          selectInput("council_map", "Select Council area:", choices = NULL)
        ),
        selectInput("colors", "Color Scheme", rownames(subset(
          brewer.pal.info, category %in% c("seq")
        )), selected ="BuPu"),
        h4("Variable Description"),
        uiOutput("varDescription3"),
      ),
      column(8, leafletOutput(
        "my_map", width = "100%", height = 600),
      br(),
      fluidRow(
        column(6, downloadButton("download_map_jpeg", "Download map (JPEG)")),
        column(6, downloadButton("download_map_html", "Download map (HTML)"))
        ),
      ),
      h4("Independent samples t-test"),
      p("Comparing data zone values for the selected Council area and SIMD indicator"),
      downloadButton("download_ttest", "Download t-test table (CSV)"),
      DTOutput("ttest_output")
        )
      )
    )
  )
)


### ============================================================
### SERVER
### ============================================================

server <- function(input, output, session) {

  ## In progress
  # observe(session$setCurrentTheme(
  #   if (isTRUE(input$dark_mode)) dark else light
  # ))

  ## ---- Google Analytics 4 Event Tracking ----
  ## Sends anonymous usage events to GA4
  ## No personally identifiable information is collected
  ## Only tab navigation and feature usage (e.g. downloads).

  observeEvent(input$main_navbar, {
    session$sendCustomMessage("trackPageview", input$main_navbar)
  })

  observeEvent(input$download_data_2016, {
    session$sendCustomMessage("ga_event", list(event = "download_dataset", dataset = "SIMD_2016"))
  })

  observeEvent(input$download_data_2020, {
    session$sendCustomMessage("ga_event", list(event = "download_dataset", dataset = "SIMD_2020"))
  })

  observeEvent(input$download_summary, {
    dataset_label <- if (input$dataset == "data_2020") "SIMD_2020" else "SIMD_2016"
    session$sendCustomMessage("ga_event", list(
      event = "download_summary_stats",
      dataset = dataset_label,
      output_type = "summary_statistics"
    ))
  })

  observeEvent(input$download_ranked, {
    dataset_label <- if (input$dataset == "data_2020") "SIMD_2020" else "SIMD_2016"
    session$sendCustomMessage("ga_event", list(
      event = "download_ranked_table",
      dataset = dataset_label,
      output_type = "ranked_table"
    ))
  })

  ## ---- Google Analytics: Plot & Map Export Tracking ----

  observeEvent(input$download_hist, {
    session$sendCustomMessage("ga_event", list(
      event = "export_plot", dataset = input$dataset, output_type = "histogram"
    ))
  })

  observeEvent(input$download_density, {
    session$sendCustomMessage("ga_event", list(
      event = "export_plot", dataset = input$dataset, output_type = "density_plot"
    ))
  })

  observeEvent(input$download_boxplot, {
    session$sendCustomMessage("ga_event", list(
      event = "export_plot", dataset = input$dataset, output_type = "boxplot"
    ))
  })

  observeEvent(input$download_scatter, {
    session$sendCustomMessage("ga_event", list(
      event = "export_plot", dataset = input$dataset1, output_type = "scatterplot"
    ))
  })

  observeEvent(input$download_hexbin, {
    session$sendCustomMessage("ga_event", list(
      event = "export_plot", dataset = input$dataset1, output_type = "hexbin"
    ))
  })

  observeEvent(input$download_map_html, {
    session$sendCustomMessage("ga_event", list(
      event = "export_map", dataset = input$dataset2, output_type = "html"
    ))
  })

  observeEvent(input$download_map_jpeg, {
    session$sendCustomMessage("ga_event", list(
      event = "export_map", dataset = input$dataset2, output_type = "jpeg"
    ))
  })

  ## ---- Dataset selection helper ----
  # Replaces three near-identical if/else blocks with one shared function

  get_dataset <- function(choice) {
    if (choice == "data_2020") data_2020 else data_2016
  }

  datasetInput  <- reactive({ get_dataset(input$dataset) })
  datasetInput1 <- reactive({ get_dataset(input$dataset1) })
  datasetInput2 <- reactive({ get_dataset(input$dataset2) })

  ## ---- Download selected SIMD dataset ----


  output$download_2016simd <- downloadHandler(
    filename = function() "SIMD_2016_data.csv",
    content = function(file) {
      write.csv(data_2016, file, row.names = FALSE)
    }
  )

  output$download_2020simd <- downloadHandler(
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

  # Number at end -> selects variable (based on its order in df)
  observeEvent(input$dataset1, {
    req(datasetInput1())
    update_var_choices(session, datasetInput1(), "covariate1", 1)
    update_var_choices(session, datasetInput1(), "covariate2", 2)
    update_var_choices(session, datasetInput1(), "covariate3", 3)
  })

  observeEvent(input$dataset2, {
    req(datasetInput2())
    update_var_choices(session, datasetInput2(), "covariate4",3)
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

  summaryStatsData <- reactive({
    req(datasetInput(), input$selected_var, input$dataset)

    dataset_label <- if (input$dataset == "data_2020") "SIMD 2020" else "SIMD 2016"

    datasetInput() %>%
      dplyr::summarise(
        Dataset = dataset_label,
        Mean = round(mean(.data[[input$selected_var]], na.rm = TRUE), 3),
        Median = round(median(.data[[input$selected_var]], na.rm = TRUE), 3),
        `Std. Dev` = round(sd(.data[[input$selected_var]], na.rm = TRUE), 3),
        `Gini coefficient` = round(ineq::Gini(.data[[input$selected_var]]), 3)
      )
  })

  output$summaryStats <- DT::renderDataTable({
    req(summaryStatsData())

    DT::datatable(
      summaryStatsData(),
      selection = "none", rownames = FALSE,
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

  ## ---- Download table stats ----

  output$download_summary <- downloadHandler(
    filename = function() {
      year_label <- if (input$dataset == "data_2020") "2020" else "2016"
      paste0("summary_stats_", input$selected_var, "_", year_label, ".csv")
    },
    content = function(file) {
      write.csv(summaryStatsData(), file, row.names = FALSE)
    }
  )

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

  HistPlot <- reactive({
    req(datasetInput(), input$selected_var)

    p <- ggplot(datasetInput(), aes(x = .data[[input$selected_var]])) +
      geom_histogram(bins = input$bins, fill = input$bincolor, color = "black") +
      labs(x = variableLabel()) +
      theme_classic(base_size = 14) +
      theme(axis.text = element_text(size = 12, face = "bold"))

    if (input$addmean_hist) {
      mean_val <- mean(datasetInput()[[input$selected_var]], na.rm = TRUE)
      p <- p + geom_vline(xintercept = mean_val, lwd = 1, lty = 2)
    }

    p
  })

  ## ---- Download plot ----

  output$HistPlot <- renderPlot({ HistPlot() })

  output$download_hist <- downloadHandler(
    filename = function() paste0("histogram_", input$selected_var, ".jpeg"),
    content = function(file) {
      req(HistPlot())
      ggsave(file, plot = HistPlot(), width = 8, height = 5, dpi = 300)
    }
  )

  ## ---- Density plot ----

  DensityPlot <- reactive({
    req(datasetInput(), input$selected_var)

    p <- ggplot(datasetInput(), aes(x = .data[[input$selected_var]])) +
      geom_density(alpha = 0.4) +
      labs(x = variableLabel()) +
      theme_classic(base_size = 14) +
      theme(axis.text = element_text(size = 12, face = "bold"))

    if (input$addmean_density) {
      mean_val <- mean(datasetInput()[[input$selected_var]], na.rm = TRUE)
      p <- p + geom_vline(xintercept = mean_val, lwd = 1, lty = 2)
    }

    p
  })


  ## ---- Download plot ----

  output$DensityPlot <- renderPlot({ DensityPlot() })

  output$download_density <- downloadHandler(
    filename = function() paste0("density_", input$selected_var, ".jpeg"),
    content = function(file) {
      req(DensityPlot())
      ggsave(file, plot = DensityPlot(), width = 8, height = 5, dpi = 300)
    }
  )

  ## ---- Boxplot (2016 vs 2020 comparison) ----

  BoxPlot <- reactive({
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

  ## ---- Download plot ----

  output$BoxPlot <- renderPlot({ BoxPlot() })

  output$download_boxplot <- downloadHandler(
    filename = function() paste0("boxplot_", input$selected_var, ".jpeg"),
    content = function(file) {
      ggsave(file, plot = BoxPlot(), width = 8, height = 5, dpi = 300)
    }
  )

  ## ---- Ranked Data Zones ----

  rankedData <- reactive({
    req(datasetInput(), input$selected_var, input$rank_n, input$rank_order)

    df <- datasetInput() %>%
      dplyr::select(Data_Zone, Council_area, Value = all_of(input$selected_var))

    df <- if (input$rank_order == "Highest") {
      df %>% arrange(desc(Value))
    } else {
      df %>% arrange(Value)
    }

    head(df, input$rank_n)
  })

  output$rankedTable <- DT::renderDataTable({
    df <- rankedData()
    names(df)[names(df) == "Value"] <- variableLabel()

    DT::datatable(df, rownames = FALSE, options = list(dom = "t", pageLength = 50))
  })

  output$download_ranked <- downloadHandler(
    filename = function() paste0("ranked_", input$rank_order, "_", input$selected_var, ".csv"),
    content = function(file) {
      df <- rankedData()
      names(df)[names(df) == "Value"] <- variableLabel()
      write.csv(df, file, row.names = FALSE)
    }
  )

  ## ---- Variable description for Bivariate tab ---

  output$varDescription2 <- renderUI({
    req(input$covariate2)
    var_info <- var_names_combined %>% filter(Column == input$covariate2)

    tagList(
      tags$p(tags$strong("Variable:"), var_info$label),
      tags$p(tags$strong("Indicator Type:"), var_info$`Indicator type`),
      tags$p(tags$strong("Description:"), var_info$Description)
    )
  })


  ## ---- Scatterplot / Hexbin labels ----

  x_variableLabel <- reactive({
    var_names_combined$label[var_names_combined$Column == input$covariate1]
  })

  y_variableLabel <- reactive({
    var_names_combined$label[var_names_combined$Column == input$covariate2]
  })

  z_variableLabel <- reactive({
    var_names_combined$label[var_names_combined$Column == input$covariate3]
  })


  ## ---- Scatterplot ----

  scatterPlotObj <- reactive({
    req(plot_data(), input$covariate1, input$covariate2)

    ggplot(plot_data(), aes(x = .data[[input$covariate1]],
                                 y = .data[[input$covariate2]])) + #,size = Total_population
      geom_point(aes(size = Total_population,
                     text = paste0(Council_area, " (", Data_Zone, ")")),
                 alpha = 0.5, position = "jitter", colour = input$bincolor1) +
      scale_size(range = c(.1, 5), name = "Population (Thousands)") +
      geom_smooth(method = "lm", se = FALSE, color = "black",show.legend = FALSE) +
      labs(x = x_variableLabel(), y = y_variableLabel()) +
      theme_classic(base_size = 14) +
      theme(axis.text = element_text(size = 12, face = "bold")) +
      guides(size = guide_legend(
        override.aes = list(
          shape = 21,
          fill = input$bincolor1,
          colour = "black",
          alpha = 0.5)))

  })

  ## ---- Correlation output ---
  output$corText <- renderUI({

    req(input$addcor, plot_data(), input$covariate1, input$covariate2)

    cor_test <- cor.test(plot_data()[[input$covariate1]], plot_data()[[input$covariate2]], method = "pearson")
    r_val <- round(cor_test$estimate, 2)
    p_label <- if (cor_test$p.value < 0.001) "p < 0.001" else paste0("p = ", signif(cor_test$p.value, 2))

    tags$p(tags$strong("Pearson correlation: "), paste0("R = ", r_val, ", ", p_label))
  })

  # output$scatterPlot <- renderPlotly({ scatterPlotObj()})
  output$scatterPlot <- renderPlotly({ ggplotly(scatterPlotObj(), tooltip = "text") })


  ## ---- Extracting fig legend ----
  output$sizeLegend <- renderPlot({

    req(scatterPlotObj())
    legend <- cowplot::get_legend(scatterPlotObj() + theme(legend.position = "right"))
    cowplot::ggdraw(legend)
  })

  ## ---- Download plot ----
  output$download_scatter <- downloadHandler(
    filename = function() paste0("scatterplot_", input$covariate1, "_vs_", input$covariate2, ".jpeg"),
    content = function(file) {
      ggsave(file, plot = scatterPlotObj(), width = 8, height = 6, dpi = 300)
    }
  )
  ## ---- Hexbin ----

  HexbinPlotObj <- reactive({

    req(plot_data(), input$covariate1, input$covariate2,input$covariate3,input$hex_fun)

    summary_fun <- switch(input$hex_fun,mean = mean,median = median, max = max,min = min)


    ggplot(plot_data(), aes(x = .data[[input$covariate1]], y = .data[[input$covariate2]],
                            z = .data[[input$covariate3]]))+ #
      stat_summary_hex(
        aes(fill = after_stat(value)),
        bins = 30,fun = summary_fun,colour = "white",linewidth = 0.2) +
      scale_fill_viridis_c(direction = -1, name =
                             paste(tools::toTitleCase(input$hex_fun), z_variableLabel())) +
      labs(x = x_variableLabel(), y = y_variableLabel()) +
      theme_classic(base_size = 14) +
      theme(axis.text = element_text(size = 12, face = "bold"),
            legend.position = "none")


  })

  ## ---- Extracting fig legend ----
  output$HexsizeLegend <- renderPlot({

    req(HexbinPlotObj())
    legend <- cowplot::get_legend(HexbinPlotObj() + theme(legend.position = "right"))
    cowplot::ggdraw(legend)
  })

  ## ---- Donwload plot ----

  output$HexbinPlot <- renderPlotly({ ggplotly(HexbinPlotObj(), tooltip = "fill") })

  output$download_hexbin <- downloadHandler(
    filename = function() paste0("hexbin_", input$covariate1, "_vs_", input$covariate2, ".jpeg"),
    content = function(file) {
      ggsave(file, plot = HexbinPlotObj(), width = 8, height = 6, dpi = 300)
    }
  )


  ## ---- Interactive Map (Scotland-wide) ----


  observe({
    req(datasetInput2(),input$covariate4 )
    council_names <- unique(datasetInput2()$Council_area)
    updateSelectInput(session, "council_map", choices = council_names,
                      selected = "Glasgow City")

  })

  # Variable description for reference
  output$varDescription3 <- renderUI({
    req(input$covariate4)
    var_info <- var_names_combined %>% filter(Column == input$covariate4)

    tagList(
      tags$p(tags$strong("Variable:"), var_info$label),
      tags$p(tags$strong("Indicator Type:"), var_info$`Indicator type`),
      tags$p(tags$strong("Description:"), var_info$Description)
    )
  })


  variableLabel1 <- reactive({
    var_names_combined$label[var_names_combined$Column == input$covariate4]
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

    req(datasetInput2(), input$covariate4)

    map_dataset <- Scotland_local_auth2016_revised %>%
      left_join(datasetInput2(), by = "Data_Zone")

    if (!input$show_all_scotland) {

      req(input$council_map)

      map_dataset <- map_dataset %>%
        filter(Council_area == input$council_map)

    }

    map_dataset

  })

  my_map_obj <- reactive({

    req(map_data(), input$covariate4, input$colors)

    values <- map_data()[[input$covariate4]]

    pal <- colorNumeric(input$colors, map_data()[[input$covariate4]])

    leaflet(map_data()) %>%
      addProviderTiles("Esri.WorldImagery", options = tileOptions(minZoom = 6, maxZoom = 16),
                       group = "World Imagery") %>%
      addProviderTiles("Esri.WorldGrayCanvas", group = "Grey Canvas") %>%
      addProviderTiles("Stadia.StamenTonerLite", group = "Toner Lite") %>%
      addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
      addLayersControl(baseGroups = c("World Imagery", "Grey Canvas",
                                      "Toner Lite", "CartoDB")) %>%
      addPolygons(
        smoothFactor = 0.2,
        fillColor = ~pal(get(input$covariate4)),
        fillOpacity = 1,
        color = "lightblue",
        weight = 1.5,
        highlight = highlightOptions(weight = 5, color = "#666", fillOpacity = 0.35, bringToFront = FALSE),
        label = ~paste0(Data_Zone, ", ", variableLabel1(), ": ", scales::comma(get(input$covariate4))),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textOnly = TRUE, textsize = "15px", direction = "auto"
        )
      ) %>%
      addLegend("bottomright", pal = pal, values = ~get(input$covariate4),
                title = variableLabel1(), opacity = 0.75)
  })

  output$my_map <- renderLeaflet({ my_map_obj()})


  ## ---- Map exports (HTML and JPEG, separate buttons) ----

  output$download_map_html <- downloadHandler(
    filename = function() {
      dataset_label <- if (input$dataset2 == "data_2020") "SIMD_2020" else "SIMD_2016"
      paste0("map_", input$covariate4, "_", dataset_label, ".html")
    },
    content = function(file) {
      saveWidget(my_map_obj(), file)
    }
  )

  output$download_map_jpeg <- downloadHandler(
    filename = function() {
      dataset_label <- if (input$dataset2 == "data_2020") "SIMD_2020" else "SIMD_2016"
      paste0("map_", input$covariate4, "_", dataset_label,".jpeg")
    },

    content = function(file) {

      saveWidget(my_map_obj(), "temp.html", selfcontained = FALSE)
      webshot("temp.html", file ,
              cliprect = "viewport")
    }
  )


  ##  --- Welch T-Test of Independence ---

  ttestData <- reactive({
    req(input$covariate4)
    values_2016 <- data_2016[[input$covariate4]]
    values_2020 <- data_2020[[input$covariate4]]
    broom::tidy(t.test(values_2016, values_2020, var.equal = FALSE))
  })

  output$ttest_output <- renderDT({
    req(ttestData())

    DT::datatable(
      ttestData(),
      selection = "none", rownames = FALSE,
      class = "table table-primary",
      options = list(
        dom = "t", ordering = FALSE,
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().body()).addClass('table-light');",
          "}"
        )
      )
    ) %>% formatRound(columns = which(sapply(ttestData(), is.numeric)), digits = 4)
  })

  ## ---- Download t-test output ----

  output$download_ttest <- downloadHandler(
    filename = function() paste0("Welch_t-test_", input$covariate4, "_2016_vs_2020.csv"),
    content = function(file) {
      write.csv(ttestData(), file, row.names = FALSE)
    }
  )
 }

# Run the application and enjoy!
shinyApp(ui = ui, server = server)
