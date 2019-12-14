library(shinydashboard)
library(DT)
library(plotly)
library(tidyverse)

# define bip subcategories
bip_subs <- c("Average Losses (%)" = "avg_loss",
              "Beekeepers" = "beekeepers",
              "Colonies" = "colonies",
              "Exclusive Beekeepers (%)" = "exclusive_beekeepers",
              "Exclusive Colonies (%)" = "exclusive_colonies",
              "Backyard Operations" = "operation_backyard",
              "Commercial Operations" = "operation_commercial",
              "Sideline Operations" = "operation_sideline",
              "Total Losses (%)" = "total_loss")

# Read in EPEST and CDL datasets
epest_state <- read_csv("epest_state.csv")
cdl <- read_csv("cdl.csv")

dashboardPage(
  dashboardHeader(title = "Tabs"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Sources", tabName = "data_sources"),
      menuItem("EPEST Maps", tabName = "epest"),
      menuItem("EPEST National", tabName = "epest_natl"),
      menuItem("Cropland Data Layer Maps", tabName = "cdl"),
      menuItem("Cropland Data Layer Table", tabName = "cdl2"),
      menuItem("Bee Colony Losses Maps", tabName = "bip"),
      menuItem("Simple Correlation", tabName = "correlations"),
      menuItem("Multiple Linear Regression", tabName = "mlr"),
      menuItem("Random Forest", tabName = "rf"),
      menuItem("Principle Component Analysis", tabName = "pca"))),
  
  dashboardBody(
    tabItems(
      ########################################################################################
      # data information TAB
      tabItem(tabName = "data_sources",
        fluidRow(
          h2("Purpose"),
          p("This app brings in data for three sources relating to agriculture and honey bee 
            health. It allows the user to examine potential relationships between and within
            the various datasets."),
          br(),
          h2("Data Sources"),
          h3("EPEST (", em("Estimated Annual Agricultural Pesticide Use", ")")),
          p("The",
            a("Pesticide National Synthesis Project", 
              href = "https://water.usgs.gov/nawqa/pnsp/usage/maps/about.php"),
            "at the USGS provides estimates for farm pesticide usage across the continental 
            United States at the county level. There are two estimates provided, EPest-low and
            EPest-high. These differ in how unreported pesticide use for particular crops are
            handled. The low estimates assume these are zero, while the high estimate uses
            data from neighboring areas to estimate these unreported cases. Data is available
            from 1992 to 2017, although the 2017 data does not include the state of
            California."),
          br(),
          h3("CDL (", em("Cropland Data Layer", ")")),
          p("The USDA National Agricultural Statistics Service provides the",
            a("Cropland Data Layer", 
              href = "https://nassgeodata.gmu.edu/CropScape/"),
            " which uses moderate resolution satellite imagery to estimate land usage across
            the continental US. Land use categories are mainly focused on agricultural crop
            types; however, there are also categories for developed and natural land types.
            While the first year of information available is 1997, in this year only data for
            North Dakota is present. As this program progressed through the years, additional
            states were added and increased resolution and more thorough ground truthing were
            implemented. This app only incorporates data from 2008 to 2018 and is based on 
            previously complied data at the state level."),
          br(),
          h3("Honey Bee Colony Data"),
          p("The Bee Informed Partnership is a non profit dedicated to supporting beekeepers
            with goals of improving colony health and increasing colony survivorship. They 
            collect data from surveys each year beginning with winter of 2007/08 on honey
            bee colony survivorship and beekeeping operations. The data used in this app was
            scraped from their",
            a("Colony Loss Map website.",
              href = "https://research.beeinformed.org/loss-map/"),
            "While data is collected at the state level, some data is not reported if a 
            minimum number of beekeeper responses is not met. Also, annual and summer losses
            were not tracked until later in the survey."),
          br(),
          h2("Future Plans"),
          p("There are many additional items that this app could incorporate. For example,
            integrating the EPEST data with honey bee ecotoxicity endpoints could be 
            included, such as the Acute Insecticide Toxicity Load as shown below."),
          withMathJax(),
          h4("$$AITL_o = \\left( \\frac{mass\\;pesticide}{contact\\;LD50} \\right) \\times 
                   \\left( \\frac{half\\;life}{\\ln 2} \\right) \\times scaling\\;factor$$")
        )),
      
      ########################################################################################          
      # USGS EPEST data exploration TAB
      tabItem(tabName = "epest",
        fluidRow(
          column(width = 2,
            # Pick EPEST value to work with
            box(title = "EPEST choice", width = NULL, solidHeader = TRUE, status = "primary",
                radioButtons("epest_choice",
                             "Choose EPEST values to map",
                             c("High", "Low"))),
            
            # user picks which compounds to examine           
            box(title = "Compounds", width = NULL, solidHeader = TRUE, status = "primary",
                checkboxGroupInput("compounds",
                                   "Pick compounds to examine",
                                   choices = unique(epest_state$compound)))),
                
          column(width = 10,
            # display which compounds have been checked
            box(title = "Selected Pesticides", solidHeader = TRUE, status = "success",
                width = NULL, textOutput("selected_epest_cmpds")),
            
            # add in EPEST choropleths by year
            box(title = "US Map of Pesticide Usage (EPEST) by Year", solidHeader = TRUE,
                status = "success", width = NULL,
                plotOutput("epest_choropleths", height = "700px"),
                downloadButton("epest_choropleth_dl.png")),
            
            # add in bar plot of nationwide EPEST values by year           
            box(title = "Nationwide Pesticide Usage (EPEST)", width = NULL, 
                solidHeader = TRUE, status = "success",
                plotOutput("epest_us"))))),
      
      ########################################################################################          
      # USGS EPEST data exploration TAB
      tabItem(tabName = "epest_natl",
        fluidRow(
          column(width = 4,
            # Pick EPEST value to work with
            box(title = "EPEST choice", width = NULL, solidHeader = TRUE, status = "primary",
                radioButtons("epest_choice_natl",
                "Choose EPEST values to map",
                c("High", "Low"))),
                       
            # Pick years to look at
            box(title = "Years", width = NULL, solidHeader = TRUE, status = "primary", 
                sliderInput("epest_yr_natl", label = NULL,
                            min = 1992, max = 2017, value = c(1992, 2017))),
          
            # Pick minimum mass applied
            box(title = "Mass Applied Cutoff (log kg)", width = NULL, solidHeader = TRUE,
                status = "primary",
                sliderInput("epest_mass_natl",
                            "Narrow pesticides to those applied greater than:",
                            min = 0, max = 8, step = 1, value = 5)),
          
            # user picks which compounds to examine           
            box(title = "Compounds", width = NULL, solidHeader = TRUE, status = "primary",
                uiOutput("cmpd_select"))), 
          
          column(width = 8,
            #yearly summaries
            box(title = "Summaries by Year", width = NULL, solidHeader = TRUE, 
                status = "success",
                tableOutput("epest_natl_table"))))),
      
      ########################################################################################
      # Cropland Data Layer Maps Exploration Tab
      tabItem(tabName = "cdl",
        fluidRow(
          column(width = 2,
            # Pick CDL category to work with
            box(title = "Land Use Category", width = NULL, solidHeader = TRUE, 
                status = "primary",
                checkboxGroupInput("land_use",
                                   "Pick category to examine",
                                   choices = sort(unique(cdl$category))))),
                
          column(width = 10,
            # display which land use categories have been checked
            box(title = "Selected Land Use Categories", solidHeader = TRUE, status = "success",
                width = NULL, textOutput("selected_cdl_cats")),
                 
            # add in CDL choropleths by year
            box(title = "US Map of Land Usage (CDL) by Year", solidHeader = TRUE,
                status = "success", width = NULL,
                plotOutput("cdl_choropleths", height = "700px")),

            # add in bar plot of nationwide CDL values by year
            box(title = "Nationwide Land Usage (CDL)", width = NULL,
                solidHeader = TRUE, status = "success",
                plotOutput("cdl_us"))))),
      
      ########################################################################################
      # Cropland Data Layer Scroll Table Exploration Tab
      tabItem(tabName = "cdl2",
        fluidRow(
          column(width = 2,
          # Pick CDL category to work with
          box(title = "Land Use Category", width = NULL, solidHeader = TRUE, 
              status = "primary",
              checkboxGroupInput("land_use_2",
                                 "Pick category to examine",
                                 choices = sort(unique(cdl$category))))),
                
          column(width = 10,
            # display which land use categories have been checked
            box(title = "Selected Land Use Categories", solidHeader = TRUE, status = "success",
               width = NULL, 
               textOutput("selected_cdl_cats_2")),
                       
            box(title = "Selected CDL Data", solidHeader = TRUE, status = "success",
                width = NULL, 
                DTOutput("cdl_table"),
                downloadButton("select_cdl_table.csv"))))),
      
      ########################################################################################
      # Colony Losses (BIP) data explorationg tab
      tabItem(tabName = "bip",
        fluidRow(
          column(width = 2,
            # Input for which loss infomration to use
            box(title = "Span for Colony Losses", width = NULL, solidHeader = TRUE,
                status = "primary",
                radioButtons("colony_loss_span", 
                             "Pick span to examine",
                             choices = c("Annual", "Summer", "Winter"))),
            
            # Input for multi-state operations
            box(title = "Multi-State Operations", width = NULL, solidHeader = TRUE,
                status = "primary",
                radioButtons("mso", label = NULL,
                                   choices = c("Exclude", "Include"))),
            # Input for bip categories
            box(title = "Data categories", width = NULL, solidHeader = TRUE,
                status = "primary",
                radioButtons("bip_category",
                             "Choose data to examine",
                             choices = bip_subs))),
      
          column(width = 10,
            # add BIP choropleths by year
            box(title = "US Map of Colony Data by Year", solidHeader = TRUE,
                status = "success", width = NULL,
                plotOutput("bip_choropleths", height = "700px"))))),
      
      ########################################################################################
      
      tabItem(tabName = "correlations",
        fluidRow(
          column(width = 2,
            # select data source for x axis
            box(title = "X Axis", solidHeader = TRUE, status = "primary", width = NULL,
                radioButtons("simple_x_axis",
                             "Choose which dataset to use",
                             choices = c("Pesticide Use (EPEST)", 
                                         "Land Use (CDL)",
                                         "Bee Colony Data (BIP)")),
                conditionalPanel(condition = "input.simple_x_axis == 'Pesticide Use (EPEST)'",
                                 radioButtons("x_axis_value_epest", label = "Pesticide",
                                              choices = unique(epest_state$compound))),
                conditionalPanel(condition = "input.simple_x_axis == 'Land Use (CDL)'",
                                 radioButtons("x_axis_value_cdl", label = "CDL Category",
                                              choices = sort(unique(cdl$category)))),
                conditionalPanel(condition = "input.simple_x_axis == 'Bee Colony Data (BIP)'",
                                 radioButtons("x_axis_value_bip1", label = "Span for Colony Losses",
                                              choices = c("Annual", "Summer", "Winter")),
                                 radioButtons("x_axis_value_bip2", label = "Multi-State Operations",
                                              choices = c("Exclude", "Include")),
                                 radioButtons("x_axis_value_bip3", label = "Data categories",
                                              choices = bip_subs)))),
          
          column(width = 2,
            # select data source for y axis
            box(title = "Y Axis", solidHeader = TRUE, status = "primary", width = NULL,
                radioButtons("simple_y_axis",
                             "Choose which dataset to use",
                             choices = c("Pesticide Use (EPEST)", 
                                         "Land Use (CDL)",
                                         "Bee Colony Data (BIP)")),
                conditionalPanel(condition = "input.simple_y_axis == 'Pesticide Use (EPEST)'",
                                 radioButtons("y_axis_value_epest", label = "Pesticide",
                                              choices = unique(epest_state$compound))),
                conditionalPanel(condition = "input.simple_y_axis == 'Land Use (CDL)'",
                                 radioButtons("y_axis_value_cdl", label = "CDL Category",
                                              choices = sort(unique(cdl$category)))),
                conditionalPanel(condition = "input.simple_y_axis == 'Bee Colony Data (BIP)'",
                                 radioButtons("y_axis_value_bip1", label = "Span for Colony Losses",
                                              choices = c("Annual", "Summer", "Winter")),
                                 radioButtons("y_axis_value_bip2", label = "Multi-State Operations",
                                              choices = c("Exclude", "Include")),
                                 radioButtons("y_axis_value_bip3", label = "Data categories",
                                              choices = bip_subs)))),
          
          column(width = 8,
            # correlation graph
            box(title = "Simple Correlation", solidHeader = TRUE, status = "success", 
                width = NULL,
                plotlyOutput("simple_correlation")),
            
            box(title = "Plotted data", solidHeader = TRUE, status = "success",
                width = NULL,
                dataTableOutput("corr_table"))
            ))),
        
        ########################################################################################
        tabItem(tabName = "mlr",
          fluidRow(
            column(width = 2,
              # Pick EPEST level
              box(title = "EPEST choice", solidHeader = TRUE, status = "primary", width = NULL,
                  radioButtons("epest_choice_mlr",
                               "Choose EPEST values to map",
                               c("High", "Low"))),
              # Pick pesticides
              box(title = "Compounds", width = NULL, solidHeader = TRUE, status = "primary",
                  checkboxGroupInput("compounds_mlr",
                                     "Pick compounds to examine",
                                     choices = unique(epest_state$compound)))), 
            
            column(width = 2,
              # Pick land uses
              box(title = "Land Use Category", width = NULL, solidHeader = TRUE, 
                  status = "primary",
                  checkboxGroupInput("land_use_mlr",
                                     "Pick category to examine",
                                     choices = sort(unique(cdl$category))))),
            
            column(width = 2,
              # Pick bee data
              box(title = "Span for Colony Losses", width = NULL, solidHeader = TRUE,
                  status = "primary",
                  radioButtons("colony_loss_span_mlr", 
                               "Pick span to examine",
                               choices = c("Annual", "Summer", "Winter"))),
              # Input for multi-state operations
              box(title = "Multi-State Operations", width = NULL, solidHeader = TRUE,
                  status = "primary",
                  radioButtons("mso_mlr", label = NULL,
                               choices = c("Exclude", "Include"))),
              # Input for bip categories
              box(title = "Data categories", width = NULL, solidHeader = TRUE,
                  status = "primary",
                  radioButtons("bip_category_mlr",
                               "Choose data to examine",
                               choices = c("Average Losses (%)", "Total Losses (%)")))),
            
            column(width= 6,
              box(title = "Multiple Linear Regression Fit Summary", width = NULL,
                  solidHeader = TRUE, status = "success",
                  verbatimTextOutput("mlr_summary")),
              box(title = "Predicted Versus Actual Winter Loss Percentages",
                  width = NULL, solidHeader = TRUE, status = "success",
                  plotOutput("mlr_plot")))
              
              )), 
      
        ########################################################################################
        tabItem(tabName = "rf",
          fluidRow(
            column(width = 2,
              box(title = "Choose mtry value", width = NULL, solidHeader = TRUE,
                  status = "primary",
                  radioButtons("rf_mtry", label = NULL,
                               choices = 2:25))),
            
            column(width = 10,
              box(title = "Random Forest Fit Summary", width = NULL, solidHeader = TRUE,
                  status = "success",
                  verbatimTextOutput("rf_summary")),
              
              box(title = "Predicted Versus Actual Winter Loss Percentages",
                  width = NULL, solidHeader = TRUE, status = "success",
                  plotOutput("rf_plot")))
          ) # fluidRow
          ), #tabItem
          
        ########################################################################################
        tabItem(tabName = "pca",
          fluidRow(
            column(width = 2,
              # Pick EPEST level
              box(title = "EPEST choice", solidHeader = TRUE, status = "primary", width = NULL,
                  radioButtons("epest_choice_pca",
                               "Choose EPEST values to map",
                               c("High", "Low"))),
              # Pick pesticides
              box(title = "Compounds", width = NULL, solidHeader = TRUE, status = "primary",
                  checkboxGroupInput("compounds_pca",
                                     "Pick compounds to examine",
                                     choices = unique(epest_state$compound)))), 
                
            column(width = 2,
              # Pick land uses
              box(title = "Land Use Category", width = NULL, solidHeader = TRUE, 
                  status = "primary",
                  checkboxGroupInput("land_use_pca",
                                     "Pick category to examine",
                                     choices = sort(unique(cdl$category))))),
                
            column(width= 8,
              # PCA summary
              box(title = "Principal Component Analysis Summary up to PC5", width = NULL,
                  solidHeader = TRUE, status = "success",
                  verbatimTextOutput("pca_summary")),
              
              # PCA Biplot
              box(title = "PCA Biplot", width = NULL, solidHeader = TRUE,
                  status = "success",
                  plotOutput("pca_biplot", height = "800px")),
              
              # PCA Screeplots
              box(title = "PCA Screeplot", width = NULL, solidHeader = TRUE,
                  status = "success",
                  plotOutput("pca_screeplot"))))))))