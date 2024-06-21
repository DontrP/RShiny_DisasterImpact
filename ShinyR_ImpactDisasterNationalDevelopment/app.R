# Libraries ----
options(repos = c(CRAN = "https://cran.rstudio.com"))

if (!requireNamespace("rnaturalearthdata", quietly = TRUE)) {
  message("Installing rnaturalearthdata from CRAN")
  install.packages("rnaturalearthdata", repos = "https://cran.rstudio.com")
}

require(tidyverse)
require(shiny)
require(leaflet)
require(rnaturalearth)
require(countrycode)
require(sf)
require(plotly)
require(ggiraph)
require(ggpubr)
require(ggtext)
require(scales)
require(viridis)
require(RColorBrewer)
require(shinyWidgets)
require(htmltools)
require(bslib)

##############################-
# Data Preparation ----
##############################-

## Files path ----
hdiPath <- "FinalHDIData.csv"
disasterPath <- "FinalDisasterData.csv"

## Read files ----
hdiData <- read_csv(hdiPath, show_col_types = FALSE)
disasterData <- read_csv(disasterPath, show_col_types = FALSE)
# world polygon map. ref: https://rdrr.io/cran/rnaturalearth/man/ne_countries.html
worldMap <- ne_countries(scale = "medium", returnclass = "sf")
worldMap <- worldMap %>%
  select(adm0_a3 ,name_long, geometry)

## Prepare data for visualisation ----
### Aggregate all disaster statistics ----
disasterStatistics <- disasterData %>%
  group_by(iso3, Country, Year, Region) %>%
  summarise(DisasterCount = n(),
            SumTotalDeaths = sum(TotalDeaths, na.rm = TRUE),
            SumTotalAffected = sum(TotalAffected, na.rm = TRUE), .groups = "drop")

### Combine hdi and disaster statistics ----
combineData <- hdiData %>%
  left_join(disasterStatistics, by = c("iso3", "year" = "Year")) %>%
  select(-Country) %>%
  mutate(Region = ifelse(is.na(Region), countrycode(iso3, "iso3c", "un.region.name"), Region),
         SumTotalDeaths = ifelse(SumTotalDeaths == 0, 0.1, SumTotalDeaths),
         SumTotalAffected = ifelse(SumTotalAffected == 0, 0.1, SumTotalAffected))

### Define Robinson projection for leaflet ----
# ref: https://rstudio.github.io/leaflet/articles/projections.html#defining-a-custom-crs
# ref: https://stackoverflow.com/questions/31070949/leaflet-map-with-wms-and-custom-projection
# ref: https://leafletjs.com/reference.html#crs
robinsonCRS <- leafletCRS(crsClass = "L.Proj.CRS", code = "Custom:Robinson", # can't find epsg for robinson. so setup custom one
                          proj4def = '+proj=robin +lon_0=0 +datum=WGS84 +units=m +no_defs', 
                          resolutions = 1.5^(27:17))

### Set limit for indicator axis
indicatorAxisLimits <- sapply(c("hdi", "le", "eys", "mys", "gnipc"), function(indices) {
  valueRange <- range(combineData[[indices]], na.rm = TRUE)
  list(min = floor(valueRange[1]), max = ceiling(valueRange[2]))
}, simplify = FALSE)

### Set x limit for bubble plot
allValue <- c(combineData$SumTotalDeaths, combineData$SumTotalAffected)
bubbleXLimits <- range(allValue, na.rm = TRUE)
# set offset on the side
bubbleXLimits[1] <- bubbleXLimits[1] / 2.5
bubbleXLimits[2] <- bubbleXLimits[2] * 2.5

### Define indicator full name for charts ----
indicatorName <- c(
  "hdi" = "Human Development Index",
  "le" = "Life Expectancy",
  "eys" = "Expected Years of Schooling",
  "mys" = "Mean Years of Schooling",
  "gnipc" = "Gross National Income per Capita"
)

##############################-
# User Interface ----
##############################-

userInterface <- navbarPage(
  "DISASTER IMPACTS ON NATIONAL DEVELOPMENT",
  
  ## Main page (content)
  tabPanel("Main", 
           fluidPage(
             
             ## HTML styling ----
             tags$head(
               #### Styling text and background ----
               # ref: https://www.appsilon.com/post/howto-css-and-shiny
               # ref: https://shiny.posit.co/r/articles/build/packaging-javascript/
               # ref: https://shiny.posit.co/r/articles/build/tag-glossary/
               # ref: https://stackoverflow.com/questions/35778338/custom-legend-with-r-leaflet
               # ref: https://stackoverflow.com/questions/64669524/tagsstyle-applied-to-all-leaflet-maps-in-r-shiny
               tags$style(HTML(".leaflet-container {background-color: #FFFFFF !important; margin: 5px; padding: 10px;}
               hr {border : 0.5px solid #ccc; margin-top: 20px;} 
               h1 {font-family: 'Garamond', serif; font-weight: bold; text-align: center; margin-bottom: 20px;}
               h2 {font-family: 'Garamond', serif; font-weight: bold; text-align: left; margin-bottom: 10px;}
               h4 {font-family: 'Helvetica', Arial, sans-serif; font-weight: bold; font-style: italic; margin-bottom: 10px;}
               body {font-family: 'Helvetica', Arial, sans-serif; font-size: 16px; text-align: justify; line-height: 1.5; padding: 15px;}")), # end Leaflet map background
               
               #### Custom listener to link visuals ----
               # ref: https://shiny.posit.co/r/articles/build/js-send-message/
               # ref: https://unleash-shiny.rinterface.com/custom-templates-interactivity
               tags$script(HTML("
               Shiny.addCustomMessageHandler('updateHeatmapYear', function(message){
               // set variable
               var barSelectedYear = message.year;
               // elements with data id
               $('rect[data-id]').each(function() {
               // set each tile in heatmap and get data id
               var tileYear = $(this).attr('data-id');
               // if same year is triggered highlight the tile
               if (tileYear == barSelectedYear) {
               $(this).css('fill', 'blue');
               } else {$(this).css('opacity',1);}});});")) # end Custom listener to link visuals
             ), # end HTML styling
             
             ### Title panel ----
             titlePanel(h1("DISASTER IMPACTS ON NATIONAL DEVELOPMENT")), # end Title panel
             
             ### Introduction ----
             fluidRow( 
                      p("Over the years, data has shown that some countries have been trapped at specific levels of development. 
                        Despite global improvements in development indicators such as Gross National Income Per Capita (GDP Per Capita), 
                        many countries are still unable to keep up with those in higher-ranked nations. 
                        According to the United Nations Office on Disaster Risk Reduction (UNDRR) (2023), 
                        it is suggested that disasters can significantly impact the national development level. 
                        Thus, this visualisation explores the relationship between disasters and development levels, 
                        using the Human Development Index (HDI) as the measure of development. 
                        We examine how different types of disasters impact countries in terms of health, education, and economic levels. 
                        We highlight how countries at similar development levels experience similar types of disasters, 
                        contrasting with those at different development levels. 
                        The findings can shed light on disasters' critical role in hindering national development. 
                        Most importantly, policymakers can develop targeted strategies to mitigate the effects by 
                        understanding the specific impacts of different disasters. 
                        Furthermore, recognising disaster patterns among countries can also help in tailoring 
                        region-specific disaster risk reduction and resilience-building measures.")), # end Introduction
             
             # Add break line. ref: https://gist.github.com/bretonics/c4f82f3db918779fae7a4a136b02ace4
             hr(), 
             
             ### Map section ----
             fluidRow(style = "margin-top: 8px;",
               
               ##### Map text block ----
               column(4, style = "background-color: #F1F1F1;", h2("WHAT, WHEN, WHERE"),
                      h4(style = "text-align: left;", "The Overview of Disaster Occurrence Against Human Development Index"),
                      
                      p(style = "margin-top: 30px; margin-bottom: 30px;",
                        "The map visualisation on the right displays each country's HDI value and the occurrence of various disaster subgroups from 1990 to 2022. 
                        Each country is shaded based on its HDI value, ranging from low (light blue) to high (dark blue). 
                        The map features coloured dots representing different disaster subgroups: biological (dark blue), 
                        climatological (blue), geophysical (green), hydrological (bright green), industrial accidents (yellow), 
                        meteorological (orange), miscellaneous accidents (red), and transport-related (dark red)."),
                      
                      p(style = "margin-top: 30px; margin-bottom: 30px;", 
                        "The default view shows data from 1990. From this view, we can see that certain disaster subgroups were more common in specific regions. 
                        For example, in 1990, North America experienced more frequent 'Meteorological' disasters compared to South America."), 
                      
                      p(style = "margin-bottom: 150px;", 
                        "Interact with the map by hovering over areas, selecting specific disaster subgroups from the dropdown menu, 
                        or using the year slider to see changes over time. 
                        For comprehensive instructions on utilising all interactive features, please refer to the 'User's Guide' at the top of the page."),
                      
                      ), # end Map text block
               
               ##### Map display ----
               column(8,
                      ###### Map input ----
                      wellPanel(
                        ###### Map disaster input ----
                        # Picker input. ref: https://github.com/kaplanas/Shiny-Lego/blob/master/ui.R
                        pickerInput(width = "100%", "mapDisasterInput", "Disaster Subgroup:",
                                    choices = unique(disasterData$DisasterSubgroup),
                                    options = list(`actions-box` = TRUE),
                                    multiple = TRUE,
                                    selected = unique(disasterData$DisasterSubgroup)), # end Map disaster input
                        ###### Map year input ----
                        # ref: https://shiny.posit.co/r/gallery/interactive-visualizations/google-charts/
                        # ref: https://stackoverflow.com/questions/30846198/shiny-changing-slider-animation-speed
                        sliderInput("mapYearInput", "Year", min = 1990, max = 2022,
                                    value = 1990, step = 1, sep = "", ticks = TRUE, 
                                    animate = animationOptions(playButton = icon("play", "fa-2x"),
                                                               pauseButton = icon("pause", "fa-2x")), width = "100%") # end Map year input
                        ), # end Map input
                      
                      ###### Map output ----
                      h4("Global Human Development Index and Disaster Occurrences Map", style = "text-align: center; font-style: normal"),
                      fluidRow(leafletOutput("Map", height = "500px")) # end Map output
                      ) # end Map display
             ), # end Map section
             
             hr(),
             
             ### Bubble plots section ----
             fluidRow(style = "margin-top: 8px;",
               
               ##### Bubble plots text block ----
               column(4, style = "background-color: #F1F1F1;", h2("INDICES UNDER IMPACT"),
                      h4("The Effects of Disasters on Education, Health, and Economic Levels."),
                      
                      p(style = "margin-top: 30px; margin-bottom: 30px;", 
                        "Two bubble charts on the right illustrate the relationship between development measures 
                        (Overall HDI values, Life Expectancy, Expected Years of Schooling, Mean Years of Schooling, and Gross National Income per Capita) 
                        and two different disaster impacts—total deaths and total affected—across various countries from 1990 to 2022. 
                        Each chart represents countries as bubbles, scaled by disaster frequency and coloured by region. 
                        The left chart correlates total disaster-related deaths with development measures, 
                        while the right chart relates the total number of people affected (including those injured, rendered homeless, or otherwise impacted) 
                        to development measures."),
                      
                      p(style = "margin-top: 30px; margin-bottom: 30px;", 
                        "These charts provide insights into how disaster severity correlates with different development measures annually. 
                        For instance, the default view shows disaster impacts on HDI values in 1990, 
                        revealing that most disasters result in more people being affected than fatalities. 
                        Patterns can be observed, such as European countries clustering at the top of the chart, 
                        reflecting high values for development measures despite the frequency and impact of the disaster, 
                        while African countries spread at the lower end. Some countries, like India, 
                        experience more frequent and higher impacts from disasters yet still have higher measure values than some countries."),
                      
                      p(style = "margin-bottom: 30px;", 
                        "Interact with the charts to gain deeper insights by hovering over bubbles to highlight countries, 
                        selecting a country to label the bubble, or using the year slider to view changes over time. 
                        For comprehensive instructions on utilising all interactive features, please refer to the 'User's Guide' at the top of the page.")
                      
                      ), # end Bubble plots text block
               
               ##### Bubble plots Display ----
               column(8,
                      ###### Bubble inputs ----
                      wellPanel(
                        ####### Bubble indicators input ----
                        column(6, selectInput("bubbleIndicatorInput", "Indicator:",
                                              choices = c(
                                                "Human Development Index" = "hdi", 
                                                "Life Expectancy" = "le",
                                                "Expected Years of Schooling" = "eys",
                                                "Mean Years of Schooling" = "mys", 
                                                "Gross National Income per Capita" = "gnipc")
                                              )), # end Bubble indicators input
                        ####### Bubble country input ----
                        column(6, pickerInput("bubbleCountryInput", "Country:",
                                              choices = unique(combineData$country),
                                              options = list(`actions-box` = TRUE, `live-search` = TRUE),
                                              multiple = TRUE, selected = NULL
                                              )), # end Bubble country input
                        ####### Bubble year input ----
                        sliderInput("bubbleYearInput", "Year", min = 1990, max = 2022, value = 1990, step = 1,
                                    sep = "", ticks = TRUE, width = "100%",
                                    animate = animationOptions(interval = 800, 
                                                               playButton = icon("play", "fa-2x"),
                                                               pauseButton = icon("pause", "fa-2x"))) # end Bubble year input
                      ), # end bubble inputs

                      ###### Bubble plots output ----
                      fluidRow(style = "margin-top: 50px;", ggiraphOutput("bubblePlots", width ="100%", height = "600px")) # end Bubble plot output
                      ) # end Bubble plots Display
             ), # end Bubble plots section
             
             hr(),
             
             ### Bar ranking section ----
             fluidRow(
               ##### Bar ranking text block ----
               column(12, style = "background-color: #F1F1F1;", h2("RANKING RIGIDITY AND DISASTER TYPOLOGY"),
                      
                      h4("The Lack of Movement in Global Country Indices and Disaster Types"),
                      
                      fluidRow(
                      
                      column(4, 
                             p(style = "margin-top: 30px; margin-bottom: 30px;", 
                               "The bar chart below ranks countries by their development measure values, 
                               with higher development indices positioned to the left. 
                               Each bar represents a country and is color-coded by region. 
                               The default view shows HDI values from 1990, 
                               providing a baseline for comparing countries' development rankings at the start of the period. 
                               It is clear that European countries cluster on the left side, while most African countries cluster on the right. 
                               Select different measures from the dropdown menu, use the slider to progress through different years, 
                               and observe the stability in country rankings over time.")),
                      
                      column(4, 
                             p(style = "margin-top: 30px; margin-bottom: 30px;", 
                               "Beneath the bar chart, a series of detailed heatmaps offer insights into disaster types and their impact over the same timeline. 
                               These heatmaps are divided into three tabs, each focusing on different disaster-related statistics: 
                               disaster frequency, total deaths, and total affected by disaster type. 
                               Each tab contains heatmaps organised into panels for different disaster subgroups. 
                               The colour intensity in the panels varies, with darker shades indicating higher values and lighter shades indicating lower values. 
                               Empty cells signify no disaster occurrence. The default view shows global disaster frequency, 
                               highlighting that floods (a 'Hydrological' disaster) are the most frequent type, while impacts (an 'Extra-terrestrial' disaster) 
                               occurred only once in 2013.")
                             ),
                      column(4, 
                             p(style = "margin-top: 30px; margin-bottom: 30px;", 
                               "Interact with the visualisation by selecting a country from the dropdown menu to annotate the chosen country on the bar chart 
                               and adjust the heatmaps to display disaster-related statistics specific to that country. 
                               Or use the year slider’s play function to observe changes in a country’s ranking over time on the bar chart and see how 
                               disaster statistics evolve annually on the heatmap. 
                               For comprehensive instructions on utilising all interactive features, 
                               please refer to the 'User's Guide' at the top of the page.")
                             )),
                      
                      ###### Bubble indicator and country input 
                      column(3,
                             selectInput("barIndicatorInput", "Indicator: ",
                                         choices = c(
                                           "Human Development Index" = "hdi", 
                                           "Life Expectancy" = "le",
                                           "Expected Years of Schooling" = "eys",
                                           "Mean Years of Schooling" = "mys", 
                                           "Gross National Income per Capita" = "gnipc")),
                             pickerInput("barCountryInput", "Country: ",
                                         choices = c("Label none" = "", unique(combineData$country)),
                                         options = list(`live-search` = TRUE),
                                         multiple = FALSE)
                      ), # end Bubble indicator and country input
                      ###### Bubble year input
                      column(9, sliderInput("barYearInput", "Year", min = 1990, max = 2022, value = 1990, step = 1,
                                            sep = "", width = "100%", ticks = TRUE,
                                            animate = animationOptions(interval = 800, 
                                                                       playButton = icon("play", "fa-2x"),
                                                                       pauseButton = icon("pause", "fa-2x")))) # end Bubble year input
                      ), # end bar ranking text block

             ), # end Bar ranking inputs
               
               ##### Bar output ----
               column(12, plotlyOutput("barPlot", width = "100%", height = "600px")), # end Bar output
               
               ##### Heatmap output ----
               fluidRow(
                 # ref: https://shiny.posit.co/r/articles/build/tabsets/
                 navset_tab(
                   nav_panel("Frequency", girafeOutput("heatmapFreq", width = "100%")),
                   nav_panel("Deaths", girafeOutput("heatmapDeaths", width = "100%")),
                   nav_panel("Affected", girafeOutput("heatmapAffected", width = "100%"))
               ) # end Heatmaps sections
               
             ), # end Bar ranking section
           )), # end Main page (content)
  
  ## User Guide----
  # ref: https://shiny.posit.co/r/articles/build/html-ui/
  tabPanel("User's Guide",
           fluidPage(h2("GUIDE TO INTERACTIVE USAGE OF THE VISUALISATION"),
                     p("This section provides comprehensive guide to utilise interactive features in this visualisation."),
                     br(),
                     
                     ### Map guide ----
                     fluidRow(style = "margin-top: 5px; margin-bottom: 5px;",
                              column(6, imageOutput("MapUserGuide")),
                              column(6, style = "background-color: #F1F1F1;",
                                     h3("Map Visualisation", style = "font-weight: bold;"),
                                     br(),
                                     HTML("
                                     <b>Overview</b><br>
                                     This visualisation allows you to explore yearly data from 1990 to 2022, 
                                     focusing on Human Development Index (HDI) values and disaster subgroups.</br>
                                     </br>
                                     
                                     <b>1. Selecting disaster subgroup</b>
                                     <li><b>Dropdown Menu:</b> Select one or more disaster subgroups to compare their occurrences over different years.</li>
                                     <li><b>No Selection:</b> If no subgroup is selected, the map will display only the change in HDI values from 1990 to 2022.</li><br>
                                     
                                     <b>2. Using the year slider</b>
                                     <li><b>Play/Pause Button:</b> Click to automatically view the progression year by year from 1990 to 2022.</li>
                                     <li><b>Manual Slider:</b> Drag the circle button to select a specific year and explore the data for that year.</li><br>
                                     
                                     <b>3. Navigating the map</b>
                                     <li><b>Zoom In/Out:</b> Use the zoom controls to get a closer or broader view of the map.</li>
                                     <li><b>Pan:</b> Click and drag to move the map and focus on specific regions or countries.</li><br>
                                     
                                     <b>4. Interacting with the map</b>
                                     <li><b>Hover:</b> Hover over a country or a disaster circle marker to see detailed information about the country’s HDI or specific disaster occurrences.</li>"),
                                     p(style = "margin-bottom: 20px;"))
                              ), # end Map Guide
                     
                     br(),
                     
                     ### Bubble user guide ----
                     fluidRow(style = "margin-top: 5px; margin-bottom: 5px;",
                              column(6, imageOutput("BubbleUserGuide")),
                              column(6, style = "background-color: #F1F1F1;",
                                     h3("Bubble Chart Visualisation", style = "font-weight: bold;"),
                                     br(),
                                     HTML("
                                     <b>Overview</b><br>
                                     This visualisation tool allows you to explore the relationship between disaster impacts 
                                     and various Human Development Index (HDI) subindices from 1990 to 2022.</br>
                                     </br>
                                     
                                     <b>1. Selecting elements from dropdown menus</b>
                                     <li><b>Country Selection:</b> Choose one or more countries to label and track their progression across years. 
                                     Note that some countries might not appear in certain years if there are no disaster occurrences.</li>
                                     <li><b>Indicator Selection:</b> Select indicators to view the relationship between disaster impacts and HDI subindices, such as Life Expectancy, 
                                          Expected Years of Schooling, Mean Years of Schooling, Gross National Income per Capita.</li></br>
                                          
                                     <b>2. Using the year slider</b>
                                     <li><b>Play/Pause Button:</b> Press to automatically see the yearly progression from 1990 to 2022.</li>
                                     <li><b>Manual Slider:</b> Drag the circle button to select a specific year and explore the data for that year.</li><br>
                                    
                                     <b>3. Zooming and panning</b>
                                     <li><b>Zoom In:</b> Use the ‘zoom with rectangle’ option at the top of the charts to focus on specific areas.</li>
                                     <li><b>Reset Zoom:</b> Click the ‘reset pan/zoom’ option at the top of the charts to return to the original view.</li><br>
                                     
                                     <b>4. Interacting with bubbles</b>
                                     <li><b>Hover:</b> Hover over a bubble to temporarily highlight the country and reveal detailed information about its indicator values and disaster impacts.</li>
                                     <li><b>Click:</b> Click on a bubble to emphasise/de-emphasise it.</li>
                                     <li><b>Lasso Selection:</b> Use the lasso selection tool at the top of the charts to select multiple bubbles.</li>

                                     "),
                                     p(style = "margin-bottom: 30px;"))
                              ), # end Bubble User guide
                     
                     br(),
                     
                     ### Bar and heatmap user guide ----
                     fluidRow(style = "margin-top: 5px; margin-bottom: 5px;",
                              column(6, imageOutput("BarUserGuide"), imageOutput("HeatmapsUserGuide")),
                              column(6, style = "background-color: #F1F1F1;",
                                     h3("Bar and Heatmap Chart Visualisation", style = "font-weight: bold;"),
                                     br(),
                                     HTML("
                                     <b>Overview</b><br>
                                     This visualisation tool allows you to compare countries' development level and explore detailed information on 
                                     disaster types each country experienced.</br>
                                     </br>
                                                                          
                                     <b>1. Selecting elements from dropdown menus</b>
                                     <li><b>Indicator Selection:</b> Select indicators to change the information on the bar to different HDI indices, such as Life Expectancy, 
                                          Expected Years of Schooling, Mean Years of Schooling, and Gross National Income per Capita.</li>
                                     <li><b>Country Selection:</b> Choose a country to label it on the bar and track their progression across years, and change the heatmap view 
                                     to show disaster statistics of selected country.</li><br>
                                     
                                     <b>2. Using the year slider</b>
                                     <li><b>Play/Pause Button:</b> Press to automatically see the yearly progression from 1990 to 2022 on the bar chart and emphasise heatmap's tiles
                                     related to specific year.</li>
                                     <li><b>Manual Slider:</b> Drag the circle button to select a specific year and explore the data for that year.</li><br>
                                     
                                     <b>3. Downloading the bar chart</b>
                                     <li><b>Download:</b> Use download tool at the top-right of the chart to download the chart as PNG files.</li><br>
                                     
                                     <b>4. Interacting with bar chart</b>
                                     <li><b>Hover:</b> Hover over a bubble to reveal detailed information about a country and indicator's value.</li>
                                     <li><b>Click/Double click:</b> Click on the color legend to de-emphasise the countries of the region, or double click to emphasise them</li><br>
                                     
                                     <b>5. Clicking on heatmaps tabs</b>
                                     <li><b>Tabs:</b> Select different tabs to see different disaster statistics.</li><br>
                                     
                                     
                                     <b>6. Interacting with heatmap chart</b>
                                     <li><b>Hover:</b> Hover over a tile to reveal detailed information about disaster statistics and temporarily emphasise all tiles in the same year.</li>
                                     <li><b>Click:</b> Click on the tile to emphasise/de-emphasise all tiles in the same year.</li>
                                     <li><b>Lasso Selection:</b> Use the lasso selection tool at the top-right of the charts to select emphasise multiple tiles.</li>
                                     <li><b>Download:</b> Use download tool at the top-right of the chart to download the chart as PNG files.</li>
                                     "),
                                     p(style = "margin-bottom: 30px;"))
                     ), # end Bar and heatmap user guide
                     
           )), # end user guide page
  
  ## About ----
  tabPanel("About",
           fluidPage(h2("ABOUT THIS VISUALISATION"),
                     column(6,
                       p(style = "margin-top: 20px; margin-bottom: 20px;",
                       "The visualisation utilises three datasets, including: "),
                     HTML("
                          <li><b>The Emergency Events Data (EM-DAT Data)</b> was developed by the Centre for Research on the Epidemiology of Disasters (CRED) in 1988.
                          The data provides a detailed record of mass disasters and their impacts on health and economy at a national level, 
                          including information on over 26,000 disasters worldwide from 1900 to today, sourced from various credible agencies.
                          The version used in this visualisation is updated at the beginning of 2024 (version 2024.02). 
                          The latest version (version 2024.03) can be accessed directly from <a href = 'https://www.emdat.be'>EM-DAT website</a>.</li><br>
                          
                          <li><b>Human Development Index (HDI) Data</b>, derived from the UNDP’s Human Development Report 2023-24, 
                          encompasses comprehensive annual metrics from 1990 through 2022, detailing human development indices for 205 countries.
                          This dataset, updated yearly, includes a variety of data types structured in tabular form, 
                          featuring over 1,000 columns. This dataset serves as a critical resource for analyzing trends in human development across various dimensions and 
                          is essential for research aimed at exploring specific developmental questions. The data is accessible publicly 
                          from <a href = 'https://hdr.undp.org/data-center/human-development-index#/indicies/HDI'> UNDP's Human Development Index website</a>.</li><br>
                          
                          <li><b>Global Administrative Areas (GeoPackage) Version 4.1</b>, last updated in 2022, comprises detailed spatial data covering 254 nations with a total of 356,508 records. 
                          The dataset includes precise geographical boundaries for administrative areas represented as polygons or multipolygons. 
                          It systematically organises each country's data hierarchically from national boundaries (level 0) down to smaller administrative divisions like provinces or districts (up to level 5). 
                          The dataset is publicly accessible and can be downloaded directly from <a href='https://gadm.org/data.html'>Global Administrative Areas Database</a>.</li>")),
                     column(6, 
                            p(style = "margin-top: 20px; margin-bottom: 20px;",
                              HTML("<b>Developer:</b><br>Don Piumsuwan")))
                     ) 
           ) # end about page
  
  ) # end navbarPage


##############################-
# Server ----
##############################-  

server <- function(input, output, session) {
  
  ## Map ----
  output$Map <- renderLeaflet({
    
    ### Input ----
    yearInput <- as.character(input$mapYearInput)
    disasterInput <- input$mapDisasterInput
    
    # Get HDI input
    inputHDIData <- hdiData %>%
      filter(year == yearInput) %>%
      select(iso3, country, hdi, year)
    
    # Get disaster input
    # If select all
    if (length(disasterInput) == length(unique(disasterData$DisasterSubgroup))) {
      inputDisasterData <- disasterData %>% filter(Year == yearInput, !is.na(CentralLatitude), !is.na(CentralLongitude))
    } else { # If select
      inputDisasterData <- disasterData %>% filter(Year == yearInput, 
                                                DisasterSubgroup %in% disasterInput, 
                                                !is.na(CentralLatitude), !is.na(CentralLongitude))}

    
    # Get map
    mapData <- left_join(worldMap, inputHDIData, by = c("adm0_a3" = "iso3"))
    
    ### Color and label ----
    # Set choropleth colour. ref: https://rstudio.github.io/leaflet/articles/colors.html
    choroplethPal <- colorBin("Blues", domain = mapData$hdi, bins = seq(0.0, 1.0, by = 0.2), na.color = "darkgrey", pretty = TRUE)
    
    # Set dot colour
    dotPal <- colorFactor(palette = viridis(length(unique(disasterData$DisasterSubgroup)), option = "H"), 
                          domain = unique(disasterData$DisasterSubgroup))
    
    # Set choropleth label
    # ref: https://www.drdataking.com/post/how-to-add-multiple-lines-label-on-a-leaflet-map/
    # ref: https://rstudio.github.io/leaflet/articles/popups.html
    hdiLabel <- ~ifelse(is.na(hdi),
                        paste("<div style = 'background-color: lightgrey; padding: 5px; font-size: 12px;'>",
                              "<b>Country: </b>", name_long,
                              "<br><b>HDI Value: </b> No HDI information available.",
                              "</div>"),
                        paste("<div style = 'background-color: lightblue; padding: 5px; box-shadow: 3px 3px rgba(0,0,0,0.25); font-size: 12px;'>",
                              "<b>Country: </b>", country,
                              "<br><b>HDI Value: </b>", round(hdi, 2),
                              "<br><b>Year: </b>", year,
                              "</div>")) %>% lapply(HTML)
    
    # Set disaster label
    # ref: https://www.w3schools.com/tags/tag_wbr.asp
    # ref: https://developer.mozilla.org/en-US/docs/Web/CSS/overflow-wrap
    wrapLocationText <- function(text) {
      text <- gsub(",", ",<wbr>", text)
      text <- gsub(";", ";<wbr>", text)
      text <- gsub(":", ":<wbr>", text)
      text <- gsub("\\(", "<wbr>(", text)
      return(text)
    }
    disasterLabel <- ~paste("<div style = 'background-color: lightyellow; padding: 5px; box-shadow: 3px 3px rgba(0,0,0,0.25); font-size: 12px; max-width: 800px; 
                            overflow-wrap: break-word; hyphens: auto; text-align: left;'>", 
                            "<b>Disaster Subgroup: </b>", DisasterSubgroup,
                            "<br><b>Disaster Type: </b>", DisasterType,
                            "<br><b>Year: </b>", Year, 
                            "<br><b>Country: </b>", Country,
                            "<br><b>Location: </b>", wrapLocationText(Location),
                            "</div>") %>% lapply(HTML)

    ### Create Map ----
    leafletMap <- leaflet(mapData, options = leafletOptions(crs = robinsonCRS)) %>%
      setView(lng = 30, lat = -20, zoom = 0) %>%
      addScaleBar(position = "bottomleft") %>%
      ##### Add choropleth ----
      # ref: https://rstudio.github.io/leaflet/articles/choropleths.html
      addPolygons(fillColor = ~choroplethPal(hdi), fillOpacity = 1, color = "darkgrey", weight = 1,
                  # Add label. ref: https://rstudio.github.io/leaflet/articles/popups.html
                  label = hdiLabel, labelOptions = labelOptions(direction = "auto"),
                  # Add highlight when hover
                  highlightOptions = highlightOptions(weight = 5, color = "yellow", fillColor = "gold", fillOpacity = 0.7, bringToFront = FALSE))
    
    
    ##### Add dot ----
    if (nrow(inputDisasterData) > 0) {
      leafletMap <- leafletMap %>%
        addCircleMarkers(data = inputDisasterData, lat = ~CentralLatitude, lng = ~CentralLongitude,
                         radius = 3, fillOpacity = 1, stroke = TRUE, color = "white", weight = 1.2, fillColor = ~dotPal(DisasterSubgroup),
                         label = disasterLabel, labelOptions = labelOptions(direction = "auto"))}
    
    ##### Add legends ----
    # ref: https://rstudio.github.io/leaflet/reference/addLegend.html
    # ref: https://rdrr.io/github/tomroh/leaflegend/man/addLeafLegends.html
    leafletMap <- leafletMap %>%
      
      addLegend("bottomright", pal = dotPal, values = ~inputDisasterData$DisasterSubgroup,
                title = "<div style='font-size: 14px;'>Disaster Subgroup</div>",
                labFormat = labelFormat(prefix = "<span style='font-size: 12px;'>", suffix = "</span>")) %>%
      
      addLegend("topright", pal = choroplethPal, values = ~hdi[!is.na(hdi)], 
                title = "<div style='font-size: 14px;'>HDI Value</div>",
                labFormat = labelFormat(prefix = "<span style='font-size: 12px;'>", suffix = "</span>"))
    
    leafletMap
  }) # end Map
  
  ## Bubble plots ----
  # http://davidgohel.github.io/ggiraph/
  # https://www.ardata.fr/ggiraph-book/starting.html
  # https://rpubs.com/durraniu/interactive_plots
  # https://sean.rbind.io/posts/2019-05-08-dashboards/
  
  output$bubblePlots <- renderggiraph({
    
    ### Inputs ----
    yearInput <- as.character(input$bubbleYearInput)
    countryInput <- input$bubbleCountryInput
    indicatorInput <- input$bubbleIndicatorInput
    indicatorFullName <- indicatorName[[indicatorInput]]
    
    # Data by input
    bubbleData <- combineData %>%
      filter(year == yearInput, !is.na(DisasterCount), DisasterCount > 0) %>%
      select(country, year, hdi, le, eys, mys, gnipc, Region, DisasterCount, SumTotalDeaths, SumTotalAffected)
    
    # Set region color
    regionColor <- brewer.pal(min(5, length(unique(combineData$Region))), "Set1")
    
    # Set share circle size
    circleMin <- min(combineData$DisasterCount, na.rm = TRUE)
    circleMax <- max(combineData$DisasterCount, na.rm = TRUE)
    
    ### Tooltips ----
    bubbleTooltipDeaths <- paste("<b>Country:</b>", bubbleData$country,
                                 "<br><b>Year:</b>", bubbleData$year,
                                 "<br><b>Total Deaths:</b>", comma(floor(bubbleData$SumTotalDeaths)),
                                 "<br><b>Indicator Value:</b>", round(bubbleData[[indicatorInput]], 2),
                                 "<br><b>Total Disaster Frequency:</b>", comma(bubbleData$DisasterCount))

    bubbleTooltipAffected <- paste("<b>Country:</b>", bubbleData$country,
                                   "<br><b>Year:</b>", bubbleData$year,
                                   "<br><b>Total Affected:</b>", comma(floor(bubbleData$SumTotalAffected)),
                                   "<br><b>Indicator Value:</b>", round(bubbleData[[indicatorInput]], 2),
                                   "<br><b>Total Disaster Frequency:</b>", comma(bubbleData$DisasterCount))

    ### Create bubble death plots ----
    bubbleDeaths <- ggplot(bubbleData, aes(x = SumTotalDeaths, y = .data[[indicatorInput]], color = Region, size = DisasterCount, alpha = 0.95)) +
      geom_point_interactive(aes(data_id = country, tooltip = bubbleTooltipDeaths)) +
      scale_color_manual(values = regionColor) +
      scale_x_log10(limits = bubbleXLimits,
                    breaks = c(0, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000), 
                    labels = c("0", "1", "10", "100", "1k", "10k", "100k", "1M", "10M", "100M")) +
      scale_y_continuous(limits = c(indicatorAxisLimits[[indicatorInput]]$min, indicatorAxisLimits[[indicatorInput]]$max)) +
      scale_size_continuous(range = c(1,5),limits = c(circleMin, circleMax), breaks = c(0, 20, 40, 60, 80, 100)) +
      labs(title = paste("<span style='color: darkred;'>Total Deaths</span> vs.<br>", "<span style='color: navy;'>", indicatorFullName, "</span>"),
           x = "Total Deaths",
           y = indicatorFullName, 
           size = "Disaster Frequency") +
      guides(alpha = "none") +
      theme_minimal() +
      theme(legend.position = "none",
            plot.title = element_markdown(size = 9.5, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 7, face = "bold", color = "black"),
            axis.text = element_text(size = 7, color = "black"),
            legend.text = element_text(size = 7, color = "black"), 
            legend.title = element_text(size = 7, face = "bold", color = "black"),
            legend.key.size = unit(0.7, "cm"),
            legend.box = "vertical")
    
    # Add linked annotations
    if (length(countryInput) > 0) {
      bubbleDeaths <- bubbleDeaths + 
        geom_text_interactive(aes(label = ifelse(country %in% countryInput, country, "")),
                              color = "black", fontface = "bold", size = 2.5, hjust = 0.5, vjust = -0.8, alpha = 1)
    }

    ### Create bubble affected plots ----
    bubbleAffected <- ggplot(bubbleData, aes(x = SumTotalAffected, y = .data[[indicatorInput]], color = Region, size = DisasterCount, alpha = 0.95)) +
      geom_point_interactive(aes(data_id = country, tooltip = bubbleTooltipAffected)) +
      scale_color_manual(values = regionColor) +
      scale_x_log10(limits = bubbleXLimits,
                    breaks = c(0, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000), 
                    labels = c("0", "1", "10", "100", "1k", "10k", "100k", "1M", "10M", "100M")) +
      scale_y_continuous(limits = c(indicatorAxisLimits[[indicatorInput]]$min, indicatorAxisLimits[[indicatorInput]]$max)) +
      scale_size_continuous(range = c(1,5), limits = c(circleMin, circleMax), breaks = c(0, 20, 40, 60, 80, 100)) +
      labs(title = paste("<span style='color: darkred;'>Total Affected</span> vs.<br>", "<span style='color: navy;'>", indicatorFullName, "</span>"),
           x = "Total Affected",
           y = indicatorFullName,
           size = "Disaster Frequency") +
      guides(alpha = "none") +
      theme_minimal() +
      theme(legend.position = "none",
            plot.title = element_markdown(size = 9.5, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 7, face = "bold", color = "black"),
            axis.text = element_text(size = 7, color = "black"),
            legend.text = element_text(size = 7, color = "black"), 
            legend.title = element_text(size = 7, face = "bold", color = "black"),
            legend.key.size = unit(0.7, "cm"),
            legend.box = "vertical")
    
    # Add linked annotations
    if (length(countryInput) > 0) {
      bubbleAffected <- bubbleAffected +
        geom_text_interactive(aes(label = ifelse(country %in% countryInput, country, "")),
                              color = "black", fontface = "bold", size = 2.5, hjust = 0.5, vjust = -0.8, alpha = 1)
    }
    
    ### Combine the bubble plots ----
    # ref: https://rpkgs.datanovia.com/ggpubr/reference/ggarrange.html
    # ref: https://davidgohel.github.io/ggiraph/reference/ggiraph.html
    # ref: https://www.ardata.fr/ggiraph-book/customize.html
    # ref: https://albert-rapp.de/posts/ggplot2-tips/17_ggiraph/17_ggiraph.html
    combinedBubblePlot <- ggiraph(ggobj = ggarrange(bubbleDeaths, bubbleAffected, ncol = 2, nrow = 1, 
                                              common.legend = TRUE, legend = "bottom"),width_svg = 7, height_svg = 4.5, 
                                  options = list(opts_hover(css = ""),
                                                 opts_hover_inv(css = "opacity:0.4;"),
                                                 opts_sizing(rescale = FALSE),
                                                 opts_toolbar(fixed = TRUE)),
                            # ref: https://stackoverflow.com/questions/46259947/is-there-a-way-to-preselect-points-in-ggiraph-r-shiny
                            hover_css = "fill-opacity: 1; r: 8px; stroke: black; stroke-width:2px;",
                            zoom_max = 3, selected_css = "fill-opacity: 1; stroke: black; stroke-width:2px;")
    
    return(combinedBubblePlot)

  }) # end Bubble plots
  
  
  ## Bar plots ----
  output$barPlot <- renderPlotly({

    ### Inputs ----
    yearInput = input$barYearInput
    indicatorInput = input$barIndicatorInput
    countryInput = input$barCountryInput
    indicatorFullName <- indicatorName[[indicatorInput]]


    # Get data by input
    barData <- combineData %>%
      filter(year == yearInput) %>%
      select(country, year, Region, indicatorInput) %>%
      na.omit() %>%
      distinct(country, .keep_all = TRUE) %>%
      arrange(.data[[indicatorInput]]) %>%
      mutate(rank = rev(row_number()),
             country = factor(country, levels = rev(country)))

    # Set color for region
    regionColor <- brewer.pal(min(5, length(unique(combineData$Region))), "Set1")
    
    # Set tooltip
    barTooltip <- paste("<b>Country: </b>", barData$country,
                         "<br><b>Region: </b>", barData$Region,
                         "<br><b>Rank: </b>", barData$rank,
                         "<br><b>Year: </b>", barData$year,
                         "<br><b>Value: </b>", comma(round(barData[[indicatorInput]], 2)))
    
    # Set ylimit 
    yLimits <- indicatorAxisLimits[[indicatorInput]]
    
    ### Create bar plot ----
    barRankingPlot <- plot_ly(barData, x = ~country, y = ~.data[[indicatorInput]], type = "bar", orientation = "v", color = ~Region, colors = regionColor,
                              text = barTooltip,
                              hoverinfo = "text") %>%
      layout(title = paste("<b style='color: black;'>Countries Ranked by</b> <b style='color: navy;'>", 
                            indicatorFullName, "</b> <b style='color: black;'>in</b> <b style='color: darkgreen;'>", yearInput, "</b>"),
             yaxis = list(title = indicatorFullName,
                          titlefont = list(size = 14, color = "black", face = "bold"),
                          range = c(yLimits$min, yLimits$max),
                          tickfont = list(size = 10, color = "black", face = "bold"),
                          fixedrange = TRUE),
             xaxis = list(title = "", automargin = TRUE, tickangle = -90, tickmode = "linear", ticklen = 5, tickfont = list(size = 9, color = "black")),
             margin = list(t=100),
             legend = list(title = "Region", orientation = "h", x = 0.5, y = 1.05, xanchor = "center", yanchor = "bottom"),
             dragmode = FALSE, hovermode = "closest") %>%
      # Restrict user interaction
      # ref: https://plotly.com/r/configuration-options/
      config(modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", 
                                        "hoverClosestCartesian", "hoverCompareCartesian"))
      # Annotate the bar with arrow to highlight country basesd on user input.
      # ref: https://plotly.com/r/text-and-annotations/
    if (!is.null(countryInput) && countryInput != "" && countryInput %in% barData$country) {
        maxIndicatorValue <- max(barData[[indicatorInput]][barData$country == countryInput], na.rm = TRUE) * 1
        barRankingPlot <- barRankingPlot %>%
          layout(annotations = list(x = countryInput, y = maxIndicatorValue, xref = "x", yref = "y",
            text = paste0(countryInput, "<br>Rank: ",barData$rank[barData$country == countryInput]), 
            showarrow = TRUE, arrowhead = 1, arrowsize = 1, ax = 0, ay = -25, font = list(size = 12, family = "Helvetica")
          ))
      }
    
    return(barRankingPlot)
  
  }) # end Bar plots

  # Add observe to link with heatmap
  observeEvent(input$barYearInput, {
    session$sendCustomMessage(type = 'updateHeatmapYear', message = list(year = input$barYearInput))
  })
  
  ## Heatmaps Freq ----
  # https://albert-rapp.de/posts/ggplot2-tips/17_ggiraph/17_ggiraph.html
  # https://stackoverflow.com/questions/76419893/interactive-hover-effects-of-adjacent-giraffe-graphs-not-synced-when-using-patch
  output$heatmapFreq <- renderGirafe({
    
    ### Inputs ----
    countryInput <- input$barCountryInput
    yearInput <- input$barYearInput
    
    # Freq data
    heatmapFreqData <- disasterData %>%
      filter(if(!is.null(countryInput) && nchar(countryInput) > 0) Country == countryInput else TRUE) %>%
      group_by(Year, DisasterSubgroup, DisasterType) %>%
      filter(Year <= 2022) %>%
      summarise(Count = n(), .groups = "drop") %>%
      # Add color to panel title
      mutate(Year = factor(Year), 
             DisasterSubgroup = case_when(
               DisasterSubgroup == "Biological" ~ "<span style='color: #30123B; font-weight: bold; font-size: 8px;'>Biological</span>",
               DisasterSubgroup == "Climatological" ~ "<span style='color: #414CCA; font-weight: bold; font-size: 8px;'>Climatological</span>",
               DisasterSubgroup == "Extra-terrestrial" ~ "<span style='color: #2B82B2; font-weight: bold; font-size: 8px;'>Extra-terrestrial</span>",
               DisasterSubgroup == "Geophysical" ~ "<span style='color: #1B9E78; font-weight: bold; font-size: 8px;'>Geophysical</span>",
               DisasterSubgroup == "Hydrological" ~ "<span style='color: #86BD44; font-weight: bold; font-size: 8px;'>Hydrological</span>",
               DisasterSubgroup == "Industrial accident" ~ "<span style='color: #F7D032; font-weight: bold; font-size: 8px;'>Industrial accident</span>",
               DisasterSubgroup == "Meteorological" ~ "<span style='color: #F39C19; font-weight: bold; font-size: 8px;'>Meteorological</span>",
               DisasterSubgroup == "Miscellaneous accident" ~ "<span style='color: #E36232; font-weight: bold; font-size: 8px;'>Miscellaneous accident</span>",
               DisasterSubgroup == "Transport" ~ "<span style='color: #BA202C; font-weight: bold; font-size: 8px;'>Transport</span>",
               TRUE ~ paste("<span style='color: lightgrey;'>", DisasterSubgroup, "</span>")
             ),
             DisasterTypeWrap = str_wrap(DisasterType, width = 19))

    
    #### Set title ----
    freqTitle <- ifelse(is.null(countryInput) || countryInput == "", 
                        paste("<span style='color: darkred; font-family: Arial;'>Disaster Frequency</span> by Type and Year in <span style='color: turquoise4;'> All Countries</span>"), 
                        paste("<span style='color: darkred; font-family: Arial;'>Disaster Frequency</span> by Type and Year in", "<span style='color: turquoise4;'>", countryInput, "</span>"))
    
    #### Set tooltip ----
    heatmapFreqTooltip = paste("<b>Year:</b>", heatmapFreqData$Year,
                               "<br><b>Disaster Type:</b>", heatmapFreqData$DisasterType, 
                               "<br><b>Number of Occurrences:</b>", comma(heatmapFreqData$Count))
    
    heatmapFreqData$Year <- as.numeric(as.character(heatmapFreqData$Year))
    
    ### Create freq heatmap
    freqHeatmap <- ggplot(heatmapFreqData, aes(x = Year, y = DisasterTypeWrap, fill = Count, tooltip = heatmapFreqTooltip, data_id = Year)) +
      geom_tile_interactive() + 
      scale_fill_viridis_c(option = "F", direction = -1, begin = 0.1, end = 1, labels = comma) +
      scale_x_continuous(breaks = seq(1990, 2022, by = 2)) +
      labs(title = freqTitle, fill = "Disaster Frequency") +
      theme_dark() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5.2, color = "black", vjust = 0.5),
            axis.text.y = element_text(hjust = 1, size = 5, color = "black"),
            axis.title = element_blank(),
            axis.ticks = element_line(color = "black", size = 0.3),
            legend.title = element_text(size = 6, face = "bold"),  
            legend.text = element_text(size = 6),   
            legend.key.size = unit(1, "lines"),
            legend.key.width = unit(3, "lines"),
            legend.position = "bottom",
            plot.title = element_markdown(size = 8, face = "bold", hjust = 0.5),
            strip.background = element_rect(fill = "#F4F4F4", color = "#F4F4F4"),
            strip.text.x = element_markdown(face = "bold")) +
      facet_wrap(~ DisasterSubgroup, scales = "free_y", ncol=3)
    
    # Create interactive ggiraph object
    # https://davidgohel.github.io/ggiraph/reference/index.html
    girafe(ggobj =freqHeatmap, width_svg = 8, height_svg = 4.2, 
           options = list(opts_hover(css = ""), opts_hover_inv(css = "opacity:0.05;"), opts_sizing(rescale = TRUE), opts_toolbar(fixed = TRUE)))
  }) # end heatmap freq
  
  ## Heatmaps Deaths ----
  output$heatmapDeaths <- renderGirafe({
    
    ### Inputs ----
    countryInput <- input$barCountryInput
    yearInput <- input$barYearInput
    
    # Freq data
    heatmapDeathData <- disasterData %>%
      filter(if(!is.null(countryInput) && nchar(countryInput) > 0) Country == countryInput else TRUE) %>%
      group_by(Year, DisasterSubgroup, DisasterType) %>%
      filter(Year <= 2022) %>%
      summarise(SumTotalDeaths = sum(TotalDeaths, na.rm = TRUE), .groups = "drop") %>%
      mutate(Year = factor(Year), 
             DisasterSubgroup = case_when(
               DisasterSubgroup == "Biological" ~ "<span style='color: #30123B; font-weight: bold; font-size: 8px;'>Biological</span>",
               DisasterSubgroup == "Climatological" ~ "<span style='color: #414CCA; font-weight: bold; font-size: 8px;'>Climatological</span>",
               DisasterSubgroup == "Extra-terrestrial" ~ "<span style='color: #2B82B2; font-weight: bold; font-size: 8px;'>Extra-terrestrial</span>",
               DisasterSubgroup == "Geophysical" ~ "<span style='color: #1B9E78; font-weight: bold; font-size: 8px;'>Geophysical</span>",
               DisasterSubgroup == "Hydrological" ~ "<span style='color: #86BD44; font-weight: bold; font-size: 8px;'>Hydrological</span>",
               DisasterSubgroup == "Industrial accident" ~ "<span style='color: #F7D032; font-weight: bold; font-size: 8px;'>Industrial accident</span>",
               DisasterSubgroup == "Meteorological" ~ "<span style='color: #F39C19; font-weight: bold; font-size: 8px;'>Meteorological</span>",
               DisasterSubgroup == "Miscellaneous accident" ~ "<span style='color: #E36232; font-weight: bold; font-size: 8px;'>Miscellaneous accident</span>",
               DisasterSubgroup == "Transport" ~ "<span style='color: #BA202C; font-weight: bold; font-size: 8px;'>Transport</span>",
               TRUE ~ paste("<span style='color: lightgrey;'>", DisasterSubgroup, "</span>")
             ),
             DisasterTypeWrap = str_wrap(DisasterType, width = 19))
    
    
    #### Set title ----
    deathTitle <- ifelse(is.null(countryInput) || countryInput == "", 
                        paste("<span style='color: darkred; font-family: Arial;'>Total Deaths</span> by Disaster Type and Year in <span style='color: turquoise4;'> All Countries</span>"), 
                        paste("<span style='color: darkred; font-family: Arial;'>Total Deaths</span> by Disaster Type and Year in", "<span style='color: turquoise4;'>", countryInput, "</span>"))
    
    #### Set tooltip ----
    heatmapDeathTooltip = paste("<b>Year:</b>", heatmapDeathData$Year,
                               "<br><b>Disaster Type:</b>", heatmapDeathData$DisasterType, 
                               "<br><b>Number of Deaths:</b>", comma(heatmapDeathData$SumTotalDeaths))
    
    heatmapDeathData$Year <- as.numeric(as.character(heatmapDeathData$Year))
    
    ### Create freq heatmap
    deathHeatmap <- ggplot(heatmapDeathData, aes(x = Year, y = DisasterTypeWrap, fill = SumTotalDeaths, tooltip = heatmapDeathTooltip, data_id = Year)) +
      geom_tile_interactive() + 
      scale_fill_viridis_c(option = "F", direction = -1, begin = 0.1, end = 1, labels = comma) +
      scale_x_continuous(breaks = seq(1990, 2022, by = 2)) +
      labs(title = deathTitle, fill = "Number of Deaths") +
      theme_dark() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5.2, color = "black", vjust = 0.5),
            axis.text.y = element_text(hjust = 1, size = 5, color = "black"),
            axis.title = element_blank(),
            axis.ticks = element_line(color = "black", size = 0.3),
            legend.title = element_text(size = 6, face = "bold"),  
            legend.text = element_text(size = 6),   
            legend.key.size = unit(1, "lines"),
            legend.key.width = unit(3, "lines"),
            legend.position = "bottom",
            plot.title = element_markdown(size = 8, face = "bold", hjust = 0.5),
            strip.background = element_rect(fill = "#F4F4F4", color = "#F4F4F4"),
            strip.text.x = element_markdown(face = "bold")) +
      facet_wrap(~ DisasterSubgroup, scales = "free_y", ncol=3)
    
    # Create interactive ggiraph object
    # https://davidgohel.github.io/ggiraph/reference/index.html
    girafe(ggobj =deathHeatmap, width_svg = 8, height_svg = 4.2, 
           options = list(opts_hover(css = ""), opts_hover_inv(css = "opacity:0.05;"), opts_sizing(rescale = TRUE), opts_toolbar(fixed = TRUE)))
  }) # end heatmap freq
  
  ## Heatmaps Affected ----
  output$heatmapAffected <- renderGirafe({
    
    ### Inputs ----
    countryInput <- input$barCountryInput
    yearInput <- input$barYearInput
    
    # Freq data
    heatmapAffectedData <- disasterData %>%
      filter(if(!is.null(countryInput) && nchar(countryInput) > 0) Country == countryInput else TRUE) %>%
      group_by(Year, DisasterSubgroup, DisasterType) %>%
      filter(Year <= 2022) %>%
      summarise(SumTotalAffected = sum(TotalAffected, na.rm = TRUE), .groups = "drop") %>%
      mutate(Year = factor(Year), 
             DisasterSubgroup = case_when(
               DisasterSubgroup == "Biological" ~ "<span style='color: #30123B; font-weight: bold; font-size: 8px;'>Biological</span>",
               DisasterSubgroup == "Climatological" ~ "<span style='color: #414CCA; font-weight: bold; font-size: 8px;'>Climatological</span>",
               DisasterSubgroup == "Extra-terrestrial" ~ "<span style='color: #2B82B2; font-weight: bold; font-size: 8px;'>Extra-terrestrial</span>",
               DisasterSubgroup == "Geophysical" ~ "<span style='color: #1B9E78; font-weight: bold; font-size: 8px;'>Geophysical</span>",
               DisasterSubgroup == "Hydrological" ~ "<span style='color: #86BD44; font-weight: bold; font-size: 8px;'>Hydrological</span>",
               DisasterSubgroup == "Industrial accident" ~ "<span style='color: #F7D032; font-weight: bold; font-size: 8px;'>Industrial accident</span>",
               DisasterSubgroup == "Meteorological" ~ "<span style='color: #F39C19; font-weight: bold; font-size: 8px;'>Meteorological</span>",
               DisasterSubgroup == "Miscellaneous accident" ~ "<span style='color: #E36232; font-weight: bold; font-size: 8px;'>Miscellaneous accident</span>",
               DisasterSubgroup == "Transport" ~ "<span style='color: #BA202C; font-weight: bold; font-size: 8px;'>Transport</span>",
               TRUE ~ paste("<span style='color: lightgrey;'>", DisasterSubgroup, "</span>")
             ),
             DisasterTypeWrap = str_wrap(DisasterType, width = 19))
    
    
    #### Set title ----
    affectedTitle <- ifelse(is.null(countryInput) || countryInput == "", 
                        paste("<span style='color: darkred; font-family: Arial;'>Total Affected</span> by Disaster Type and Year in <span style='color: turquoise4;'> All Countries</span>"), 
                        paste("<span style='color: darkred; font-family: Arial;'>Total Affected</span> by Disaster Type and Year in", "<span style='color: turquoise4;'>", countryInput, "</span>"))
    
    #### Set tooltip ----
    heatmapAffectedTooltip = paste("<b>Year:</b>", heatmapAffectedData$Year,
                                "<br><b>Disaster Type:</b>", heatmapAffectedData$DisasterType, 
                                "<br><b>Number of Affected:</b>", comma(heatmapAffectedData$SumTotalAffected))
    
    heatmapAffectedData$Year <- as.numeric(as.character(heatmapAffectedData$Year))
    
    ### Create freq heatmap
    affectedHeatmap <- ggplot(heatmapAffectedData, aes(x = Year, y = DisasterTypeWrap, fill = SumTotalAffected, tooltip = heatmapAffectedTooltip, data_id = Year)) +
      geom_tile_interactive() + 
      scale_fill_viridis_c(option = "F", direction = -1, begin = 0.1, end = 1, labels = comma) +
      scale_x_continuous(breaks = seq(1990, 2022, by = 2)) +
      labs(title = affectedTitle, fill = "Number of Affected") +
      theme_dark() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5.2, color = "black", vjust = 0.5),
            axis.text.y = element_text(hjust = 1, size = 5, color = "black"),
            axis.title = element_blank(),
            axis.ticks = element_line(color = "black", size = 0.3),
            legend.title = element_text(size = 6, face = "bold"),  
            legend.text = element_text(size = 6),   
            legend.key.size = unit(1, "lines"),
            legend.key.width = unit(3, "lines"),
            legend.position = "bottom",
            plot.title = element_markdown(size = 8, face = "bold", hjust = 0.5),
            strip.background = element_rect(fill = "#F4F4F4", color = "#F4F4F4"),
            strip.text.x = element_markdown(face = "bold")) +
      facet_wrap(~ DisasterSubgroup, scales = "free_y", ncol=3)
    
    # Create interactive ggiraph object
    # ref: https://davidgohel.github.io/ggiraph/reference/index.html
    girafe(ggobj =affectedHeatmap, width_svg = 8, height_svg = 4.2, 
           options = list(opts_hover(css = ""), opts_hover_inv(css = "opacity:0.05;"), opts_sizing(rescale = TRUE), opts_toolbar(fixed = TRUE)))
  }) # end heatmap freq
  
  # Link higlight year
  # ref: https://shiny.posit.co/r/articles/build/js-send-message/
  observe({
    year <- input$barYearInput
    session$sendCustomMessage(type = "updateHeatmapYear", message = list(Year = year))
  })
  
  ## Image user guide ----
  # ref: https://shiny.posit.co/r/articles/build/images/
  ### Map image ----
  output$MapUserGuide <- renderImage({
    list(src = "MapUserGuide.png", width = "670px",
         height = "580px",
         alt = "This image show the interface of map visualisation.",
         style = "display: block; margin-left: auto; margin-right: auto;")
  }, deleteFile = FALSE)
  
  ### Bubble image ----
  output$BubbleUserGuide <- renderImage({
    list(src = "BubbleUserGuide.png", width = "670px",
         height = "600px",
         alt = "This image show the interface of bubble charts visualisation.",
         style = "display: block; margin-left: auto; margin-right: auto;")
  }, deleteFile = FALSE)
  
  ### bar image ----
  output$BarUserGuide <- renderImage({
    list(src = "BarUserGuide.png", width = "670px",
         height = "380px",
         alt = "This image show the interface of bar charts visualisation.",
         style = "display: block; margin-left: auto; margin-right: auto;")
  }, deleteFile = FALSE)
  
  ### heatmap image ----
  output$HeatmapsUserGuide <- renderImage({
    list(src = "HeatmapUserGuide.png", width = "670px",
         height = "400px",
         alt = "This image show the interface of heatmaps charts visualisation.",
         style = "display: block; margin-left: auto; margin-right: auto;")
  }, deleteFile = FALSE)
  
} # end server


shinyApp(userInterface, server)


