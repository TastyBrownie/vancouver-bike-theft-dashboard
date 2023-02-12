library('shiny')
library('tidyverse')
library('bslib')
library('plotly')

source('global.R')


# This is a dashboard I made to display reported bike thefts in Vancouver from 2003 onward. Theft data was
# obtained from the VPD via https://geodash.vpd.ca/opendata/. I added K-means clustering because I felt like it.
# The bar chart illustrates the rise and decline
# of thefts in vancouver over the years.


#ui definition. Determines html and css for the app.
ui <- fluidPage(
  #quick bootstrap styling
  theme = bs_theme(version=5,bootswatch='minty'),
  #html structure ######################################
  headerPanel("Vancouver Bike Thefts"),
  fluidRow(
  p('This is a dashboard I made to display reported bike thefts in Vancouver from 2003 onward. Theft data was
    obtained from the VPD via',
    tags$a(href="https://geodash.vpd.ca/opendata/",
    "geodash.vpd.ca"),'while the map data was obtained from the vancouver
    open data portal. I added K-means clustering to show the counts in each area of the city. Hover over 
    a cluster to see the number of thefts in it
    The bar chart illustrates the rise and decline
    of thefts in vancouver over the years. The plots look better when the window is maximized.'),
  ) %>% column(width=5),
  fluidRow(
    #user inputs: a dropdown menu and slider input. The dropdown menu determines
    selectizeInput(inputId = 'year', label = 'Year', choices = levels(bicycleTheftData$YEAR)) %>% column(width=2),
    sliderInput(inputId = 'kClusters', label = 'K Clusters',min=1,max=10,value=5) %>% column(width=3)
  ),
  fluidRow(
    plotlyOutput(outputId = "map") %>% column(width=5,.),
    plotOutput(outputId = "trend") %>% column(width=5,.)
  )
  ########################################################
)
  
#server logic definition. Called each time a user connects or a widget is changed
server <- function(input,output){
  
  # returns a map of vancouver with bike theft clusters from a given year to the
  # "map" plot output.
  output$map <- renderPlotly({
    data = bicycleTheftData %>% filter(YEAR == input$year)
    coords = data[,c('X','Y')]
    data$cluster = kmeans(coords,input$kClusters,iter.max=100)$cluster
    data = data %>% add_count(cluster)
    renderMap(data, shorelineData)
  })
  
  # returns a bar chart showing yearly total thefts
  # to the trend output
  output$trend <- renderPlot({
    renderTrend(bicycleTheftData)
  },bg="transparent")
  
}

shinyApp(ui = ui, server = server)

