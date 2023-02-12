
library('tidyverse')
library('sf')
library('plotly')


#load bicycle theft data
bicycleTheftData <- read_csv('bicycle_thefts.csv') %>% 
  filter(X!=0,Y!=0) %>% # drop values at unknown location
  filter(YEAR != 2023) %>%
  mutate(YEAR=as.factor(YEAR))


#TODO: save polygon as is so cleaning unnecessary
#load shoreline map data
##################################################################
#read data
shorelineData <- read_sf("shoreline-data/shoreline-2002.shp") %>%
  mutate(geometry = st_transform(geometry,crs = "EPSG:32610" )) 

#get coords
coords <- st_coordinates(shorelineData$geometry[1])

#get eastern most coord
rightmost_index <- which.max(coords[,1])
rightmost_point <- coords[rightmost_index,]

#add new point
shorelinePoints <- shorelineData$geometry[2]
new_point <- rightmost_point
new_points <- st_coordinates(shorelinePoints) %>% rbind(rightmost_point)

#create new linestring from new points and replace old one in geometry
new_linestring <- st_linestring(new_points[,c("X","Y")]) 
shorelineData$geometry[2] <- new_linestring
shorelineData <- shorelineData %>% mutate(geometry=geometry %>% st_cast('POLYGON'))
####################################################################################

#function for generating a plot of the bicycle thefts on a vancouver map
renderMap <- function(theftData, shorelineData){
  print(theftData)
  p <- ggplot() +
    theme(panel.background = element_rect(fill='lightblue')) + # water
    theme(legend.position='none') + # allows ggplotly to suppress legends
    geom_sf(data = shorelineData,fill='grey') +  #land
    geom_point(data = theftData, aes(x=X,y=Y,text=n,color=factor(cluster),alpha=0.1)) + #thefts
    scale_colour_hue() + # colour clusters
    theme(plot.background = element_blank()) # remove white frame of plot
  ggplotly(p,tooltip='text') %>% layout(xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE))
}

#function for generating a trend plot of thefts over time
renderTrend <- function(theftData){
  theftData %>% ggplot() +
    aes(x=YEAR,fill=..count..) +
    geom_bar() +
    coord_cartesian(ylim=c(1000,3000)) +
    ylab('Number of Thefts') +
    scale_fill_continuous() +
    theme(plot.background = element_blank())
}

