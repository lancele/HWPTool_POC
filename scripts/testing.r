library(rgdal)
library(sf)
library(googleway)

# source('ogr2googleway.r')
wbd_hu10 <- readOGR('shp', 'wbd_HU10')
wbd_rb1 <- readOGR('shp', 'wbd_RB1')
#shp <- ogr2google(wbd_hu10, crs_default)
rb1 <- st_as_sf(wbd_rb1)
hu10 <- st_as_sf(wbd_hu10)

#source('scripts/model.r')
#load('rdata/gbmdata.RData')
load('rdata/testing.rdata')

map_key <- "AIzaSyB9pJKeJvUtdREshpDi49RxyJM_6FUBreg"
#x <- tempdat$CRAM_Overall_Score.x
x <- abs(rnorm(200))
server <- function(input, output) {
  
  tf_name <- function(phrase, type = input$transform_func){
    switch(type,
           log10 = bquote(log[10] * '(' * .(phrase) * ')'),
           log1p = bquote(log[10] * '(' * .(phrase) + 1 * ')'),
           ln = bquote(ln * '(' * .(phrase) * ')'),
           sqrt = bquote(sqrt(.(phrase))),
           sq = bquote('(' * .(phrase) * ')'^{2}),
           sq4 = bquote(sqrt(.(phrase), 4)),
           phrase)
  }
  tf <- switch(input$transform_func,
               None = function(y) {y},
               log10 = log10,
               log1p = log1p,
               ln = log,
               sqrt = sqrt,
               sq = function(y) {y^2},
               sq4 = function(y) {y^(1/4)})
  
  output$distPlot <- renderPlot({
    
    bins <- seq(min(tf(x)), max(tf(x)), length.out = input$bins + 1)
    hist(tf(x), breaks = bins, col = 'darkblue', border = 'white',
         xlab = tf_name('CRAM Overall Score'),
         main = 'Histogram of CRAM Scores')
  })
  
  set_key(map_key)
  output$map <- renderGoogle_map({
    google_map(location = c(38.516897, -122.797747)) %>%
      add_polygons(data = rb1)
  })
}
