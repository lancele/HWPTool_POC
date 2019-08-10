# Note: percent map is designed to work with the counties data set
# It may not work correctly with other data sets if their row order does 
# not exactly match the order in which the maps package plots counties
library(rgdal)
library(raster)
#library(maps)
#library(maproj)
# crs_str <- paste("+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs", 
#                  "+ellps=GRS80 +towgs84=0,0,0", sep='')
crs_str <- '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'
crs_pro <- CRS(crs_str)
#MMI_wModoc <- read.csv('tables/CA_MMIs_wMod.csv')
MMI <- read.csv('tables/CA_MMIs_noMod.csv')
catchments <- readOGR('shp', 'RB1catchments')

wbdRB1 <- readOGR('shp', 'wbd_RB1')

ind <- catchments$COMID %in% MMI$CATCHID
catchmentsRB1 <- catchments[ind, ]
ind <- MMI$CATCHID %in% catchments$COMID
MMI_RB1 <- MMI[ind, ]

ind <- match(catchmentsRB1$COMID, MMI_RB1$CATCHID)
catchmentsRB1$Vulnerability <- MMI_RB1$NormWatershedVulnerability[ind]
catchmentsRB1$Condition <- MMI_RB1$NormWatershedHealth[ind]
catchmentsRB1$StreamHealth <- MMI_RB1$NormStreamHealth[ind]

catchmentsRB1 <- spTransform(catchmentsRB1, crs_pro)
rasterRB1 <- raster(catchmentsRB1, crs = projection(catchmentsRB1),
             resolution = .0025)
extent(rasterRB1) <- extent(catchmentsRB1)
rasterVulnerability <- rasterize(catchmentsRB1, rasterRB1, 
                               field = 'Vulnerability',
                               fun = 'mean')
rasterCondition <- rasterize(catchmentsRB1, rasterRB1, 
                             field = 'Condition',
                             fun = 'mean')
rasterStrHealth <- rasterize(catchmentsRB1, rasterRB1, 
                             field = 'StreamHealth',
                             fun = 'mean')

writeRaster(rasterVulnerability, 'raster/WatershedVulnerability.tif', format = 'GTiff')

chloropleth <- function(var, shp){}


convert2raster <- function(shp, raster)

map('state', regions = 'california')
map(shp, add=T)
percent_map <- function(var, color, legend.title, min = 0, max = 100) {

  # generate vector of fill colors for map
  shades <- colorRampPalette(c("white", color))(100)
  
  # constrain gradient to percents that occur between min and max
  var <- pmax(var, min)
  var <- pmin(var, max)
  percents <- as.integer(cut(var, 100, 
    include.lowest = TRUE, ordered = TRUE))
  fills <- shades[percents]

  # plot choropleth map
  map("county", fill = TRUE, col = fills, 
    resolution = 0, lty = 0, projection = "polyconic", 
    myborder = 0, mar = c(0,0,0,0))
  
  # overlay state borders
  map("state", col = "white", fill = FALSE, add = TRUE,
    lty = 1, lwd = 1, projection = "polyconic", 
    myborder = 0, mar = c(0,0,0,0))
  
  # add a legend
  inc <- (max - min) / 4
  legend.text <- c(paste0(min, " % or less"),
    paste0(min + inc, " %"),
    paste0(min + 2 * inc, " %"),
    paste0(min + 3 * inc, " %"),
    paste0(max, " % or more"))
  
  legend("bottomleft", 
    legend = legend.text, 
    fill = shades[c(1, 25, 50, 75, 100)], 
    title = legend.title)
}