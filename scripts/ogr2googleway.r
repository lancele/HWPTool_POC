require(rgdal)
# function to convert SpatialPolygonsDataFrame to format for display 
# on google maps api for shiny
crs_default <- CRS('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0')
ogr2google <- function(shp, crs=crs_default){
   shp <- spTransform(shp, crs_default)
   polyNum <- length(shp@polygons)
   pl_encode <- c()
   pl_count <- c()
   n <- 0
   # iterate through each polygon in multipolygon object
   for(j in 1:polyNum){
     polys <- shp@polygons[[j]]@Polygons
     polylineNum <- length(polys)
     m <- 0
     for(i in 1:polylineNum){
       n <- n+1
       m <- m+1
       coords <- polys[[i]]@coords
       pl_encode[n] <- encode_pl(lon=coords[,1], lat=coords[,2])
     }
     pl_count <- c(pl_count, rep(j, m))
   }
   df <- data.frame(id = pl_count,
                    polyline = pl_encode,
                    stringsAsFactors = FALSE)
   return(df)
}
#polyNum <- length(wbd_rb1@polygons[[1]]@Polygons)

# wbd_hu10 <- readOGR('shp', 'wbd_HU10')
# crs <- CRS('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0')
# shp <- wbd_hu10
# shp <- spTransform(wbd_hu10, crs)
# polyNum <- length(shp@polygons)
# pl_encode <- c()
# pl_count <- c()
# 
# n <- 0
# for(j in 1:polyNum){
#   polys <- shp@polygons[[j]]@Polygons
#   polylineNum <- length(polys)
#   m <- 0
#   for(i in 1:polylineNum){
#     n <- n+1
#     m <- m+1
#     coords <- polys[[i]]@coords
#     pl_encode[n] <- encode_pl(lon=coords[,1], lat=coords[,2])
#   }
#   pl_count <- c(pl_count, rep(j, m))
# }
# 
# df <- data.frame(id = pl_count,
#                  polyline = pl_encode,
#                  stringsAsFactors = FALSE)
