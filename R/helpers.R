#' Helper functions to perform common geospatial tasks
#'
#' @export
#' @import rgdal
#' 


# Quick transform of spatial objects to WGS84; useful for Leaflet mapping

transform_xy <- function(sp_object) {
  sp_xy <- spTransform(sp_object, CRS("+proj=longlat +datum=WGS84"))
  sp_xy
}


