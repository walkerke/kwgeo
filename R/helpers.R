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

# Create nice quantile labels for Leaflet mapping (which defaults to percentiles in the legend)

quantile_labels <- function(vec, n) {
  qs <- round(quantile(vec, seq(0, 1, 1/n), na.rm = TRUE), 1)
  len <- length(qs) - 1
  qlabs <- c()
  for (i in 1:len) {
    j <- i + 1
    v <- paste0(as.character(qs[i]), "-", as.character(qs[j]))
    qlabs <- c(qlabs, v)
  }
  final_labs <- c(qlabs, "Data unavailable")
  final_labs
}



