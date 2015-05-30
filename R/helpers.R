#'  Easily merge a data frame to a spatial data frame
#'
#'  The pages of StackOverflow are littered with questions about how to merge a regular data frame to a
#'  spatial data frame in R.  The \code{merge} function from the sp package operates under a strict set of
#'  assumptions, which if violated will break your data.  This function wraps a couple StackOverflow answers
#'  I've seen that work in a friendlier syntax.
#' @param spatial_data A spatial data frame to which you want to merge data.
#' @param data_frame A regular data frame that you want to merge to your spatial data.
#' @param by_sp The column name you'll use for the merge from your spatial data frame.
#' @param by_df The column name you'll use for the merge from your regular data frame.
#' @export

geo_join <- function(spatial_data, data_frame, by_sp, by_df) {

  spatial_data@data <- data.frame(spatial_data@data,
                                  data_frame[match(spatial_data@data[[by_sp]],
                                                   data_frame[[by_df]]), ])

  spatial_data


}

#'  Quick transform of spatial objects to WGS84
#'
#'  The function will use \code{spTransform} from the \code{rgdal} package to automatically transform your
#'  spatial data to the WGS84 geographic coordinate system.  As this is the coordinate system required for mapping
#'  with the \code{leaflet} package, it can come in handy when mapping spatial objects in R.
#' @param sp_object an R object of class \code{Spatial*} that you'd like to transform to WGS84.
#' @import rgdal
#' @export

transform_xy <- function(sp_object) {
  sp_xy <- spTransform(sp_object, CRS("+proj=longlat +datum=WGS84"))
  sp_xy
}



#'  Create nice-looking quantile labels for Leaflet mapping.
#'
#'  At present, the amazing \code{leaflet} package uses percentiles in its quantile legends; however, sometimes you
#'  want to show the actual values.  This function allows you to do just that.
#' @param vec The column of data that you are visualizing on your choropleth map
#' @param n The number of classes you've chosen
#' @export

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



