### r2cartodb: a function to import data to your CartoDB account.

# Set your working directory before calling the function.
# Also, the function depends on the rgdal and httr packages, and requires that you have Rtools installed
# (and that you have Rtools in your PATH if you are on Windows) if you want to upload spatial
# data frames.

library(rgdal)
library(httr)

# Advisable to make the layer name different from your imported data, if applicable.

r2cartodb <- function(obj, layer_name, account_id, api_key) {

  dir <- getwd()

  cartodb_url <- paste0("https://",
                        account_id,
                        ".cartodb.com/api/v1/imports/?api_key=",
                        api_key)

  if (class(obj) == "data.frame") {

    # Will import a basic table or a spatial table if it can detect long/lat columns.
    # The function first writes a CSV then uploads it.

    csv_name <- paste0(layer_name, ".csv")

    write.csv(obj, csv_name)

    POST(cartodb_url,
         encode = "multipart",
         body = list(file = upload_file(csv_name)),
         verbose())

  } else if (grep("Spatial", class(obj)) == 1) {

    # Assuming here that you're using a Spatial*DataFrame, which can be written to a shapefile.  The
    # function will first write to a shapefile and then upload.

    writeOGR(obj = obj,
             dsn = dir,
             layer = layer_name,
             driver = "ESRI Shapefile",
             overwrite_layer = TRUE)

    pattern = paste0(layer_name, "\\.*")

    files <- list.files(pattern = pattern)

    zip_name <- paste0(layer_name, ".zip")

    zip(zipfile = zip_name, files = files)

    POST(cartodb_url,
         encode = "multipart",
         body = list(file = upload_file(zip_name)),
         verbose())

  } else {

    stop("The r2cartodb function requires a data frame or spatial data frame")

  }
}
