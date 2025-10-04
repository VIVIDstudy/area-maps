#' Download ArcGIS GeoPackage
#'
#' Download an ArcGIS-hosted GeoPackage file.
#'
#' @param arcgis_id Character vector of length one, the ArcGIS layer/item identifier of the resource to download
#' @param directory_path Character vector of length one, the path of the directory in which the file will be saved
#'
#' @return A character vector, of length one, of the downloaded GeoPackage file
downloadArcGISGeoPackage <- function(arcgis_id,
                                     directory_path = "") {

  url <- paste0("https://hub.arcgis.com/api/download/v1/items/",
                arcgis_id,
                "/geoPackage?redirect=false&layers=0")
  user_agent_string <- getUserAgent()

  i = 0
  while(i < 10) {
    download_request_response <- httr::GET(url,
                                           httr::user_agent(user_agent_string))

    download_request_body <- download_request_response |>
      httr::content()

    if(!is.null(download_request_body$resultUrl)) break

    Sys.sleep(0.3)
  }
  if(is.null(download_request_body$resultUrl)) {
    warning("Request for ArcGIS GeoPackage download failed.")
    return(FALSE)
  }

  filepath <- paste0(directory_path,
                     "/",
                     basename(download_request_body$resultUrl))

  utils::download.file(download_request_body$resultUrl,
                       filepath,
                       headers = c("User-Agent" = getUserAgent()))

  return(filepath)
}
