#' Download and Extract Files in Zip Archives
#'
#' Download and extract files in a zip archives hosted on the internet.
#'
#' @param url URL of zip archive to download
#' @param unzip_files Character vector of the file paths of files to extract
#' @param unzip_directory Character vector of length one, the path of the directory in which the file(s) will be saved
#'
#' @return Character vector of file paths of extracted files
downloadExtractZipFile <- function(url,
                                   unzip_directory = "",
                                   unzip_files = NULL) {

  temp_filename <- tempfile()

  utils::download.file(url,
                       temp_filename,
                       headers = c("User-Agent" = getUserAgent()))

  filepaths <- utils::unzip(temp_filename,
                            files = unzip_files,
                            exdir = unzip_directory,
                            junkpaths = TRUE)

  unlink(temp_filename)

  return(filepaths)
}
