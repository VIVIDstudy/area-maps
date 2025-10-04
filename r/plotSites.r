#' Plot Sites
#'
#' @param sites_csv Character vector of length one, the full path to CSV file of the sites to be plotted (and their postcodes)
#' @param data_directory Character vector of length one, the path to the data directory
#' @param img_name Character vector of length one, if present an image of the plot is saved to disk at this location
#' @param img_type Character vector of length one, image file type extension
#' @param img_width Numeric vector of length one, output image width in cm
#' @param img_height Numeric vector of length one, output image height in cm
#'
#' @return ggplot2 plot
#' @export
plotSites <- function(sites_csv,
                      data_directory,
                      img_name = NA,
                      img_type = "png",
                      img_width = 8.5,
                      img_height = 10.2,
                      img_dim_units = "cm") {

  geom_data <- prepareGeoData(sites_csv,
                              data_directory)

  sites_map <- composePlot(geom_data)

    if(!is.na(img_name) & length(img_name) == 1) {

      sites_map_print <- composePlot(geom_data,
                                     font_size = 0.015 * getHeightInPts(img_height, img_dim_units),
                                     for_print = TRUE)

      ggplot2::ggsave(filename = paste0(img_name, ".", img_type),
                      plot = sites_map_print,
                      width = img_width,
                      height = img_height,
                      units = img_dim_units,
                      create.dir = TRUE)
    }

  return(sites_map)
}

composePlot <- function(geom_data,
                        font_size = 11,
                        for_print = FALSE) {

  plot_font_size_scale <- 1 / (ggplot2::.pt)^as.integer(for_print)

  plot <- ggplot2::ggplot(geom_data[["sites_geom"]]) +
    ggplot2::geom_sf(data = geom_data[["england_wales_population_density_geom"]],
                     ggplot2::aes(fill = population_density),
                     lwd = 0) +
    ggplot2::scale_fill_gradient(low = "#fee8c8",
                                 high = "#e34a33",
                                 guide = "none") +
    ggplot2::geom_sf(data = geom_data[["england_regions_goem"]],
                     fill = NA,
                     alpha = 0.4) +
    ggplot2::geom_sf(ggplot2::aes(colour = notes),
                     shape = 21,
                     fill = NA,
                     stroke = 0.6 * plot_font_size_scale,
                     size = 1.5 * plot_font_size_scale) +
    ggrepel::geom_text_repel(ggplot2::aes(label = site_name,
                                          colour = notes,
                                          geometry = geometry),
                             stat = "sf_coordinates",
                             size = 3 * plot_font_size_scale,
                             min.segment.length = 0,
                             segment.size = 0.5 * plot_font_size_scale) +
    ggplot2::scale_colour_manual(values = c("National site" = "blue",
                                            "New contributor" = "purple"),
                                 na.value = "black",
                                 guide = "none") +
    ggplot2::theme_minimal(base_size = font_size) +
    ggplot2::coord_sf(crs = 27700) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   plot.background = ggplot2::element_rect(fill = "#d5e4eb",
                                                           linewidth = 0),
                   plot.caption = ggplot2::element_text(hjust = 0,
                                                        colour = "#666")) +
    ggplot2::labs(caption = paste(c("Contains OS data \u00A9 Crown copyright and database right 2025",
                                    "Contains Royal Mail data \u00A9 Royal Mail copyright and Database right 2025",
                                    "Contains information from NHS England licenced under the Open Government Licence v3.0",
                                    "Contains data from the Office for National Statistics licensed under the Open Government Licence v3.0"),
                                  collapse = "\n"))

  return(plot)
}

getHeightInPts <- function(height,
                           unit) {

  if(unit == "in") {
    to_inches_scale = 1
  } else if(unit == "cm") {
    to_inches_scale = 1 / 2.54
  } else if(unit == "mm") {
    to_inches_scale = 1 / 25.4
  } else if(unit == "px") {
    to_inches_scale = 1 / 300
  } else {
    error("Unknow unit")
  }

  return(height * to_inches_scale * 72)
}
