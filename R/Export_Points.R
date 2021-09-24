#' Export created points
#'
#' @param Points The name of the SF file with the spatial points.
#' @param ID_Col Name of the ID column, defaults to "ID".
#' @param format a character, it can be SHP, GPX or RDS, default to "SHP".
#' @param name The name of the file without the extension, defaults to "Samples".
#'
#' @return An exported SHP, GPX or RDS file
#' @export
#'
#' @examples
#' data(Bios)
#'
#' a <- Stratify(Bios)
#'
#' library(raster)
#'
#' plot(a$FinalStack, colNA = "black")
#'
#' FinalRaster <- a$FinalStack
#'
#' Points <- Random_Stratified_Min_Dist(ClassRaster = FinalRaster,
#'                                      MinDist = 2000,
#'                                      n = 30,
#'                                      n_to_test = 100)
#'
#' Export_Points(Points, name = "Selected")

Export_Points <- function(Points, ID_Col = "ID", format = "SHP", name = "Samples"){
  if(format == "SHP"){
    sf::write_sf(Points, paste0(name, ".shp"))
  } else if(format == "RDS"){
    saveRDS(Points, paste0(name, ".rds"))
  } else if(format == "GPX"){
    ToGPX <- Points %>%
      dplyr::select(ID_Col) %>%
      dplyr::rename(name = ID_Col) %>%
      sf::as_Spatial()

    rgdal::writeOGR(ToGPX["name"], driver="GPX", layer="waypoints", dsn= paste0(name, ".gpx"))
  }
}
