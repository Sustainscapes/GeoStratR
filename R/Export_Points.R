#' Export created points
#'
#' @param Points The name of the SpatVector file with the spatial points.
#' @param ID_Col Name of the ID column, defaults to "ID".
#' @param format a character, it can be SHP, GPX or RDS, default to "SHP".
#' @param name The name of the file without the extension, defaults to "Samples".
#'
#' @return An exported SHP, GPX or RDS file
#' @importFrom terra writeVector as.data.frame
#' @export
#'
#' @examples
#' data(Bios)
#' library(terra)
#' Bios <- terra::unwrap(Bios)
#'
#' a <- Stratify(Bios)
#'
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
#' # remove created files
#' file.remove(list.files(pattern = "Selected"))

Export_Points <- function(Points, ID_Col = "ID", format = "SHP", name = "Samples"){
  if(format == "SHP"){
    terra::writeVector(Points, paste0(name, ".shp"))
  } else if(format == "RDS"){
    saveRDS(terra::as.data.frame(Points, geom="XY"), paste0(name, ".rds"))
  } else if(format == "GPX"){
    Points <- Points[ID_Col]
    names(Points) <- "name"
    terra::writeVector(Points["name"], filetype="GPX", options=c("GPX_USE_EXTENSIONS=YES"), layer="waypoints", filename =  paste0(name, ".gpx"))
  }
}
