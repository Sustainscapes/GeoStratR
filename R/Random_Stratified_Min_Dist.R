#' Stratify a raster Stack into the best fitting classes
#'
#' @param ClassRaster a raster with the classes of your site as integers.
#' @param MinDist Minimum distance of a random point to the sampling points and cells with other classes.
#' @param BorderDist Minumum distance of a random point to the border of its class, if NULL if reverst to MinDist.
#' @param n Number of points per class can be one number or a vector specifying de number per class.
#' @param n_to_test Number of points to test for the minimum distance.
#'
#' @return An sf with the points and their classes
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr slice_sample
#' @importFrom dplyr ungroup
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom purrr map reduce
#' @importFrom terra unique as.polygons buffer spatSample crs intersect vect
#' @importFrom spThin thin
#' @export
#'
#' @examples
#' data(Bios)
#'
#' library(terra)
#'
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
Random_Stratified_Min_Dist <- function(ClassRaster = NULL, MinDist = NULL, BorderDist = NULL, n = NULL, n_to_test = 100){

  x <- y <- Sp <- Class <- ID <- NULL

  Values <- terra::unique(ClassRaster)[,1]

  Contours <- terra::as.polygons(ClassRaster)

  Samples <- terra::spatSample(ClassRaster, n_to_test, as.points = T, na.rm = T, method = "stratified", warn = F)

 ## Eliminate close to the edges
  if(is.null(BorderDist)){
    BorderDist <- MinDist
  }

  Buffer <- terra::buffer(Contours, -MinDist)

  Samples <- terra::intersect(Buffer, Samples)
  Samples <- Samples[,1]


 Samples <- Samples |>
   terra::project("+proj=longlat +datum=WGS84 +no_defs") |>
   as.data.frame(geom = "xy") |>
   dplyr::mutate(Sp = "Temp")


 Thined <- spThin::thin(Samples, lat.col = "y", long.col = "x", spec.col = "Sp", thin.par = MinDist/1000,locs.thinned.list.return = T, write.files = F, write.log.file = F, verbose = F, reps = 1)
 Thined <- Thined[[1]]
 colnames(Thined) <- c("x", "y")
 Thined <- dplyr::left_join(Thined, Samples)|>
   dplyr::select(-Sp)|>
   dplyr::group_by(Class)|>
   dplyr::slice_sample(n = n)|>
   dplyr::ungroup()

 Thined <- Thined|>
   dplyr::group_split(Class)

   Thined <- Thined |>
     purrr::map(~tibble::rowid_to_column(.x, var = "ID")) |>
     purrr::reduce(rbind)


 DIGITS <- max(floor(log10(Thined$ID)))

 Thined <- Thined |>
   mutate(ID = formatC(ID, digits = DIGITS, flag = "0"), ID = paste0(Class, ID)) |>
   terra::vect(geom = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +no_defs")|>
   terra::project(terra::crs(ClassRaster))

  return(Thined)
}
