#' Stratify a raster Stack into the best fitting classes
#'
#' @param ClassRaster a raster with the classes of your site as integers.
#' @param MinDist Minimum distance of a random point to the sampling points and cells with other classes.
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
#' @importFrom raster reclassify
#' @importFrom raster sampleRandom
#' @importFrom raster unique
#' @importFrom sf st_as_sf
#' @importFrom spThin thin
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
Random_Stratified_Min_Dist <- function(ClassRaster = NULL, MinDist = NULL, n = NULL, n_to_test = 100){
  Values <- raster::unique(ClassRaster)

  Contours <- stars::st_as_stars(FinalRaster) %>%
    stars::st_contour() %>%
    sf::st_cast(to = "MULTILINESTRING")

  Samples <- list()
  for(i in 1:length(Values)){
    m <- c(Values[i] - 0.1, Values[i] + 0.1, Values[i],
           -Inf, Values[i] - 0.1, NA,
           Values[i] + 0.1, Inf, NA)
    m <- matrix(m, ncol=3, byrow=TRUE)
    Temp <- reclassify(ClassRaster, m)
    Samples[[i]] <- raster::sampleRandom(Temp, size = n_to_test, na.rm = T, xy = T) %>%
      as.data.frame() %>%
      dplyr::mutate(Class = LETTERS[i], Sp = "Temp") %>%
      dplyr::select(x, y, Class, Sp)
  }


 Samples <- do.call("rbind", Samples)

 ## Eliminate close to the edges

 Temp <- Samples %>%
   st_as_sf(coords = c("x", "y"), crs = raster::projection(ClassRaster))

 Temp <- Temp %>%
   sf::st_distance(Contours) %>%
   as.matrix() %>%
   apply(2, as.numeric) %>%
   apply(1, min)

 Cond <- Points$Temp < MinDist

 Samples[Cond,]

 Thined <- spThin::thin(Samples, lat.col = "y", long.col = "x", spec.col = "Sp", thin.par = MinDist/1000,locs.thinned.list.return = T, write.files = F, write.log.file = F, verbose = F, reps = 1)
 Thined <- Thined[[1]]
 colnames(Thined) <- c("x", "y")
 Thined <- dplyr::left_join(Thined, Samples) %>%
   dplyr::select(-Sp) %>%
   dplyr::group_by(Class) %>%
   dplyr::slice_sample(n = n) %>%
   dplyr::ungroup() %>%
   st_as_sf(coords = c("x", "y"), crs = raster::projection(ClassRaster))

  return(Thined)
}
