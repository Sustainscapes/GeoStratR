#' Stratify a raster Stack into the best fitting classes
#'
#' @param Stack a spatraster with all the variables used for generating the classes.
#' @param LowGroup an integer number, the lowest number of groups to test for.
#' @param HighGroup an integer number, the highest number of groups to test for.
#' @param Criterion either calinski or ssi, the criterion used for selecting the best number of groups.
#'
#' @return a list with the raster for the best classes and a dataframe with the best results
#' @importFrom dplyr mutate arrange desc
#' @importFrom janitor clean_names
#' @importFrom terra as.data.frame values names
#' @importFrom tibble as_tibble
#' @importFrom vegan cascadeKM
#' @importFrom stringr str_remove_all
#' @export
#'
#' @examples
#' data(Bios)
#' Bios <- terra::unwrap(Bios)
#' a <- Stratify(Bios)
#'
#' library(terra)
#'
#' plot(a$FinalStack, colNA = "black")

Stratify <- function(Stack = NULL, LowGroup = 2, HighGroup = 10, Criterion = "calinski"){
  Groups <- n_groups <- calinski <- ssi <- NULL
  DF <- Stack |>
    terra::as.data.frame(cells = T)

  Strata <- vegan::cascadeKM(DF[,-1], inf.gr = LowGroup, sup.gr = HighGroup, criterion = Criterion)

  Results <- Strata$results |>
    as.data.frame() |>
    janitor::clean_names() |>
    t() |>
    as.data.frame()

  Results$Groups <- rownames(Results)

  Results <- Results |>
    dplyr::mutate(n_groups = stringr::str_remove_all(Groups, "_groups"),
                  n_groups = as.numeric(stringr::str_remove_all(n_groups, "x")))

  Partition <- Strata$partition |>
    as.data.frame() |>
    janitor::clean_names()

  if(Criterion == "calinski"){
    Select <- Results |>
      dplyr::filter(calinski == max(calinski))
  }

  if(Criterion == "ssi"){
    Select <- Results |>
      dplyr::filter(ssi == max(ssi))
  }

  Final <- Partition |>
    dplyr::select(Select$Groups)

  FinalStack <- Stack[[1]]
  terra::values(FinalStack) <- NA

  terra::values(FinalStack)[DF$cell] <- Final[,1]
  names(FinalStack) <- "ID"

  Classes <- data.frame(ID = terra::unique(FinalStack)[,1], Class = LETTERS[terra::unique(FinalStack)[,1]])

  levels(FinalStack) <- Classes

  Results <- Results |>
    dplyr::select(-Groups) |>
    tibble::as_tibble()

  if(Criterion == "calinski"){
    Results <- Results |>
      dplyr::arrange(dplyr::desc(calinski)) |>
      tibble::as_tibble()
  }

  if(Criterion == "ssi"){
    Results <- Results |>
      dplyr::arrange(dplyr::desc(ssi)) |>
      tibble::as_tibble()
  }

  return(list(Results = Results, FinalStack = FinalStack))
}
