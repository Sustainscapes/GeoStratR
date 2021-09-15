#' Stratify a raster Stack into the best fitting classes
#'
#' @param Stack a raster stack with all the variables used for generating the classes.
#' @param LowGroup an integer number, the lowest number of groups to test for.
#' @param HighGroup an integer number, the highest number of groups to test for.
#' @param Criterion either calinski or ssi, the criterion used for selecting the best number of groups.
#'
#' @return a list with the raster for the best classes and a dataframe with the best results
#' @importFrom dplyr filter_all
#' @importFrom dplyr mutate
#' @importFrom janitor clean_names
#' @importFrom raster as.data.frame
#' @importFrom raster values
#' @importFrom tibble rowid_to_column
#' @importFrom vegan cascadeKM
#' @importFrom stringr str_remove_all
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

Stratify <- function(Stack = NULL, LowGroup = 2, HighGroup = 10, Criterion = "calinski"){
  Groups <- n_groups <- calinski <- ssi <- NULL
  DF <- Stack %>%
    raster::as.data.frame() %>%
    tibble::rowid_to_column(var = "ID") %>%
    dplyr::filter_all(~!is.na(.x))

  Strata <- vegan::cascadeKM(DF[,-1], inf.gr = LowGroup, sup.gr = HighGroup, criterion = Criterion)

  Results <- Strata$results %>% as.data.frame() %>% janitor::clean_names() %>% t() %>% as.data.frame()
  Results$Groups <- rownames(Results)

  Results <- Results %>% dplyr::mutate(n_groups = stringr::str_remove_all(Groups, "_groups"),
                                       n_groups = as.numeric(stringr::str_remove_all(n_groups, "x")))

  Partition <- Strata$partition %>% as.data.frame() %>% janitor::clean_names()

  if(Criterion == "calinski"){
    Select <- Results %>% dplyr::filter(calinski == max(calinski))
  }

  if(Criterion == "ssi"){
    Select <- Results %>% dplyr::filter(ssi == max(ssi))
  }

  Final <- Partition %>% dplyr::select(Select$Groups)

  FinalStack <- Stack[[1]]
  raster::values(FinalStack) <- NA

  raster::values(FinalStack)[DF$ID] <- Final[,1]

  Results <- Results %>%
    dplyr::select(-Groups) %>%
    tibble::as_tibble()

  return(list(Results = Results, FinalStack = FinalStack))
}
