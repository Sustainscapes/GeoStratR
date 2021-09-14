Stratify <- function(Stack = NULL, LowGroup = 2, HighGroup = 10, Criterion = "calinski"){
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
  values(FinalStack) <- NA

  values(FinalStack)[DF$ID] <- Final[,1]

  Results <- Results %>%
    dplyr::select(-Groups) %>%
    tibble::as_tibble()

  return(list(Results = Results, FinalStack = FinalStack))
}
