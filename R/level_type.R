#' Function for generating herd type level table
#'
#' @param data_level
#'
#' @export
level_type <- function(data_level){

  pop_type_sum <- data_level %>%
    #filter(!is.na(SDLevelText)) %>%
    group_by(HerdTypeText,SDInfText,SDLevelText) %>%
    summarise(Frequency=n()) %>%
    mutate(Proportion = round(Frequency/sum(Frequency),2),
           Percent = 100*Proportion)

  # pop_type_cross <- pop_type_sum %>%
  #   mutate(TableValue = str_c(Frequency,' (',Percent,' %)')) %>%
  #   pivot_wider(id_cols=SDLevelText, names_from = SDInfText, values_from = TableValue)

  return(pop_type_sum)

}
