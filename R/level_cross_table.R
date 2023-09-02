#' Function for generating cross table for overall population
#'
#' @param data_level
#'
#' @export
level_cross_table <- function(data_level){

  pop_sum <- data_level %>%
    filter(!is.na(SDLevelText)) %>%
    group_by(SDInfText,SDLevelText) %>%
    summarise(Frequency=n()) %>%
    mutate(Proportion = round(Frequency/sum(Frequency),2),
           Percent = 100*Proportion)

  pop_cross <- pop_sum %>%
    mutate(TableValue = str_c(Frequency,' (',Percent,' %)')) %>%
    pivot_wider(id_cols=SDLevelText, names_from = SDInfText, values_from = TableValue)

  return(pop_cross)

}
