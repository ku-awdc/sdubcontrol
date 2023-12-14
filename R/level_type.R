#' Function for generating herd type level table
#'
#' @param data_level input data
#'
#' @export
level_type <- function(data_level){

  pop_type_sum <- data_level %>%
    #filter(SDLevel != 0) %>%
    group_by(HerdTypeID,HerdTypeText,SDInfText,SDLevelText) %>%
    summarise(Frequency=n()) %>%
    mutate(Proportion = round(Frequency/sum(Frequency),2),
           Percent = 100*Proportion,
           HerdTypeText = factor(HerdTypeText))

  pop_sum <- data_level %>%
    #filter(SDLevel != 0) %>%
    group_by(HerdTypeID,HerdTypeText,SDLevelText) %>%
    summarise(Frequency=n()) %>%
    mutate(Proportion = round(Frequency/sum(Frequency),2),
           Percent = 100*Proportion,
           HerdTypeText = factor(HerdTypeText),
           SDInfText = 'Total')

  pop_type_cross <- bind_rows(pop_type_sum,pop_sum) %>%
    mutate(TableValue = str_c(Frequency,' (',Percent,' %)')) %>%
    pivot_wider(id_cols=c('HerdTypeText','SDLevelText'), names_from = SDInfText, values_from = TableValue) %>%
    arrange('HerdTypeText','SDLevelText')

  return(pop_type_cross)

}
