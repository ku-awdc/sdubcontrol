#' Function for generating sample frequency table
#'
#' @param data_level
#'
#' @export
sampfreq_table <- function(data_level){

  pop_samp_sum <- data_level %>%
    mutate(SampText=factor(ifelse(SampSum>0,1,0), levels=c(0,1), labels=c('Not sampled','Sampled'))) %>%
    group_by(HerdTypeText,SampText) %>%
    summarise(Frequency=n()) %>%
    mutate(Proportion = round(Frequency/sum(Frequency),2),
           Percent = 100*Proportion)

  pop_samp_cross <- pop_samp_sum %>%
    mutate(TableValue = str_c(Frequency,' (',Percent,' %)')) %>%
    pivot_wider(id_cols=HerdTypeText, names_from = SampText, values_from = TableValue)

  return(pop_samp_cross)

}
