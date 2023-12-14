#' Function for generating heifer proportion histogram
#'
#' @param data_pop input data
#'
#' @export

calfprop_hist <- function(data_pop){
  data_pop_ed <- data_pop %>%
    arrange(HerdTypeID) %>%
    mutate(HerdTypeText=factor(HerdTypeText))

  ggplot(data_pop_ed, aes(x=CalfProp)) +
    geom_histogram(fill='lightblue', bins=60) +
    facet_wrap(~HerdTypeText, ncol = 1) +
    labs(x='Proportion of calves') +
    theme_minimal() +
    theme(text = element_text(size = 16))

}
