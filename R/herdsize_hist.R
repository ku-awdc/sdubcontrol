#' Function for generating herd size histogram
#'
#' @param data_pop
#'
#' @export

herdsize_hist <- function(data_pop){
  data_pop_ed <- data_pop %>%
    arrange(HerdTypeID) %>%
    mutate(HerdTypeText=factor(HerdTypeText))

  ggplot(data_pop_ed, aes(x=CattleYears)) +
    geom_histogram(fill='lightblue', bins=60, boundary=0) +
    facet_wrap(~HerdTypeText, ncol = 1, scales = 'free') +
    theme_minimal() +
    theme(text = element_text(size = 16))

}
