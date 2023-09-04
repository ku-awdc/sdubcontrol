#' Function for generating herd type bar chart
#'
#' @param data_pop
#'
#' @export

herdtype_bar <- function(data_pop){
  data_pop_ed <- data_pop %>%
    arrange(HerdTypeID) %>%
    mutate(HerdTypeText=factor(HerdTypeText))

  ggplot(data_pop_ed, aes(y=reorder(HerdTypeText, desc(HerdTypeText)))) +
    geom_bar(fill='lightblue') +
    labs(y='') +
    theme_minimal() +
    theme(text = element_text(size = 16))

}
