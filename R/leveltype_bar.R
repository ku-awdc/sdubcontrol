#' Function for generating herd type level bar chart
#'
#' @param data_level input data
#'
#' @export

leveltype_bar <- function(data_level){

  pop_type_sum <- data_level %>%
    group_by(HerdTypeID,HerdTypeText,SDLevelText,SDInfText) %>%
    summarise(Frequency=n()) %>%
    mutate(Proportion = round(Frequency/sum(Frequency),2),
           Percent = 100*Proportion,
           HerdTypeText = factor(HerdTypeText))

  type_bar <- ggplot(pop_type_sum,aes(x=Percent, y=reorder(SDLevelText, desc(SDLevelText)), fill=SDInfText, alpha=SDLevelText)) +
    geom_bar(stat="identity", position = position_stack(reverse = TRUE)) +
    scale_fill_manual(values=c('grey','firebrick1')) +
    scale_alpha_manual(values=c(1,1,0.3),guide = 'none') +
    facet_wrap(~HerdTypeText, ncol = 1) +
    #geom_text(aes(label=Frequency), size=3, position = position_stack(reverse = TRUE,vjust = 0.5)) +
    theme_minimal() +
    theme(legend.position = "top",
          legend.title = element_blank(),
          axis.title.y=element_blank(),
          text = element_text(size = 16))

  return(type_bar)

}
