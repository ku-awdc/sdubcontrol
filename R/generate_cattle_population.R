#' Generate cattle herd population function
#'
#' @param N
#' @param matrix_distr
#'
#' @import tidyverse
#'
#' @export
generate_cattle_population <- function(N,matrix_distr){

  type_size_distr <- as_tibble(matrix_distr) %>%
    mutate(across(3:9, as.numeric))

  type_var <- t(rmultinom(N, size = 1, prob = type_size_distr$TypeProp))
  colnames(type_var) <- type_size_distr$HerdTypeText
  type_var <- as_tibble(type_var)
  type_var <- type_var %>%
    mutate(HerdTypeText = case_when(type_var[,1]==1 ~ colnames(type_var[,1]),
                                    type_var[,2]==1 ~ colnames(type_var[,2]),
                                    type_var[,3]==1 ~ colnames(type_var[,3]),
                                    type_var[,4]==1 ~ colnames(type_var[,4]),
                                    type_var[,5]==1 ~ colnames(type_var[,5]),
                                    type_var[,6]==1 ~ colnames(type_var[,6]))) %>%
    left_join(select(type_size_distr, HerdTypeID, HerdTypeText), by='HerdTypeText')


  pop <- tibble(HerdID = str_c('Herd_',1:N),
                HerdTypeID = type_var$HerdTypeID,
                HerdTypeText = type_var$HerdTypeText)

  type_size_distr_calc <- type_size_distr %>%
    #rowwise() %>%
    mutate(HeifProp_alpha = round(((1 - HeifProp_mean) / HeifProp_sd^2 - 1 / HeifProp_mean) * HeifProp_mean^2,3),
           HeifProp_beta = round(HeifProp_alpha * (1 / HeifProp_mean - 1),3),
           CalfProp_alpha = round(((1 - CalfProp_mean) / CalfProp_sd^2 - 1 / CalfProp_mean) * CalfProp_mean^2,3),
           CalfProp_beta = round(CalfProp_alpha * (1 / CalfProp_mean - 1),3))


  pop_type_size <- pop %>%
    left_join(select(type_size_distr_calc, HerdTypeID,HerdTypeText,
                     CattleYears_meanlog,CattleYears_sdlog,
                     HeifProp_alpha,HeifProp_beta,
                     CalfProp_alpha,CalfProp_beta),
              by=c('HerdTypeID','HerdTypeText')) %>%
    rowwise() %>%
    mutate(CattleYears = round(rlnorm(1,CattleYears_meanlog,CattleYears_sdlog),1),
           HeifProp = round(rbeta(1,HeifProp_alpha,HeifProp_beta),3),
           CalfProp = round(rbeta(1,CalfProp_alpha,CalfProp_beta),3),
           HeiferYears = round(CattleYears*HeifProp,1),
           CalfYears = round(CattleYears*CalfProp,1)) %>%
    select(HerdID,HerdTypeID,HerdTypeText,CattleYears,HeifProp,HeiferYears,CalfProp,CalfYears)

  return(pop_type_size)
}
