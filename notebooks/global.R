library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyMatrix)

# Text for the section Introduction
# introduction <- list(p(""),
#                      p(""),
#                      p(""),
#                      p(""),
#                      p(""))


#### Cattle herd population ####

# Default population size
N <- 13869

# Load data on Danish herd types and size distributions
type_size_distr <- readRDS('size_type_distr.rds') %>%
  select(1,2,10,3:8) %>%
  rename(TypeProp = Prop)
# convert data to matrix to display as default for manual input
matrix_distr <- as.matrix(type_size_distr)

# Generate cattle herd population function
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


#### Diagnostic tests ####

# Text for the section Diagnostic tests
tslaugh <- list(h3('Slaughter samples'))
theif <- list(h3('Heifer samples'))
tcalf <- list(h3('Calves samples'))
tadult <- list(h3('Adult samples'))
tvol <- list(h3('Voluntary samples'))

# Default values for the section Diagnostic tests

# Animal blood sample sensitivity
seAni <- 0.95
# Animal blood sample specificity
spAni <- 0.98
# Probability of slaughter samples for each herd type
probSampSlaugh = c(0,0.5,0.5,0.5,0.5,0.5)
# Probability of heifer samples for each herd type
probSampHeif = c(c(0,1,1,0,0,0),c(1,1,1,1,1,1))
# Number of Heifers sampled
nHeif <- 8
# Probability of calf samples for each herd type
probSampCalf = c(c(0,0,0,0,0,0),c(1,1,1,1,1,1))
# Number of Heifers sampled
nCalf <- 8
# Probability of adult samples for each herd type
probSampAdult = c(c(0,0,0,0,0,0),c(0,1,1,1,1,1))
# Number of Heifers sampled
nAdult <- 8
# Probability of voluntary sampling for each herd type
probSampVol <- c(c(0,0,0,0,0,0),c(0,0,0,0,0,0))
# Number of voluntary samples - Consider change to proportion
nVol <- 8


# Culture clinical sample sensitivity
seClin <- 0.98
# Culture clinical sample specificity
spClin <- 0.99
# Probability of clinical disease
probClinInf <- 0.1 # infected herd
probClin <- 0.02 # non-infected herd

# Bulk tank milk sample sensitivity
seMilk <- 0.80
# Bulk tank milk sample specificity
spMilk <- 0.80
# Probability of sampling for each herd type
probSampMilk = c(1,0,0,0,0,0)


#### True prevalence ####
# Default values for the prevalence
# Herd level prevalence
prevHerd <- 0.4
# With-in farm level prevalence
prevAni <- 0.4


#### Surveillance output ####
# Function for generating surveillance output
sdubcontrol <- function(data_pop,prevHerd,prevAni,probSamp,seAni,spAni,nHeif,nCalf,nAdult,nVol,seClin,spClin,probClinInf,probClin,seMilk,spMilk,burnin=3) {

  #SDLevel <- matrix(NA, nrow=N, ncol=burnin)

  pop_ed <- data_pop %>%
    mutate(SDInf = rbinom(1,1,prevHerd),
           SDInfText = factor(SDInf, levels=c(0,1), labels=c('Non infected','Infected')),
           SDLevel=1,
           CalfHerdID = ifelse(CalfYears > 0,1,0),
           HeifHerdID = ifelse(HeiferYears > 0,1,0),
           AdultHerdID = ifelse(CalfHerdID==0 & HeifHerdID==0,1,0),
           nCalf_2 = case_when(CalfYears<=15 ~ 8,
                               CalfYears>15 & CalfYears<=30 ~ 14,
                               CalfYears>30 & CalfYears<=50 ~ 20,
                               CalfYears>50 ~ 25),
           nCalf_2 = ifelse(nCalf_2==8 & nCalf_2>CalfYears,CalfYears,nCalf_2),
           nHeif_2 = case_when(HeiferYears<=15 ~ 8,
                               HeiferYears>15 & HeiferYears<=30 ~ 14,
                               HeiferYears>30 & HeiferYears<=50 ~ 20,
                               HeiferYears>50 ~ 25),
           nHeif_2 = ifelse(nHeif_2==8 & nHeif_2>HeiferYears,HeiferYears,nHeif_2),
           nAdult_2 = case_when(CattleYears<=15 ~ 8,
                                CattleYears>15 & CattleYears<=30 ~ 14,
                                CattleYears>30 & CattleYears<=50 ~ 20,
                                CattleYears>50 ~ 25),
           nAdult_2 = ifelse(nAdult_2==8 & nAdult_2>CattleYears,CattleYears,nAdult_2)
    ) %>%
    left_join(probSamp, by='HerdTypeText')

  for(t in 1:burnin) {

    pop_ed <- pop_ed %>%
      mutate(SampSlaugh = rbinom(1,1,probSampSlaugh),
             SampHeif = rbinom(1,1,ifelse(SDLevel==2 & HeifHerdID==1,probSampHeif_level2,probSampHeif_level1)),
             SampCalf = rbinom(1,1,ifelse(SDLevel==2 & CalfHerdID==1,probSampCalf_level2,probSampCalf_level1)),
             SampAdult = rbinom(1,1,ifelse(SDLevel==2 & AdultHerdID==1,probSampAdult_level2,probSampAdult_level1)),
             SampVol = rbinom(1,1,ifelse(SDLevel==2,probSampVol_level2,probSampVol_level1)),
             SampSusp = ifelse(SDInf==1,
                               rbinom(1,1,probClinInf),
                               rbinom(1,1,probClin)),
             SampMilk = rbinom(1,1,probSampMilk)
      ) %>%
      mutate(ResultSlaugh = ifelse(SDInf==1,
                                   SampSlaugh*rbinom(1,1,(prevAni*seAni+(1-prevAni)*(1-spAni))),
                                   SampSlaugh*rbinom(1,1,1-spAni)),
             ResultHeif = ifelse(SDInf==1,
                                 SampHeif*rbinom(1,1,1-(1-(prevAni*seAni+(1-prevAni)*(1-spAni)))^ifelse(SDLevel==1,nHeif,nHeif_2)),
                                 SampHeif*rbinom(1,1,1-(1-(1-spAni))^nHeif)),
             ResultCalf = ifelse(SDInf==1,
                                 SampCalf*rbinom(1,1,1-(1-(prevAni*seAni+(1-prevAni)*(1-spAni)))^ifelse(SDLevel==1,nCalf,nHeif_2)),
                                 SampCalf*rbinom(1,1,1-(1-(1-spAni))^nCalf)),
             ResultAdult = ifelse(SDInf==1,
                                  SampAdult*rbinom(1,1,1-(1-(prevAni*seAni+(1-prevAni)*(1-spAni)))^ifelse(SDLevel==1,nAdult,nAdult_2)),
                                  SampAdult*rbinom(1,1,1-(1-(1-spAni))^nAdult)),
             ResultVol = ifelse(SDInf==1,
                                SampVol*rbinom(1,1,1-(1-(prevAni*seAni+(1-prevAni)*(1-spAni)))^nVol),
                                SampVol*rbinom(1,1,1-(1-(1-spAni))^nVol)),
             ResultSusp = ifelse(SDInf==1,
                                 SampSusp*rbinom(1,1,seClin),
                                 SampSusp*rbinom(1,1,1-spClin)),
             ResultMilk = ifelse(SDInf==1,
                                 SampMilk*rbinom(1,1,seMilk),
                                 SampMilk*rbinom(1,1,1-spMilk))
      ) %>%
      mutate(SampSum = sum(SampSlaugh,SampHeif,SampCalf,SampAdult,SampVol,SampSusp,SampMilk),
             ResultSum = sum(ResultSlaugh,ResultHeif,ResultCalf,ResultAdult,ResultVol,ResultSusp,ResultMilk),
             SDLevel = case_when(SampSum>0 & ResultSum==0 ~ 1,
                                 SampSum>0 & ResultSum>0 ~ 2,
                                 SDLevel==1 & SampSum==0 ~ 1,
                                 SDLevel==2 & SampSum==0 ~ 2),
             SDLevelText = factor(SDLevel, levels=c(1,2), labels=c('Level 1','Level 2'))
      )

    #SDLevel[,t] <- pop_ed$SDLevel

  }

  return(pop_ed)

}


# Function for generating cross table for overall population
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

# Function for generating herd type level table
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
