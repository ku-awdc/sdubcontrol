#' Function for generating surveillance output
#'
#' @param data_pop
#' @param prevHerd
#' @param prevAni
#' @param probSamp
#' @param seAni
#' @param spAni
#' @param nHeif
#' @param nCalf
#' @param nAdult
#' @param nVol
#' @param seClin
#' @param spClin
#' @param probClinInf
#' @param probClin
#' @param seMilk
#' @param spMilk
#' @param burnin
#'
#' @export
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
