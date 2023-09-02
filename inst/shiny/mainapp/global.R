library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyMatrix)
library(sdubcontrol)

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
data("CattleDenmark", envir=environment())
stopifnot(is.data.frame(CattleDenmark))
type_size_distr <- CattleDenmark %>%
  select(1,2,10,3:8) %>%
  rename(TypeProp = Prop)
# convert data to matrix to display as default for manual input
matrix_distr <- as.matrix(type_size_distr)



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


