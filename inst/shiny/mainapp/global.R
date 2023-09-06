library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyMatrix)
library(sdubcontrol)

# Text for the section Introduction
introduction <- list(p("The sdubcontrol package is meant as a tool to support decisions on a surveillance control program for Salmonella Dublin in cattle."),
                     p("In the current setup the surveillance programme in the package classifies cattle herds into Level 1 (probably not infected with Salmonella Dublin) and Level 2 (probably infected with Salmonella Dublin) and the package outputs a sensitivity and specificity for this classification assuming a true infection prevalence."),
                     p("The package allows the user to input a cattle herd population with specified herd types (e.g. Dairy herd) and herd sizes.",
                       "This is done in the menu 'Cattle population'. Data on the cattle herd population may be uploaded directly or may be simulated from summary data."),
                     p("Subsequently the user may choose what tests for Salmonella Dublin to include in the surveillance program. This is done in the menu 'Diagnostic tests'.",
                       "The current version of the package includes animal blood samples, samples taken at clinical suspicion of Salmonella Dublin and bulk tank milk samples.",
                       "For each test type the sensitivity and specificity may be adjusted. Furthermore, the user may specify how often different herd types or age-groups are sampled."),
                     p("Finally, the package output how herds are classified into Level 1 and Level 2 given the above inputs and a true infection prevalence."))

# explain <- list(p(""),
#                      p(""),
#                      p(""),
#                      p(""),
#                      p(""))


#### Cattle herd population ####

# Default population size (DK 13869)
N <- 5432

# Load data on Danish herd types and size distributions
data("CattleDenmark", envir=environment())
stopifnot(is.data.frame(CattleDenmark))
type_size_distr <- CattleDenmark %>%
  select(1,2,10,3:8) %>%
  rename(TypeProp = Prop)
# convert data to matrix to display as default for manual input
matrix_distr <- as.matrix(type_size_distr)

# Text for the section Cattle population
tpop <- list(h3('Head of cattle herd population data'))
ttype <- list(h3('Herd type distribution'))
tsize <- list(h3('Herd size distributions'))
thprop <- list(h3('Distributions of the proportion of heifers'))
tcprop <- list(h3('Distributions of the proportion of calves'))

#### Diagnostic tests ####

# Text for the section Diagnostic tests
tslaugh <- list(h3('Slaughter samples'))
theif <- list(h3('Heifer samples'))
tcalf <- list(h3('Calves samples'))
tadult <- list(h3('Adult samples'))
tvol <- list(h3('Voluntary samples'))

# Default values for the section Diagnostic tests

# Animal blood sample sensitivity
seAni <- 0.90
# Animal blood sample specificity
spAni <- 0.96
# Probability of slaughter samples for each herd type
probSampSlaugh = c(0,0.1,0.1,0.3,0.9,0.3)
# Probability of heifer samples for each herd type
probSampHeif = c(c(0,0.25,0.5,0,0,0),c(0.5,0.5,0.5,0.5,0.5,0.5))
# Number of Heifers sampled
nHeif <- 8
# Probability of calf samples for each herd type
probSampCalf = c(c(0,0,0,0,0,0),c(1,1,1,1,1,1))
# Number of Heifers sampled
nCalf <- 8
# Probability of adult samples for each herd type
probSampAdult = c(c(0,0,0,0,0,0),c(0,0.5,0.5,0.5,0.5,0.5))
# Number of Heifers sampled
nAdult <- 8
# Probability of voluntary sampling for each herd type
probSampVol <- c(c(0,0,0,0,0,0),c(0,0,0,0,0,0))
# Number of voluntary samples - Consider change to proportion
nVol <- 8


# Culture clinical sample sensitivity
seClin <- 0.80
# Culture clinical sample specificity
spClin <- 0.99
# Probability of clinical disease
probClinInf <- 0.1 # infected herd
probClin <- 0.02 # non-infected herd

# Bulk tank milk sample sensitivity
seMilk <- 0.95
# Bulk tank milk sample specificity
spMilk <- 0.96
# Probability of sampling for each herd type
probSampMilk = c(1,0,0,0,0,0)


#### True prevalence ####
# Default values for the prevalence
# Herd level prevalence
prevHerd <- 0.1
# With-in farm level prevalence
prevAni <- 0.1


