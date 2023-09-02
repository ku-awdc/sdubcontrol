dashboardPage(


  dashboardHeader(title='sdubcontrol'),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro"),
      menuItem("Cattle population", tabName = "pop"),
      menuItem("Diagnostic tests", tabName = "test"),
      menuItem("Surveillance classifications", tabName = "level")
    )
  ),

  dashboardBody(
    tabItems(
      tabItem(tabName = "intro",
              titlePanel(title="Introduction")
              #introduction

      ),
      tabItem(tabName = "pop",
              titlePanel(title="Input parameters for the cattle herd population"),
              fluidRow(
                column(width = 12,
                       box(width = 12,
                           title = "Option 1: Upload cattle herd population data", status='primary', solidHeader = TRUE,
                           #pop_data_explain,
                           fileInput("upload_pop", "Upload a file")
                       ),
                       box(width = 12,
                           title = "Option 2: Simulate cattle herd population", status='primary', solidHeader = TRUE,
                           #simulate_text,
                           numericInput('N_inp','Enter total population size',value=N),
                           radioButtons("distr_pref", "Please, indicate how you prefer to provide data on herd type and herd size distributions",
                                        c('Upload data'=1,'Enter data manually'=2), selected=2),
                           conditionalPanel(
                             condition = "input.distr_pref == 1",
                           fileInput("upload_distr", "Upload a file"),
                           ),
                           matrixInput("enter_distr", 'Edit values in the below matrix',
                                       value = matrix_distr,
                                       cols=list(names=TRUE), rows=list(names=FALSE)),
                           actionButton("gen_pop", "Simulate cattle population")
                       ),
                       box(width = 12,
                           title = "Population demografics", status='primary', solidHeader = TRUE,
                           tableOutput("view"),
                       ),

                )
              )
      ),
      tabItem(tabName = "test",
              titlePanel(title="Input parameters for the diagnostic tests"),
              fluidRow(
                column(width = 12,
                       box(width = 12,
                           title = "Animal blood samples", status='primary', solidHeader = TRUE,
                           numericInput('seAni_inp','Enter blood sample sensitivity',value=seAni),
                           numericInput('spAni_inp','Enter blood sample specificity',value=spAni),
                           tslaugh,
                           matrixInput("SlaughSamp_inp", 'Enter the probability of samples being taken at slaughter for each herd type',
                                       value = matrix(c(rep('type',6),probSampSlaugh),
                                                      nrow=6,
                                                      dimnames=list(c(1:6),c('HerdTypeText','probSampSlaugh'))),
                                       cols=list(names=TRUE), rows=list(names=FALSE)),
                           theif,
                           numericInput('nHeif_inp','Enter the number of heifers being sampled at each sampling round',value=nHeif),
                           matrixInput("HeifSamp_inp", 'Enter the probability of heifers being sampled for each herd type',
                                       value = matrix(c(rep('type',6),probSampHeif),
                                                      nrow=6,
                                                      dimnames=list(c(1:6),c('HerdTypeText','probSampHeif_level1','probSampHeif_level2'))),
                                       cols=list(names=TRUE), rows=list(names=FALSE)),
                           tcalf,
                           matrixInput("CalfSamp_inp", 'Enter the probability of calves being sampled for each herd type',
                                       value = matrix(c(rep('type',6),probSampCalf),
                                                      nrow=6,
                                                      dimnames=list(c(1:6),c('HerdTypeText','probSampCalf_level1','probSampCalf_level2'))),
                                       cols=list(names=TRUE), rows=list(names=FALSE)),
                           numericInput('nCalf_inp','Enter the number of calves being sampled at each sampling round',value=nCalf),
                           tadult,
                           matrixInput("AdultSamp_inp", 'Enter the probability of calves being sampled for each herd type',
                                       value = matrix(c(rep('type',6),probSampAdult),
                                                      nrow=6,
                                                      dimnames=list(c(1:6),c('HerdTypeText','probSampAdult_level1','probSampAdult_level2'))),
                                       cols=list(names=TRUE), rows=list(names=FALSE)),
                           numericInput('nAdult_inp','Enter the number of calves being sampled at each sampling round',value=nAdult),
                           tvol,
                           matrixInput("VolSamp_inp", 'Enter the probability of voluntary sampling for each herd type',
                                       value = matrix(c(rep('type',6),probSampVol),
                                                      nrow=6,
                                                      dimnames=list(c(1:6),c('HerdTypeText','probSampVol_level1','probSampVol_level2'))),
                                       cols=list(names=TRUE), rows=list(names=FALSE)),
                           numericInput('nVol_inp','Enter the number of voluntary samples at each sampling round',value=nVol),
                       ),
                       box(width = 12,
                           title = "Clinical animal sample cultures", status='primary', solidHeader = TRUE,
                           numericInput('seClin_inp','Enter culture sensitivity',value=seClin),
                           numericInput('spClin_inp','Enter culture specificity',value=spClin),
                           numericInput('probClinInf_inp','Enter probability of observing clinical signs indicating S. Dublin  in an infected herd',value=probClinInf),
                           numericInput('probClin_inp','Enter probability of observing clinical signs indicating S. Dublin in a non-infected herd',value=probClin),
                       ),
                       box(width = 12,
                           title = "Bulk tank milk samples", status='primary', solidHeader = TRUE,
                           numericInput('seMilk_inp','Enter milk sample sensitivity',value=seMilk),
                           numericInput('spMilk_inp','Enter milk sample specificity',value=spMilk),
                           matrixInput("MilkSamp_inp", 'Enter the probability of bulk tank milk being sampled for each herd type',
                                       value = matrix(c(rep('type',6),probSampMilk),
                                                      nrow=6,
                                                      dimnames=list(c(1:6),c('HerdTypeText','probSampMilk'))),
                                       cols=list(names=TRUE), rows=list(names=FALSE)),
                       ),

                )
              )
      ),
      tabItem(tabName = "level",
              titlePanel(title="Herd level classifications resulting from the surveillance program"),
              fluidRow(
                column(width = 12,
                       box(width = 3,
                           title = "Salmonalla Dublin prevalence", status='primary', solidHeader = TRUE,
                           sliderInput("prevHerd_inp", label="Set true herd level prevalence for S. Dublin", min = 0, max = 1, value = prevHerd),
                           sliderInput("prevAni_inp", label="Set within farm level prevalence for S. Dublin", min = 0, max = 1, value = prevAni),
                           actionButton("gen_level", "Output surveillance classifications")
                       ),
                       box(width = 3,
                           title = "Overall herd levels", status='primary', solidHeader = TRUE,
                           tableOutput("cross"),
                       ),
                       box(width = 3,
                           title = "Herd type levels", status='primary', solidHeader = TRUE,
                           tableOutput("typelevel"),
                       ),
                )
              )


      )
    )
  )
)
