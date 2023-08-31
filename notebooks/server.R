shinyServer(

  function(input, output, session) {

    # Introduction text
    # output$introduction <- renderText({introduction})

    # distrdata <- reactive({
    #
    #   File <- input$upload_distr
    #   req(File)
    #
    #   read.table(File$datapath, header = TRUE, sep = "\t")
    # })

    popdata <- reactive({

      generate_cattle_population(input$N_inp, input$enter_distr)

    })

    # Text output for section on diagnotics tests
    output$tslaugh <- renderText({tslaugh})
    output$theif <- renderText({theif})
    output$tcalf <- renderText({tcalf})
    output$tvol <- renderText({tvol})

    observeEvent(input$enter_distr, {
      updateMatrixInput(session, "SlaughSamp_inp",
                        value=matrix(c(input$enter_distr[,2],probSampSlaugh),
                                     nrow=6,
                                     dimnames=list(c(1:6),c('HerdTypeText','probSampSlaugh'))))
    })

    observeEvent(input$enter_distr, {
      updateMatrixInput(session, "HeifSamp_inp",
                        value=matrix(c(input$enter_distr[,2],probSampHeif),
                                     nrow=6,
                                     dimnames=list(c(1:6),c('HerdTypeText','probSampHeif_level1','probSampHeif_level2'))))
    })

    observeEvent(input$enter_distr, {
      updateMatrixInput(session, "CalfSamp_inp",
                        value=matrix(c(input$enter_distr[,2],probSampCalf),
                                     nrow=6,
                                     dimnames=list(c(1:6),c('HerdTypeText','probSampCalf_level1','probSampCalf_level2'))))
    })

    observeEvent(input$enter_distr, {
      updateMatrixInput(session, "AdultSamp_inp",
                        value=matrix(c(input$enter_distr[,2],probSampCalf),
                                     nrow=6,
                                     dimnames=list(c(1:6),c('HerdTypeText','probSampAdult_level1','probSampAdult_level2'))))
    })

    observeEvent(input$enter_distr, {
      updateMatrixInput(session, "VolSamp_inp",
                        value=matrix(c(input$enter_distr[,2],probSampVol),
                                     nrow=6,
                                     dimnames=list(c(1:6),c('HerdTypeText','probSampVol_level1','probSampVol_level2'))))
    })

    observeEvent(input$enter_distr, {
      updateMatrixInput(session, "MilkSamp_inp",
                        value=matrix(c(input$enter_distr[,2],probSampMilk),
                                     nrow=6,
                                     dimnames=list(c(1:6),c('HerdTypeText','probSampMilk'))))
    })

    probSamp <- reactive({

      data <- as_tibble(input$SlaughSamp_inp) %>%
        left_join(as_tibble(input$HeifSamp_inp), by='HerdTypeText') %>%
        left_join(as_tibble(input$CalfSamp_inp), by='HerdTypeText') %>%
        left_join(as_tibble(input$AdultSamp_inp), by='HerdTypeText') %>%
        left_join(as_tibble(input$VolSamp_inp), by='HerdTypeText') %>%
        left_join(as_tibble(input$MilkSamp_inp), by='HerdTypeText') %>%
        mutate(across(2:11, as.numeric))

      return(data)

    })

    leveldata <- reactive({

      popdata_arg <- popdata()
      probSamp_arg <- probSamp()

      data <- sdubcontrol(popdata_arg,input$prevHerd_inp,input$prevAni_inp,
                          probSamp_arg,
                          input$seAni_inp,input$spAni_inp,input$nHeif_inp,input$nCalf_inp,input$nVol_inp,
                          input$seClin_inp,input$spClin_inp,input$probClinInf_inp,input$probClin_inp,
                          input$seMilk_inp,input$spMilk_inp)

      return(data)


    })

    # Output for population demographics
    output$view <- renderTable({
      if(input$gen_pop) {
        head(popdata())
      }
    })

    # Output for surveillance levels
    output$test <- renderTable({
      if(input$gen_pop) {
        head(leveldata())
      }
    })

    output$cross <- renderTable({
      if(input$gen_level) {
        level_cross_table(leveldata())
      }
    })

    output$typelevel <- renderTable({
      if(input$gen_level) {
        level_type(leveldata())
      }
    })






  }
)
