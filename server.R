library(shinydashboard)
library(shinyjs)
library(rgdal)
library(rgeos)
library(tidyr)
library(ggplot2)
library(hydroGOF)
library(ggpubr)
library(xtable)
library(gbm)
library(leaflet)
library(htmltools)
library(colourpicker)
library(DT)
library(tools)

server <- function(input, output, session) {
  source('scripts/misc.r')
  source('scripts/misc_funcs.R')
  source('scripts/brt.functions.R')
  
  #### Allocate reactive values/outputs ####
  dataSelect <- reactiveValues()
  
  dataSelection <- reactive({
    if(input$dataInput == 'upload'){
      dataSelect$dataset <- input$datasetUpload
      dataSelect$vars_meta <- input$metadataUpload
      dataSelect$catchments <- input$shpUpload
    }else{
      dataSelect$dataset <- read.csv('tables/RussianRiver_data_obs.csv')
      dataSelect$dataset_preds <- read.csv('tables/RussianRiver_data_preds.csv')
      dataSelect$vars_meta <- read.csv('tables/CA_vars_meta.csv',
                                       stringsAsFactors = F)
      dataSelect$catchments <- readOGR('shp', 'Russian_catchments',
                                       verbose = F)
    }
    vars_meta <- dataSelect$vars_meta
    dataSelect$originalDataset <- dataSelect$dataset
    dataSelect$resvars <- as.character(vars_meta[vars_meta$vartype == 'res',
                                                 'shortname'])
    dataSelect$predvars <- as.character(vars_meta[vars_meta$vartype == 'res',
                                                  "shortname"])
    vars <- as.list(dataSelect$resvars)
    names(vars) <- vars_meta$fullnames[vars_meta$shortname %in% dataSelect$resvars]
    dataSelect$varsChoices <- vars
  })
  
  isolate(dataSelection())
  
  modelResults <- reactiveValues()
  modelResults$gbm <- vector(mode = 'list')
  modelResults$valPred <- vector(mode = 'list')
  modelResults$testPred <- vector(mode = 'list')
  modelResults$summary <- vector(mode = 'list')
  
  dataOutput <- reactiveValues()
  
  #### Input Data ####
  
  observeEvent(input$uploaded, {
    dataSelect$dataset <- read.csv(input$datasetUpload$datapath)
    dataSelect$vars_meta <- read.csv(input$metadataUpload$datapath)
    dataSelect$catchmentsZIP <- input$shpUpload
    filenames <- unzip(dataSelect$catchmentsZIP$datapath, list=TRUE)
    unzip(dataSelect$catchmentsZIP$datapath, exdir = getwd())
    shpfile <- tools::file_path_sans_ext(filenames[1,1])
    dataSelect$catchments <- readOGR('.', shpfile)
  })
  
  observeEvent(input$startAssessment, {
    updateTabItems(session, 'sidebarMenu', 
                   selected = 'dataInputTab')
  })
  
  output$download2014 <- downloadHandler(
    filename = function() { paste('CA_2014_input.csv') },
    content = function(file) {
      file.copy('tables/CAdata_obs.csv', file)
    },
    contentType = "text/csv"
  )
  output$download2014meta <- downloadHandler(
    filename = function() {
      'CA_2014_input_meta.csv'
    },
    content = function(file) {
      file.copy('tables/CA_vars_meta.csv', file)
    },
    contentType = "text/csv"
  )

  #### Data Pre-processing ####
  
  observeEvent(input$gotoPreprocessing, {
    updateTabItems(session, 'sidebarMenu', 
                   selected = 'preprocessTab')
  })
  
  observeEvent(input$skipToMetrics,{
    updateTabItems(session, 'sidebarMenu',
                   selected = 'metricsTab')
  })
  
  varGroup <- reactive({
    vars_meta <- dataSelect$vars_meta
    resNames <- as.list(vars_meta[vars_meta$vartype == "res", "shortname"])
    names(resNames) <- vars_meta[vars_meta$vartype == "res", "fullnames"]
    preNames <- as.list(vars_meta[vars_meta$vartype == "exp", "shortname"])
    names(preNames) <- vars_meta[vars_meta$vartype == "exp", "fullnames"]
    list("Response" = resNames, "Predictor" = preNames)
  })

  observe({
    varName <- reactive({names(varGroup())})
    updateRadioButtons(session, "varType", choices = varName())
  })
  
  observe({
    varList <- reactive({varGroup()[[input$varType]]})
    updateSelectInput(session, "varSelect", choices = varList())
  })

  tf <- reactive({
    switch(input$trans_func,
               None = function(y) {y},
               log10 = log10,
               log1p = log1p,
               ln = log,
               sqrt = sqrt,
               sq = function(y) {y^2},
               sq4 = function(y) {y^(1/4)})
  })
  
  output$distPlot <- renderPlot({
    vars_meta <- dataSelect$vars_meta
    dataset <- dataSelect$dataset
    x <- dataset[, input$varSelect]
    xname <- vars_meta$fullnames[match(input$varSelect, vars_meta$shortname)]
    if( is.numeric(x) ){
      xplot <- tf()(dataset[, input$varSelect]) %>% na.exclude()
      if( is.numeric(xplot) & (sum(!is.finite(xplot)) == 0) ){
        dataset %>% drop_na(input$varSelect) %>%
        ggplot(aes(x = tf()(get(input$varSelect)))) +
          geom_histogram(aes(y = ..density..), bins = input$bins) +
          geom_density(alpha = 0.2, fill = 'lightblue') +
          theme(plot.title = element_text(size = 20, hjust = 0.5),
                panel.border = element_rect('black', fill = NA, size = 0.5),
                axis.title = element_text(size = 15),
                axis.text = element_text(size = 12)) + 
          labs(title = paste('Histogram and Probability Density of', xname),
               x = tf_name(as.character(xname), type = input$trans_func),
               y = 'Count')
      }else{
        plot.new()
        plot.window(c(-1,1), c(-1,1))
        text(0,0, 'Error: transformation resulted in non-numeric values',
             cex = 1.5)
      }
    }else{
      plot.new()
      plot.window(c(-1,1), c(-1,1))
      text(0,0, 'Transformation not available for categorical variables',
           cex = 1.5)
    }
  })
  
  observeEvent(input$transVar, {
    x <- suppressWarnings(tf()(dataSelect$dataset[, input$varSelect]))
    dataSelect$dataset[, input$varSelect] <- x
    updateRadioButtons(session, 'trans_func', 
                       selected = 'None')
  })

  observeEvent(input$transAllVar, {
    dclass <- c()
    for( i in 1:ncol(dataSelect$dataset) ){ 
      dclass[i] <- class(dataSelect$dataset[, i])
    }
    d <- suppressWarnings(apply(obs[, dclass == 'numeric'], 2, tf()))
    dataSelect$dataset[, dclass == 'numeric'] <- d
    updateRadioButtons(session, 'trans_func', 
                       selected = 'None')
  })
  
  observeEvent(input$transReset, {
    dataSelect$dataset <- dataSelect$originalDataset
    updateRadioButtons()
  })
  
  observeEvent(input$gotoStep4, {
    dataSelect$dataset <- remove_nan_inf(dataSelect$dataset)
    vars_meta <- dataSelect$vars_meta
    dataset <- dataSelect$dataset
    resvars <- dataSelect$resvars
    gbmdata <- vector(mode = 'list')
    for(j in resvars){
      tempdata <- dataset[is.na(dataset[, j]) == F, ]
      prop <- c(1 - input$valFrac - input$testFrac, 
                input$valFrac, input$testFrac)
      gbmdata[[j]] <- splitsets(tempdata, prop)
    }
    dataSelect$gbmdata <- gbmdata
    updateTabItems(session, 'sidebarMenu',
                   selected = 'modelFitTab')
  })
  
  observeEvent(input$trans2014, {
    load('rdata/gbmdata.Rdata')
    dataSelect$gbmdata <- gbmdata
    updateTabItems(session, 'sidebarMenu',
                   selected = 'modelFitTab')
  })
  
  #### Model Fitting ####

  output$modelVar <- renderUI({
    selectInput('varModelRun', 'Select response variable to model:',
                choices = dataSelect$varsChoices)
  })
  
  observeEvent(input$runModel, {
    gbmdata <- dataSelect$gbmdata
    gbmRes <- gbmdata[[input$varModelRun]]
    gbm.x <- which(names(gbmRes$train) %in% dataSelect$predvars)
    gbm.y <- which(names(gbmRes$train) == input$varModelRun)

    runModel <- function(input){
      x <- gbm.step(data=gbmRes$train, gbm.x, gbm.y,
                    family = input$modelFamily,
                    tree.complexity = input$treeComplexity, 
                    learning.rate = input$learnRate,
                    bag.fraction = input$bagFrac,
                    n.folds = input$kfolds,
                    max.trees = input$maxTrees)
      return(x)
    }
    
    withCallingHandlers({
      shinyjs::html('modelRunning', '')
      modelResults$gbm[[input$varModelRun]] <- runModel(input)
      },
      message = function(m) {
        shinyjs::html(id = 'modelRunning', 
                      html = paste(m$message), add = TRUE)
      })
    modelResults$valPred[[input$varModelRun]] <-
      predict.gbm(modelResults$gbm[[input$varModelRun]], 
                  dataSelect$gbmdata[[input$varModelRun]]$val,
                  n.trees = modelResults$gbm[[input$varModelRun]]$gbm.call$best.trees,
                  type = 'response')
    modelResults$testPred[[input$varModelRun]] <-
      predict.gbm(modelResults$gbm[[input$varModelRun]], 
                  dataSelect$gbmdata[[input$varModelRun]]$test,
                  n.trees = modelResults$gbm[[input$varModelRun]]$gbm.call$best.trees,
                  type = 'response')
    modelResults$summary[[input$varModelRun]] <-
      summary(modelResults$gbm[[input$varModelRun]], plotit = FALSE)
  })
  
  observeEvent(input$runModelAllVar, {
    gbmdata <- dataSelect$gbmdata
    vars_meta <- dataSelect$vars_meta
    resvarsFull <- as.character(vars_meta$fullnames[
      vars_meta$shortname %in% dataSelect$resvars])
    predvars <- as.character(vars_meta[vars_meta$vartype == 'exp', 'shortname'])
    withProgress(message = 'Running models for all responses', value = 0, {
      n <- 0
      m <- length(dataSelect$resvars)
      for(i in dataSelect$resvars){
        n <- n + 1
        incProgress(1/m, detail = paste('\n', resvarsFull[n]))
        gbmRes <- gbmdata[[i]]
        gbm.x <- which(names(gbmRes[[1]]) %in% predvars)
        gbm.y <- which(names(gbmRes[[1]]) == i)
        
        modelResults$gbm[[i]] <- gbm.step(data=gbmRes$train, gbm.x, gbm.y,
                                         family = input$modelFamily,
                                         tree.complexity = input$treeComplexity, 
                                         learning.rate = input$learnRate,
                                         bag.fraction = input$bagFrac,
                                         n.folds = input$kfolds,
                                         max.trees = input$maxTrees,
                                         verbose = F, silent = T)
        modelResults$valPred[[i]] <-
          predict.gbm(modelResults$gbm[[i]], 
                      dataSelect$gbmdata[[i]]$val,
                      n.trees = modelResults$gbm[[i]]$gbm.call$best.trees,
                      type = 'response')
        modelResults$testPred[[i]] <-
          predict.gbm(modelResults$gbm[[i]], 
                      dataSelect$gbmdata[[i]]$test,
                      n.trees = modelResults$gbm[[i]]$gbm.call$best.trees,
                      type = 'response')
        modelResults$summary[[i]] <-
          summary(modelResults$gbm[[i]], plotit = FALSE)
      }
    })
  })
  
  observeEvent(input$useModels2014, {
    withProgress(message = 'Loading 2014 models...', value = 100, {
      load('rdata/results_gbm.RData')
      modelResults$gbm <- results.gbm
      updateTabItems(session, 'sidebarMenu',
                     selected = 'diagnosticsTab')
      modelResults$use2014 <- TRUE
      load('rdata/results_preds.RData')
      load('rdata/results_summary.RData')
      modelResults$valPred <- results.preds
      modelResults$testPred <- results.preds
      modelResults$summary <- results.summary
    })
  })
  
  observeEvent(input$gotoStep5, {
    updateTabItems(session, 'sidebarMenu',
                   selected = 'diagnosticsTab')
  })

  #### Diagnostics ####
  
  observe({
    varDiag <- reactive({dataSelect$varsChoices})
    updateSelectInput(session, "varDiag", 
                       choices = varDiag())
  })

  output$diagScatter <- renderPlot({
    
    gbmModel <- modelResults$gbm[[input$varDiag]]
    gbmdata <- dataSelect$gbmdata[[input$varDiag]]
    trainDat <- data.frame(obs = gbmdata$train[, input$varDiag],
                           train = fitted(gbmModel))
    valDat <- data.frame(obs = gbmdata$val[, input$varDiag],
                         val = modelResults$valPred[[input$varDiag]])
    if( !is.null(modelResults$use2014) ){
      testDat <- valDat
      names(testDat)[2] <- 'test'
    }else{
      testDat <- data.frame(obs = gbmdata$test[, input$varDiag],
                            test = modelResults$testPred[[input$varDiag]])
    }
    
    ymin <- signif(min(c(trainDat$obs, valDat$obs, testDat$obs,
                        trainDat$train, valDat$val, testDat$test)), 4)
    ymax <- signif(max(c(trainDat$obs, valDat$obs, testDat$obs,
                        trainDat$train, valDat$val, testDat$test)), 4)
  
    trainPlot <- ggplot(trainDat, aes(x = obs, y = train)) + geom_point() +
      ylim(ymin, ymax) +
      geom_abline(size = 0.75, show.legend = F,
                  aes(slope = 1, intercept = 0, colour = '1:1 Line')) +
      geom_smooth(method = 'lm', size = 0.75, 
                  aes(colour = 'Fit Line and 95 CI')) +
      scale_colour_manual(name = "Legend", 
                          values = c('black', 'blue')) + 
      labs(title = 'Training Set') +
      theme(plot.title = element_text(size = 14, hjust = 0.5),
            panel.border = element_rect('black', fill = NA, size = 0.5),
            axis.text = element_text(size = 12),
            axis.title = element_blank(),
            legend.position = c(0.2, 0.85), 
            legend.background = element_rect(colour = 'black', 
                                             fill = 'grey90', size = 1, 
                                             linetype = 'solid'))
    
    valPlot <- ggplot(valDat, aes(x = obs, y = val)) + geom_point() +
      ylim(ymin, ymax) + geom_abline(size = 0.75) +
      geom_smooth(method = 'lm', size = 0.75, colour = 'darkgreen') +
      labs(title = 'Validation Set') +
      theme(plot.title = element_text(size = 14, hjust = 0.5),
            panel.border = element_rect('black', fill = NA, size = 0.5),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_blank(),
            axis.title = element_blank())

    testPlot <- ggplot(testDat, aes(x = obs, y = test)) + geom_point() +
      ylim(ymin, ymax) + geom_abline(size = 0.75) +
      geom_smooth(method = 'lm', size = 0.75, colour = 'red') +
      labs(title = 'Testing Set') +
      theme(plot.title = element_text(size = 14, hjust = 0.5),
            panel.border = element_rect('black', fill = NA, size = 0.5),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_blank(),
            axis.title = element_blank())

    allPlots <- ggarrange(trainPlot, valPlot, testPlot, nrow = 1, ncol = 3)

    annotate_figure(allPlots,
                    left = text_grob('Model Predictions', size = 14, rot = 90),
                    bottom = text_grob('Observations', size = 14))
  })
  
  output$gofStatsTable <- renderUI({
    
    gbmModel <- modelResults$gbm[[input$varDiag]]
    gbmdata <- dataSelect$gbmdata[[input$varDiag]]
    trainDat <- data.frame(obs = gbmdata$train[, input$varDiag],
                           train = fitted(gbmModel))
    valDat <- data.frame(obs = gbmdata$val[, input$varDiag],
                         val = modelResults$valPred[[input$varDiag]])
    if( !is.null(modelResults$use2014) ){
      testDat <- valDat
      names(testDat)[2] <- 'test'
    }else{
      testDat <- data.frame(obs = gbmdata$test[, input$varDiag],
                            test = modelResults$testPred[[input$varDiag]])
    }
    tab <- gofNames
    tab$Training <- gof(trainDat$train, trainDat$obs, do.spearman = T)
    tab$Validation <- gof(valDat$val, valDat$obs, do.spearman = T)
    tab$Test <-  gof(testDat$obs, testDat$test, do.spearman = T)
    gofExclude <- c(2, 4, 5, 7, 10, 11, 13, 14, 18, 19, 20)
    tab <- tab[-gofExclude, ]

    outTab <- print.xtable(xtable(tab), 
                           sanitize.text.function = function(x){ 
                             a <- gsub(' ', '~', x)
                             b <- paste('\\mathrm{',a,'}',sep='')
                             return(b)},
                           floating = FALSE, tabular.environment = 'array',
                           comment = FALSE, print.results = FALSE)
    
    withMathJax(outTab)
  })
  
  observeEvent(input$infillVar, {
    withProgress(message = 'Infilling response data...', value = 100, {
    gbmModel <- modelResults$gbm[[input$varDiag]]
    ind <- is.na(dataSelect$dataset[, input$varDiag])
    dataSelect$dataset[ind, input$varDiag] <-
      predict.gbm(gbmModel,
                  dataSelect$dataset[ind, ],
                  n.trees = gbmModel$gbm.call$best.trees,
                  type = 'response')
    })
  })
  
  observeEvent(input$infill_allVar, {
    vars_meta <- dataSelect$vars_meta
    resvarsFull <- as.character(vars_meta$fullnames[
      vars_meta$shortname %in% dataSelect$resvars])
    withProgress(message = 'Infilling all responses', value = 0, {
      n <- 0
      m <- length(dataSelect$resvars)
      for(i in dataSelect$resvars){
        n <- n + 1
        incProgress(1/m, detail = paste('\n', resvarsFull[n]))
        gbmModel <- modelResults$gbm[[i]]
        ind <- is.na(dataSelect$dataset[, i])
        dataSelect$dataset[ind, i] <-
          predict.gbm(gbmModel,
                      dataSelect$dataset[ind, ],
                      n.trees = gbmModel$gbm.call$best.trees,
                      type = 'response')
      }
    })
  })
  
  observeEvent(input$gotoStep6, {
    updateTabItems(session, 'sidebarMenu', selected = 'metricsTab')
  })
  
  #### Metric Calculation ####

  varInd <- reactive({
    x <- as.list(as.character(dataSelect$vars_meta$shortname))
    names(x) <- dataSelect$vars_meta$fullnames
    return(x)
  })

  observe({
    updateSelectInput(session, "varInd", 
                      choices = varInd(),
                      selected = varInd()[dataSelect$vars_meta[, input$varMMI]])
  })
  
  observe({
    # TODO: make this not hard coded or at least require the input data
    # to have certain column headings; also change vars_meta_MMI to just vars_meta
    MMI <- matrix(nrow = nrow(dataSelect$dataset),
                  ncol = ncol(dataSelect$vars_meta[, -c(1:4)]))
    MMI <- as.data.frame(MMI)
    names(MMI) <- names(dataSelect$vars_meta)[-c(1:4)]
    dataOutput$MMI <- MMI
  })
  
  observeEvent(input$calcMMI, {
    varUse <- as.character(input$varInd)
    dataset <- dataSelect$dataset[, varUse]
    MMI <- dataOutput$MMI
    # TODO: need to change this so only one metadata object
    vars_meta <- dataSelect$vars_meta
    direction <- vars_meta[vars_meta$shortname %in% varUse, 'PositiveDir']
    
    # remove NAs
    dataset <- dataset[, !is.na(direction)]
    direction <- na.exclude(direction)

    if( length(varUse) == 1){
      MMI[, input$varMMI] <- indexDirAlign(dataset, direction)
    }else{
      MMI_comp <- mapply(indexDirAlign, x = dataset, dir = direction)
      MMI[, input$varMMI] <- indexDirAlign(
        apply(MMI_comp, 1, mean, na.rm=T), TRUE)
    }
    
    dataOutput$MMI <- MMI
  })
  
  observeEvent(input$calcMMI_2014, {
    if(input$dataInput == 'upload'){
      dataset <- dataSelect$dataset
    }else{
      dataset <- dataSelect$dataset_preds
    }
    
    vars_meta <- dataSelect$vars_meta
    MMI <- dataOutput$MMI
    n <- 0
    for(i in names(MMI)){
      n <- n + 1
      #varUse <- as.character(vars_meta$shortname[vars_meta[, input$varMMI]])
      varUse <- as.character(vars_meta$shortname[vars_meta[, i]])
      direction <- vars_meta$PositiveDir[vars_meta$shortname %in% varUse]
      if( length(varUse) == 1){
        MMI[, n] <- indexDirAlign(dataset[, varUse], direction)
      }else{
        MMI_comp <- mapply(indexDirAlign, x = dataset[, varUse],
                           dir = direction)
        MMI[, n] <- indexDirAlign(apply(MMI_comp, 1, mean, na.rm=T), TRUE)
      }
    }
    
    dataOutput$MMI <- MMI
    
  })
  
  observeEvent(input$explore, {
    updateTabItems(session, 'sidebarMenu', selected = 'exploreTab')
  })
    
  #### Explore ####
  dispOutVar <- list('Watershed' = 'Watershed Condition',
                     'PhysBio' = 'Physical and Biological Habitat Condition',
                     'WaterQuality' = 'Water Quality Condition',
                     'Landscape' = 'Overall Landscape Condition',
                     'Natural' = 'Natural Condition',
                     'Anthropogenic' = 'Anthropogenic Condition',
                     'InstreamBio' = 'Instream Biological Condition',
                     'ClimateChange' = 'Climate Change Vulnerability',
                     'LandCover' = 'Land Cover Vulnerability',
                     'WaterUse' = 'Water Use Vulnerability',
                     'Fire' = 'Fire Vulnerability')
  #### Leaflet ####
  colr <- colorRampPalette(c('red', 'blue'))
  
  output$map <- renderLeaflet({
    bbox <- as.vector(dataSelect$catchments@bbox)
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
                       ) %>%
      fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
  })
  
  observe({
    colrSym <- colorRampPalette(c(input$col1, input$col2))
    colrSymOut <- colrSym(nrow(dataSelect$catchments))[
      order(dataOutput$MMI[, input$dispOutVar])]
    labs <- lapply(seq(nrow(dataSelect$catchments@data)), function(i){
      paste0('Reach Code: ', dataSelect$catchments@data[i, 'REACHCODE'],
             '<br>',dispOutVar[[input$dispOutVar]], ': ',
             sprintf( '%.3f', dataOutput$MMI[i, input$dispOutVar]) )
    })
    leafletProxy('map') %>% clearShapes() %>% clearMarkers() %>%
      addPolygons(data = dataSelect$catchments, 
                  fillColor = colrSymOut, stroke = F,
                  fillOpacity = 1-input$mapTrans/100,
                  popup = lapply(labs, HTML))
  })
  
  output$summaryTable <- renderTable({
    dat <- cbind(as.character(dataSelect$catchments$REACHCODE),
                 sprintf('%.3f', dataOutput$MMI[, input$dispOutVar]))
    datOrdered <- dat[order(dat[, 2]), ]
    sdDat <- sd(dataOutput$MMI[, input$dispOutVar], na.rm=T)
    sumTab <- rbind(datOrdered[1, ], 
                    datOrdered[round(nrow(dat)/2), ],
                    datOrdered[nrow(dat), ],
                    c('Not Applicable', sprintf('%.3f', sdDat)))
    sumTab <- as.data.frame(sumTab)
    names(sumTab) <- c('Reach Code', 'Value')
    sumTab <- sumTab[, c('Value', 'Reach Code')]
    row.names(sumTab) <- c('Minimum', 'Median', 'Maximum', 'Std. Dev.')
    dataOutput$sumTab <- sumTab
    sumTab
  }, include.rownames= TRUE, spacing = 'm', width = 300)
  
  observeEvent(input$dispSumCatchments,{
    ind_shp <- match(dataOutput$sumTab$'Reach Code'[1:3], 
                     dataSelect$catchments$REACHCODE)
    dispPts <- gCentroid(dataSelect$catchments[ind_shp, ], byid = TRUE)
    
    leafletProxy('map') %>%
      addMarkers(
        lng = dispPts$x, lat = dispPts$y,
        label = c('Minimum', 'Median', 'Maximum'),
        labelOptions = labelOptions(noHide = T, textsize = '15px')
      )
  })
  
  #### View Data ####
  output$viewDataset <- DT::renderDataTable(dataSelect$dataset,
                                        options = list(pageLength = 5,
                                                       scrollX = TRUE)
  )
  output$viewMetadata <- DT::renderDataTable(dataSelect$vars_meta,
                                         options = list(pageLength = 5,
                                                        scrollX = TRUE))
  #### end ####
}

