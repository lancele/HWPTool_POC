library(shiny)
library(shinydashboard)
library(googleway)
library(rgdal)
library(leaflet)
library(colourpicker)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  withMathJax(),
  titlePanel("Healthy Watersheds Initiative"),
  sidebarLayout(
    sidebarPanel(
      h2('Overview'),
      p('This web application produces metrics detailed in the California
        Integrated Assessment of Watershed Health (2014), whose purpose was
        to identify health watersheds and characterize relative watershed 
        health across the state. The primary indices are Watershed Vulnerability,
        Watershed Condition, and Stream Health. As a pilot example, we will
        show results only for the North Coast Region. Please follow the steps
        shown as tabs on the right.'),
      br(),
      HTML('<center><img src="waterboards_logo_high_res.jpg" height="250"
           width="350" height="243"></center>')
      ),
    mainPanel(
      tabsetPanel(id = 'tabSteps',
                  #### Step 1 ####
                  tabPanel("Step 1",
                           h3('Input Data'),
                           br(),
                           HTML('<center><img src="indicators_concept_framework.PNG"
                                height="250" width="733"></center>'),
                           p('Please select an option for data input. These data
                             contain parameters in accordance with the 2014 Assessment.
                             The image above is Figure 3 from the Assessment conceptual 
                             framework showing six distinct watershed and aquatic
                             ecosystem attributes and the parameters that compose them.
                             The dataset plus meta data from 2014 are available as an
                             example. All data must be in CSV format. If uploading a
                             catchment GIS data, please zip the contents.'),
                           br(),
                           fluidRow(
                             column(2, 
                                    radioButtons('dataInput', 'Data Input Options:',
                                                 choices = list('Use 2014 data' = 'use2014',
                                                                'Upload data' = 'upload'),
                                                 selected = 'use2014')),
                             br(),
                             br()
                           ),
                           conditionalPanel(
                             condition = "input.dataInput == 'upload'",
                             fluidRow(
                               column(2,
                                      fileInput('datasetUpload', 'Choose dataset:', 
                                                multiple = FALSE,
                                                accept = c("text/csv",
                                                           "text/comma-separated-values,
                                                           text/plain",
                                                           ".csv"))),
                               column(2,
                                      fileInput('metadataUpload', 'Choose meta-data:', 
                                                multiple = FALSE,
                                                accept = c("text/csv",
                                                           "text/comma-separated-values,
                                                           text/plain",
                                                           ".csv"))),
                               column(3,
                                      fileInput('shpUpload', 'Choose catchment shapefile:', 
                                                multiple = FALSE,
                                                accept = c("application/zip",
                                                           ".zip")))
                             )
                                      ),
                           conditionalPanel(
                             condition = "input.dataInput == 'use2014'",
                             fluidRow(
                               column(2,
                                      downloadButton("download2014",
                                                     "Download 2014 Data")),
                               column(2,
                                      downloadButton("download2014meta",
                                                     "Download 2014 meta-data"))
                             )
                           ),
                           br(),
                           actionButton('gotoStep2', 'Next Step', width = 200)
                             ),
                  #### Step 2 ####
                  tabPanel("Step 2",
                           h3('Model Selection'),
                           br(),
                           p('The 2014 Assessment produced metrics for 135,255 catchments
                             within the National Hydrography Dataset (NHD Plus); however,
                             not all catchments had the same data availability. In particular,
                             missing for many catchments were stream health indicators for 
                             habitat condition (e.g. CRAM, PHAB scores); water quality
                             (instream median summer: conductivity, nitrate concentration,
                             and turbidity); and instream biological condition (CSCI scores).
                             To perform a geographically complete assessment, these missing
                             data must be filled in. The 2014 Assessment used more readily
                             available landscape variables as predictors in a model to
                             produce the missing data. This model is a machine learning 
                             method called Boosted Regression Trees (BRT), but other 
                             methods are available for this assessment.'),
                           br(),
                           radioButtons('infill_model', 'Please choose a data infilling 
                                        method:',
                                        choices = 
                                          list('Boosted Regression Trees (BRT)' = 'brt',
                                               'Artificial Neural Network (ANN)' = 'ann',
                                               'Multiple Linear Regression' = 'lm',
                                               'Dataset complete and no infilling needed' = 'skip')),
                           actionButton('gotoStep3', 'Next Step', width = 200)
                           ),
                  #### Step 3 #####
                  tabPanel("Step 3",
                           h3('Data Preparation'),
                           br(),
                           p('Before running the data infilling model, we will perform
                             pre-processing. Response and predictor data should be 
                             transformed such that they are approximately normal. 
                             Presence/absence data are assumed to be normal. Please 
                             assess the variables and choose a transformation,
                             if appropriate. Transformations resulting in nonfinite
                             values (e.g. logarithm on negative numbers) will
                             also remove the respective data rows. Also, please
                             specify a proportion of the dataset to reserve for
                             model validation.'),
                           fluidRow(
                             column(12,
                                    fluidRow(
                                      column(4,
                                             radioButtons('varType', 
                                                          'Select variable type:',
                                                          choices = ""),
                                             selectInput('varSelect', 'Select variable:',
                                                         choices = ""),
                                             radioButtons('trans_func', 'Select transformation:',
                                                          choices = list('None' = 'None',
                                                                         '\\(\\log_{10}(x)\\)' = 'log10',
                                                                         '\\(\\log_{10}(x + 1)\\)' = 'log1p',
                                                                         '\\(\\ln(x)\\)' = 'ln',
                                                                         '\\(\\sqrt{x}\\)' = 'sqrt',
                                                                         '\\(x^{2}\\)' = 'sq',
                                                                         '\\(\\sqrt[4]{x}\\)' = 'sq4'),
                                                          selected = 'None'),
                                             actionButton('transVar', 
                                                          'Transform variable'),
                                             actionButton('transAllVar',
                                                          'Transform all with selected'),
                                             br(), br(),
                                             actionButton('transReset', 'Reset to original'),
                                             br(), br(),
                                             actionButton('gotoStep4', 'Next Step', width = 200),
                                             br(), br(),
                                             actionButton('trans2014', 
                                                          'Use 2014 Assessment transforms
                                                          and go to Step 4')
                                             
                                             ),
                                      column(8, align = 'center',
                                             sliderInput('bins', 'Number of bins',
                                                         width = '80%',
                                                         min = 1, max = 50, value=25),
                                             plotOutput('distPlot'),
                                             br(),
                                             fluidRow(
                                               column(4, align = 'center',
                                                      numericInput('valFrac',
                                                                   'Enter proportion of
                                                                   dataset for validation:',
                                                                   value = 0.25, width = 200,
                                                                   min = 0, max = 1)
                                               ),
                                               column(4, align = 'center',
                                                      numericInput('testFrac',
                                                                   'Enter proportion of
                                                                   dataset for testing:',
                                                                   value = 0, width = 200,
                                                                   min = 0, max = 1)
                                               )
                                             )
                                               )
                                             )
                             )
                             )
                           ),
                  #### Step 4 #####
                  tabPanel("Step 4",
                           h3('Boosted Regression Trees'),
                           br(),
                           p('BRT, are a particular application of', em('boosting'),
                             'which is a family of machine learning techniques that
                             convert weak prediction models to strong ones. The BRT
                             method produces an ensemble of weak models in the form 
                             of decision trees, and the method also performs',
                             em('k-fold'), 'cross validation to validate the ensemble.
                             Please select the parameters for building the BRT model. 
                             The model must be run for each response variable. 
                             See Elith, Leathwick, and Hastie (2008) for more details.'),
                           br(),
                           p('Reference: Elith, J., Leathwick, J.R., & Hastie, T.
                             (2008). A working guide to boosted regression trees.',
                             em('Journal of Animal Ecology, 77,'), '802-813'),
                           br(),
                           uiOutput('modelVar'),
                           br(),
                           fluidRow(
                             column(2,
                                    selectInput('modelFamily', 'Distribution of response:',
                                                choices = list('Normal' = 'gaussian',
                                                               'Biomial' = 'bernoulli',
                                                               'Poisson' = 'poisson',
                                                               'Laplace' = 'laplace'),
                                                selected = 'gaussian')
                             ),
                             column(2,
                                    numericInput('treeComplexity', 
                                                 'Tree Complexity',
                                                 value = 5)
                             ),
                             column(2,
                                    numericInput('learnRate', 'Learning Rate:',
                                                 value = 0.004)
                             ),
                             column(2,
                                    numericInput('bagFrac', 'Bag Fraction:',
                                                 value = 0.5)
                             ),
                             column(2,
                                    numericInput('maxTrees', 'Max number of trees:',
                                                 value = 10000)),
                             column(2,
                                    numericInput('kfolds', 'Number of folds:',
                                                 value = 10)
                             )
                           ),
                           br(),
                           actionButton('runModel', 'Run Model'),
                           br(),
                           actionButton('runModelAllVar', 'Run Model for all variables'),
                           br(),
                           br(),
                           p('Runtime output:'),
                           verbatimTextOutput('modelRunning'),
                           tags$head(tags$style("#modelRunning{color:red; font-size:12px; 
                                                font-style:italic; overflow-y:scroll; 
                                                max-height: 200px; background: ghostwhite;}")),
                           br(),
                           br(),
                           actionButton('gotoStep5', 'Next Step', width = 200),
                           br(),
                           br(),
                           actionButton('useModels2014', 'Use 2014 BRT models and go to Next Step')
                           ),
                  #### Step 5 ####
                  tabPanel('Step 5',
                           br(),
                           h3('Diagnostics'),
                           br(),
                           p('This step checks the fitted models and provides
                             tools to the user to perform diagnostics.'),
                           br(),
                           selectInput('varDiag', 'Select response variable:',
                                       choices = ''),
                           br(),
                           plotOutput('diagScatter'),
                           br(),
                           fluidRow(
                             column(6,
                                    h4('Goodness of Fit Statistics'),
                                    uiOutput('gofStatsTable')
                             ),
                             column(1),
                             column(5,
                                    actionButton('infillVar',
                                                 'Infill missing response data with 
                                                 model results'), 
                                    br(), br(),
                                    actionButton('infill_allVar', 
                                                 'Infill all response data'),
                                    br(), br(),
                                    actionButton('gotoStep6', 'Next Step'),
                                    br(), br()
                                    )
                           )
                  ),
                  #### Step 6 ####
                  tabPanel('Step 6',
                           br(),
                           h3('Calculate Metrics'),
                           p('Narratively and from US EPA (2012), the 2014 Assessment defined 
                             the following multimetric indices (MMI):'),
                           p(strong('Watershed Condition'), 'describes the degree to which certain
                             structural characteristics of a watershed (those most relevant to
                             aquatic ecosystems) are in their natural or pre-settlement state
                             The indicators of watershed condition comprise:', 
                             em('Percent Natural Land Cover, Percent Intact Active River Area,
                                Sedimentation Risk, Percent Artificial Drainage area, Dam
                                Storage Ratio,'), 'and', em('Road Crossing Density.'), 'Watershed
                             Condition includes indicators for both natural and anthropogenic 
                             landscape conditions.'),
                           p(strong('Stream Health'), 'is characterized by the physical, chemical,
                             and biological makeup of a stream. Indicators for this metric
                             were modeled and infilled in the previous steps. Repeated 
                             here:'),
                           HTML("
                                <ol style='list-style-type: square; font-size: 14px'>
                                <li>
                                Physical and biological habitat condition accounted for in CRAM
                                (California Rapid Assessment Method) scores and Perennial Streams 
                                Assessment's (PSA) overall physical habitat (PHAB) score.
                                </li>
                                <li>Water quality condition comprised of three metrics:
                                <ol style='list-style-type: disc'>
                                <li>Stream median summer conductivity</li>
                                <li>Stream median summer nitrate concentration</li>
                                <li>Stream median summer turbidity</li>
                                </ol>
                                </li>
                                <li>
                                Instream biological condition uses the California Stream Condition
                                Index (CSCI) score
                                </li>
                                </ol>
                                "),
                           p(strong('Watershed Vulnerability'), 'is defined as the potential for 
                             future degradation of watershed processes and aquatic ecosystems. Many
                             indicators were included for this metric, and they comprise parameters
                             related to: climate change; land cover change; water use, and fire.'),
                           p('In this step we can change the indicators that comprise a given metric.
                             Please use the prompt below or use the 2014 metric definitions. 
                             Watershed Vulnerability and Stream Health indices are currently fixed
                             and decompose into several MMIs that can be modified. However, indicators
                             for Watershed Condition may be changed. Note: the indicators loaded for
                             a selected MMI are from the 2014 assessment.'),
                           br(),
                           fluidRow(
                             column(4,
                                    selectInput('varMMI', 'Select MMI:',
                                                choices = 
                                                  list('Watershed Condition' = 'Watershed',
                                                       'Physical and Biological Habitat Condition' = 'PhysBio',
                                                       'Water Quality Condition' = 'WaterQuality',
                                                       'Overall Landscape Condition' = 'Landscape',
                                                       'Natural Condition' = 'Natural',
                                                       'Anthropogenic Condition' = 'Anthropogenic',
                                                       'Instream Biological Condition' = 'InstreamBio',
                                                       'Climate Change Vulnerability' = 'ClimateChange',
                                                       'Land Cover Vulnerability' = 'LandCover',
                                                       'Water Use Vulnerability' = 'WaterUse',
                                                       'Fire Vulnerability' = 'Fire'))
                             ),
                             column(4,
                                    selectInput('varInd', 
                                                'Select indicators:',
                                                multiple = TRUE,
                                                choices = '')
                             )
                           ),
                           actionButton('calcMMI', 'Calculate Indices'),
                           br(), br(),
                           actionButton('calcMMI_2014', 'Use 2014 definitions and Calculate'),
                           br(), br(),
                           actionButton('explore', 'Explore Results'),
                           br(), br(),
                           verbatimTextOutput('testText')
                           ),
                  #### Explore ####
                  tabPanel('Explore',
                           br(),
                           h3('Explore Results'),
                           # verbatimTextOutput('testText'),
                           br(),
                           selectInput('dispOutVar', 'Choose output to map:',
                                       choices = 
                                         list('Watershed Condition' = 'Watershed',
                                              'Physical and Biological Habitat Condition' = 'PhysBio',
                                              'Water Quality Condition' = 'WaterQuality',
                                              'Overall Landscape Condition' = 'Landscape',
                                              'Natural Condition' = 'Natural',
                                              'Anthropogenic Condition' = 'Anthropogenic',
                                              'Instream Biological Condition' = 'InstreamBio',
                                              'Climate Change Vulnerability' = 'ClimateChange',
                                              'Land Cover Vulnerability' = 'LandCover',
                                              'Water Use Vulnerability' = 'WaterUse',
                                              'Fire Vulnerability' = 'Fire'),
                                       selected = NULL),
                           h5(strong('Choose colors for display:')),
                           br(),
                           fluidRow(
                             column(3,
                                    colourInput('col1', 'Low = 0', 'red', returnName = TRUE)
                             ),
                             column(3,
                                    colourInput('col2', 'High = 1', 'blue', returnName = TRUE)
                             ),
                             column(3,
                                    numericInput('mapTrans', 'Transparency %', 50, min=0, max=100)
                             )
                           ),
                           leafletOutput('map'),
                           br(),
                           fluidRow(
                             column(3,
                                    div(tableOutput('summaryTable'),
                                        style = "font-size:15px")),
                             column(3,
                                    actionButton('dispSumCatchments', 
                                                 'Highlight catchments on map')
                             )
                           )
                  )
                  #### End ####
                           )
                             )
                           )
  )
