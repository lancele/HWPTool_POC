library(shiny)
library(shinydashboard)

source('ui_textBlocks.R')

introTab <- tabItem('introTab', 
                    titlePanel('Introduction'),
                    introText,
                    br(),
                    actionButton('startAssessment', 'START'),
                    HTML('<center><img src="indicators_concept_framework.PNG"
             height="250" width="733"></center>')
)

inputDataTab <- 
  tabItem('dataInputTab',
          titlePanel('Select Data'),
          dataInputText,
          br(),
          fluidRow(
            column(2, 
                   radioButtons('dataInput', 'Data Input Options:',
                                choices = list('Use 2014 data' = 'use2014',
                                               'Upload data' = 'upload'),
                                selected = NULL)),
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
            ),
            actionButton('uploaded', 'Finalize uploaded data')
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
          actionButton('gotoPreprocessing', 'Go to Data Imputation', width = 200),
          br(),
          br(),
          actionButton('skipToMetrics', 'Go to Watershed Metrics', width = 200)
  )