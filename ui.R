library(colourpicker)
library(shinyjs)
library(shiny)
library(shinydashboard)
library(rgdal)
library(leaflet)

source('ui_tabs_intro_inputData.R')
source('ui_tabs_preprocess.R')
source('ui_tabs_modelFit.R')
source('ui_tabs_metrics_explore.R')

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = 'sidebarMenu',
    menuItem('Introduction', tabName = 'introTab'),
    menuItem('Select Data', tabName = 'dataInputTab'),
    menuItem('Data Imputation', tabName = 'dataPrepTabs',
             menuSubItem('Data Preprocessing', tabName = 'preprocessTab'),
             menuSubItem('Model Fitting', tabName = 'modelFitTab'),
             menuSubItem('Diagnostics', tabName = 'diagnosticsTab')),
    menuItem('Watershed Metrics', tabName = 'metricsTab'),
    menuItem('Explore Results', tabName = 'exploreTab'),
    menuItem('View Data', tabName = 'viewData'),
    br(),
    HTML('<center><img style="max-width: 200px; height:auto;"
        src="waterboards_logo_high_res.jpg"
        width="350" height="243"></center>')
  )
)


body <- dashboardBody(
  tags$head(tags$style(HTML(
    '.dashboadBodyTitle { 
    font-size: 20px;
    line-height: 50px;
    text-align: left;
    font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
    padding: 0 15px;
    overflow: hidden;
    color: white;
    }
    '))),
  tags$script(HTML('
$(document).ready(function() {
$("header").find("nav").append(\'<span class="dashboadBodyTitle"> Healthy Watersheds Initiative </span>\');
})'
  )),
  shinyjs::useShinyjs(),
  withMathJax(),
  tabItems(
    #### Intro ####
    introTab,
    #### Input Data ####
    inputDataTab,
    #### Data Pre-processing ####
    preprocessTab,
    #### Model Fitting ####
    modelFitTab,
    #### Diagnostics ####
    tabItem('diagnosticsTab',
            titlePanel('Diagnostics'),
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
              column(4,
                     actionButton('infillVar',
                                  'Impute missing response data with 
                                  model results'), 
                     br(), br(),
                     actionButton('infill_allVar', 
                                  'Impute all response data'),
                     br(), br(),
                     actionButton('gotoStep6', 'Next Step'),
                     br(), br()
                     )
            )
    ),
    #### Calculate Metrics ####
    metricsTab,
    #### Explore ####
    exploreTab,
    #### View Data ###
    tabItem('viewData',
            titlePanel('View Data'),
            p('Note: to change dataset, go back to', strong('Input Data'),
              'tab and change the data input options radio button.'),
            radioButtons('viewDataChoice', 'Choose what to view:',
                         choices = list('Dataset' = 'viewDataset',
                                        'Metadata' = 'viewMetadata'),
                         selected = 'viewDataset'),
            
            conditionalPanel(
              condition = "input.viewDataChoice=='viewDataset'",
              DT::dataTableOutput('viewDataset')
            ),
            conditionalPanel(
              condition = "input.viewDataChoice=='viewMetadata'",
              DT::dataTableOutput('viewMetadata')
            )
    )
    #### End ####
  )
)

ui <- dashboardPage(
  dashboardHeader(),
  sidebar,
  body,
  title = 'Healthy Watersheds Initiative'
)