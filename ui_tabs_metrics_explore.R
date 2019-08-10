library(shiny)
library(shinydashboard)

metricsTab <- 
  tabItem('metricsTab',
          titlePanel('Watershed Metrics'),
          p('In this step you may change the indicators that comprise a given metric.
            The Assessment defined seven sub-indices to compose the
            Stream Health and Watershed Vulnerability metrics.
            These sub-indices are:'),
          HTML("
<ol style='list-style-type: square; font-size: 14px' type='1'>
 <li>
  Physical and Biological Habitat Condition
 </li>
 <li>
  Water Quality
 </li>
 <li>
  Instream Biological Condition
 </li>
 <li>
  Climate Change
 </li>
 <li>
  Land Cover Change
 </li>
 <li>
  Water Use
 </li>
 <li>
  Fire
 </li>
</ol>
               "),
          p('Please use the prompt below or use the 2014 metric definitions. 
            Watershed Vulnerability and Stream Health metrics are fixed to their
            sub-indices. However, indicators for Watershed Condition may be changed. 
            Note: the indicators loaded for a selected MMI are from the 2014 Assessment.'),
          selectInput('metricGraphic', 'Select graphic corresponding to metric:',
                      choices = list('Watershed Condition' = 'watershedCondition',
                                     'Stream Health' = 'streamHealth',
                                     'Watershed Vulnerability' = 'watershedVuln'),
                      selected = 'watershedCondition'),
          conditionalPanel(
            condition = "input.metricGraphic=='watershedCondition'",
            HTML('<img style="max-width: 300px; height:auto;" 
                 src="watershed_condition_indicators.PNG">')
          ),
          conditionalPanel(
            condition = "input.metricGraphic=='streamHealth'",
            HTML('<img style="max-width: 600px; height:auto;" 
                 src="stream_health_indicators.PNG">')
            ),
          conditionalPanel(
            condition = "input.metricGraphic=='watershedVuln'",
            HTML('<img style="max-width: 600px; height:auto;" 
                 src="watershed_vulnerability_indicators.PNG">')
            ),
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
          br(), br()
  )

exploreTab <- 
  tabItem('exploreTab',
        titlePanel('Explore Results'),
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
                    selected = 'Watershed'),
        h5(strong('Choose colors for display:')),
        fluidRow(
          column(3,
                 colourpicker::colourInput('col1', 'Low = 0', 'red', returnName = TRUE)
          ),
          column(3,
                 colourpicker::colourInput('col2', 'High = 1', 'blue', returnName = TRUE)
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