library(shiny)
library(shinydashboard)

preprocessTab <- 
  tabItem('preprocessTab',
        titlePanel('Data Preprocessing'),
        p('Before imputing missing data, some preprocessing may be
          warranted. Response and predictor variables should be 
          transformed such that they are approximately normal. 
          Do not transform variables that are binary (e.g. presence/absence.
          data) or are categorical in type. Please assess the variables
          and choose a transformation, if appropriate. Transformations 
          resulting in nonfinite values (e.g. logarithm on negative numbers)
          will also remove the respective data rows. Also, please
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
  )