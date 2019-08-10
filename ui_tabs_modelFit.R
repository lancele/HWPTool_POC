library(shiny)
library(shinydashboard)

modelFitTab <- 
  tabItem('modelFitTab',
          titlePanel('Boosted Regression Trees'),
          p('The Assessment produced metrics for 135,255 catchments
            within the National Hydrography Dataset (NHD Plus); however,
            not all catchments had the same data availability. In particular,
            missing for many catchments were stream health indicators for 
            habitat condition (e.g. CRAM, PHAB scores); water quality
            (instream median summer: conductivity, nitrate concentration,
            and turbidity); and instream biological condition (CSCI scores).
            To perform a geographically complete assessment, these missing
            data must be filled in. The Assessment used more readily
            available landscape variables as predictors in a model to
            produce the missing data. This model is a machine learning 
            method called Boosted Regression Trees (BRT). but other 
            methods are available for this assessment.
            BRT are a particular application of', em('boosting'),
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
  )