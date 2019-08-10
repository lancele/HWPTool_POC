a <- tabItem('introTab', 
        titlePanel('Introduction'),
        p('This web application produces metrics detailed in the California
          Integrated Assessment of Watershed Health (2014), whose purpose was
          to identify health watersheds and characterize relative watershed 
          health across the state. The primary indices are Watershed Vulnerability,
          Watershed Condition, and Stream Health. As a pilot example, we will
          show results only for the Russian River. Please follow the steps
          shown as tabs on the left.'),
        br(),
        HTML('<center><img src="indicators_concept_framework.PNG"
             height="250" width="733"></center>')
        )