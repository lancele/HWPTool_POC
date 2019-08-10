library(shiny)

introText <-
  div(
    p(
      "Healthy waters are a vital part of California's identity and economy. 
      The state's high-quality streams, lakes, and wetlands provide a wealth of 
      recreational opportunities, clean drinking water, and other ecosystem 
      services to residents and visitors alike. Their continued function and 
      status as healthy aquatic ecosystems depends in large part on the 
      implementation of protection measures to prevent direct impacts and to 
      maintain key watershed features and processes. A more concerted effort to 
      protect high-quality waters by state agencies and other organizations can 
      support the effectiveness of current efforts to restore impaired waters 
      and circumvent the need for costly restoration in the future."
    ),
    p(
      "The purpose of the California Integrated Assessment of Watershed Health 
      (the Assessment) is to identify healthy watersheds and characterize relative
      watershed health across the state to guide future protection initiatives. 
      A healthy watershed has the structure and function in place to support 
      healthy aquatic ecosystems. It is characterized as having either in its 
      entirety, or as key components: intact and functioning headwaters, wetlands, 
      floodplains, riparian corridors, biotic refugia, instream and lake habitat, 
      and biotic communities; natural vegetation in the landscape; natural hydrology 
      (e.g., range of instream flows and lake levels); sediment transport and 
      fluvial geomorphology; and natural disturbance regimes expected for its 
      location."
    ),
    p("The goals of the Assessment were to:"),
    HTML("
         <ol style='list-style-type: decimal; font-size: 14px' type='1'>
         <li>
         Integrate multi-disciplinary data to both identify healthy watersheds
         and characterize the relative health of watersheds across the state;
         </li>
         <li>
         Make watershed health data and information readily available to a 
         variety of state, federal, and  local programs for watershed protection 
         planning; and
         </li>
         <li>
         Encourage inter-agency partnerships and collaboration to build upon 
         previous efforts to assess watershed health and protect healthy watersheds.
         </li>
         </ol>
         "),
    p("This web application produces metrics detailed in the California Integrated
      Assessment of Watershed Health (2014), which defined six multi-metric 
      indices or MMI (see figure below). For California's assessment, three additional 
      indices were generated:"), 
    p(strong('Watershed Condition'), "describes the degree to which certain
      structural characteristics of a watershed--that is, those most relevant to
      aquatic ecosystems--are in their natural or pre-settlement state."),
    p(strong('Stream Health'), "is characterized by the physical, chemical,
      and biological makeup of a stream."),
    p(strong('Watershed Vulnerability'), 'is defined as the potential for 
      future degradation of watershed processes and aquatic ecosystems. Many
      indicators were included for this metric, and they comprise parameters
      related to: climate change; land cover change; water use, and fire.'),
    p("For more information, please go to the", strong('Watershed Metrics'),
      "tab. As a pilot example in this web application, we will show results only for the",
      strong("Russian River Watershed."), "Please click below to start the process:")
  )

dataInputText <-
  div(
    p('This application requires three data inputs: shapefile(s) specifying the geographic 
      units; a table with indicator data for each geographic unit; and a table with
      metadata specifying how indicators relate to each multi-metric index. You may use
      data prepared for the Assessment in 2014, or you may upload your own data. Geographic
      units in the Assessment were based on reach-scale watershed catchments of the
      National Hydrography Dataset Plus (NHDPlus), Version 1 (USEPA and USGS, 2005). 
      Examples of indicator parameters are water quality measurements (e.g. dissolved 
      oxygen), percent agricultural land cover, and road crossing density. Please go to the', 
      strong('View Data'), 'tab for details on what fields are required for this application.'),
    p('The metadata file should contain the following case-sensitive fields for 
      each indicator:'),
    HTML("
<ol style='list-style-type: square; font-size: 14px' type='1'>
 <li>
  <b>fullnames</b>: full name of an indicator. Ex. <i>Dissolved Oxygen</i>, <i>Percent Slope</i>
 </li>
 <li>
  <b>shortname</b>: shortened name for indicator and corresponds to a column in the dataset. 
  Ex. <i>DO</i>, <i>PctSlope</i>.
 </li>
 <li>
  <b>PositiveDir</b>: boolean (TRUE/FALSE) column defining the directional alignment for
  an indicator, specifying whether the indicator is good or bad. Ex. <i>PositiveDir</i> for 
  <i>DO</i> would be TRUE because higher DO generally means good water quality; 
  <i>PctSlope</i> alignment would be FALSE, because steeper slopes generally lead to 
  greater erosion and sediment discharge.
 </li>
 <li>
  <b>vartype</b>: field can take on one of three values: <i>ID, exp</i>, or <i>res</i>. This
  field specifies whether the indicator should be used as a response or explanatory variable
  in the missing data imputation step. <i>ID</i> means that the field is not an indicator, but
  simply a column for use in identifying catchments.
 </li>
 <li>
  The remaining columns are the multi-metric indices. Each column is a boolean (TRUE/FALSE),
  specifying whether the indicator should used in the calculation of that metric. Ex.
  <i>DO</i> would be TRUE for the Water Quality metric.
 </li>
</ol>
         "),
    p('All tables must be in CSV format. If you are uploading GIS data, please zip 
      the contents.', strong('Note'), 'if the dataset contains missing data for any catchment,
      you must go to the', strong('Data Imputation'), 'series of steps before proceeding. If
      dataset is complete, go ahead to the', strong('Watershed Metrics'), 'tab.')
  )