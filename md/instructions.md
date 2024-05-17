This [Shiny](https://shiny.posit.co/) application is designed to
ingest very specifically structured excel spreadsheets containing
Darwin Harbour sediment monitoring data and produce various analyses
and visualisations. The application is served from a
[docker](https://www.docker.com/) container to the localhost and the
default web browser.

Docker containers can be thought of a computers running within other
computers. More specifically, a container runs an instance of image
built using a series of specific instructions that govern the entire
software environment. As a result, containers run from the same image
will operate (virtually) identically regardless of the host
environment. Furthermore, since the build instructions can specify
exact versions of all software components, containers provide a way of
maximising the chances that an application will continue to run as
designed into the future despite changes to operating environments and
dependencies.

This shiny application comprises five pages (each accessable via the
sidebar menu on the left side of the screen):

1. a **Landing** page (this page) providing access to the settings and
   overall initial instructions
2. a **Dashboard** providing information about the progression of
   tasks in the analysis pipeline
3. a **Data** page providing overviews of data in various stages
4. an **Exploratory Data Analysis** page providing graphical data
   summaries

Each page will also contain instructions to help guide you through
using or interpreting the information. In some cases, this will take
the from of an info box (such as the current box). In other cases, it
will take the form of little <span class="fas fa-circle-info"></span>
symbols whose content is revealed with a mouse hover.

There are numerous stages throughout the analysis pipeline that may
require user review (for example examining the exploratory data
analysis figures to confirm that the data are as expected).
Consequently, it is necessary for the user to manually trigger each
successive stage of the pipeline.  The stages are:

- Stage 1 - Prepare environment 
  <details><summary>More info</summary>
  <p class = "details-info">
  This stage is run automatically on startup and essentially sets up the operating environment.
  </p>
  </details>
- Stage 2 - Obtain data
  <details><summary>More info</summary>
  <p class = "details-info">
  This stage comprises of the following steps:
  
  - reading in the excel files within the nominated input path
  - validating the input data according to a set of validation rules
  - constructing various spatial objects for mapping and spatial aggregation purposes
  
  The tables within the **Raw data** tab of the **Data** page will also be populated.
  </p>
  </details>
- Stage 3 - Process data
  <details><summary>More info</summary>
  <p class = "details-info">
  This stage comprises of the following steps:
  
  - apply limit of reporing values (LoRs)
  - pivot the data into a longer format that is more suitable for analysis and graphing
  - join in the metadata to each associated sheet
  - make a unique key
  - collate the all the data together from across the multiple sheets and files into a single data set
  - incorporate the spatial data
  - tidy the field names
  - apply data standardisations
  - create a site lookup table to facilitate fast incorporation of spatial information into any outputs.
  
  The tables within the **Processed data** tab of the **Data** page will also be populated.
  </p>
  </details>
- Stage 4 - Exploratory data analysis
  <details><summary>More info</summary>
  <p class = "details-info">
  This stage comprises of the following steps:
  
  - retrieve the processed data.
  - construct spatio-temporal design plots conditioned on initial sampling semester
  - construct variable temporal design plots conditioned on harbour zone
  - construct site level temporal trends for each variable
  - construct zone level temporal and spatial visualisations for each variable
  
  The exploratory data figures of the **Exploratory Data Analysis** page will also be populated.
  </p>
  </details>
- Stage 5 - Temporal analyses
  <details><summary>More info</summary>
  <p class = "details-info">
  This stage comprises of the following steps:
  
  - retrieve the processed data
  - prepare the data for modelling
  - prepare appropriate model formulae for each zone, variable,
    standardisation type
  - prepare appropriate model priors for each zone, variable,
    standardisation type 
  - prepare appropriate model template 
  - fit the models for each zone, variable, standardisation type
  - perform model validations for each zone, variable, standardisation
    type
  - estimate all the contrasts for each model and collate all the
    effects

  </p> </details>

Underneath the sidebar menu there are a series of buttons that control
progression through the analysis pipeline stages. When a button is
blue (and has a play icon), it indicates that the Stage is the next
Stage to be run in the pipeline. Buttons with a grey border and label
are disabled.

Clicking on button will run that stage. Once a stage has run, the
button will change to either green (success), yellow (orange) or red
(failures) indicating whether errors/warnings were encountered or not.
If the stage was completed successfully, the bottom corresponding to
the next available stage will be activated.


