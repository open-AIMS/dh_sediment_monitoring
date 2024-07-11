To be valid, input data must be excel files (*.xlsx) comprising at
least the following sheets (each of which must at least have the
fields listed in their respective tables):

  - metals

    <div class="table-minimal">

    | Field      | Description                                        | Validation conditions                                |
    |------------|----------------------------------------------------|------------------------------------------------------|
    | Sample_ID  | unique sample ID                                   | must contain characters                              |
    | *¹ (mg/kg) | observed concentration of metal in sediment sample | must contain only numbers or start with a '<' symbol |
    
    1: where the '*' represents a one or two character chemical symbol
    (such as 'Ag' or 'V'). There should be numerous of these fields
    </div>

  - hydrocarbons

    <div class="table-minimal">

    | Field     | Description                                                                          | Validation conditions                                |
    |-----------|--------------------------------------------------------------------------------------|------------------------------------------------------|
    | Sample_ID | unique sample ID                                                                     | must contain characters                              |
    | >C*¹        | observed concentration of hydrocarbons within a specific size bin in sediment sample | must contain only numbers or start with a '<' symbol |
    
    : {tbl-colwidths="[10,50,40]"} 
    
    1: where the '*' represents a string of characters defining the
    size bin (such as '10 _C16'). There should be numerous of these
    fields </div>

  - total_carbons

    <div class="table-minimal">

    | Field     | Description                                                          | Validation conditions                                |
    |-----------|----------------------------------------------------------------------|------------------------------------------------------|
    | Sample_ID | unique sample ID                                                     | must contain characters                              |
    | TOC (%)   | observed total organic carbon (as a percentage of the sample weight) | must contain only numbers |
   
    : {tbl-colwidths="[10,50,40]"} 
   
    </div>

  - metadata

    <div class="table-minimal">

    | Field                      | Description                                                          | Validation conditions                     |
    |----------------------------|----------------------------------------------------------------------|-------------------------------------------|
    | IBSM_site                  | name of the site from the perspective of IBSM                        | must contain characters (or be blank)     |
    | Site_ID                    | a unique site ID                                                     | must contain characters (cannot be blank) |
    | Sample_ID                  | unique sample ID (the key to data sheets)                            | must contain characters (cannot be blank) |
    | Original_SampleID          | unique sample ID                                                     | must contain characters                   |
    | Latitude                   | site latitude                                                        | must be numeric (and negative)            |
    | Longitude                  | site longitude                                                       | must be numeric                           |
    | Acquire_date_time          | date and time sample was collected (D/M/YYYY hh:mm:ss)               | must be in datetime format                |
    | Sampler                    | name of person responsible for collecting sample (ignored)           | ignored                                   |
    | Notes                      | project description (ignored)                                        | ignored                                   |
    | Baseline_site              | the unique site ID of the corresponding baseline sample              | must contain characters (cannot be blank) |
    | Baseline_acquire_date_site | the date and time of the corresponding baseline sample               | must be in datetime format                |
  
    : {tbl-colwidths="[25,35,40]"} 
  
    </div>

  - notes - this sheet is not processed or validated

::: {.callout-note}
All input data must be placed in the `/input` folder of the Project
prior to starting the app.
:::


