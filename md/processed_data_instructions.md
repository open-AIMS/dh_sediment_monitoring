The **Processed data** panel displays the processed data. As part of
the processing, the following new fields will be created:


<div class="table-minimal">

| Field               | Description                                                                                                                                                      |
|---------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Sample_key          | this is a unique ID for each sample and is created as the combination of `Sample_ID`, `Acquired_date_time` and any replicate/duplicate tokens in the `Sample_ID` |
| *¹ (mg/kg)          | observed concentration of metal in sediment sample                                                                                                               |
| Type                | whether the record pertains to a metal or hydrocarbon                                                                                                            |
| Year_cal            | calendar year of sample collection                                                                                                                               |
| Year_fiscal         | financial year of sample collection                                                                                                                              |
| Year_water          | water year (1st Oct - 30 Sept) of sample collection                                                                                                              |
| Year                | calendar year of sample collection                                                                                                                               |
| Baseline            | whether the record is a baseline record (TRUE) or not (FALSE)                                                                                                    |
| ZoneName            | name of the Darwin Harbour Zone                                                                                                                                  |
| Region              | Darwin Harbour Region number                                                                                                                                     |
| RegionName          | name of the Darwin Harbour Region                                                                                                                                |
| Zone                | Darwin Harbour Zone number                                                                                                                                       |
| Area                | Darwin Harbour Area (Inner or Outer)                                                                                                                             |
| Site                | ID of the sampling site                                                                                                                                          |
| Var                 | name of the measurement                                                                                                                                          |
| Values              | the observed measurement                                                                                                                                         |
| Value_type          | whether the value is a standardised value or not                                                                                                                 |
| Fe/Al               | Fe:Al where appropriate                                                                                                                                          |
| Fe_Al_normalisation | what the sample **would be** normalised against Fe or Al (or not applicable) if it could be normalised                                                           |
| Normalised against  | what the sample **was** normalised against (if it was normalised)                                                                                                |
| Normalisation flag  | whether the normalisation has switched between Fe and Al over time for this site                                                                                 |

: {tbl-colwidths="[20,80]"} 
   
</div>
