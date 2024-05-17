To run this tool, please adhere to the following steps:

1. review the _Path Settings_ (specifically checking the **"Data input dir"**
   and ensuring that there is at least one data file listed in the box
   under this setting
2. review the _Run Settings_. In particular,
   - consider whether you need to **Clear the previous data** -
     clicking the button to do so. Clearing the previous data deletes
     all cache and ensure that the analyses are performed fresh.
     **This is recommended whenever the input data changes**. Not
     clearing the previous data allows the user to skip directly to
     later run stages if earlier stages have already been run.
   - consider the Limit of Reporting (LoR) setting. 
     - the default is to set the value equal to the specified limit of
       reporting for a value (such values must start with a "<") and
       will be flagged as "left" censored. Models that accommodate
       censored data take a probabilistic approach to inferring the
       likely distribution of all observations including those beyond
       the limit of reporting and are considered more appropriate.
     - the alternative is the more traditional approach of replacing
       the value with 1/2 of the limit of reporting value and using
       this in the analyses. Whilst traditional, this approach tends
       to make the resulting values into outliers and thus problematic
       in analyses.
3. navigate the _Dashboard_ via the menu on the left sidebar
