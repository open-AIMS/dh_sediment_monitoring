
source("40_temporal_analysis.R")
source("05_stats_functions.R")

## Define reactive values. 
analysis_data <- reactiveVal()
dat_value <- reactiveVal()
variable_value <- reactiveVal()
mod_value <- reactiveVal()
effect_years_value <- reactiveVal()
effect_scale_value <- reactiveVal()

observeEvent(input$runAnalysisCode, {
  status::display_status_terminal()
  prom <- promises::future_promise({
    module_temporal()
    readRDS(file = paste0(data_path, "modelled/data_all.RData"))
  })
  prom %...>%
    analysis_data() %...>%
    ## Must wrap the following in braces if it wants to pass in status_$status
    {
      toggle_buttons(status_$status, stage =  5, bttn1 = "runAnalysisCode", bttn2 = NULL)
      shinyjs::enable(selector = "a[data-value='analysis']")
      addCssClass(selector = "a[data-value='analysis']", class = "activeLink")
    }
    ## Hide the async operation from Shiny by not having the promise
    ## be the last expression. Without doing so, the main process will
    ## still be waiting.
    NULL
})


