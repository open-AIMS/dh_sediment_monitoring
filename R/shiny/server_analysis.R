
source("40_temporal_analysis.R")
source("05_stats_functions.R")

## Define reactive values. 
analysis_data <- reactiveVal()
levels_pool_overview <- reactiveVal()
initial_levels_pool_overview <- reactiveVal()

levels_pool_diagnostics <- reactiveVal()
mod_value <- reactiveVal()

levels_pool_details <- reactiveVal()
mod_value_details <- reactiveVal()

dat_value <- reactiveVal()
variable_value <- reactiveVal()
effect_years_value <- reactiveVal()                   ## data pool of all combinations of categories
effect_scale_value <- reactiveVal()
sub_data <- reactiveVal()                             ## data pool from which to select dropdowns


observeEvent(input$runAnalysisCode, {
  status::display_status_terminal()
  prom <- promises::future_promise({
    module_temporal()   ##put this back..
    readRDS(file = paste0(data_path, "modelled/data_all.RData")) |>
      mutate(Normalised_against = ifelse(is.na(Normalised_against), "", Normalised_against))
  })
  prom %...>%
    analysis_data() %...>%
    ## Must wrap the following in braces if it wants to pass in status_$status
    {
      toggle_buttons(status_$status, stage =  5, bttn1 = "runAnalysisCode", bttn2 = NULL)
      shinyjs::enable(selector = "a[data-value='analysis']")
      addCssClass(selector = "a[data-value='analysis']", class = "activeLink")

      ## define the pool of combinations available for the dropdowns
      sub_dat <- analysis_data() |>
        dplyr::select(Type, Value_type, Normalised_against, summ_e) |>
        unnest(c(summ_e)) |>
        dplyr::select(scale, Type, Value_type, Normalised_against, year, contrast) |>
        distinct()
      levels_pool_overview(sub_dat)
      ## Initial levels
      sub_dat1 <- sub_dat |>
        filter(scale == "zone",
          Type == "metals",
          Value_type == "Standardised") |>
        filter(Normalised_against == first(Normalised_against)) |>
        arrange(desc(year)) |> 
        filter(str_detect(contrast, ".*Baseline.*")) |>
        droplevels() |>
        slice(1)
      initial_levels_pool_overview(sub_dat1)

      levels_pool <- analysis_data() |>
        filter(scale == "zone") |> 
        dplyr::select(ZoneName, Var, Value_type, Normalised_against) |>
        distinct()
      levels_pool_diagnostics(levels_pool)

      levels_pool1 <- analysis_data() |>
        dplyr::select(scale, ZoneName, Site, Area, Var, Value_type, Normalised_against) |>
        distinct() |>
        ## mutate(ZoneName = ifelse(scale == "zone", ZoneName, Site))
        mutate(ZoneName = case_when(
          scale == "zone" ~ ZoneName,
          scale == "site" ~ Site,
          scale == "area" ~ Area
        ))
      levels_pool_details(levels_pool1)
      
    }
    ## Hide the async operation from Shiny by not having the promise
    ## be the last expression. Without doing so, the main process will
    ## still be waiting.
    NULL
})


