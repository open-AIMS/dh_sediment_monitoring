source("40_temporal_analysis.R")
source("05_stats_functions.R")

## dat_value <- reactiveVal()
## variable_value <- reactiveVal()
## effect_years_value <- reactiveVal()
## effect_scale_value <- reactiveVal()

output$analysis_main <- renderUI({
  req(analysis_data())

  data <- analysis_data()
  
  fluidPage(
    fluidRow(
      box(
        class = "analysis-zone-box",
        status = "info",
        width = 6,
        solidHeader = TRUE,
        column(width = 3, selectInput("analysis_main_scale_selector", "Select Scale:", choices = unique(data$scale), selected = "zone")),
        column(width = 3, selectInput("analysis_main_zone_selector", "Select Site:", choices = unique(data$ZoneName))),
        column(width = 3, selectInput("analysis_main_var_selector", "Select var type:", choices = unique(data$Var))),
        column(width = 3, selectInput("analysis_main_value_type_selector", "Select value type:", choices = unique(data$Value_type))),
        column(width = 3, selectInput("analysis_main_normalised_against_selector", "Normalisation type:", choices = unique(data$Normalised_against)))
      ),
      box(
        title = "General instructions",
        status = "info",
        width = 6,
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        htmltools::includeMarkdown("../md/temporal_analysis_instructions.md"),
      )
    ),
    fluidRow(
      # fluidRow(
      tabsetPanel(
        id = "analysis_tabs",
        tabPanel(
          title = "Modelled trends",
          icon = icon("database"),
          id = "analysis_trends_tab",
          uiOutput("analysis_trends"),
          ),
        tabPanel(
          title = "Modelled effects",
          icon = icon("database"),
          id = "analysis_effects_tab",
          fluidRow(
            column(width = 2, selectInput("analysis_main_effectsscale_selector", "Select scale:",
              choices = c("fold", "percentage"), selected = "percentage"
            ))
          ),
          uiOutput("analysis_effects")
        ),
      )
    )
  )
})


#If it is supplied (e.g. from the input value) use it, otherwise,
#get from first combination....
select_detail <- function(focal) {
 req(levels_pool_details())
 current_scale <- input$analysis_main_scale_selector
 current_zone <- input$analysis_main_zone_selector
 current_var <- input$analysis_main_var_selector
 current_value_type <- input$analysis_main_value_type_selector
 if (focal == "zone") {
   temp <-
     levels_pool_details() |>
     filter(scale == current_scale) |>
     pull(ZoneName) |>
     unique()
 }
 if (focal == "var") {
   temp <-
     levels_pool_details() |>
     filter(scale == current_scale) |>
     filter(ZoneName == current_zone) |>
     pull(Var) |>
     unique()
 }
 if (focal == "value_type") {
   temp <-
     levels_pool_details() |>
     filter(scale == current_scale) |>
     filter(ZoneName == current_zone) |>
     filter(Var == current_var) |> 
     pull(Value_type) |>
     unique()
 }
 if (focal == "normalised_against") {
   temp <-
     levels_pool_details() |>
     filter(scale == current_scale) |>
     filter(ZoneName == current_zone) |>
     filter(Var == current_var) |> 
     filter(Value_type == current_value_type) |> 
     pull(Normalised_against) |>
     unique()
 }
 if (focal == "end") {
   temp <-
     levels_pool_details() |>
     filter(scale == current_scale) |>
     filter(ZoneName == current_zone) |>
     filter(Var == current_var) |> 
     filter(Value_type == current_value_type) |> 
     filter(Normalised_against == current_normalised_against) |> 
     pull(Normalised_against) |>
     unique()
 }
  temp
}


select_details_data <- function() {
    req(analysis_data())
    data <- analysis_data() |>
        mutate(ZoneName = case_when(
          scale == "zone" ~ ZoneName,
          scale == "site" ~ Site,
          scale == "area" ~ Area
        ))
      ## mutate(ZoneName = ifelse(scale == "zone", ZoneName, Site))
    ## print(data |>
    ##   filter(scale == input$analysis_main_scale_selector) |>
    ##   filter(ZoneName == input$analysis_main_zone_selector) |>
    ##   filter(Var == input$analysis_main_var_selector) |>
    ##   filter(Value_type == input$analysis_main_value_type_selector) |>
    ##   filter(Normalised_against == input$analysis_main_normalised_against_selector))
    ## mod <- get_filtered_model_details(data,
    ##   sscale = input$analysis_main_scale_selector,
    ##   zone = input$analysis_main_zone_selector,
    ##   var = input$analysis_main_var_selector,
    ##   type = input$analysis_main_value_type_selector,
    ##   normalised_against = input$analysis_main_normalised_against_selector
    ## )

    dat <- get_filtered_data(data,
      scale = input$analysis_main_scale_selector,
      zone = input$analysis_main_zone_selector,
      var = input$analysis_main_var_selector,
      type = input$analysis_main_value_type_selector,
      normalised_against = input$analysis_main_normalised_against_selector
    )
    dat
}


observeEvent(input$analysis_main_scale_selector, {
  updateSelectInput(session,
    "analysis_main_zone_selector",
    choices = select_detail(focal = "zone")
  )
  if (input$analysis_main_zone_selector %in% select_detail(focal = "zone"))
    mod_value_details(select_details_data())
})

observeEvent(input$analysis_main_zone_selector, {
  updateSelectInput(session,
    "analysis_main_var_selector",
    choices = select_detail(focal = "var")
  )
  if (input$analysis_main_var_selector %in% select_detail(focal = "var"))
    mod_value_details(select_details_data())
})

observeEvent(input$analysis_main_var_selector, {
  updateSelectInput(session,
    "analysis_main_value_type_selector",
    choices = select_detail(focal = "value_type")
  )
  if (input$analysis_main_value_type_selector %in% select_detail(focal = "value_type"))
    mod_value_details(select_details_data())
})
observeEvent(input$analysis_main_value_type_selector, {
  updateSelectInput(session,
    "analysis_main_normalised_against_selector",
    choices = select_detail(focal = "normalised_against")
  )
  if (input$analysis_main_normalised_against_selector %in% select_detail(focal = "normalised_against"))
    mod_value_details(select_details_data())
})
observeEvent(input$analysis_main_normalised_against_selector, {
    mod_value_details(select_details_data())
})

## observeEvent(
##   c(
##     input$analysis_main_scale_selector,
##     input$analysis_main_zone_selector,
##     input$analysis_main_var_selector,
##     input$analysis_main_value_type_selector
##   ),
##   {
##     req(analysis_data())
##     data <- analysis_data()
    
##     variable <- input$analysis_main_var_selector
##     variable_value(variable)
##     dat <- get_filtered_data(data,
##       scale = input$analysis_main_scale_selector,
##       zone = input$analysis_main_zone_selector,
##       var = input$analysis_main_var_selector,
##       type = input$analysis_main_value_type_selector
##     )
##     ## print("this")
##     ## print(dat)
##     ## print("that")
##     dat_value(dat)
##   }
## )

## observeEvent(input$analysis_main_scale_selector, {
##     req(analysis_data())
##     data <- analysis_data()
##   ## print(data)
##   current_site <- input$analysis_main_zone_selector
##   if (input$analysis_main_scale_selector == "site") {
##     site <- data |>
##       filter(scale == input$analysis_main_scale_selector) |>
##       pull(Site) |>
##       unique()
    
##   } else {
##     site <- data |>
##       filter(scale == input$analysis_main_scale_selector) |>
##       pull(ZoneName) |>
##       unique()
##   }
##   ## print(site)
##   if (!current_site %in% site) {
##     updateSelectInput(session,
##       "analysis_main_zone_selector",
##       choices = site
##     )
##   }
## })

## observeEvent(input$analysis_main_effectsscale_selector, {
##     effect_scale_value(input$analysis_main_effectsscale_selector)
## })

output$analysis_trends <- renderUI({
  fluidPage(
    id = "analysis_trends",
    fluidRow(
      h2(paste0("Model parameter estimates")),
      column(
        width = 7,
        tags$div(reactableOutput(outputId = "analysis-cellmeans-table"))
      ),
      column(
        width = 7,
        tags$div(plotOutput(
          outputId = "analysis-trend-plot",
          height = "300px", width = "900px"
        ))),
      column(width = 5,
        box(title = "Information",
          status = "info",solidHeader = TRUE,
          htmltools::includeMarkdown("../md/temporal_analysis_summary.md"),
          )),
      ),
  )
})

## ## observeEvent(input$analysis_main_effectsscale_selector,
## ##   {
## ##     req(effect_scale_value)
## ##     effect_scale_value(input$analysis_main_effectsscale_selector)
## ##   }
## ## )


lor_table_symbol <- function(value) {
  if (is.na(value)) return("")
  if (value) {
    tagAppendAttributes(shiny::icon("flag"), style = paste("color: black; font-weight:900;"))
  } else {
    ""
  }
}

output[["analysis-cellmeans-table"]] <- reactable::renderReactable({
  ## req(dat_value())
  ## dat <- dat_value()
  req(mod_value_details())
  dat <- mod_value_details()
  ## print("Cellmeans table")
  ## print(dat)
  d <- dat$nm_cm |>
    _[[1]]
  if (!is.null(d)) {
    d <- d |>
      readRDS()
    ## print(d)
    d <- d |> 
      get_cellmeans_summ()
    ## get the lor_flag
    dd <- dat |>
      dplyr::select(processed_data) |>
      unnest(processed_data) |>
      dplyr::select(Year, lor_flag) |>
      mutate(Year = as.character(Year)) |>
      distinct()
   d |>
     mutate(across(c(median, lower, upper), ~ round(.x, 3))) |>
     left_join(dd, by = c("contrast" = "Year")) |>
     reactable(
       columns = list(lor_flag = colDef(
         cell = function(value) lor_table_symbol(value)
       ))
     )
  } else {
    tribble(~"The model does not contain both baseline and reporting data, and thus no comparisons can be provided") |>
      reactable()
  }
})

select_details_model <- function() {
    req(analysis_data())
    data <- analysis_data() |>
        mutate(ZoneName = case_when(
          scale == "zone" ~ ZoneName,
          scale == "site" ~ Site,
          scale == "area" ~ Area
        ))
      ## mutate(ZoneName = ifelse(scale == "zone", ZoneName, Site))
    ## print("select_details_model")
    ## print(data)
    ## print(data |> filter(scale == input$analysis_main_scale_selector) |>
    ##         filter(ZoneName == input$analysis_main_zone_selector) |>
    ##         filter(Var == input$analysis_main_var_selector) |>
    ##         filter(Value_type == input$analysis_main_value_type_selector) |>
    ##         filter(Normalised_against == input$analysis_main_normalised_against_selector)
    ##         )
    ## print("And then")

    ## print(data |> filter(scale == input$analysis_main_scale_selector) |>
    ##   filter(ZoneName == input$analysis_main_zone_selector) |>
    ##   filter(Var == input$analysis_main_var_selector) |>
    ##   filter(Value_type == input$analysis_main_value_type_selector) |>
    ##   filter(Normalised_against == input$analysis_main_normalised_against_selector) |> _[["fit"]])
    mod <- get_filtered_model_details(data,
      sscale = input$analysis_main_scale_selector,
      zone = input$analysis_main_zone_selector,
      var = input$analysis_main_var_selector,
      type = input$analysis_main_value_type_selector,
      normalised_against = input$analysis_main_normalised_against_selector
    )
 mod 
}




output[["analysis-trend-plot"]] <- renderPlot({
  ## print("I am here")
  req(mod_value_details())
  dat <- mod_value_details()
 ## print(dat)
  dt <- dat$processed_data |>
    _[[1]]
  ## dt <- select_details_model()$data 
  ## print(dt)
  ## if (input$analysis_main_scale_selector == "site") {
  ##   dt <- dt |>
  ##     filter(Site == input$analysis_main_zone_selector) |>
  ##     droplevels()
  ## }
  ## print(dt)

  ## req(dat_value())
  ## req(effect_scale_value())
  ## print("make plot")
  ## print(effect_scale_value())
  ## dat <- dat_value()
  d <- dat$nm_cm |>
    _[[1]]
  if (!is.null(d)) {
    d <- d |>
      readRDS() |>
      get_cellmeans_summ() |>
      mutate(across(c(median, lower, upper), ~ round(.x, 3))) |>
      filter(contrast != "Baseline")
    d <- d |>
      mutate(Year = as.numeric(as.character(contrast)))
    ## if (input$analysis_main_scale_selector != "site") {
    if (input$analysis_main_scale_selector != "site") {
      d <- d |>
        mutate(Baseline = ifelse(Year < 2021, TRUE, FALSE))
    }
    d |>
      ggplot(aes(x = Year, y = median)) +
      geom_point(
        data = dt,
        aes(
          y = Values,
          x = as.numeric(as.character(cYear)),
          group = Site
        ),
        color = "gray"
      ) +
      geom_line(
        data = dt,
        aes(
          y = Values,
          x = as.numeric(as.character(cYear)),
          group = Site
        ),
        color = "gray"
      ) +
      geom_line(linetype = "dashed") +
      geom_pointrange(aes(ymin = lower, ymax = upper, colour = Baseline)) +
      ylab(label = input$analysis_main_var_selector) +
      xlab(label = "") +
      theme_classic(14)
  }
})


output$analysis_effects <- renderUI({
  fluidPage(
    id = "analysis_effects",
    fluidRow(
      h2(paste0("Model parameter estimates (", input$analysis_main_effectsscale_selector, " scale)")),
      column(
        width = 7,
        tags$div(reactableOutput(outputId = "analysis-effects-table")),
        downloadButton("download_effects_data", "Download as csv"),
      ),
      column(
        width = 7,
        tags$div(plotOutput(
          outputId = "analysis-effects-plot",
          height = "300px", width = "900px"
        ))),
      column(width = 5,
        box(title = "Information",
          status = "info",solidHeader = TRUE,
          htmltools::includeMarkdown("../md/temporal_analysis_summary.md"),
          )),
      ),
    )
})

output[["analysis-effects-table"]] <- reactable::renderReactable({
  ## req(dat_value())
  ## dat <- dat_value()
  req(mod_value_details())
  dat <- mod_value_details()
  d <- dat$summ_e |>
    _[[1]]
  if (!is.null(d)) {
    d <- d |>
      derive_change()
    if (input$analysis_main_effectsscale_selector == "percentage") {
     d <- d |>
       mutate(across(c(median, lower, upper), ~ 100 * (.x - 1)))
    }
   d |> 
    mutate(across(c(median, lower, upper, Pl, Pg), ~ round(.x, 3))) |> 
    ## reactable()
      reactable(columns = list(change = colDef(style = function(value) {
        list(
          background = change_palette[value],
          color = change_palette[value]
        )
      })))
  } else {
    tribble(~"The model does not contain both baseline and reporting data, and thus no comparisons can be provided") |>
      reactable()
  }
})

output$download_effects_data <- downloadHandler(
  filename = function() {
    ## req(dat_value())
    ## dat <- dat_value()
  req(mod_value_details())
  dat <- mod_value_details()
    dat$nm_e |>
      _[[1]] |>
      basename() |>
      str_replace("posteriors_", "") |>
      str_replace("rds", "csv")
  },
  content = function(file) {
    req(dat_value())
    dat <- dat_value()
    data <- dat$summ_e |>
      _[[1]]
    if (input$analysis_main_effectsscale_selector == "percentage") {
      data <- data |>
        mutate(across(c(median, lower, upper), function(x) 100 * (x - 1)))
    }
    write.csv(data, file)
  }
)

output[["analysis-effects-plot"]] <- renderPlot({
  ## req(dat_value())
  ## dat <- dat_value()
  req(mod_value_details())
  dat <- mod_value_details()
  d <- dat$nm_e |>
    _[[1]]
  if (!is.null(d)) {
    d <- d |>
      readRDS()
    d_s <- d |>
      summarise_draws(Pl = ~ mean(.x < 1), Pg = ~ mean(.x > 1)) |>
      derive_change() |>
      dplyr::select(contrast, change)
    d <- d |>
      left_join(d_s, by = "contrast") |> 
      ggplot(aes(x = .value, y = contrast)) +
      ggridges::geom_density_ridges(aes(fill = factor(change)), alpha = 1, show.legend = FALSE) +
      geom_vline(xintercept = 1, linetype = "dashed") +
      scale_x_continuous("Fractional difference (% change) between each year and the baseline period",
        trans = "log2",
        breaks = scales::extended_breaks(n = 8),
        labels = function(x) round(100 * (x - 1), 2)
      ) +
      scale_fill_manual(values = change_palette, limits = 1:7 ) +
      theme_classic(14) +
      theme(axis.title.y = element_blank())
    if (input$analysis_main_effectsscale_selector == "fold") {
     d <- d +
      scale_x_continuous("Fractional difference (fold change) between each year and the baseline period",
        trans = "log2",
        breaks = scales::extended_breaks(n = 8),
        labels = function(x) x
      ) 
    }
    d
  }
})


