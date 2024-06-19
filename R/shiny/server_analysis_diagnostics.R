source("40_temporal_analysis.R")
source("05_stats_functions.R")


output$analysis_diagnostics <- renderUI({
  req(analysis_data())
  data <- analysis_data()
  fluidPage(
    fluidRow(
      box(
        title = "Status of models",
        status = "primary",
        width = 12,
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        # status = "success",
        # shinyTree("tree", theme = "default", checkbox = FALSE),
        reactable::renderReactable({
          dd <- data |>
            filter(scale == "zone") |> 
            dplyr::select(ZoneName, Var, Value_type, valid) |>
            unnest(c(valid)) |>
            dplyr::select(-nm) |>
            dplyr::mutate(across(c(ks, ds, q, o), function(x) ifelse(x > 0.05, TRUE, FALSE))) |>
            dplyr::mutate(valid = ifelse(rowSums(across(c(ks, ds, q, o))) == 4, TRUE, FALSE)) |>
            arrange(valid)
          # print(dd)
          dd |> reactable(filterable = TRUE)
        })
      )
    ),
    fluidRow(
      box(
        class = "analysis-zone-box",
        status = "info",
        width = 6,
        solidHeader = TRUE,
        column(width = 3, selectInput("analysis_zone_selector", "Select Site:", choices = unique(data$ZoneName))),
        column(width = 3, selectInput("analysis_var_selector", "Select var type:", choices = unique(data$Var))),
        column(width = 3, selectInput("analysis_value_type_selector", "Select value type:", choices = unique(data$Value_type))),
        column(width = 3, selectInput("analysis_normalised_against_selector", "Normalisation type:", choices = unique(data$Normalised_against)))
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
          title = "Model Validation",
          icon = icon("database"),
          id = "analysis_validation_tab",
          uiOutput("analysis_validation")
        ),
        tabPanel(
          title = "Model summaries",
          icon = icon("database"),
          id = "analysis_summary_tab",
          uiOutput("analysis_summary")
        ),
        )
    )
  )
})

output$analysis_validation <- renderUI({
  tabsetPanel(
    id = "analysis_validation_tabs",
    tabPanel(
      title = "Prior vs Posterior",
      id = "analsis_validation_prior-posterior_tab",
      fillRow(
        flex = NA,
        tags$div(plotOutput(
          outputId = "analysis-prior-posterior",
          height = "500px", width = "900px"
        ), style = "margin-right:50px"),
        box(
          title = span(icon("info", style = "margin-right: 10px;"), "Prior vs Posterior plots"),
          #class = "caption-box",
          status = "info",
          width = 2,
          solidHeader = TRUE,
          htmltools::includeMarkdown("../md/prior_vs_posterior.md"),
          )
      )
    ),
    tabPanel(
      title = "Traceplots",
      id = "analsis_validation_traceplots_tab",
      fillRow(
        flex = NA,
        tags$div(plotOutput(
          outputId = "analysis-traceplot",
          height = "500px", width = "500px"
        ), style = "margin-right:50px"),
        box(
          title = span(icon("info", style = "margin-right: 10px;"), "Traceplots"),
          status = "info",
          width = 6,
          solidHeader = TRUE,
          htmltools::includeMarkdown("../md/traceplots.md"),
          )
      )
    ),
    tabPanel(
      title = "AC plots",
      id = "analsis_validation_ac_tab",
      fillRow(
        flex = NA,
        tags$div(plotOutput(
          outputId = "analysis-ac",
          height = "500px", width = "500px"
        ), style = "margin-right:50px"),
        box(
          title = span(icon("info", style = "margin-right: 10px;"), "Traceplots"),
          status = "info",
          width = 3,
          solidHeader = TRUE,
          htmltools::includeMarkdown("../md/acplots.md"),
          )
      )
    ),
    tabPanel(
      title = "Rhat plots",
      id = "analsis_validation_rhat_tab",
      fillRow(
        flex = NA,
        tags$div(plotOutput(
          outputId = "analysis-rhat",
          height = "500px", width = "500px"
        ), style = "margin-right:50px"),
        box(
          title = span(icon("info", style = "margin-right: 10px;"), "Rhat plots"),
          status = "info",
          width = 4,
          solidHeader = TRUE,
          htmltools::includeMarkdown("../md/rhatplots.md"),
          )
      )
    ),
    tabPanel(
      title = "ESS plots",
      id = "analsis_validation_ess_tab",
      fillRow(
        flex = NA,
        tags$div(plotOutput(
          outputId = "analysis-ess",
          height = "500px", width = "500px"
        ), style = "margin-right:50px"),
        box(
          title = span(icon("info", style = "margin-right: 10px;"), "ESS plots"),
          status = "info",
          width = 5,
          solidHeader = TRUE,
          withMathJax(),
          htmltools::includeMarkdown("../md/essplots.md"),
          )
      )
    ),
    tabPanel(
      title = "Posterior Probability Check",
      id = "analsis_validation_ppc_tab",
      fillRow(
        flex = NA,
        tags$div(plotOutput(
          outputId = "analysis-ppc",
          height = "500px", width = "500px"
        ), style = "margin-right:50px"),
        box(
          title = span(icon("info", style = "margin-right: 10px;"), "ESS plots"),
          status = "info",
          width = 5,
          solidHeader = TRUE,
          htmltools::includeMarkdown("../md/ppc.md"),
          )
      )
    ),
    tabPanel(
      title = "DHARMa residuals",
      id = "analsis_validation_dharma_tab",
      fillRow(
        flex = NA,
        tags$div(plotOutput(
          outputId = "analysis-dharma",
          height = "300px", width = "900px"
        ), style = "margin-right:50px"),
        box(
          title = span(icon("info", style = "margin-right: 10px;"), "DHARMa plots"),
          status = "info",
          width = 3,
          solidHeader = TRUE,
          htmltools::includeMarkdown("../md/dharma.md"),
          )
      )
    )
  )
})

output$analysis_summary <- renderUI({
  fluidPage(
    id = "analysis_summary",
    fluidRow(
      h2("Model parameter estimates (link scale)", style = "margin-left: 15px;"),
      column(
        width = 7,
        tags$div(reactableOutput(outputId = "analysis-summary-table"))
      ),
      column(width = 5,
        box(title = "Information",
          status = "info",solidHeader = TRUE,
          htmltools::includeMarkdown("../md/temporal_analysis_summary.md"),
          )),
      )
  )
})



output[["analysis-prior-posterior"]] <- renderPlot({
  req(mod_value())
  mod <- mod_value()
  g1 <- mod |>
    SUYR_prior_and_posterior() |>
    suppressWarnings() |>
    suppressMessages()
  g1
})

output[["analysis-traceplot"]] <- renderPlot({
  req(mod_value())
  mod <- mod_value()
  g1 <- mod$fit |>
    rstan::stan_trace() |>
    suppressWarnings() |>
    suppressMessages()
  g1
})

output[["analysis-ac"]] <- renderPlot({
  req(mod_value())
  mod <- mod_value()
  g1 <- mod$fit |>
    rstan::stan_ac() |>
    suppressWarnings() |>
    suppressMessages()
  g1
})

output[["analysis-rhat"]] <- renderPlot({
  req(mod_value())
  mod <- mod_value()
  g1 <- mod$fit |>
    rstan::stan_rhat() |>
    suppressWarnings() |>
    suppressMessages()
  g1
})

output[["analysis-ess"]] <- renderPlot({
  req(mod_value())
  mod <- mod_value()
  g1 <- mod$fit |>
    rstan::stan_ess() |>
    suppressWarnings() |>
    suppressMessages()
  g1
})

output[["analysis-ppc"]] <- renderPlot({
  req(mod_value())
  mod <- mod_value()
  g1 <- mod |>
    pp_check(type = "dens_overlay", ndraws = 200) |>
    suppressWarnings() |>
    suppressMessages()
  g1
})

output[["analysis-dharma"]] <- renderPlot({
  req(mod_value())
  mod <- mod_value()
  ## print(summary(mod))
  ## print(length(brms::standata(mod)$Y))
  resids_d <- mod |>
    make_brms_dharma_res(integerResponse = FALSE) |>
    suppressWarnings() |>
    suppressMessages()
  wrap_elements(~ testUniformity(resids_d)) +
    wrap_elements(~ plotResiduals(resids_d, quantreg = FALSE)) +
    wrap_elements(~ testDispersion(resids_d)) +
    plot_layout(nrow = 1)
})
    
output[["analysis-summary-table"]] <- reactable::renderReactable({
  req(mod_value())
  mod <- mod_value()
  mod |>
    posterior::as_draws_df(variable = "^b_.*|^sd_.*", regex = TRUE) |>
    posterior::summarise_draws(
      median,
      HDInterval::hdi,
      length,
      rhat,
      ess_bult = posterior::ess_bulk,
      ess_tail = posterior::ess_tail
    ) |>
    mutate(across(where(is.numeric), round, 3)) |> 
    reactable()
})

#If it is supplied (e.g. from the input value) use it, otherwise,
#get from first combination....
select_diag <- function(focal) {
 req(levels_pool_diagnostics())
 current_zone <- input$analysis_zone_selector
 current_var <- input$analysis_var_selector
 current_value_type <- input$analysis_value_type_selector
 if (focal == "var") {
   temp <-
     levels_pool_diagnostics() |>
     filter(ZoneName == current_zone) |>
     pull(Var) |>
     unique()
 }
 if (focal == "value_type") {
   temp <-
     levels_pool_diagnostics() |>
     filter(ZoneName == current_zone) |>
     filter(Var == current_var) |> 
     pull(Value_type) |>
     unique()
 }
 if (focal == "normalised_against") {
   temp <-
     levels_pool_diagnostics() |>
     filter(ZoneName == current_zone) |>
     filter(Var == current_var) |> 
     filter(Value_type == current_value_type) |> 
     pull(Normalised_against) |>
     unique()
 }
 if (focal == "end") {
   temp <-
     levels_pool_diagnostics() |>
     filter(ZoneName == current_zone) |>
     filter(Var == current_var) |> 
     filter(Value_type == current_value_type) |> 
     filter(Normalised_against == current_normalised_against) |> 
     pull(Normalised_against) |>
     unique()
 }
  temp
}


select_diag_model <- function() {
    req(analysis_data())
    data <- analysis_data() |> filter(scale == "zone")
    mod <- get_filtered_model(data,
      zone = input$analysis_zone_selector,
      var = input$analysis_var_selector,
      type = input$analysis_value_type_selector,
      normalised_against = input$analysis_normalised_against_selector
    )
 mod 
}


observeEvent(input$analysis_zone_selector, {
  updateSelectInput(session,
    "analysis_var_selector",
    choices = select_diag(focal = "var")
  )
  if (input$analysis_var_selector %in% select_diag(focal = "var"))
    mod_value(select_diag_model())
})
observeEvent(input$analysis_var_selector, {
  updateSelectInput(session,
    "analysis_value_type_selector",
    choices = select_diag(focal = "value_type")
  )
  if (input$analysis_value_type_selector %in% select_diag(focal = "value_type"))
    mod_value(select_diag_model())
})
observeEvent(input$analysis_value_type_selector, {
  updateSelectInput(session,
    "analysis_normalised_against_selector",
    choices = select_diag(focal = "normalised_against")
  )
  if (input$analysis_normalised_against_selector %in% select_diag(focal = "normalised_against"))
    mod_value(select_diag_model())
})
observeEvent(input$analysis_normalised_against_selector, {
  mod_value(select_diag_model())
})


















## observeEvent(input$analysis_zone_selector, {
##   req(levels_pool_diagnostics())

##   ## Get the currently selected values
##   current_zone <- input$analysis_zone_selector #unique(sub_data()$scale)
##   current_var <- input$analysis_var_selector 
##   current_value_type <- input$analysis_value_type_selector
##   sub_dat <- levels_pool_diagnostics() |>
##     filter(ZoneName == current_zone) |>
##     droplevels() 
##     updateSelectInput(session,
##       "analysis_type_selector",
##       choices = unique(sub_dat$Var)
##     )
##     updateSelectInput(session,
##       "analysis_value_type_selector",
##       choices = unique(sub_dat$Value_type)
##     )
##     updateSelectInput(session,
##       "analysis_normalised_against_selector",
##       choices = unique(sub_dat$Normalised_against)
##     )
##   ## print(input$analysis_zone_selector)
##   mod_value(select_model())
## })

## observeEvent(input$analysis_var_selector, {
##   req(levels_pool_diagnostics())

##   ## Get the currently selected values
##   current_zone <- input$analysis_zone_selector #unique(sub_data()$scale)
##   current_var <- input$analysis_var_selector 
##   current_value_type <- input$analysis_value_type_selector
##   sub_dat <- levels_pool_diagnostics() |>
##     filter(ZoneName == current_zone,
##       Var == current_var) |>
##     droplevels() 
##   ## print("Type selector change")
##   ## print(unique(sub_dat$Value_type))
##   ## print(unique(sub_dat))
##     updateSelectInput(session,
##       "analysis_value_type_selector",
##       choices = unique(sub_dat$Value_type)
##     )
##     updateSelectInput(session,
##       "analysis_normalised_against_selector",
##       choices = unique(sub_dat$Normalised_against)
##     )
##   print(paste("input$analysis_value_type_selector:", input$analysis_value_type_selector))
##   mod_value(select_model())
## })

## observeEvent(input$analysis_value_type_selector, {
##   req(levels_pool_diagnostics())

##   ## print(levels_pool_diagnostics())
##   ## Get the currently selected values
##   current_zone <- input$analysis_zone_selector #unique(sub_data()$scale)
##   current_var <- input$analysis_var_selector 
##   current_value_type <- input$analysis_value_type_selector
##   sub_dat <- levels_pool_diagnostics() |>
##     filter(ZoneName == current_zone,
##       Var == current_var,
##       Value_type == current_value_type) |>
##     droplevels() 
##   print("Value type change")
##   print(sub_dat)
##   print(unique(sub_dat$Normalised_against))
##     updateSelectInput(session,
##       "analysis_normalised_against_selector",
##       choices = unique(sub_dat$Normalised_against),
##       selected = unique(sub_dat$Normalised_against)[1]
##     )
## print(input$analysis_normalised_against_selector)
## print(input$analysis_normalised_against_selector)
##   ## mod_value(select_model())
## })

## ## observeEvent(input$analysis_normalised_against_selector, {
## ##   req(levels_pool_diagnostics())
## ##   print("inside normalised against selector change")
## ## print(input$analysis_normalised_against_selector)
## ##   mod_value(select_model())
## ## })






## ## observeEvent(
## ##   c(
## ##     input$analysis_zone_selector,
## ##     input$analysis_var_selector,
## ##     input$analysis_value_type_selector,
## ##     input$analysis_normalised_against_selector
## ##   ),
## ##   {

## ##   print("In overall observeEvent")
## ##     req(analysis_data())
## ##     data <- analysis_data()
## ## print(input$analysis_normalised_against_selector)
## ## ##     variable <- input$analysis_var_selector
## ## ##     variable_value(variable)
## ## ##     ## dat <- get_filtered_data(data,
## ## ##     ##   scale = input$analysis_scale_selector,
## ## ##     ##   zone = input$analysis_zone_selector,
## ## ##     ##   var = input$analysis_var_selector,
## ## ##     ##   type = input$analysis_value_type_selector
## ## ##     ## )
## ## ## ##     dat_value(dat)
## ## ##     mod <- get_filtered_model(data,
## ## ##       zone = input$analysis_zone_selector,
## ## ##       var = input$analysis_var_selector,
## ## ##       type = input$analysis_value_type_selector,
## ## ##       normalised_against = input$analysis_normalised_against_selector
## ## ##     )
## ## ##     mod_value(mod)
## ##   }
## ## )
