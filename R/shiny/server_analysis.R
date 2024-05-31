
source("40_temporal_analysis.R")
source("05_stats_functions.R")



observeEvent(input$runAnalysisCode, {
  print("Made it to here")
  if (!file.exists(paste0(data_path, "modelled/data_all.RData"))) {
    promise <- future_promise({
      module_temporal()
    })
  }
  if (file.exists(paste0(data_path, "modelled/data_all.RData"))) {
    data <- readRDS(file = paste0(data_path, "modelled/data_all.RData"))
    effect_years <- data |>
      dplyr::select(ZoneName, Type, Var, Value_type, effects) |>
      unnest(c(effects)) |>
      dplyr::select(ZoneName, Type, Var, Value_type, year) |>
      distinct()
  
  output$analysis_overview <- renderUI({
    fluidPage(
      fluidRow(
        box(
          title = "Status of models",
          status = "primary",
          width = 12,
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          column(
            width = 8,
            reactable::renderReactable({
              dd <- data |>
                dplyr::select(ZoneName, Var, Value_type, valid) |>
                unnest(c(valid)) |>
                dplyr::select(-nm) |>
                dplyr::mutate(across(c(ks, ds, q, o), function(x) ifelse(x > 0.05, TRUE, FALSE))) |>
                dplyr::mutate(valid = ifelse(rowSums(across(c(ks, ds, q, o))) == 4, TRUE, FALSE)) |>
                arrange(valid)
              dd |> reactable(filterable = TRUE)
            })
          ),
          column(
            width = 4,
            box(
              title = "General instructions",
              status = "info",
              width = 12,
              solidHeader = TRUE,
              htmltools::includeMarkdown("../md/temporal_analysis_instructions.md"),
              )
          )
        )
      ),
      fluidRow(
        box(
          class = "analysis-zone-box",
          status = "info",
          width = 6,
          solidHeader = TRUE,
          # column(width = 3, selectInput("analysis_overview_zone_selector", "Select Site:", choices = unique(data$ZoneName))),
          column(width = 3,
            selectInput("analysis_overview_value_type_selector", "Select value type:",
              choices = unique(data$Value_type))),
          column(width = 3,
            selectInput("analysis_overview_type_selector", "Select variable type:",
              choices = c("All", unique(data$Type)))),
          column(
            width = 3,
            selectInput("analysis_overview_year_selector", "Select variable type:",
              choices = unique(effect_years$year)
            )
          )
        ),
        box(
          title = "General instructions",
          status = "info",
          width = 6,
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          htmltools::includeMarkdown("../md/temporal_analysis_overview_instructions.md"),
          )
      ),
      fluidRow(
        box(
          title = "Overview",
          status = "primary",
          width = 10,
          solidHeader = TRUE,
          reactable::renderReactable({
            data |>
              dplyr::select(-data, -fit) |>
              unnest(c(effects)) |>
              dplyr::select(ZoneName, Type, Var, Value_type, contrast, change, year) |>
              filter(
                Value_type == input$analysis_overview_value_type_selector,
                year == input$analysis_overview_year_selector
              ) |> 
              ## Type == input$analysis_overview_type_selector,
              ## str_detect(contrast, "2023")) |>
              filter(case_when(input$analysis_overview_type_selector != "All" ~ Type == input$analysis_overview_type_selector,
                TRUE ~ TRUE)) |> 
              dplyr::select(-Value_type, -contrast, -year) |>
                      mutate(ZoneName = factor(as.character(ZoneName))) |>
                      arrange(ZoneName, Type, Var) |> 
              pivot_wider(id_cols = everything(), names_from = ZoneName, values_from = change) |>
              reactable(pagination = FALSE,
                defaultColDef = colDef(style = function(value) {
                  list(
                    background = change_palette[value],
                    color = change_palette[value]
                  )
                }))
          })
        )
      )
    )
  })

  output$analysis_main <- renderUI({
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
          column(width = 3, selectInput("analysis_var_selector", "Select value type:", choices = unique(data$Var))),
          column(width = 3, selectInput("analysis_value_type_selector", "Select value type:", choices = unique(data$Value_type)))
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
          tabPanel(
            title = "Plots",
            icon = icon("database"),
            id = "analysis_plots_tab",
            uiOutput("analysis_plots")
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
        ),
      fluidRow(
        h2("Comparison between baseline (2019/2020) and each year (fold scale)", style = "margin-left: 15px;"),
        column(
          width = 7,
          tags$div(reactableOutput(outputId = "analysis-effects-table"))
        ),
        column(width = 5,
          box(title = "Information",
            status = "info",solidHeader = TRUE,
            htmltools::includeMarkdown("../md/temporal_analysis_effects.md"),
            )),
      )
    )
  })
  
  output$analysis_plots <- renderUI({
    fluidPage(
      id = "analysis_plots",
      fluidRow(
        h2("Full posteriors for the baseline (2019/2020) and each subsequent year", style = "margin-left: 15px;"),
        column(
          width = 7,
          tags$div(plotOutput(
            outputId = "analysis-partial-plot",
            height = "300px", width = "900px"
          ))),
        column(width = 5,
          box(title = "Information",
            status = "info",
            solidHeader = TRUE,
            htmltools::includeMarkdown("../md/partial_posteriors.md") |>
                    str_replace("VALUE_TYPE", input$analysis_value_type_selector) |>
                    str_replace("VAR", input$analysis_var_selector) |>
                    str_replace("ZONE", input$analysis_zone_selector) |>
                    HTML()
            )
        )
       ),
      fluidRow(
        h2("Full posteriors of the comparisons between baseline (2019/2020) and each year (fold scale)", style = "margin-left: 15px;"),
        column(
          width = 7,
          tags$div(plotOutput(
            outputId = "analysis-effects-plot",
            height = "300px", width = "900px"
          ))
        ),
        column(width = 5,
          box(title = "Information",
            status = "info",
            solidHeader = TRUE,
            htmltools::includeMarkdown("../md/contrast_posteriors.md") |>
                    str_replace("VALUE_TYPE", input$analysis_value_type_selector) |>
                    str_replace("VAR", input$analysis_var_selector) |>
                    str_replace("ZONE", input$analysis_zone_selector) |>
                    HTML()
            )
        )
      )
    )
  })
  
  observeEvent(c(input$analysis_zone_selector, input$analysis_var_selector, input$analysis_value_type_selector), {
    variable <- input$analysis_var_selector
    dat <- get_filtered_data(data,
      zone = input$analysis_zone_selector,
      var = input$analysis_var_selector,
      type = input$analysis_value_type_selector
    )
    mod <- get_filtered_model(data,
      zone = input$analysis_zone_selector,
      var = input$analysis_var_selector,
      type = input$analysis_value_type_selector
    )

    output[["analysis-prior-posterior"]] <- renderPlot({
      g1 <- mod |>
        SUYR_prior_and_posterior() |>
        suppressWarnings() |>
        suppressMessages()
      g1
    })
    ## output[["analysis-prior-posterior_caption"]] <- renderText({
    ##   paste0("**Conclusions**:\n-asd")
    ## })
    
    output[["analysis-traceplot"]] <- renderPlot({
      g1 <- mod$fit |>
        rstan::stan_trace() |>
        suppressWarnings() |>
        suppressMessages()
      g1
    })

    output[["analysis-ac"]] <- renderPlot({
      g1 <- mod$fit |>
        rstan::stan_ac() |>
        suppressWarnings() |>
        suppressMessages()
      g1
    })

    output[["analysis-rhat"]] <- renderPlot({
      g1 <- mod$fit |>
        rstan::stan_rhat() |>
        suppressWarnings() |>
        suppressMessages()
      g1
    })

    output[["analysis-ess"]] <- renderPlot({
      g1 <- mod$fit |>
        rstan::stan_ess() |>
        suppressWarnings() |>
        suppressMessages()
      g1
    })

    output[["analysis-ppc"]] <- renderPlot({
      g1 <- mod |>
        pp_check(type = "dens_overlay", ndraws = 200) |>
        suppressWarnings() |>
        suppressMessages()
      g1
    })

    output[["analysis-dharma"]] <- renderPlot({
      print(summary(mod))
      resids_d <- mod |>
        make_brms_dharma_res(integerResponse = FALSE) |>
        suppressWarnings() |>
        suppressMessages()
      ## print(resids_d)
      wrap_elements(~ testUniformity(resids_d)) +
        wrap_elements(~ plotResiduals(resids_d, quantreg = FALSE)) +
        wrap_elements(~ testDispersion(resids_d)) +
        plot_layout(nrow = 1)
    })
    
    output[["analysis-summary-table"]] <- reactable::renderReactable({
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
    
    output[["analysis-effects-table"]] <- reactable::renderReactable({
      if (length(unique(dat$Baseline))>1) {   # only calcualte when both baseline and report years present
      ## if (any(str_detect(get_variables(mod), "cYear.*"))) {
      # baseline_and_years_summ(dat, mod) |> reactable()
      ## change_palette <- function(x) {
      ##   rgb(colorRamp(c(
      ##     "#7fb7d7", "#ffffbf", "#fc8d59"
      ##         ))(x), maxColorValue = 255)
      ## }
        #print(change_palette(6))
        compare_baseline_vs_years_summ(dat, mod) |>
          mutate(across(where(is.numeric), round, 3)) |>
          ## mutate(change = case_when(
          ##   Pl > 0.95 ~ 7,
          ##   Pl > 0.9 ~ 6,
          ##   Pl > 0.85 ~ 5,
          ##   Pg > 0.95 ~ 1,
          ##   Pg > 0.9 ~ 2,
          ##   Pg > 0.85 ~ 3,
          ##   .default = 4
          ## )) |> 
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

    output[["analysis-partial-plot"]] <- renderPlot({
      ## mod |>
      ##   conditional_effects()
      baseline_and_years_posteriors(dat, mod) |>
              mutate(Baseline = ifelse(contrast == "Baseline", TRUE, FALSE)) |>
              ggplot(aes(x = .value, y = contrast, fill = Baseline)) +
              ggridges::geom_density_ridges(alpha = 0.4) +
              xlab(label = variable) +
              theme_classic(14) +
              theme(axis.title.y = element_blank())
    })
    
    output[["analysis-effects-plot"]] <- renderPlot({
        compare_baseline_vs_years_posteriors(dat, mod) |>
                ## mod |>
                ##   emmeans(~cYear) |>
                ##   pairs() |>
                ##   gather_emmeans_draws() |>
                ##   mutate(.value = exp(.value)) |>
                ggplot(aes(x = .value, y = contrast)) +
                geom_vline(xintercept = 1, linetype = "dashed") +
                ggridges::geom_density_ridges(alpha = 0.4) +
                scale_x_continuous("Fractional difference (fold change) between each year and the baseline period", trans = "log2", breaks = scales::extended_breaks(n = 8)) +
                theme_classic(14) +
                theme(axis.title.y = element_blank())
    })
  })


  toggle_buttons(status_$status, stage =  5, bttn1 = "runEDACode", bttn2 = "runAnalysisCode")
    }
})

