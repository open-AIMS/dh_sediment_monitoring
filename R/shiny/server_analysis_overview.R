output$analysis_overview <- renderUI({
  req(analysis_data())
  data <- analysis_data()
  effect_years <- data |>
    ## dplyr::select(ZoneName, Type, Var, Value_type, effects) |>
    dplyr::select(ZoneName, Type, Var, Value_type, summ_e) |>
    unnest(c(summ_e)) |>
    dplyr::select(scale, ZoneName, Type, Var, Value_type, year, contrast) |>
    distinct()
  effect_years_value(effect_years)
  
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
          width = 7,
          solidHeader = TRUE,
          # column(width = 3, selectInput("analysis_overview_zone_selector", "Select Site:", choices = unique(data$ZoneName))),
          column(width = 2,
            selectInput("analysis_overview_scale_type_selector", "Select scale:",
              choices = unique(data$scale), selected = "zone")),
          column(width = 2,
            selectInput("analysis_overview_value_type_selector", "Select value type:",
              choices = unique(data$Value_type), selected = "Standardised")),
          column(width = 3,
            selectInput("analysis_overview_type_selector", "Select variable type:",
              choices = c("All", unique(data$Type)), selected = "hydrocarbons")),
          column(
            width = 2,
            selectInput("analysis_overview_year_selector", "Select focal year:",
              choices = unique(effect_years$year), selected = max(effect_years$year)
            )
          ),
          column(
            width = 3,
            selectInput("analysis_overview_contrast_selector", "Select contrast:",
              choices = unique(effect_years$contrast),
              selected = str_subset(unique(effect_years$contrast), paste0(max(effect_years$year), ".*Baseline"))
            )
          )
        ),
        box(
          title = "General instructions",
          status = "info",
          width = 5,
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
            if (input$analysis_overview_scale_type_selector == "zone") {
              d <- data |>
                dplyr::select(-scale) |> 
                ## dplyr::select(-data, -fit) |>
                ## unnest(c(effects)) |>
                unnest(c(summ_e)) |>
                dplyr::select(scale, ZoneName, Type, Var, Value_type, contrast, change, year) |>
                filter(
                  scale == input$analysis_overview_scale_type_selector,
                  Value_type == input$analysis_overview_value_type_selector,
                  year == input$analysis_overview_year_selector,
                  contrast == input$analysis_overview_contrast_selector
                ) |> 
                ## Type == input$analysis_overview_type_selector,
                ## str_detect(contrast, "2023")) |>
                filter(case_when(
                  input$analysis_overview_type_selector != "All" ~ Type == input$analysis_overview_type_selector,
                  TRUE ~ TRUE
                )) |>
                dplyr::select(-Value_type, -contrast, -year) |>
                  mutate(ZoneName = factor(as.character(ZoneName))) |>
                  arrange(ZoneName, Type, Var) |>
                  pivot_wider(id_cols = everything(), names_from = Var, values_from = change) |>
                  dplyr::select(-scale, -Type) |>
                  mutate(ZoneName = as.character(ZoneName)) |> 
                reactable(
                  pagination = FALSE,
                  defaultColDef = colDef(style = function(value) {
                    list(
                      background = change_palette[value],
                      color = change_palette[value]
                    )
                  })
                )
            } else if (input$analysis_overview_scale_type_selector == "site") {
              d <- data |>
                dplyr::select(-scale) |> 
                ## dplyr::select(-data, -fit) |>
                ## unnest(c(effects)) |>
                unnest(c(summ_e)) |>
                dplyr::select(scale, Site, Type, Var, Value_type, contrast, change, year) |>
                filter(
                  scale == input$analysis_overview_scale_type_selector,
                  Value_type == input$analysis_overview_value_type_selector,
                  year == input$analysis_overview_year_selector,
                  contrast == input$analysis_overview_contrast_selector
                ) |> 
                ## Type == input$analysis_overview_type_selector,
                ## str_detect(contrast, "2023")) |>
                filter(case_when(
                  input$analysis_overview_type_selector != "All" ~ Type == input$analysis_overview_type_selector,
                  TRUE ~ TRUE
                )) |>
                dplyr::select(-Value_type, -contrast, -year) |>
                  mutate(Site = factor(as.character(Site))) |>
                  arrange(Site, Type, Var) |>
                  pivot_wider(id_cols = everything(), names_from = Var, values_from = change) |>
                dplyr::select(-scale, -Type) |> 
                mutate(Site = as.character(Site)) |>
                reactable(
                  pagination = FALSE,
                  defaultColDef = colDef(style = function(value) {
                    list(
                      background = change_palette[value],
                      color = change_palette[value]
                    )
                  })
                )
            }
            d
          })
        )
      )
    )
})

