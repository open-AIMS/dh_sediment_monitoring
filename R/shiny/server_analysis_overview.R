
output$analysis_overview <- renderUI({
  req(analysis_data())
  data <- analysis_data()
  ## effect_years <- data |>
  ##   ## dplyr::select(ZoneName, Type, Var, Value_type, effects) |>
  ##   dplyr::select(ZoneName, Type, Var, Value_type, Normalised_against, summ_e) |>
  ##   unnest(c(summ_e)) |>
  ##   dplyr::select(scale, ZoneName, Type, Var, Value_type, Normalised_against, year, contrast) |>
  ##   distinct()
  ## effect_years_value(effect_years)
  ## req(effect_years_value())
  req(initial_levels_pool_overview())
  req(levels_pool_overview())
  print(initial_levels_pool_overview())
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
          width = 12,
          solidHeader = TRUE,
          # column(width = 3, selectInput("analysis_overview_zone_selector", "Select Site:", choices = unique(data$ZoneName))),
          column(width = 2,
            selectInput("analysis_overview_scale_type_selector", "Scale:",
              choices = unique(levels_pool_overview()$scale),
              selected = initial_levels_pool_overview()$scale)),
          column(width = 2,
            selectInput("analysis_overview_type_selector", "Variable type:",
              ## choices = c("All", unique(levels_pool_overview()$Type)),
              ## selected = "All")),
              choices = unique(levels_pool_overview()$Type),
              selected = initial_levels_pool_overview()$Type)),
          column(width = 2,
            selectInput("analysis_overview_value_type_selector", "Value type:",
              choices = unique(levels_pool_overview()$Value_type),
              selected = initial_levels_pool_overview()$Value_type)),
          column(width = 2,
            selectInput("analysis_overview_normalised_type_selector", "Normalisation type:",
              choices = unique(levels_pool_overview()$Normalised_against),
              selected = initial_levels_pool_overview()$Normalised_against)),
          column(
            width = 2,
            selectInput("analysis_overview_year_selector", "Focal year:",
              choices = unique(levels_pool_overview()$year),
              selected = initial_levels_pool_overview()$year
            )
          ),
          column(
            width = 2,
            selectInput("analysis_overview_contrast_selector", "Select contrast:",
              choices = unique(levels_pool_overview()$contrast),
              selected = initial_levels_pool_overview()$contrast
              ## selected = str_subset(unique(levels_pool_overview()$contrast), paste0(max(levels_pool_overview()$year), ".*Baseline"))
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
                dplyr::select(scale, ZoneName, Type, Var, Value_type, contrast, Normalised_against, change, year) |>
                filter(
                  scale == input$analysis_overview_scale_type_selector,
                  Normalised_against == input$analysis_overview_normalised_type_selector,
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
                  dplyr::select(-scale, -Type, -Normalised_against) |>
                  mutate(ZoneName = as.character(ZoneName)) |> 
                overview_reactable()
                ## reactable(
                ##   pagination = FALSE,
                ##   defaultColDef = colDef(style = function(value) {
                ##     list(
                ##       background = change_palette[value],
                ##       color = change_palette[value]
                ##     )
                ##   })
                ## )
            } else if (input$analysis_overview_scale_type_selector == "site") {
              d <- data |>
                mutate(lor_flag = map(.x = processed_data, .f =  ~ any(.x$lor_flag))) |> 
                dplyr::select(-scale) |> 
                ## dplyr::select(-data, -fit) |>
                ## unnest(c(effects)) |>
                unnest(c(summ_e)) |>
                dplyr::select(scale, Site, Type, Var, Value_type, contrast,
                  Normalised_against, change, year, lor_flag) |>
                filter(
                  scale == input$analysis_overview_scale_type_selector,
                  Normalised_against == input$analysis_overview_normalised_type_selector,
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
                mutate(change = ifelse(lor_flag, paste(change, "LOR"), change)) |>
                dplyr::select(-lor_flag) |>
                  pivot_wider(id_cols = everything(), names_from = Var, values_from = change) |>
                dplyr::select(-scale, -Type, -Normalised_against) |> 
                mutate(Site = as.character(Site)) |>
                overview_reactable()
                ## reactable(
                ##   pagination = FALSE,
                ##   defaultColDef = colDef(style = function(value) {
                ##     list(
                ##       background = change_palette[value],
                ##       color = change_palette[value]
                ##     )
                ##   })
                ## )
            } else if (input$analysis_overview_scale_type_selector == "area") {
              print(input$analysis_overview_scale_type_selector)
              print(data |>
                      dplyr::select(-scale) |>
                      unnest(c(summ_e)) |>
                      dplyr::select(scale, Area, Type, Var, Value_type, contrast, Normalised_against, change, year) |>
                      ## filter(
                      ##   scale == input$analysis_overview_scale_type_selector#,
                      ##   ## Normalised_against == input$analysis_overview_normalised_type_selector,
                      ##   ## Value_type == input$analysis_overview_value_type_selector,
                      ##   ## year == input$analysis_overview_year_selector,
                      ##   ## contrast == input$analysis_overview_contrast_selector
                      ## ) |> 
                      pull(Area) |> unique())

              print(data |>
                dplyr::select(-scale) |>
                unnest(c(summ_e)) |>
                dplyr::select(scale, Area, Type, Var, Value_type, contrast, Normalised_against, change, year))
              d <- data |>
                dplyr::select(-scale) |>
                unnest(c(summ_e)) |>
                dplyr::select(scale, Area, Type, Var, Value_type, contrast, Normalised_against, change, year) |>
                filter(
                  scale == input$analysis_overview_scale_type_selector,
                  Normalised_against == input$analysis_overview_normalised_type_selector,
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
                mutate(Area = factor(as.character(Area))) |>
                arrange(Area, Type, Var) |>
                pivot_wider(id_cols = everything(), names_from = Var, values_from = change) |>
                dplyr::select(-scale, -Type, -Normalised_against) |>
                mutate(Area = as.character(Area)) |>
                overview_reactable()
                ## reactable(
                ##   pagination = FALSE,
                ##   defaultColDef = colDef(style = function(value) {
                ##     list(
                ##       background = change_palette[value],
                ##       color = change_palette[value]
                ##     )
                ##   }),
                ##   bordered = TRUE,
                ##   theme = reactableTheme(borderColor = "#dfe2e5",)
                ## )
            }
            d
          })
        )
      )
    )
})

overview_reactable <- function(d) {
  d |> 
    reactable(
      pagination = FALSE,
      defaultColDef = colDef(
        align = "center",
        cell =  function(value) lor_symbol(value),
        style = function(value) {
          list(
            background = suppressWarnings(ifelse(is.na(value), "#c6c6c6",
              change_palette[as.numeric(str_replace(value, " LOR", ""))])),
            color = suppressWarnings(ifelse(is.na(value), "#c6c6c6", change_palette[as.numeric(value)]))
            ## background = ifelse(is.na(value), "#c6c6c6", change_palette[value]),
            ## color = change_palette[value]
          )
        }),
      bordered = TRUE,
      theme = reactableTheme(borderColor = "#dfe2e5",)
    )
}

lor_symbol <- function(value) {
  if (is.na(value)) return("")
  if (str_detect(value, "LOR")) {
    tagAppendAttributes(shiny::icon("flag"), style = paste("color: black; font-weight:900;"))
  } else {
    value
    ## tagAppendAttributes(shiny::icon("circle-xmark"), style = paste("color: red; font-weight:900;"))
  }
}

#If it is supplied (e.g. from the input value) use it, otherwise,
#get from first combination....
select_dat <- function(focal) {
 req(levels_pool_overview())
 current_scale <- input$analysis_overview_scale_type_selector
 current_type <- input$analysis_overview_type_selector
 current_value_type <- input$analysis_overview_value_type_selector
 current_normalised_against <- input$analysis_overview_normalised_type_selector
 current_year <- input$analysis_overview_year_selector
 if (focal == "type") {
   temp <-
     levels_pool_overview() |>
     filter(scale == current_scale) |>
     pull(Type) |>
     unique()
 }
 if (focal == "value_type") {
   temp <-
     levels_pool_overview() |>
     filter(scale == current_scale) |>
     filter(Type == current_type) |>
     pull(Value_type) |>
     unique()
 }
 if (focal == "normalised_against") {
   temp <-
     levels_pool_overview() |>
     filter(scale == current_scale) |>
     filter(Type == current_type) |>
     filter(Value_type == current_value_type) |>
     pull(Normalised_against) |>
     unique()
 }
 if (focal == "year") {
   temp <-
     levels_pool_overview() |>
     filter(scale == current_scale) |>
     filter(Type == current_type) |>
     filter(Value_type == current_value_type) |>
     filter(Normalised_against == current_normalised_against) |>
     arrange(desc(year)) |> 
     pull(year) |>
     unique()
 }
 if (focal == "contrast") {
   temp <-
     levels_pool_overview() |>
     filter(scale == current_scale) |>
     filter(Type == current_type) |>
     filter(Value_type == current_value_type) |>
     filter(Normalised_against == current_normalised_against) |>
     filter(year == current_year) |>
     pull(contrast) |>
     unique() 
     temp <- unique(c(temp[str_which(temp, "Baseline")], temp))
 }
  temp
}


observeEvent(input$analysis_overview_scale_type_selector, {
  updateSelectInput(session,
    "analysis_overview_type_selector",
    choices = select_dat(focal = "type")
  )
})

observeEvent(input$analysis_overview_type_selector, {
  updateSelectInput(session,
    "analysis_overview_value_type_selector",
    choices = select_dat(focal = "value_type")
  )
})
observeEvent(input$analysis_overview_value_type_selector, {
  updateSelectInput(session,
    "analysis_overview_normalised_type_selector",
    choices = select_dat(focal = "normalised_against")
  )
})
observeEvent(input$analysis_overview_normalised_type_selector, {
  updateSelectInput(session,
    "analysis_overview_year_selector",
    choices = select_dat(focal = "year")
  )
})
observeEvent(input$analysis_overview_year_selector, {
  updateSelectInput(session,
    "analysis_overview_contrast_selector",
    choices = select_dat(focal = "contrast")
  )
})

