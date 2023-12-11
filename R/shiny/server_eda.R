source("30_eda.R")


## Trigger to run 20_process_data.R
observeEvent(input$runEDACode, {
  ## sedMod::module_eda()
  module_eda()
  
  data <- readRDS(file = paste0(data_path, "processed/data.RData"))
  ## Temporal EDA
  ## plots <- sedMod::eda_temporal(data)
  plots <- eda_temporal(data)
  output$eda_temporal <- renderUI({
    thetabs <- lapply(
      seq_along(plots$Initial_quarter),
      function(x) {
        nm <- paste0("eda_temporal_plot_", x)
        verticalTabPanel(
          box_height = "80px",
          title = HTML(paste0("Sampling semester:<br>", plots$Initial_quarter[x])),
          fillRow(
            flex = NA,
            tags$div(plotOutput(nm, height = "750px", width = "900px"), style = "margin-right:50px"),
            box(
              class =  "caption-box",
              status = "info",
              width = 1,
              solidHeader =  TRUE,
              textOutput(paste0(nm, "_caption"))
            )
          )
        )
      }
    )
    do.call(verticalTabsetPanel, thetabs)
  }
  )

  observe({
    lapply(seq_along(plots$Initial_quarter), function(x) {
      output[[paste0("eda_temporal_plot_", x)]] <- renderPlot({
        plots[x, "Plot"][[1]][[1]]
      })
      output[[paste0("eda_temporal_plot_", x, "_caption")]] <- renderText(
        paste0(
          "The figure to the left depicts the temporal sampling design
focussing on \"Sites\" that were first monitored in the ", plots$Initial_quarter[x],
" semester. The y-axis (rows) represent the Sampling sites (based on the Site names
they were first assigned).  Blue points represent the samples collected
that are considered \"Baseline\" or \"Reference\" samples from which
subsequent samples at the corresponding site are gauged.  Red points
represent non-\"Baseline\" samples. Points are jointed by lines to help
identify discontinued sampling (where no line exists) and where no Baselines
have been defined (when the left point of a sequence is red)."
)
)
    })
  })

  
  ## Type Temporal EDA
  ## plots_type <- sedMod::eda_type_temporal(data)
  plots_type <- eda_type_temporal(data)
  output$eda_type_temporal <- renderUI({
    thetabs <- lapply(1:nrow(plots_type),
      function(x) {
        nm <- paste0("eda_type_temporal_plot_", x)
        verticalTabPanel(
          box_height = "80px",
          title = HTML(paste0(plots_type$ZoneName[x], ":<br>", plots_type$Type[x])),
          fillRow(
            flex = NA,
            tags$div(plotOutput(nm, height = "750px", width = "1000px"), style = "margin-right:50px"),
            box(
              class =  "caption-box",
              status = "info",
              width = 1,
              solidHeader =  TRUE,
              textOutput(paste0(nm, "_caption"))
            )
          )
        )
      })
    do.call(verticalTabsetPanel, thetabs)
  })
  observe({
    lapply(1:nrow(plots_type), function(x) {
      output[[paste0("eda_type_temporal_plot_", x)]] <- renderPlot({
        plots_type[x, "Plot"][[1]][[1]]
      })
      output[[paste0("eda_type_temporal_plot_", x, "_caption")]] <- renderText(
        paste0(
          "The figure to the left depicts the temporal sampling design
focussing on each of the ", plots_type$Type[x], " within the ",
plots_type$ZoneName[x], " \"Sites\". The y-axis (rows) represent the
Sampling sites (based on the Site names
they were first assigned).  Blue points represent the samples collected
that are considered \"Baseline\" or \"Reference\" samples from which
subsequent samples at the corresponding site are gauged.  Red points
represent non-\"Baseline\" samples. "
)
)
    })
  })


 ## Map
  spatial <- readRDS(file = paste0(data_path, "primary/spatial.RData"))
  data <- readRDS(file = paste0(data_path, "processed/data.RData"))
  df_sf <- data |>
    group_by(Site, Var, Year_cal, Value_type) |>
    summarise(across(c(Longitude, Latitude, Values),
      list(~ mean(.x, na.rm = TRUE)),
      .names = "{.col}"
    )) |>
    st_as_sf(coords = c('Longitude', 'Latitude'), remove = FALSE, crs =  st_crs(4326)) 

  output$eda_map <- renderUI({
    fluidRow(
      box(
        class =  "map-selections-box",
        status = "info",
        width = 2,
        solidHeader =  TRUE,
        selectInput("var_selector", "Select Variable:", choices = unique(df_sf$Var)),
        selectInput("value_type_selector", "Select value type:", choices = unique(df_sf$Value_type)),
        sliderInput("year_selector", "Select Year:", min = min(df_sf$Year_cal), max = max(df_sf$Year_cal), value = max(df_sf$Year_cal)),
        ),
     leafletOutput("eda_map_map", width = "600px", height = "600px")
    )
  })
  

 ## filtered_data <- reactive({
 ##    df_sf |> filter(
 ##      Var == input$var_selector,
 ##      Year_cal == input$year_selector,
 ##      Value_type == input$value_type_selector
 ##    )
 ##  })


  output$eda_map_map <- renderLeaflet({
    
    ## df1 <- df_sf |> filter(
    ##         Var == input$var_selector,
    ##         Year_cal == input$year_selector,
    ##         Value_type == input$value_type_selector
    ## )
    ## pal <- colorNumeric(
    ##   palette = "Blues",
    ##   domain = df1$Values
    ##   ## domain = filtered_data()$Values
    ## )
    spatial |>
      st_transform(crs = st_crs(4326)) |>
      leaflet() |>
      ## addProviderTiles(providers$CartoDB.PositronNoLabels, options = providerTileOptions(noWrap = TRUE), group = "Carto")|>
      addTiles(group = "Basemap") |>
      addPolygons(
        group = "Zones",
        weight = 1,
        color = "blue",
        fillOpacity = 0.5, fill = TRUE, fillColor = "white",
        stroke = TRUE) |>
      addScaleBar(options = scaleBarOptions(
        maxWidth = 100,
        metric = TRUE,
        imperial = TRUE,
        updateWhenIdle = TRUE
      )) |>
      addCircles(
        ## data = filtered_data(),
        ## data =  df1,
        data =  df_sf,
        color = "black",
              stroke = TRUE,
              ## fillColor = ~ pal(Values),
            weight =  1,
              radius = 100,
        fillOpacity = 1,
        group = "Sites"
      ) |>
      addLayersControl(
        ## baseGroups = c("Carto"),
        overlayGroups = c("Zones", "Sites", "Basemap"),
        options = layersControlOptions(collapsed = FALSE)
      )
    ## addControl(position = "topleft")
  })
  ## observeEvent(input$var_selector, updateLeafletProxy(output$eda_map))
# Update markers based on selectInput
  observeEvent(c(input$var_selector, input$value_type_selector, input$year_selector), {

    df1 <- df_sf |> filter(
      Var == input$var_selector,
      Year_cal == input$year_selector,
      Value_type == input$value_type_selector
    )
    lf <- leafletProxy("eda_map_map") |>
      clearGroup("Sites") |>
      removeControl(layerId = "Legend")
    if (!all(is.na(df1$Values))) {
      pal <- colorNumeric(
        palette = "Blues",
        domain = df1$Values
      )
      lf <- lf |>
        addCircles(
          data = df1,
          color = "black",
          stroke = TRUE,
          fillColor = ~ pal(Values),
          weight =  1,
          radius = 100,
          fillOpacity = 1,
          group = "Sites"
        ) |>
        addLegend(
          data =  df1,
          position = "bottomright", pal = pal, values = ~Values,
          title = paste0(input$var_selector,
            ifelse(input$value_type_selector == "Unstandardised", "", "*")),
          ## labFormat = labelFormat(prefix = "$"),
          opacity = 1,
          layerId = "Legend"
        )
    }
    lf
  })

  toggle_buttons(status_$status, stage =  4, bttn1 = "runEDACode", bttn2 = NULL)
})


