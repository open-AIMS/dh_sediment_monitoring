analysis_tab <- tabItem(
  tabName = "analysis",
  fluidPage(
    tabsetPanel(
      id = "analysis_tabs",
      tabPanel(
        title = "Analysis overview",
        icon = icon("database"),
        id = "analysis_overview_tab",
        uiOutput("analysis_overview")
        ),
      tabPanel(
        title = "Analyses details",
        icon = icon("database"),
        id = "analysis_details_tab",
        uiOutput("analysis_main")
      )
    )
    ## h2("Temporal analyses", style = "margin-left: 15px;"),
    ##   box(
    ##           class = "selector_box",
    ##           width = 9,
    ## status = "info",
    ## solidHeader = TRUE,
    ##           #    column(width = 3, selectInput("site_selector", "Select Site:", choices = unique(data$Site)))
    ##   )
  )
)
