dashboard_tab <- tabItem(
  tabName = "dashboard",
  fluidRow(
    h2("Dashboard", style = "margin-left: 15px;"),
    box(
      title = "Progress",
      width = 6,
      solidHeader = TRUE,
      status = "primary",
      tabsetPanel(
        id = "status_tabs",
        tabPanel(
          title = "Interactive",
          box(
            title = "Status ",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "success",
            shinyTree("tree", theme = "default", checkbox = FALSE),
          ),
          box(
            title = "Logs ",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "success",
            verbatimTextOutput("log_output")
          )
        ),
        tabPanel(
          title = "Terminal-like",
          box(
            title = "Progress",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "success",
            htmlOutput("status_output"),
            ## actionButton("runLoadCode", "Run Stage 2"),
            ## actionButton("runProcessCode", "Run the process data code"),
            ## actionButton("runEDACode", "Run the EDA data code")
          )
        )
      )
    ),
    
    box(
      title = span(icon("info", style = "margin-right: 10px;"), "Status Instructions"),
      width = 6,
      solidHeader = TRUE,
      status = "info",
      ## HTML(markdown::mark(text = "This is some **text**")),
      htmltools::includeMarkdown("../md/dashboard.md")
    )
  )
)
