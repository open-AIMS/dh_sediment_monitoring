## csvDownloadButton <- function(id, filename = "data.csv", label = "Download as CSV") {
##   tags$button(
##     tagList(icon("download"), label),
##     onclick = sprintf("Reactable.downloadDataCSV('%s', '%s')", id, filename)
##   )
## }


data_tab <- tabItem(
  tabName = "data",
  fluidPage(  ## This is necessary to allow content that is longer than the screen to stretch the size of the container
    ## verticalTabsetPanel(
    tabsetPanel(
      id = "data_tabs",
      tabPanel(
        title = "Raw data",
        icon = icon("database"),
        id = "raw_data_tab",
        reactableOutput("uploaded_files_table"),
        tabsetPanel(
          type = "pills",
          tabPanel(
            title = "Instructions",
            box(
              title = span(icon("info", style = "margin-right: 10px;"), "Instructions"),
              width = 6,
              solidHeader = TRUE,
              status = "info",
              htmltools::includeMarkdown("../md/data_raw_instructions.md")
            ),
            box(
              title = span(icon("info", style = "margin-right: 10px;"), "Data requirements"),
              width = 6,
              solidHeader = TRUE,
              status = "info",
              htmltools::includeMarkdown("../md/raw_data.md")
            ),
            ),
          tabPanel(
            title = "Data",
            reactableOutput("Sheet_data"),
            downloadButton("download_raw_data", "Download as csv")
          ),
          tabPanel(
            title = "Validation issues",
            reactableOutput("Sheet_issues"),
            downloadButton("download_issues_data", "Download as csv")
          )
        )
      ),
      tabPanel(
        title = "Processed data",
        icon = icon("table"),
        id = "processed_data_tab",
        box(
          width =  12,
          reactableOutput("Processed_data"),
          downloadButton("download_processed_data", "Download as csv"),
          ),
        ## csvDownloadButton("cars_table", filename = "cars.csv")
        box(
          title = span(icon("info", style = "margin-right: 10px;"), "Instructions"),
          width = 6,
          solidHeader = TRUE,
          status = "info",
          htmltools::includeMarkdown("../md/processed_data_instructions.md")
        ),
        )
    )
  )
)

