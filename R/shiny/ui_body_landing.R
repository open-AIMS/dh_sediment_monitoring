tag_styles <- function() {
 tags$style(HTML(
    "
    .label-left .form-group {
      display: flex;              /* Use flexbox for positioning children */
      flex-direction: row;        /* Place children on a row (default) */
      width: 100%;                /* Set width for container */
      max-width: 400px;
    }

    .label-left .control-label {
      margin-right: 2rem;         /* Add spacing between label and slider */
      align-self: center;         /* Vertical align in center of row */
      text-align: right;
      flex-basis: 100px;          /* Target width for label */
    }

    .label-left .form-control {
      flex-basis: 300px;          /* Target width for slider */
    }
    "
  )) 
}
settings_box <-
  fluidRow(
    ## box(plotOutput("plot1", height = 250)),
    ## tag_styles(),
    ##   tags$style(
    ##     HTML(".box.box-solid.box-primary>.box-header { background-color: purple; }")
    ## ),
    box(
      width = 6,
      title = "Path settings",
      solidHeader = TRUE,
      status = "success",
            textInput("settings_project_path_input",
                    label = tags$span(
                            "Project path: ",
                            popify(icon("info-circle"),
                                    title = "Project path",
                                    content = "This is the path to the project. It is being displayed here incase you need to know where all these files are stored.  <b>There is no need to alter this location</b>"
                            )
                    ),
                    width = "400px"
            ),
      ## Input data path input
      textInput("settings_input_dir_input",
        label = tags$span(
          "Data input dir: ",
          popify(icon("info-circle"),
            title = "Input data directory",
            content = "This is the path in which this app will search for input files. <b>You are welcome to change this to another location if you wish</b>."
          )
        ),
        width = "400px"),
      ## files located in the above path
      verbatimTextOutput("settings_input_dir_files",placeholder = TRUE),
      ## Data path input
      textInput("settings_data_dir_input",
        label = tags$span(
          "Data dir: ",
          popify(icon("info-circle"),
            title = "Data directory",
            content = "This is the path that this app will place intermediate data artifacts. <b>Typically, you should not need to alter this path.  However, if you have a need for any of the intermediate artifacts, you know where to find them.</b>."
          )
        ),
        width = "400px"),
      ## Params path input
      textInput("settings_params_dir_input",
        label = tags$span(
          "Parameters dir: ",
          popify(icon("info-circle"),
            title = "Parameters directory",
            content = "This is the path that this app will look to acquire any data that might be considered non-data inputs - such as settings, lookups etc."
          )
        ),
        width = "400px"),
      ## Output path input
      textInput("settings_output_dir_input",
        label = tags$span(
          "Output dir: ",
          popify(icon("info-circle"),
            title = "Output directory",
            content = "This is the path that this app will place individual figures and tables."
          )
        ),
        width = "400px"),
      ## Documents path input
      textInput("settings_docs_dir_input",
        label = tags$span(
          "Documents dir: ",
          popify(icon("info-circle"),
            title = "Documents directory",
            content = "This is the path that this app will place any document outputs (including reports)."
          )
        ),
        width = "400px")
    ),
    ## Temporary files setttings
    box(
            title = "Temporary files",
            width = 6,
            solidHeader = TRUE,
      status = "success",
            ## Status path input
            textInput("settings_status_path_input",
                    label = tags$span(
                            "Temporary path: ",
                            popify(icon("info-circle"),
                                    title = "Temporary path",
                                    content = "This is the path to the temporary directory used internally by the app to store files.  It is being displayed here incase you need to know where all these files are stored.  <b>there is no need to alter this location</b>"
                            )
                    ),
                    width = "400px"
            ),
      ## Status file input
      textInput("settings_status_file_input",
        label = tags$span(
          "Status filename: ",
          popify(icon("info-circle"),
            title = "Status file",
            content = "This is the name of file that stores the status information for this project.  It is being displayed here incase you need to know where all these files are stored.  <b>there is no need to alter this file name</b>"
          )
        ),
        width = "400px"),
      ## Log file input
      textInput("settings_log_file_input",
        label = tags$span(
          "Log filename: ",
          popify(icon("info-circle"),
            title = "Logfile",
            content = "This is the name of the log file that stores the logs for this project.  It is being displayed here incase you need to know where all these files are stored.  <b>there is no need to alter this file name</b>"
          )
        ),
        width = "400px")
      ),
    ## Run stages
    box(
            title = "Run settings",
            width = 6,
            solidHeader = TRUE,
      status = "success",
      ## Run stages input
      textInput("settings_run_stages_input",
        label = tags$span(
          "Run stages: ",
          popify(icon("info-circle"),
            title = "Run stages",
            content = "This indicates which stages of the analyses you wish to run.  You should provide a comma separated set of numbers.  For example, to run stages 1 through to 3, you would enter 1,2,3.  Note, run stage 1 is just setting things up."
          )
        ),
        width = "400px")
    ),
    ## valueBox(100, "Basic example"),
    ## tableOutput("mtcars")
    box(
      title = span(icon("info", style = "margin-right: 10px;"), "Settings Instructions"),
      width = 12,
      solidHeader = TRUE,
      status = "info",
      ## HTML(markdown::mark(text = "This is some **text**")),
      htmltools::includeMarkdown("../md/settings.md")
    )
  )

landing_tab <- tabItem(
  tabName = "landing",
  fluidRow(
    h2("Darwin Harbour Sediment Monitoring Analysis Platform", style = "margin-left: 15px;"),
    box(
      title = "Settings",
      width = 6,
      solidHeader = TRUE,
      status = "primary",
      p("The following settings are indicate the location and names of directories and files used for input and output by this application.  Initially, the values displayed are the defaults."),
      settings_box
    ),
    box(
      title = span(icon("info", style = "margin-right: 10px;"), "General Instructions"),
      width = 6,
      solidHeader = TRUE,
      status = "info",
      ## HTML(markdown::mark(text = "This is some **text**")),
      htmltools::includeMarkdown("../md/instructions.md")
    ),
    box(
      title = span(icon("info", style = "margin-right: 10px;"), "Data requirements"),
      width = 6,
      collapsible = TRUE,
      solidHeader = TRUE,
      status = "info",
      htmltools::includeMarkdown("../md/raw_data.md")
    ),
    )
)

