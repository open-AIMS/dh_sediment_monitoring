source("shiny/ui_header.R")
source("shiny/ui_sidebar.R")
source("shiny/ui_body.R")

ui <- dashboardPage(
        useShinyjs(),              # required in order to use any shinyjs functions
        header = header,
        ## Sidebar
        sidebar = sidebar,
        ## Body
        body = body
)
