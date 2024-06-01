source("startup_functions.R")

server <- function(input, output, session) {
  ## sedMod::start_matter()
  start_matter()
  source("shiny/global.R", local = TRUE)
  source("shiny/server_sidebar.R", local = TRUE)
  source("shiny/server_landing.R", local = TRUE)
  source("shiny/server_status.R", local = TRUE)
  source("shiny/server_raw_data.R", local = TRUE)
  source("shiny/server_processed_data.R", local = TRUE)
  source("shiny/server_eda.R", local = TRUE)
  source("shiny/server_analysis.R", local = TRUE)
  ## source("shiny/test.R", local = TRUE)
}
