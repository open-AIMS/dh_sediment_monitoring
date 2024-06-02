source("10_load_data.R")





uploaded_files <- reactiveVal(list())
output$uploaded_files_table <- reactable::renderReactable(uploaded_files()$data)

## Trigger to run 10_load_data.R
observeEvent(input$runLoadCode, {
  ## Run the load data module
  ## - reads in the data
  ## - validates the data
  ## sedMod::module_load_data()
  module_load_data()
  
  raw_data <- readRDS(file = paste0(data_path, "primary/raw_data.RData"))
  raw_data_validation <- readRDS(file = paste0(data_path, "primary/raw_data_validation.RData"))
  uploaded_files(list(data = raw_data_to_reactable(raw_data, raw_data_validation)))

  toggle_buttons(status_$status, stage =  1, bttn1 = "runLoadCode", bttn2 = "runProcessCode")
  
  shinyjs::enable(selector = "a[data-value='data']")
  addCssClass(selector = "a[data-value='data']", class = "activeLink")
})


## Make a trigger associated with clicking rows of the uploaded data table
observeEvent(input[["rowClicked"]], {
  raw_data <- readRDS(file = paste0(data_path, "primary/raw_data.RData"))
  raw_data_validation <- readRDS(file = paste0(data_path, "primary/raw_data_validation.RData"))
  ## cat("Run the function for row ", input[["rowClicked"]], "\n")
  output$Sheet_data <- reactable::renderReactable({
    dv <- raw_data_validation |>
      enframe(name = "File") |>
      mutate(data = map(.x = value, .f = ~ enframe(.x, name = "Sheet"))) |>
      dplyr::select(-value) |>
      unnest(data) |>
      dplyr::select(-value) |>
      slice(input[["rowClicked"]])
    ## Add a dynamic downloadHandler()
    output$download_raw_data <- downloadHandler(
      filename = function() {
        # Use the selected dataset as the suggested file name
        paste0("raw_data", "_", df$File, "_", df$Sheet, ".csv")
      },
      content = function(file) {
        # Write the dataset to the `file` that will be downloaded
        write.csv(raw_data[[dv$File]][[dv$Sheet]], file)
      }
    )
    ## Generate the table
    raw_data[[dv$File]][[dv$Sheet]] |>
      mutate(across(contains("(mg/kg)"), function(x) {
        ifelse(str_detect(x, "<|^$"), x, round(as.numeric(x),3))
      })) |>
      reactable(
        compact = TRUE, bordered = TRUE,
        defaultColDef = colDef(format = colFormat(digits = 3)),
        theme = reactableTheme(
          headerStyle = list(color = "white", backgroundColor = "rgb(81, 127, 185)"),
          borderWidth = "1pt",
          borderColor = "rgb(85, 85, 85)",
          style = list(fontFamily = "Helvetica, Arial, sans-serif", fontSize = "12px")
        )
      )
  })

  output$Sheet_issues <- reactable::renderReactable({
    dv <-
            raw_data_validation |>
            enframe(name = "File") |>
            mutate(data = map(.x = value, .f = ~ enframe(.x, name = "Sheet"))) |>
            dplyr::select(-value) |>
            unnest(data) |>
            ## slice(3) |>
            slice(input[["rowClicked"]]) 
    
    ## Add a dynamic downloadHandler()
    output$download_issues_data <- downloadHandler(
      filename = function() {
        # Use the selected dataset as the suggested file name
        paste0("issues_data", "_", df$File, "_", df$Sheet, ".csv")
      },
      content = function(file) {
        # Write the dataset to the `file` that will be downloaded
        write.csv(raw_data[[dv$File]][[dv$Sheet]], file)
      }
    )
    ## Generate the table
    dv |>  dplyr::select(value) |>
      unnest(cols = c(value)) |>
      dplyr::select(df) |>
      unnest(cols = c(df)) |>
      dplyr::select(description, severity, everything(), -name, -value, -expression) |>
      reactable(
        compact = TRUE, bordered = TRUE,
        theme = reactableTheme(
          headerStyle = list(color = "white", backgroundColor = "rgb(81, 127, 185)"),
          borderWidth = "1pt",
          borderColor = "rgb(85, 85, 85)",
          style = list(fontFamily = "Helvetica, Arial, sans-serif", fontSize = "12px")
        )
      )
  })
})


raw_data_to_reactable <- function(lst, lst.valid) {
        raw_data_to_info_dataframe(lst, lst.valid) |>
                data_frame_to_reactable()
}

raw_data_to_info_dataframe <- function(lst, lst.valid) {
  filenames <- paste0(input_path, names(lst)) 
  ## get the parent files
  file_info <- file.info(filenames) |>
    as.data.frame() |>
    rownames_to_column(var = "File") |>
    mutate(File = basename(File)) |>
    dplyr::select(File, Size = size, Time = mtime) #|>
    #mutate(Sheet = "")
  ## patterns <- c("^[dD]ata$|LoRs|[Mm]etadata|notes|Notes")
  ## Get patterns from global env

  children_info <-
    enframe(lst, name = "File") |>
    mutate(data = map(.x = value, .f = ~ enframe(.x, name = "Sheet"))) |>
    dplyr::select(-value) |>
    unnest(data) |>
    filter(str_detect(Sheet, paste(patterns, collapse = "|"))) |>
    dplyr::select(-value)
  info <-
          file_info |>
          full_join(children_info) |>
          full_join(get_validation_info(lst.valid)) |>
    arrange(File) 
  info
}

data_frame_to_reactable <- function(df) {
  df |>
          dplyr::select(-value) |>
          reactable(
            groupBy = "File",
            resizable = TRUE,
            compact =  TRUE,
            ## selection = "single",
            columns = list(
                    File = colDef(minWidth = 1),
                    Size = colDef(aggregate = "max", minWidth = 1),
                    Time = colDef(aggregate = "max", minWidth = 1),
                    Sheet = colDef(aggregate = "unique", minWidth = 1),
                    Status = colDef(
                      minWidth = 1,
                      html =  TRUE,
                            cell = function(value) status_symbol(value),
                            aggregate =  aggregate_table(Status)#,
                    )
            ),
            bordered =  TRUE,
            theme = reactableTheme(
                    headerStyle = list(color = "white", backgroundColor = "rgb(81, 127, 185)"),
                    borderWidth = "1pt",
                    borderColor =  "rgb(85, 85, 85)",
                    style = list(fontFamily = "Helvetica, Arial, sans-serif", fontSize = "14px")
            ),
            onClick = JS("function(rowInfo, column) {
          Shiny.setInputValue('rowClicked', rowInfo.index + 1, { priority: 'event' })
        }"
        )
        )
}


status_symbol <- function(status) {
  print(status)
  if (status) {
    tagAppendAttributes(shiny::icon("circle-check"), style = paste("color: green; font-weight:900;"))
  } else {
    tagAppendAttributes(shiny::icon("circle-xmark"), style = paste("color: red; font-weight:900;"))
  }
}

aggregate_table <- function(x) {
  JS(paste0(
          "function(values) {
//console.log(values.toString())
s = values.toString().includes('false')
//console.log(s)
//console.log(s == true)
//console.log(s == 'true')
//if any of the values are false (e.g. failed)
if (s == true) {  // at least one failure
//console.log('At least one fail')
return('<i class=\"jstree-icon jstree-themeicon far fa-circle-xmark jstree-themeicon-custom\" role = \"presentation\", style = \"color: red;\"></>')
} else {  // no failures
//console.log('No fails')
return('<i class=\"jstree-icon jstree-themeicon far fa-circle-check jstree-themeicon-custom\" role = \"presentation\", style = \"color: green;\"></>')
}
}"
  ))

}
