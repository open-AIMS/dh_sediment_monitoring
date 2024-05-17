source("20_process_data.R")


## Trigger to run 20_process_data.R
observeEvent(input$runProcessCode, {
  ## sedMod::module_process_data()
  module_process_data()
  
  data <- readRDS(file = paste0(data_path, "processed/data.RData"))
  output$Processed_data <- reactable::renderReactable({
    dv <- data |>
      reactable(
        compact = TRUE, bordered = TRUE, resizable = TRUE,
        highlight = TRUE,
        wrap = FALSE,
        filterable = TRUE,
                defaultColDef = colDef(filterMethod = JS("function(rows, columnId, filterValue) {
        return rows.filter(function(row) {
          return row.values[columnId].indexOf(filterValue) !== -1
        })
      }")),
          ##pagination = FALSE, height = 600,
        ## defaultColDef = colDef(style = "white-space: nowrap;"),
        theme = reactableTheme(
          headerStyle = list(color = "white", backgroundColor = "rgb(81, 127, 185)"),
          borderWidth = "1pt",
          borderColor = "rgb(85, 85, 85)",
          style = list(fontFamily = "Helvetica, Arial, sans-serif", fontSize = "10px")
        )
      )
  })

  toggle_buttons(status_$status, stage =  3, bttn1 = "runProcessCode", bttn2 = "runEDACode")
})



output$download_processed_data <- downloadHandler(
  filename = function() {
    # Use the selected dataset as the suggested file name
    paste0("data", ".csv")
  },
  content = function(file) {
    data <- readRDS(file = paste0(data_path, "processed/data.RData"))
    # Write the dataset to the `file` that will be downloaded
    write.csv(data, file)
  }
)
