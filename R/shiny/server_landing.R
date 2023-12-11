
updateTextInput(session, "settings_project_path_input",
  value = dirname(getwd())
)

updateTextInput(session, "settings_status_path_input", value = status_$settings$status_dir$item)
updateTextInput(session, "settings_status_file_input", value = status_$settings$status_file$item)
updateTextInput(session, "settings_log_file_input", value = status_$settings$log_file$item)
updateTextInput(session, "settings_input_dir_input", value = status_$setting$input_path$item)
updateTextInput(session, "settings_data_dir_input", value = status_$setting$data_path$item)
updateTextInput(session, "settings_params_dir_input", value = status_$setting$params_path$item)
updateTextInput(session, "settings_output_dir_input", value = status_$setting$output_path$item)
updateTextInput(session, "settings_docs_dir_input", value = status_$setting$docs_path$item)
updateTextInput(session, "settings_run_stages_input", value = names(status_$status))


output$settings_input_dir_files <- renderText({
  files <- list.files(path = status_$settings$input_path$item, pattern = ".*xlsx|.*csv")
  stringr::str_wrap(paste0(files, collapse = "\t"), 45)
  })
