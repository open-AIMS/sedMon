source("20_process_data.R")


## Trigger to run 20_process_data.R
observeEvent(input$runProcessCode, {
  sedMod::module_process_data()
  
  data <- readRDS(file = paste0(data_path, "processed/data.RData"))
  output$Processed_data <- reactable::renderReactable({
    dv <- data |>
      reactable(
        compact = TRUE, bordered = TRUE,
        theme = reactableTheme(
          headerStyle = list(color = "white", backgroundColor = "rgb(81, 127, 185)"),
          borderWidth = "1pt",
          borderColor = "rgb(85, 85, 85)",
          style = list(fontFamily = "Helvetica, Arial, sans-serif")
        )
      )
  })
})


