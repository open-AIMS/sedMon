source("30_eda.R")


## Trigger to run 20_process_data.R
observeEvent(input$runEDACode, {
  sedMod::module_eda()
  
  data <- readRDS(file = paste0(data_path, "processed/data.RData"))

  ## Temporal EDA
  plots <- sedMod::eda_temporal(data)
  output$eda_temporal <- renderUI({
    thetabs <- lapply(seq_along(plots$Initial_quarter),
      function(x) {
        nm <- paste0("eda_temporal_plot_", x)
        tabPanel(
          title = paste0("Sampling semester: ", plots$Initial_quarter[x]),
          fillRow(
                  plotOutput(nm, height = "750px", width = "750px"),
                  box(
                          textOutput(paste0(nm, "_caption"))
                  )
          )
        )
      })
    do.call(tabsetPanel, thetabs)
  })
  observe({
    lapply(seq_along(plots$Initial_quarter), function(x) {
      output[[paste0("eda_temporal_plot_", x)]] <- renderPlot({
              plots[x, "Plot"][[1]][[1]]
      })
      output[[paste0("eda_temporal_plot_", x, "_caption")]] <- renderText(
        paste0(
          "The figure to the left depicts the temporal sampling design
focussing on \"Sites\" that were first monitored in the ", plots$Initial_quarter[x],
" semester. The y-axis (rows) represent the Sampling sites (based on the Site names
they were first assigned).  Blue points represent the samples collected
that are considered \"Baseline\" or \"Reference\" samples from which
subsequent samples at the corresponding site are gauged.  Red points
represent non-\"Baseline\" samples. Points are jointed by lines to help
identify discontinued sampling (where no line exists) and where no Baselines
have been defined (when the left point of a sequence is red)."
)
      )
    })
  })

  
})


