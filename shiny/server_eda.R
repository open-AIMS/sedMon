source("30_eda.R")


## Trigger to run 20_process_data.R
observeEvent(input$runEDACode, {
  sedMod::module_eda()
  
  data <- readRDS(file = paste0(data_path, "processed/data.RData"))
  ## Temporal EDA
  plots <- sedMod::eda_temporal(data)
  output$eda_temporal <- renderUI({
    thetabs <- lapply(
      seq_along(plots$Initial_quarter),
      function(x) {
        nm <- paste0("eda_temporal_plot_", x)
        verticalTabPanel(
          box_height = "80px",
          title = HTML(paste0("Sampling semester:<br>", plots$Initial_quarter[x])),
          fillRow(
            flex = NA,
            tags$div(plotOutput(nm, height = "750px", width = "900px"), style = "margin-right:50px"),
            box(
              class =  "caption-box",
              status = "info",
              width = 1,
              solidHeader =  TRUE,
              textOutput(paste0(nm, "_caption"))
            )
          )
        )
      }
    )
    do.call(verticalTabsetPanel, thetabs)
  }
  )
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

  
  ## Type Temporal EDA
  plots_type <- sedMod::eda_type_temporal(data)
  output$eda_type_temporal <- renderUI({
    thetabs <- lapply(1:nrow(plots_type),
      function(x) {
        nm <- paste0("eda_type_temporal_plot_", x)
        verticalTabPanel(
          box_height = "80px",
          title = HTML(paste0(plots_type$ZoneName[x], ":<br>", plots_type$Type[x])),
          fillRow(
            flex = NA,
                  tags$div(plotOutput(nm, height = "750px", width = "1000px"), style = "margin-right:50px"),
                  box(
              class =  "caption-box",
              status = "info",
              width = 1,
              solidHeader =  TRUE,
                          textOutput(paste0(nm, "_caption"))
                  )
          )
        )
      })
    do.call(verticalTabsetPanel, thetabs)
  })
  observe({
    lapply(1:nrow(plots_type), function(x) {
      output[[paste0("eda_type_temporal_plot_", x)]] <- renderPlot({
              plots_type[x, "Plot"][[1]][[1]]
      })
      output[[paste0("eda_type_temporal_plot_", x, "_caption")]] <- renderText(
        paste0(
          "The figure to the left depicts the temporal sampling design
focussing on each of the ", plots_type$Type[x], " within the ",
plots_type$ZoneName[x], " \"Sites\". The y-axis (rows) represent the
Sampling sites (based on the Site names
they were first assigned).  Blue points represent the samples collected
that are considered \"Baseline\" or \"Reference\" samples from which
subsequent samples at the corresponding site are gauged.  Red points
represent non-\"Baseline\" samples. "
)
      )
    })
  })
})


