eda_tab <- tabItem(
  tabName = "eda",
  tabsetPanel(
    id = "eda_tabs",
    tabPanel(
      title = "Temporal",
      icon = icon("database"),
      id = "eda_temporal_tab",
      uiOutput("eda_temporal")
      ## plotOutput("eda_temporal", height = "750px", width = "600px")
      ## tabsetPanel(
      ##   id = "eda_temporal_tabs",
      ##   type = "pills"
      ## )

  ##     reactableOutput("uploaded_files_table"),
  ##     tabsetPanel(
  ##       type = "pills",
  ##       tabPanel(
  ##         title = "Raw data",
  ##         reactableOutput("Sheet_data")
  ##       ),
  ##       tabPanel(
  ##         title = "Validation issues",
  ##         reactableOutput("Sheet_issues")
  ##       )
  ##     )
  ##   ),
  ##   tabPanel(
  ##     title = "Processed data",
  ##     icon = icon("table"),
  ##     id = "processed_data_tab",
  ##     reactableOutput("Processed_data")
    )
  )
)
