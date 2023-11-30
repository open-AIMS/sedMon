data_tab <- tabItem(
  tabName = "data",
  ## verticalTabsetPanel(
  tabsetPanel(
    id = "data_tabs",
    tabPanel(
      title = "Raw data",
      icon = icon("database"),
      id = "raw_data_tab",
      reactableOutput("uploaded_files_table"),
      tabsetPanel(
        type = "pills",
        tabPanel(
          title = "Raw data",
          reactableOutput("Sheet_data")
        ),
        tabPanel(
          title = "Validation issues",
          reactableOutput("Sheet_issues")
        )
      )
    ),
    tabPanel(
      title = "Processed data",
      icon = icon("table"),
      id = "processed_data_tab",
      reactableOutput("Processed_data")
    )
  )
)
