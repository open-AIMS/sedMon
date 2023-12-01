eda_tab <- tabItem(
  tabName = "eda",
  tabsetPanel(
    id = "eda_tabs",
    tabPanel(
      title = "Temporal",
      icon = icon("database"),
      id = "eda_temporal_tab",
      uiOutput("eda_temporal")
    ),
    tabPanel(
      title = "Temporal Type",
      icon = icon("database"),
      id = "eda_type_temporal_tab",
      uiOutput("eda_type_temporal")
    )
  )
)
