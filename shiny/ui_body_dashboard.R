dashboard_tab <- tabItem(
  tabName = "dashboard",
  fluidRow(
    h2("Dashboard"),
    box(
      title = "Box 1",
      width = 6,
      solidHeader = TRUE,
      htmlOutput("status_output"),
      actionButton("runLoadCode", "Run the load data code"),
      actionButton("runProcessCode", "Run the process data code"),
      actionButton("runEDACode", "Run the EDA data code")
    ),
    box(
            title = "Status ",
            width = 6,
            solidHeader = TRUE,
            shinyTree("tree", theme = "default", checkbox =  FALSE)
    ),
    box(
            title = "Logs ",
            width =  6,
            solidHeader = TRUE,
            verbatimTextOutput("log_output")
    )
  )
)
