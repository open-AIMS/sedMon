sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Settings", tabName = "settings", icon = icon("sliders")),
    menuItem("Data", tabName = "data", icon = icon("file-excel")),
    menuItem("Analysis", tabName = "analysis", icon = icon("calculator"))
  )
)
