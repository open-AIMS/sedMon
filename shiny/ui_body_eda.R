tag_styles <- tags$style(HTML(
  "
.vrtc-tab-panel-menu {
width: 10%;
}

div.vrtc-tab-panel-container {
margin-top: 0px;
}

.list-group-item {
 padding: 0px 15px;
}

## .caption-box {
## margin-left: 80px;
## }
.caption-box {
border: 1px solid black;
padding: 10px;
}
"
)) 

eda_tab <- tabItem(
  tabName = "eda",
  tag_styles,
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
    ),
    tabPanel(
      title = "Spatial Type",
      icon = icon("map"),
      id = "eda_type_spatial_tab",
      ## uiOutput("eda_type_spatial")
      selectInput("var_selector", "Select Variable:", choices = unique(df_sf$Var)),
      selectInput("value_type_selector", "Select value type:", choices = unique(df_sf$Value_type)),
      sliderInput("year_selector", "Select Year:", min = min(df_sf$Year_cal), max = max(df_sf$Year_cal), value = max(df_sf$Year_cal)),
      leafletOutput("eda_map", width = "600px", height = "600px")
    )
  )
)
