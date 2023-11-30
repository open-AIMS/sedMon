source("../shiny/ui_body_dashboard.R")
source("../shiny/ui_body_data.R")
source("../shiny/ui_body_eda.R")


tag_styles <- tags$style(HTML(
  "
    .label-left .form-group {
      display: flex;              /* Use flexbox for positioning children */
      flex-direction: row;        /* Place children on a row (default) */
      width: 100%;                /* Set width for container */
      max-width: 400px;
    }

    .label-left .control-label {
      margin-right: 2rem;         /* Add spacing between label and slider */
      align-self: center;         /* Vertical align in center of row */
      text-align: right;
      flex-basis: 100px;          /* Target width for label */
    }

    .label-left .form-control {
      flex-basis: 300px;          /* Target width for slider */
    }

    .fa-circle-check {
      color: green; 
    }
        
    .fa-clock {
      color: orange;
    }

    .fa-circle-exclamation {
      color: orange;
    }

    .fa-circle-xmark {
      color: red;
    }

    #log_output {
      height: 200px;
      overflow-y: auto;
      display:flex;
      flex-direction: column-reverse;
    }
    "
)) 

body <- dashboardBody(
  tag_styles,
  tabItems(
    ## Settings tab
    ## settings_tab,
    ## Dashboard tab
    dashboard_tab,
    ## Data tab
    data_tab,
    ## EDA
    eda_tab
  )
)
