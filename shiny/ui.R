source("../shiny/ui_header.R")
source("../shiny/ui_sidebar.R")
source("../shiny/ui_body.R")


tag_styles <- function() {
 tags$style(HTML(
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
    "
  )) 
}

ui <- dashboardPage(
        header = header,
        ## Sidebar
        sidebar = sidebar,
        ## Body
        body = body,
        tag_styles()
)

