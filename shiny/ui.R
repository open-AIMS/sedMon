source("../shiny/ui_header.R")
source("../shiny/ui_sidebar.R")
source("../shiny/ui_body.R")



ui <- dashboardPage(
        header = header,
        ## Sidebar
        sidebar = sidebar,
        ## Body
        body = body
)
