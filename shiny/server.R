
server <- function(input, output, session) {
  sedMod::start_matter()
  source("../shiny/server_status.R", local = TRUE)
  source("../shiny/server_raw_data.R", local = TRUE)
  source("../shiny/server_processed_data.R", local = TRUE)
  source("../shiny/server_eda.R", local = TRUE)
}
