##' A wrapper for a set of functions that should be run at the start
##'
##' This wrapper controls the following:
##' - set the initial stage (Stage 1)
##' - initialises the status list
##' - loads the necessary packages
##' - retrieve any settings from CLA or other
##' - defines the paths used across the analyses
##' - clear any transient data from various paths
##' - ensure all necessary paths are present
##' @title Start matter 
##' @param args optional arguments supplied to establish settings
##' @return NULL 
##' @author Murray Logan
start_matter <- function(args = commandArgs()) {
  status::status_set_stage(stage = 1, title = "Prepare environment")   ## set the analysis stage
  initialize()                                                         ## create the status list
  load_packages()                                                      ## load required packages
  get_settings(args)                                                   ## get settings (either from CLA or shiny)
  define_paths()                                                       ## define the location of paths/files
  ## MMP_parseCLA(args)                                                ## parse command line arguments
  if (get_current_stage() == 1) {
    ## clear data and outputs from previous runs
    cleanse_paths(
      paths = c("data_path", "output_path"),
      files = list.files(
        path = docs_path,
        pattern = ".*(.qmd|.html|.md)",
        full.names = TRUE
      )
    )
    prepare_paths()                                                    ## prepare file structure
  }
}
