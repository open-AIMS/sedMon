##' EDA module
##'
##' EDA module
##' @title EDA module
##' @return NULL
##' @author Murray Logan
##' @export
module_eda <- function() {
        status::status_set_stage(stage = 4, title = "Exploratory data analysis")

        ## Retrieve the data from primary data
        data <- retrieve_processed_data(file = paste0(data_path, "processed/data.RData"))
}


##' Retrieve processed data
##'
##' Retrieve processed data from the file (data_path and filename)
##' @title Retrieve processed data 
##' @param file 
##' @return dataframe or tibble 
##' @author Murray Logan
retrieve_processed_data <- function(file) {
  status::status_try_catch(
  {
    data <- readRDS(file = file)
    data
  },
  stage_ = 4,
  name_ = "Retrieve data",
  item_ = "retrieve_data"
  )
}

##' EDA temporal
##'
##' EDA temporal
##' @title EDA temporal
##' @param data 
##' @return nested tibble containing plots 
##' @author Murray Logan
##' @export
eda_temporal <- function(data) {
    p <- 
      data |>
      mutate(Semester = lubridate::semester(Acquire_date_time, with_year = TRUE)) |>
      mutate(Date_min = min(Acquire_date_time),
        Date_max = max(Acquire_date_time)) |>
      group_by(Site) |>
      mutate(Initial_quarter = min(Semester)) |>
      ungroup() |>
      nest_by(Initial_quarter, .keep = TRUE) |>
      mutate(Plot = list({
        data |>
                ggplot(aes(y = Site, x = Acquire_date_time)) +
                geom_line() +
                geom_point(aes(colour = Baseline)) +
                coord_cartesian(xlim = c(unique(data$Date_min), unique(data$Date_max))) +
                facet_grid(RegionName + ZoneName ~ ., scales = "free_y", space = "free_y") +
                #theme_bw() +
                theme(
                  axis.title.x = element_blank(),
                  strip.text.y = element_text(angle = 0, size = rel(1.25)),
                  strip.background = element_rect(fill = NA, colour = "black"),
                  ## axis.line = element_line()
                  panel.border = element_rect(fill = NA)
                )
      }))
    p
}


##' EDA temporal specific to each data type
##'
##' EDA temporal specific to each data type
##' @title EDA type temporal
##' @param data 
##' @return nested tibble containing plots 
##' @author Murray Logan
##' @export
eda_type_temporal <- function(data) {
 p <- 
      data |>
      mutate(Semester = lubridate::semester(Acquire_date_time, with_year = TRUE)) |>
      mutate(Date_min = min(Acquire_date_time),
        Date_max = max(Acquire_date_time)) |>
      group_by(Site) |>
      mutate(Initial_quarter = min(Semester)) |>
      ungroup() |>
      nest_by(ZoneName, Type, .keep = TRUE) |>
      mutate(Plot = list({
        data |>
                ggplot(aes(y = Site, x = Acquire_date_time)) +
                ## geom_line(color = "gray50") +
                geom_point(aes(colour = Baseline), fill = NA, shape = 16) +
                coord_cartesian(xlim = c(unique(data$Date_min), unique(data$Date_max))) +
                facet_grid(. ~ Var, scales = "free_y", space = "free_y") +
                theme(
                  axis.title.x = element_blank(),
                  axis.text.x = element_text(angle = 90, vjust = 0.5),
                  strip.text.y = element_text(angle = 0, size = rel(1.25)),
                  strip.background = element_rect(fill = NA, colour = "black"),
                  panel.border = element_rect(fill = NA)
                )
      }))
    p
}
