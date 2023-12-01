##' Process data module
##'
##' Process data module:
##' - apply LORs
##' - pivot the data longer
##' - join in the metadata
##' - collate the data from across the multiple sheets and years
##' - incorporate the spatial fields
##' - tidy field names
##' - perform standardisations
##' - create a site lookup
##' @title Process data module 
##' @return NULL 
##' @author Murray Logan
##' @export
module_process_data <- function() {
  status::status_set_stage(stage = 3, title = "Process data")

  ## Retrieve the data from primary data
  raw_data <- retrieve_data(file = paste0(data_path, "primary/raw_data.RData"))

  ## Apply LoRs
  data.list <- apply_LoRs(raw_data)

  ## Lengthen data
  data.list <- pivot_data(data.list)

  ## Join in metadata
  data.list <- join_metadata(data.list)

  ## Collate all the data together
  data <- collate_data(data.list)

  ## Add spatial information (based on Latitude and Longitude)
  data <- incorporate_spatial_data(data)

  ## Tidy up fields
  ## ## Only keep
  ## ## - Site = Baseline_site
  ## ## - Var
  ## ## - values
  ## ## - Latitute
  ## ## - Longitude
  ## ## - Year,
  ## ## - Acquire_date
  ## ## - Baseline
  ## ## - ZoneName
  ## ## - RegionName
  data <- tidy_fields(data)

  ## Apply standardisation rules
  data <- apply_standardisation_rules(data)
  saveRDS(data, file = paste0(data_path, "processed/data.RData"))

  data.spatial <- readRDS(file = paste0(data_path, "processed/data.spatial.RData"))
  site_lookup <- create_site_lookup(data)
  saveRDS(site_lookup, file = paste0(data_path, "processed/site_lookup.RData"))
}

##' Retrieve data
##'
##' Retrieve data
##' @title Retrieve data 
##' @param file a character string representing the path of the data file 
##' @return data a list of data
##' @author Murray Logan
retrieve_data <- function(file) {
  status::status_try_catch(
  {
    data <- readRDS(file = file)
    data
  },
  stage_ = 3,
  name_ = "Retrieve data",
  item_ = "retrieve_data"
  )
}

##' Apply the LORs
##'
##' Apply the LORs
##' @title Apply LORs 
##' @param raw_data a list of data
##' @return a list of data 
##' @author Murray Logan
apply_LoRs <- function(raw_data) {
  status::status_try_catch(
  {
    lor <- function(x) {
      xx <- as.numeric(str_replace(x, "^<\\s?(.*)", "\\1"))
      xx[grepl("^<.*", x)] <- xx[grepl("^<.*", x)]/2
      xx
    }
    dt <- c("metals", "hydrocarbons")
    df <- lapply(raw_data, function(df) {
      df[dt] <- lapply(dt, function(x) {
        if (x == "metals") {
          tmplt <- "^[A-Z][a-z]?\\s\\(mg/kg\\)"
        } else {
          tmplt <- "^>C[0-9].*"
        }
        df[[x]] |>
          ## mutate(LORs_flag = ifelse(str_detect("<"))) |>
          mutate(across(matches(tmplt), ~ {
            lor(.x)
          }))
      })
      df
    })
  },
  stage_ = 3,
  name_ = "Apply LoRs",
  item_ = "apply_lors"
  )
}

##' Pivot data
##'
##' Pivot data into longer data
##' @title Pivot data 
##' @param lst a list of data 
##' @return a list of pivotted data 
##' @author Murray Logan
pivot_data <- function(lst) {
  status::status_try_catch(
  {
    dt <- c("metals", "hydrocarbons", "total_carbons")
    df <- lapply(lst, function(df) {
      df[dt] <- lapply(dt, function(y) {
        tmplt <- switch(y,
                "metals" = "^[A-Z][a-z]?\\s\\(mg/kg\\)",
                "hydrocarbons" = "^>C[0-9].*",
                "total_carbons" = "TOC.*"
        )
        df[[y]] <- df[[y]] |>
          pivot_longer(
            cols = matches(tmplt),
            names_to = "Var",
            values_to = "values"
          )
        df[[y]]
      })
      df
    })
    df
  },
  stage_ = 3,
  name_ = "Pivot data",
  item_ = "pivot_data"
  )
}

##' Join metadata into data
##'
##' Join metadata into data
##' @title Join metadata
##' @param lst a list of data 
##' @return a list of data 
##' @author Murray Logan
join_metadata <- function(lst) {
  status::status_try_catch(
  {
    dt <- c("metals", "hydrocarbons", "total_carbons")
    df <- lapply(lst, function(df) {
            df[dt] <- lapply(dt, function(y) {
                    nm1 <- names(df[[y]])
                    nm2 <- names(df[["metadata"]])
                    nm <- nm1[nm1 %in% nm2]
                    if (nrow(df[[y]]) > 0) {
                            df[[y]] <- df[[y]] |>
                                    dplyr::left_join(df[["metadata"]], by = nm)
                    }
                    df[[y]]
            })
            df
    })
    df
  },
  stage_ = 3,
  name_ = "Join metadata",
  item_ = "join_metadata"
  )
}

##' Collate data
##'
##' Collate data across sheets and then across sources
##' @title Collate data
##' @param data.lst a list of data
##' @return a collated data 
##' @author Murray Logan
##' @import lubridate
collate_data <- function(data.lst) {
  status::status_try_catch(
  {
    ## First collate metals, hydrocarbons and TOC within a dataset
    dt <- c("metals", "hydrocarbons", "total_carbons")
    data_comp <- lapply(data.lst, function(df) {
      wch <- sapply(df[dt], function(x) nrow(x) > 0)
      df[dt[wch]] <- lapply(dt[wch], function(y) {
        df[[y]] |>
          mutate(Type = y)
      })
      do.call("bind_rows", df[dt[wch]])
    })
    ## data <- do.call("bind_rows", lapply(data.lst, function(x) x$data)) |>
    data <- do.call("bind_rows", data_comp) |>
      ## dplyr::select(-Sampler, -Location, -Boat, -Replicates, -Notes) |>
      dplyr::select(-Sampler, -Notes) |>
      mutate(Year_cal = year(Acquire_date_time)) |>
      mutate(Year_fiscal = floor(lubridate::quarter(Acquire_date_time, fiscal_start = 7, with_year = TRUE))) |>
      mutate(Year_water = floor(lubridate::quarter(Acquire_date_time, fiscal_start = 10, with_year = TRUE))) |>
      mutate(Year = Year_cal) |>
      mutate(Baseline = ifelse(Baseline_acquire_date_time == Acquire_date_time, TRUE, FALSE))
    data
  },
  stage_ = 3,
  name_ = "Collate data",
  item_ = "collate_data"
  )
}

##' Incorporate spatial data
##'
##' Incorporate spatial data
##' @title Incorporate spatial data
##' @param df a collated dataset 
##' @param spatial a sf object
##' @return a tibble 
##' @author Murray Logan
incorporate_spatial_data <- function(df, spatial) {
  status::status_try_catch(
  {
    spatial <- readRDS(file = paste0(data_path, "primary/spatial.RData"))
    spatial_lookup <- readRDS(file = paste0(data_path, "primary/spatial_lookup.RData"))

    df |>
      filter(!is.na(Longitude), !is.na(Latitude)) |>
      sf::st_as_sf(coords = c("Longitude", "Latitude"),
                   remove = FALSE,
                   crs = st_crs(4326)) |>
      st_transform(crs = st_crs(spatial)) |>
          sf::st_intersection(spatial) |>
      dplyr::rename(ZoneName = Zone_Name) |>
      left_join(spatial_lookup, by = "ZoneName") |>
      dplyr::select(-OBJECTID) ->
      df
    df.spatial <- df
    saveRDS(df.spatial, file = paste0(data_path, "processed/data.spatial.RData"))
    df |> sf::st_drop_geometry()
  },
  stage_ = 3,
  name_ = "Incorporate spatial data",
  item_ = "spatial_data"
  )
}

##' Tidy fields
##'
##' Tidy fields
##' Site is the definitive key used to identify sites.
##' Since the sites regularly changed names over the first few years,
##' the only way to get a key is to assign the Baseline_site as the site.
##' This is because each item in the metadata has both the name of
##' the site (Site_ID) and the name of the site when it was first sampled (Baseline_site)
##' @title Tidy fields 
##' @param df a collated dataset 
##' @return a tibble 
##' @author Murray Logan
tidy_fields <- function(df) {
  status::status_try_catch(
  {
    df |>
      mutate(Site = Baseline_site)
  },
  stage_ = 3,
  name_ = "tidy data",
  item_ = "tidy_data"
  )
}

##' Apply standardisation rules
##'
##' Apply standardisation rules
##' @title Apply standardisation rules 
##' @param df a dataset 
##' @return a tibble 
##' @author Murray Logan
apply_standardisation_rules <- function(df) {
  status::status_try_catch(
  {
    df |>
      mutate(Type = ifelse(Type == "metals", "metals", "hydrocarbons")) |>
      nest_by(Type, .keep = TRUE) |>
      mutate(data1 = list({
        standardise_data(data)
      })) |>
      pull(data1) |>
      bind_rows()
  },
  stage_ = 3,
  name_ = "Standardise data",
  item_ = "standardise_data"
  )
}

##' Standardise data
##'
##' Standardise data
##' @title Standardise data
##' @param df 
##' @return a tibble 
##' @author Murray Logan
standardise_data <- function(df) {
  if (unique(df$Type) == "metals") {
    standardise_metals(df) 
  } else {
    standardise_hydrocarbons(df) 
  }
}

##' Standardise metals
##'
##' Standardise metals
##' @title Standardise metals 
##' @param df 
##' @return a tibble 
##' @author Murray Logan
standardise_metals <- function(df) {
    df |>
      pivot_wider(
        id_cols = everything(),
        names_from = Var,
        values_from = values
      ) |>
      ## start with Ag, Co, Cu, Hg, Ni, Pb and Zn
      ## - if Fe/Al < 1.3, val*50000/Fe
      ## - if Fe/Al > 1.3, val*20000/Al
      mutate(`Fe/Al` = `Fe (mg/kg)` / `Al (mg/kg)`,
        Fe_Al_normalisation = ifelse(`Fe/Al` < 1.3, 'Al', 'Fe')) |>
      ## Determine the most recent Normalisation group - use this for normalising
      group_by(Site_ID) |>
      mutate(Normalised_against = Fe_Al_normalisation[which.max(Acquire_date_time)]) |>
      ungroup() |>
      ## Flag sites in which the normalisation group has changed (different from most recent)
      ## Although this is being applied to the entire row, it is not relevant to V and hydrocarbons
      mutate(Normalisation_flag = ifelse(Normalised_against == Fe_Al_normalisation,
        FALSE,
        TRUE)) |>
      mutate(across(
        c(
          `Ag (mg/kg)`,
          `Co (mg/kg)`,
          `Cu (mg/kg)`,
          `Hg (mg/kg)`,
          `Ni (mg/kg)`,
          `Pb (mg/kg)`,
          `Zn (mg/kg)`
        ),
        list(n = ~ ifelse(Normalised_against == "Al",
          . * 50000/`Al (mg/kg)`,
          . * 20000/`Fe (mg/kg)`
        )),
        .names = "{.fn}_{.col}"
      )) |>
      ## Vanadium
      ## val*50000/Fe
      mutate(`n_V (mg/kg)` = `V (mg/kg)` * 50000 / `Fe (mg/kg)`) |>
      ## Arsenic
      ## - for outer harbour - use ratio of As to Mn
      ## - for inner (middle and upper) - use val*50000/Fe
      ## - also include a version that is always val*5000/Fe
      mutate(
        `n_As (mg/kg)` = ifelse(RegionName == "Outer",
          `As (mg/kg)` / `Mn (mg/kg)`,
          `As (mg/kg)` * 50000 / `Fe (mg/kg)`
        ),
        `n2_As (mg/kg)` = `As (mg/kg)` * 50000 / `Fe (mg/kg)`
      ) |>
      ## Now pivot longer again, but put the standardised values in a separate field
      pivot_longer(
        cols = matches("[A-Z][a-z]?\\s\\(mg/kg\\)"),
        names_to = "Var",
        values_to = "Values"
      ) |>
      mutate(Value_type = case_when(
        str_detect(Var, "n2_") ~ "Alt_standardised",
        str_detect(Var, "n_") ~ "Standardised",
        .default = "Unstandardised"),
        Var = str_replace(Var, "n2?_","")) |>
      ## pivot_wider(names_from = Norm, values_from =  Values) |>
      mutate(Normalised_against = ifelse(!Var %in%
                                         c(
                                           "Ag (mg/kg)",
                                           "Co (mg/kg)",
                                           "Cu (mg/kg)",
                                           "Hg (mg/kg)",
                                           "Ni (mg/kg)",
                                           "Pb (mg/kg)",
                                           "Zn (mg/kg)",
                                           "As (mg/kg)"
                                         ) | Value_type == "Unstandardised",
                                         NA, Normalised_against),
             Normalised_against = ifelse(Var == "V (mg/kg)" & Value_type == "Standardised",
                                         "Fe", Normalised_against),
             Normalised_against = ifelse(Var %in% "As (mg/kg)" & RegionName == "Outer" & Value_type == "Stanardised",
                                         "Mn",
                                  ifelse(Var %in% "As (mg/kg)" & RegionName == "Inner" & Value_type == "Standardised",
                                         "Fe", Normalised_against)),
             Normalised_against = ifelse(Var == "As (mg/kg)" & Value_type == "Alt_standardised",
                                         "Fe", Normalised_against),
             Normalisation_flag = ifelse(!Var %in%
                                         c(
                                           "Ag (mg/kg)",
                                           "Co (mg/kg)",
                                           "Cu (mg/kg)",
                                           "Hg (mg/kg)",
                                           "Ni (mg/kg)",
                                           "Pb (mg/kg)",
                                           "Zn (mg/kg)"
                                         ) | Value_type == "Unstandardised",
                                         NA, Normalisation_flag)
             )
}

##' Standardise hydrocarbons
##'
##' Standardise hydrocarbons
##' @title Standardise hydrocarbons
##' @param df 
##' @return a tibble 
##' @author Murray Logan
standardise_hydrocarbons <- function(df) {
    df |>
      pivot_wider(
        id_cols = everything(),
        names_from = Var,
        values_from = values
      ) |>
      mutate(across(
              any_of(
                      c(
                              ">C10 _C16 (mg/kg)",
                              ">C16 _C34 (mg/kg)",
                              ">C34 _C40 (mg/kg)",
                              ">C10_C40 (mg/kg)"
                      )
              ),
              list(n = ~ . / `TOC (%)`)
      )) |>
      ## Now pivot longer again, but put the standardised values in a separate field
      pivot_longer(
              cols = matches("^>C[0-9].*|TOC\\s\\(%\\)"),
              names_to = "Var",
              values_to = "Values"
      ) |>
      mutate(Value_type = case_when(
              str_detect(Var, "n2_") ~ "Alt_standardised",
              str_detect(Var, "n_") ~ "Standardised",
              .default = "Unstandardised"
      ))
}

##' Create site lookup
##'
##' Createt site lookup
##' @title Create site lookup 
##' @param df 
##' @return a tibble 
##' @author Murray Logan
create_site_lookup <- function(df) {
  status::status_try_catch(
  {
    df |>
      dplyr::select(Site, Site_ID, Acquire_date_time) |>
      distinct() |>
      group_by(Site) |>
      summarise(
        First_name = Site_ID[which.min(Acquire_date_time)],
        Last_name = Site_ID[which.max(Acquire_date_time)],
        Versions = n()
      ) |>
      ungroup()
  },
  stage_ = 3,
  name_ = "Create site lookup",
  item_ = "create_site_lookup"
  )
}
