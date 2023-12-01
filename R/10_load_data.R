##' Load and validate data
##'
##' Read in the data from the nominated input_path, validate the data
##' against a set of rules (manifest like) and also generate some spatial
##' artifacts
##' @title Load data module
##' @return NULL
##' @author Murray Logan
##' @export
module_load_data <- function() {
        status::status_set_stage(stage = 2, title = "Obtain data")

        raw_data <- read_input_data(input_path)
        saveRDS(raw_data, file = paste0(data_path, "primary/raw_data.RData"))

        ## Validate the data

        ## this will create a tibble (raw_data_validations) that contains items that
        ## correspond to the sheet items in raw_data
        ## Each of the items has a status field that indicates status of each rule
        ## and each rule has a df of the cases that violate the rule
        ## use these in shiny to highlight any issues
        raw_data_validation <- validate_input_data(raw_data)
        saveRDS(raw_data_validation, file = paste0(data_path, "primary/raw_data_validation.RData"))

        ## make the spatial data
        spatial <- make_spatial_data()
        saveRDS(spatial, file = paste0(data_path, "primary/spatial.RData"))

        ## make the spatial lookup
        spatial_lookup <- make_spatial_lookup()
        saveRDS(spatial_lookup, file = paste0(data_path, "primary/spatial_lookup.RData"))
}
 
##' Read in data (xlsx files) from the nominated input_path folder
##'
##' Read in all the data files and standardise (and limit to) required
##' sheets
##' @title Read input data
##' @param input_path character representing the path from which to read the input data files
##' @return nested list of data files and their sheets 
##' @author Murray Logan
##' @export
read_input_data <- function(input_path) {
  ## Get the filenames and types of any files in the `input_path` folder
  status::status_try_catch(
  {
    files <- list.files(
      path = input_path, pattern = ".*\\.csv|.*\\.xlsx",
      full.names = TRUE
    )
    file_types <- gsub(".*(xlsx|csv)$", "\\1", files)
  } ,
  stage_ = 2,
  name_ = "Read input info",
  item_ = "read_input_info"
  )
  ## Read in the input files and construct a raw data list
  status::status_try_catch(
  {
    raw_data <- vector(mode = "list", length = length(files))
    names(raw_data) <- basename(files)
    raw_data <- Map(raw_data, seq_along(raw_data), f = function(x, i) {
        if (file_types[i] == "xlsx") {
            sheet_names <- readxl::excel_sheets(files[i])
            ## Only include the sheets matching the desired patterns
            patterns <- c("^[mM]etals$|^[hH]ydrocarbons|^[tT]otal_[cC]arbons$|^[mM]etadata|^[nN]otes")
            assign("patterns", patterns, envir = .GlobalEnv)
            sheet_names <- sheet_names |> str_subset(pattern = paste(patterns, collapse = "|"))
            sheets <- lapply(sheet_names, readxl::read_excel, path = files[i])
            ## standardise sheet names
            replacements <- c("metals", "hydrocarbons", "total_carbons", "metadata", "notes")
            sheet_names <- stringr::str_replace_all(sheet_names, patterns, replacements)
            sheets <- setNames(sheets, sheet_names)
        } else if (file_types[i] == "csv") {
            sheets <- list(data = readr::read_csv(files[i]))
        }
        x <- list(
            path = files[i],
            file_type = file_types[i]
        )
        x <- append(x, sheets)
    })
  } ,
  stage_ = 2,
  name_ = "Read input data",
  item_ = "read_input_data"
  )

}

##' Validate the input data against a series of sheet specific rules
##'
##' Validate the input data against a series of sheet specific rules
##' and return a nested list that includes validation summaries for
##' each of the sheets of each dataset
##' @title Validate input data 
##' @param raw_data 
##' @return a nested list of validation summaries 
##' @author Murray Logan
##' @export
validate_input_data <- function(raw_data) {
  status::status_try_catch(
  {
    raw_data_validation <- lapply(raw_data, function(x) {
      nms <- c("metals", "hydrocarbons", "total_carbons",
        "metadata", "notes")
      data_type <- "regular"
      v <- lapply(nms, function(sheet) {
          run_validations(
            df = x[[sheet]], data_type = data_type,
            sheet_type = sheet
          )
      })
      setNames(v, nm = nms)
    })
    valid_info <- get_validation_info(raw_data_validation)
    if (any(valid_info$Status == FALSE)) {
      warning("WARNING: Raw data validation issues")
    }
    raw_data_validation
  },
  stage_ = 2,
  name_ = "Validate input data",
  item_ = "validate_input_data"
  )
}

##' Get rules for each field type
##'
##' Get the rules for each field type
##' @title Get rules 
##' @return tibble of rules 
##' @author Murray Logan
get_rules <- function() {
  rules <- tribble(
    ~applied_to, ~name, ~rule, ~severity, ~description,
    "site", "has_field", "'<NAME>' %in% names(.)", "fail", "'<NAME>' field is missing",
    "site", "class", "class(`<NAME>`) %in% c('character', 'factor')", "fail", "'<NAME>' should be a character vector",
    "batch", "has_field", "'<NAME>' %in% names(.)", "fail", "'<NAME>' field is missing",
    "batch", "class", "class(`<NAME>`) %in% c('numeric')", "fail", "'<NAME>' should be a numeric vector",
    "year", "has_field", "'<NAME>' %in% names(.)", "fail", "'<NAME>' field is missing",
    "year", "class", "class(`<NAME>`) %in% c('numeric')", "fail",  "'<NAME>' should be a numeric",
    "year", "field_format", "field_format(`<NAME>`, pattern = '[0-9]{4}', type = 'regex')", "fail",  "'<NAME>' should be four digits",
    "metal", "has_field", "'<NAME>' %in% names(.)", "warning", "'<NAME>' field is missing",
    "metal", "class", "(class(`<NAME>`) %in% c('numeric')) | (any(grepl('^<.*', `<NAME>`))) | (all(is.na(`<NAME>`)))",  "fail", "'<NAME>' must only contain numbers (unless a '<' is present)",
    "metal", "field_format", "field_format(replace_na(as.character(`<NAME>`), \"\"), pattern = '(^[0-9\\\\.]|^<\\\\s?[0-9]*\\\\.?[0-9]*$|^$)', type = 'regex')",  "fail", "'<NAME>' values can only be either numbers or a '<'",
    "hydrocarbon", "has_field", "'<NAME>' %in% names(.)", "warning", "'<NAME>' field is missing",
    "hydrocarbon", "class", "(class(`<NAME>`) %in% c('numeric')) | (any(grepl('^<.*', `<NAME>`)))",  "fail", "'<NAME>' must only contain numbers (unless a '<' is present)",
    "hydrocarbon", "field_format", "field_format(replace_na(as.character(`<NAME>`),\"\"), pattern = '([0-9\\\\.]|^<.*$|^$)', type = 'regex')",  "fail", "'<NAME>' values can only be either numbers or a '<'",
    "toc", "has_field", "'<NAME>' %in% names(.)", "warning", "'<NAME>' field is missing",
    "toc", "class", "(class(`<NAME>`) %in% c('numeric')) | (any(grepl('^<.*', `<NAME>`)))",  "fail", "'<NAME>' must only contain numbers (unless a '<' is present)",
    "toc", "field_format", "field_format(replace_na(as.character(`<NAME>`),\"\"), pattern = '([0-9\\\\.]|^<.*$|^$)', type = 'regex')",  "fail", "'<NAME>' values can only be either numbers or a '<'",
    "latitude", "has_field", "'<NAME>' %in% names(.)",  "fail", "'<NAME>' field is missing",
    "latitude", "class", "class(`<NAME>`) %in% c('numeric')",  "fail", "'<NAME>' should be a numeric",
    "latitude", "number_format", "number_format(`<NAME>`, format = '-d*.d*')",  "fail", "'<NAME>' should be in the format -d.d",
    "longitude", "has_field", "'<NAME>' %in% names(.)",  "fail", "'<NAME>' field is missing",
    "longitude", "class", "class(`<NAME>`) %in% c('numeric')",  "fail", "'<NAME>' should be a numeric",
    "longitude", "number_format", "number_format(`<NAME>`, format = 'd*.d*')",  "fail", "'<NAME>' should be in the format d.d",
    "datetime", "has_field", "'<NAME>' %in% names(.)",  "fail", "'<NAME>' field is missing",
    "datetime", "class", "any(class(`<NAME>`) %in% c('POSIXct'))",  "fail", "'<NAME>' should be a date time",
    "sampler", "has_field", "'<NAME>' %in% names(.)",  "fail", "'<NAME>' field is missing",
    "sampler", "class", "class(`<NAME>`) %in% c('character', 'factor')",  "fail", "'<NAME>' should be a character vector",
    "boat", "has_field", "'<NAME>' %in% names(.)",  "fail", "'<NAME>' field is missing",
    "boat", "class", "class(`<NAME>`) %in% c('character', 'factor')",  "fail", "'<NAME>' should be a character vector",
    "replicates", "has_field", "'<NAME>' %in% names(.)",  "fail", "'<NAME>' field is missing",
    "replicates", "class", "class(`<NAME>`) %in% c('character', 'factor')",  "fail", "'<NAME>' should be a character vector",
    "free", "has_field", "'<NAME>' %in% names(.)",  "fail", "'<NAME>' field is missing",
    )
  return(rules)
}

##' Get the rule templates
##'
##' The rule templates outline for each sheet what field types should be present
##' @title Get rule templates 
##' @param data_type 
##' @param sheet_type 
##' @return a nested list of rule templates 
##' @author Murray Logan
get_rule_templates <- function(data_type = "regular", sheet_type = "data") {
  rule_templates <- list(
      regular = list(
          metals = tribble(
              ~item, ~applied_to,
              "Sample_ID", "site",
              "Al (mg/kg)", "metal",
              "V (mg/kg)", "metal",
              "Mn (mg/kg)", "metal",
              "Fe (mg/kg)", "metal",
              "Co (mg/kg)", "metal",
              "Ni (mg/kg)", "metal",
              "Cu (mg/kg)", "metal",
              "Zn (mg/kg)", "metal",
              "As (mg/kg)", "metal",
              "Ag (mg/kg)", "metal",
              "Cd (mg/kg)", "metal",
              "Pb (mg/kg)", "metal",
              "Ca (mg/kg)", "metal",
              "Cr (mg/kg)", "metal",
              "Sb (mg/kg)", "metal",
              "Hg (mg/kg)", "metal",
          ),
          hydrocarbons = tribble(
              ~item, ~applied_to,
              "Sample_ID", "site",
              ">C10 _C16 (mg/kg)", "hydrocarbon",
              ">C16 _C34 (mg/kg)", "hydrocarbon",
              ">C34 _C40 (mg/kg)", "hydrocarbon",
              ">C10_C40 (mg/kg)", "hydrocarbon",
          ),
          total_carbons = tribble(
              ~item, ~applied_to,
              "Sample_ID", "site",
              "TOC (%)", "toc",
          ),
          ## data = tribble(
          ##     ~item, ~applied_to,
          ##     "IBSM_site", "site",
          ##     ## "Baseline_site", "site",
          ##     ## "Baseline_year", "year",
          ##     "Sample_ID", "site",
          ##     "Original_SampleID", "site",
          ##     "Al (mg/kg)", "metal",
          ##     "V (mg/kg)", "metal",
          ##     "Mn (mg/kg)", "metal",
          ##     "Fe (mg/kg)", "metal",
          ##     "Co (mg/kg)", "metal",
          ##     "Ni (mg/kg)", "metal",
          ##     "Cu (mg/kg)", "metal",
          ##     "Zn (mg/kg)", "metal",
          ##     "As (mg/kg)", "metal",
          ##     "Ag (mg/kg)", "metal",
          ##     "Cd (mg/kg)", "metal",
          ##     "Pb (mg/kg)", "metal",
          ## ),
          ## LoRs = tribble(
          ##     ~item, ~applied_to,
          ##     "Batch", "batch",
          ##     "Al (mg/kg)", "metal",
          ##     "V (mg/kg)", "metal",
          ##     "Mn (mg/kg)", "metal",
          ##     "Fe (mg/kg)", "metal",
          ##     "Co (mg/kg)", "metal",
          ##     "Ni (mg/kg)", "metal",
          ##     "Cu (mg/kg)", "metal",
          ##     "Zn (mg/kg)", "metal",
          ##     "As (mg/kg)", "metal",
          ##     "Ag (mg/kg)", "metal",
          ##     "Cd (mg/kg)", "metal",
          ##     "Pb (mg/kg)", "metal",
          ## ),
          metadata = tribble(
              ~item, ~applied_to,
              "IBSM_site", "site",
              "Site_ID", "site",
              "Sample_ID", "site",
              "Original_SampleID", "site",
              "Latitude", "latitude",
              "Longitude", "longitude",
              "Acquire_date_time", "datetime",
              "Sampler", "sampler",
              "Notes", "free",
              "Baseline_site", "site",
              "Baseline_acquire_date_time", "datetime",
          ),
          notes = tribble(
              ~item, ~applied_to,
              "Notes", "free",
          )
      ),
      baseline = list(
          data = tribble(
              ~item, ~applied_to,
              "Baseline_site", "site",
              "Baseline_SampleID", "site",
              "OriginalBaseline_SampleID", "site",
              "Al (mg/kg)", "metal",
              "V (mg/kg)", "metal",
              "Mn (mg/kg)", "metal",
              "Fe (mg/kg)", "metal",
              "Co (mg/kg)", "metal",
              "Ni (mg/kg)", "metal",
              "Cu (mg/kg)", "metal",
              "Zn (mg/kg)", "metal",
              "As (mg/kg)", "metal",
              "Ag (mg/kg)", "metal",
              "Cd (mg/kg)", "metal",
              "Pb (mg/kg)", "metal",
              "Ca (mg/kg)", "metal",
              "Cr (mg/kg)", "metal",
              "Sb (mg/kg)", "metal",
              "Hg (mg/kg)", "metal",
          ),
          LoRs = tribble(
              ~item, ~applied_to,
              "Batch", "batch",
              "Al (mg/kg)", "metal",
              "V (mg/kg)", "metal",
              "Mn (mg/kg)", "metal",
              "Fe (mg/kg)", "metal",
              "Co (mg/kg)", "metal",
              "Ni (mg/kg)", "metal",
              "Cu (mg/kg)", "metal",
              "Zn (mg/kg)", "metal",
              "As (mg/kg)", "metal",
              "Ag (mg/kg)", "metal",
              "Cd (mg/kg)", "metal",
              "Pb (mg/kg)", "metal",
              "Ca (mg/kg)", "metal",
              "Cr (mg/kg)", "metal",
              "Sb (mg/kg)", "metal",
              "Hg (mg/kg)", "metal",
          ),
          metadata = tribble(
              ~item, ~applied_to,
              "Baseline_site", "site",
              "Baseline_acquire_date_time", "datetime",
              "Baseline_SampleID", "site",
              "OriginalBaseline_SampleID", "site",
              "Latitude", "latitude",
              "Longitude", "longitude",
              "Acquire_date_time", "datetime",
              "Sampler", "sampler",
              "Notes", "free",
              "Batch", "batch",
          ),
          notes = tribble(
              ~item, ~applied_to,
              "Notes", "free",
          )
      )
  )
  return(rule_templates[[data_type]][[sheet_type]])
}


##' Run all the validations
##'
##' Run all the validations
##' @title Run validations 
##' @param df 
##' @param data_type 
##' @param sheet_type 
##' @return a tibble of validations 
##' @author Murray Logan
##' @import validate
run_validations <- function(df, data_type, sheet_type) {
  if (nrow(df) == 0) {
    df <- df |> mutate(id = n())
  } else {
    df <- df |> mutate(id = 1:n())
  }
  targets <-
          get_rule_templates(data_type, sheet_type) |>
          dplyr::nest_by(item, .key = "rule_template", .keep = TRUE) |>
          dplyr::mutate(rules = list(rule_template |>
                                  dplyr::left_join(get_rules(), by = "applied_to", relationship = "many-to-many") |>
                                  mutate(
                                          rule = stringr::str_replace_all(rule, "<NAME>", item),
                                          description = stringr::str_replace_all(description, "<NAME>", item)
                                  )
                  ),
                  validate = list(validate::validator(.data = rules)),
                  confront = list(validate::confront(df, validate, key = "id")),
                  summary = list(summary(confront))
          )
  if (nrow(df) > 0) {
    targets <- targets |>
      mutate(df = list(validate::as.data.frame(confront) |>
                 dplyr::filter(!value) |>
                 dplyr::left_join(df, by = "id") |>
                 dplyr::left_join(rules |>
                           dplyr::select(name, severity, description) |>
                           dplyr::distinct(),
                           by = "name")
      ),
      status = list({
        msg <- "success"
        if (nrow(df) > 0) {
            if (unique(df$severity) == "fail") {
                msg <- paste("Failure:", paste(unique(df$description), collapse = ", "))
            } else {
                msg <- paste("Warning:", paste(unique(df$description), collapse = ", "))
            }
        }
        msg
      })
    )
  } else {
    targets <-
      targets |>
      dplyr::mutate(
        df = list(
          df |>
          dplyr::mutate(value = FALSE, id = NA) |>
            dplyr::bind_rows(rules |>
                               dplyr::select(name, severity, expression = rule, description) |>
                               dplyr::distinct()) |>
          dplyr::select(id, name, value, expression, everything(), severity, description)
        ),
       status = list( {
         msg <- paste("Warning:", paste(unique(df$description), collapse = ", "))
         msg
       })
      )
  }
  return(targets)
}

##' Extract the validation information from the validation tibble
##'
##' Extract the validation information from the validation tibble
##' @title Get validation info from tibble 
##' @param tbl 
##' @return a tibble of validation status 
##' @author Murray Logan
get_validation_info_from_tibble <- function(tbl) {
  tbl |>
    dplyr::mutate(Status = map(
      .x = value,
      .f = ~ {
        .x |>
          dplyr::pull(status) |>
          unlist() |>
          unique() |>
          stringr::str_detect("[fF]ailure:") |>
          (\(.) all(. == FALSE))()
      }
    )) |>
    tidyr::unnest(Status)
}

##' Get validation info
##'
##' Get the validation info
##' @title Get validation info 
##' @param lst 
##' @return list 
##' @author Murray Logan
get_validation_info <- function(lst) {
  lst |>
    tibble::enframe(name = "File") |>
    dplyr::mutate(data = map(.x = value, .f = ~ tibble::enframe(.x, name = "Sheet"))) |>
    dplyr::select(-value) |>
    tidyr::unnest(data) |>
    get_validation_info_from_tibble()
}


##' Create Darwin Harbour spatial objects
##'
##' Create Darwin Harbour spatial objects from shape files located
##' in the params_path
##' @title Make spatial data 
##' @return spatial an sf object representing the zones of Darwin Harbour 
##' @author Murray Logan
##' @import sf
make_spatial_data <- function() {
  status::status_try_catch(
  {
    x <- sf::read_sf(paste0(params_path, 'GIS/MA_WA_StudyArea.shp')) |>
      dplyr::select(OBJECTID, Zone_Name)
    middle_arm <- x |> filter(Zone_Name == "Middle Arm") |>
      mutate(OBJECTID = 4)
    west_arm <- x |> filter(Zone_Name == "West Arm") |>
      mutate(OBJECTID = 7)
    central_harbour <- x |> filter(Zone_Name == "Central Harbour") |>
      mutate(OBJECTID = 3)

    x <- sf::read_sf(paste0(params_path, "GIS/East_Arm_Sediment_Sampling.shp")) |>
      st_transform(crs = st_crs(west_arm)) |>
      dplyr::select(OBJECTID, Zone_Name)

    east_arm <- x |> filter(Zone_Name == "East Arm") |>
      mutate(OBJECTID = 5)
    elizabeth <- x |> filter(Zone_Name == "Elizabeth River") |>
      mutate(OBJECTID = 6)

    x <- sf::read_sf(paste0(params_path, "GIS/OuterHarbour_EastArm_ShoalBay.shp")) |>
      dplyr::select(OBJECTID, Zone_Name)

    outer_harbour <- x |> filter(Zone_Name == "Outer Harbour") |>
      mutate(OBJECTID = 1)

    shoalbay <- x |> filter(Zone_Name == "Shoal Bay") |>
      mutate(OBJECTID = 2)

    city <- x |> filter(is.na(Zone_Name)) |>
      mutate(OBJECTID = 10)

    central_harbour <-
      city |>
      sf::st_union(central_harbour, by_feature = TRUE) |>
      dplyr::select(OBJECTID, Zone_Name) |>
      mutate(OBJECTID = 3,
             Zone_Name = 'Central Harbour')

    spatial <-
      shoalbay |>
      rbind(outer_harbour) |>
      rbind(west_arm) |>
      rbind(east_arm) |>
      rbind(elizabeth) |>
      rbind(central_harbour) |>
      rbind(middle_arm)

    ## spatial |>
    ##   st_transform(crs = st_crs(4326)) |>
    ##   ## left_join(spatial_lookup) |>
    ##   ggplot() +
    ##   geom_sf(aes(fill = Zone_Name))
    sf::write_sf(spatial,
        dsn = paste0(data_path, "primary/GIS/darwin_harbour.shp")
    )
    spatial
  } ,
  stage_ = 2,
  name_ = "Make spatial data",
  item_ = "make_spatial_data"
  )
}

##' Create a spatial lookup that relates Zones, Regions and Areas
##'
##' Create a spatial lookup that relates Zones, Regions and Areas
##' @title Make spatial lookup 
##' @return spatial_lookup a tibble 
##' @author Murray Logan
make_spatial_lookup <- function() {
  status::status_try_catch(
  {
    spatial_lookup <- tibble::tribble(
      ~Region, ~RegionName, ~Zone, ~ZoneName,           ~Area,
      1,       "Upper",     4,     "Middle Arm",       "Inner",
      1,       "Upper",     5,     "East Arm",         "Inner",
      1,       "Upper",     6,     "Elizabeth River",  "Inner",
      1,       "Upper",     7,     "West Arm",         "Inner",
      1,       "Upper",     8,     "Buffalo Creek",    "Outer",
      1,       "Upper",     9,     "Myrmidon Creek",   "Inner",
      2,       "Middle",    3,     "Central Harbour",  "Inner",
      3,       "Outer",     1,     "Outer Harbour",    "Outer",
      3,       "Outer",     2,     "Shoal Bay",        "Outer",
      )
    saveRDS(spatial_lookup, file = paste0(data_path, "processed/spatial_lookup.RData"))
    spatial_lookup
  } ,
  stage_ = 2,
  name_ = "Make spatial lookup",
  item_ = "make_spatial_lookup"
  )
}
