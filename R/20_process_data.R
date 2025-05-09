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

  ## Construct a sample key from the SiteID and features of the Sample_ID
  data.list <- make_sample_key(data.list)
  
  ## Collate all the data together
  data <- collate_data(data.list) |>
    mutate(Type = ifelse(Type == "mercury", "metals", Type))

  ## Add spatial information (based on Latitude and Longitude)
  data <- incorporate_spatial_data(data)

  ## Tidy up fields
  ## ## Only keep
  ## ## - Site = if IBSM_site is blank, then Site_ID otherwise IBSM_site (used to be Baseline_site)
  ## ## - Var
  ## ## - values
  ## ## - lor_flag
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
    lor_value <- status_$settings$lor$item
    lor <- function(x) {
      xx <- as.numeric(str_replace(x, "^<\\s?(.*)", "\\1"))
      xx[grepl("^<.*", x)] <- xx[grepl("^<.*", x)]/lor_value
      xx
    }
    dt <- c("metals", "hydrocarbons", "mercury")
    df <- lapply(raw_data, function(df) {
      df[dt] <- lapply(dt, function(x) {
        if (x == "metals") {
          tmplt <- "^[A-Z][a-z]?\\s\\(mg/kg\\)$"
        } else if (x == "mercury") {
          tmplt <- "^[A-Z][a-z]?\\s\\(mg/kg\\)$"
        } else {
          tmplt <- "^>C[0-9].*[^l][^o][^r]$"
        }
        df[[x]] |>
          ## mutate(LORs_flag = ifelse(str_detect("<"))) |>
          mutate(across(matches(tmplt),
            list(lor = function(x) ifelse(str_detect(x, "<") & lor_value == 1,
              TRUE, FALSE)))) |> 
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

## apply_LoRs_old <- function(raw_data) {
##     lor <- function(x) {
##       xx <- as.numeric(str_replace(x, "^<\\s?(.*)", "\\1"))
##       xx[grepl("^<.*", x)] <- xx[grepl("^<.*", x)]/2
##       xx
##     }
##     dt <- c("metals", "hydrocarbons")
##     df <- lapply(raw_data, function(df) {
##       df[dt] <- lapply(dt, function(x) {
##         if (x == "metals") {
##           tmplt <- "^[A-Z][a-z]?\\s\\(mg/kg\\)"
##         } else {
##           tmplt <- "^>C[0-9].*"
##         }
##         df[[x]] |>
##           ## mutate(LORs_flag = ifelse(str_detect("<"))) |>
##           mutate(across(matches(tmplt), ~ {
##             lor(.x)
##           }))
##       })
##       df
##     })
## }

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
    dt <- c("metals", "hydrocarbons", "total_carbons", "mercury")
    df <- lapply(lst, function(df) {
      df[dt] <- lapply(dt, function(y) {
        tmplt <- switch(y,
          "metals" = "^[A-Z][a-z]?\\s\\(mg/kg\\)$",
          "mercury" = "^[A-Z][a-z]?\\s\\(mg/kg\\)$",
          "hydrocarbons" = "^>C[0-9].*[^l][^o][^r]$",
          "total_carbons" = "TOC.*"
        )
        dfa <- df[[y]] |>
          dplyr::select(-all_of(matches("_lor"))) |>
          pivot_longer(
            cols = matches(tmplt),
            names_to = "Var",
            values_to = "values"
          )
        if (y == "total_carbons") {
          dfb <- dfa |> dplyr::select(-values) |> mutate(lor_flag = FALSE)
        } else {
          dfb <- df[[y]] |>
            dplyr::select(-all_of(matches(tmplt))) |> 
            rename_with(function(x) str_replace(x, "_lor", "")) |> 
            pivot_longer(
              cols = matches(tmplt),
              names_to = "Var",
              values_to = "lor_flag"
            )
        }
        df[[y]] <- dfa |> full_join(dfb, by = c("Sample_ID", "Var"))
        ## df[[y]] <- df[[y]] |>
        ##   pivot_longer(
        ##     cols = matches(tmplt),
        ##     names_to = "Var",
        ##     values_to = "values"
        ##   )
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
    dt <- c("metals", "hydrocarbons", "total_carbons", "mercury")
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

## Make sample key
make_sample_key <- function(data.lst) {
  status::status_try_catch(
  {
    dt <- c("metals", "hydrocarbons", "total_carbons", "mercury")
    lapply(data.lst, function(df) {
      wch <- sapply(df[dt], function(x) nrow(x) > 0)
      df[dt[wch]] <- lapply(dt[wch], function(y) {
        df[[y]] |>
                mutate(Sample_key = paste0(
                  Site_ID, Acquire_date_time,
                  str_replace_na(str_extract(Sample_ID, "[rR][eE][pP]\\s?[0-9]?"), ""),
                  str_replace_na(str_extract(Sample_ID, "[dD][uU][pP]\\s?[0-9]?"), "")
                )) 
      })
      df
    })
  },
  stage_ = 3,
  name_ = "Make sample key",
  item_ = "make_key"
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
    dt <- c("metals", "hydrocarbons", "total_carbons", "mercury")
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
            mutate(Baseline = ifelse(Baseline_acquire_date_time == Acquire_date_time, TRUE, FALSE)) |>
            mutate(Replicate_flag = ifelse(str_detect(Sample_key, "rep"), TRUE, FALSE)) |> 
            mutate(Duplicate_flag = ifelse(str_detect(Sample_key, "dup|DUP"), TRUE, FALSE)) 
    ## Unfortunately, the hydrocarbons and total_carbons (TOC) have
    ## different Sample_ID (and Origianal_SampleID) for the same
    ## location/time. This will cause downstream issues. To fix this,
    ## I will split off the total_carbons, reduce the number of fields
    ## and then join them back.
    ## data <- data |>
    ##         mutate(
    ##                 Sample_ID = str_replace(Sample_ID, "-(TOC|H)", ""),
    ##                 Original_SampleID = str_replace(Original_SampleID, "-(TOC|H)", "")
    ##         )
    
    ## data_toc <- data |>
    ##         filter(Type == "total_carbons") |>
    ##         dplyr::select(Sample_ID, Original_SampleID, Site_ID, Acquire_date_time, Var, values) |>
    ##         filter(Var == "TOC (%)") |>
    ##         mutate(Type = "hydrocarbons") |>
    ##         mutate(
    ##                 Sample_ID = str_replace(Sample_ID, "-TOC", ""),
    ##                 Original_SampleID = str_replace(Original_SampleID, "-TOC", "")
    ##         ) |>
    ##         pivot_wider(id_cols = everything(), names_from = Var, values_from = values)
    ## data_hydro <- data |>
    ##         filter(Type == "hydrocarbons") |>
    ##         mutate(
    ##                 Sample_ID = str_replace(Sample_ID, "-H", ""),
    ##                 Original_SampleID = str_replace(Original_SampleID, "-H", "")
    ##         ) |>
    ##         pivot_wider(id_cols = everything(), names_from = Var, values_from = values)
    ## data_metals <- data |>
    ##   filter(Type == "metals")
    ## data_hydro |>
    ##   full_join(data_toc, by = c(
    ##     "Sample_ID", "Site_ID",
    ##     "Original_SampleID", "Acquire_date_time", "Type",
    ##     "Var"
    ##   ))
    ## data_other <- data |>
    ##   filter(Type != "total_carbons")
    ## data1 <- data_other |>
    ##         full_join(data_toc, by = c("Site_ID", "Acquire_date_time", "Type", "Var"))
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

    spatial_areas <- spatial |>
      group_by(Zone_Name) |>
      mutate(area = st_area(geometry)) |>
      st_drop_geometry() |>
      dplyr::select(Zone_Name, area) |>
      group_by(Zone_Name) |>
      summarise(area = sum(area))
    saveRDS(spatial_areas, file = paste0(data_path, "processed/spatial_areas.RData"))

    df |>
      filter(!is.na(Longitude), !is.na(Latitude)) |>
      sf::st_as_sf(coords = c("Longitude", "Latitude"),
        remove = FALSE,
        crs = st_crs(4326)) |>
      st_transform(crs = st_crs(spatial)) |>
      ## sf::st_intersection(spatial) |>  ## turns out some of the sampling sites are outside of the spatial objects!
      sf::st_join(spatial, join = st_nearest_feature) |>
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
            ## mutate(Site = Baseline_site) |>
            mutate(Site = ifelse(is.na(IBSM_site), Site_ID, IBSM_site)) |>
            dplyr::select(-Sample_ID, -Original_SampleID)
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
        standardise_data(data)## |> filter(!is.na(Site))
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
    ## df <- adjust_names_for_replicates_and_duplicates(df) 
    standardise_hydrocarbons(df)
  }
}


##' adjust_names_to_replicates_and_duplicate
##'
##' adjust_names_to_replicates_and_duplicates
##' @title Adjust the names of Sample_key for replicates and duplicates
##' @param df
##' @return a tibble
##' @author Murray Logan
## There are instances where there are replicate and duplicate hydrocarbon samples
## however, in many instances, there are no replicates or duplicates for the TOC
## As a result, the Sample_key is different for the TOC and the hydrocarbons
## and this causes issues when trying to join hydrocarbons and the TOC (which is
## essentially done via a pivot wider.
## For instances in which there are replicates or duplicates in the non TOC,
## the TOC values are duplicated.  This is achieved by:
## - filtering the data to only include the non TOC values
## - if there are replicates or duplicates, then the Sample_keys are extracted
## - the data is pivotted wider
## - the TOC values are filled down
## - the data is pivotted longer
## - the Sample_keys are filtered to only include the Replicated (or duplicated)
##   Sample_keys that have replicates or duplicates
## - the data is joined back to the original data
adjust_names_for_replicates_and_duplicates <- function(df) {
  df1 <- df |>
    nest(data = everything(), .by = c(Site_ID, Year)) |>
    mutate(data1 = map(
      .x = data,
      .f = ~ {
        .x_hydrocarbons <- .x |> filter(Var != "TOC (%)")
        .x_toc <- .x |> filter(Var == "TOC (%)")
        if ((any(.x_hydrocarbons$Replicate_flag) & !any(.x_toc$Replicate_flag)) |
              (any(.x_hydrocarbons$Duplicate_flag) & !any(.x_toc$Duplicate_flag))) {
          sample_keys_hydrocarbons <- .x_hydrocarbons |>
            pull(Sample_key) |>
            unique()
          sample_keys_toc <- .x_toc |>
            pull(Sample_key) |>
            unique()
          ## Extract a pair of Sample keys with and without the replicate/duplicate tags
          temp <- .x_hydrocarbons |>
            mutate(temp_sample_key = str_extract(Sample_key, sample_keys_toc)) |>
            dplyr::select(Sample_key, temp_sample_key) |>
            distinct() |>
            dplyr::rename(ghost_sample_key = Sample_key) |>
            dplyr::rename(old_Sample_key = temp_sample_key)
          ## use this to full join onto the TOC data in order to replicate the TOC values
          ## a number of times that is equal to the number of replicates/duplicates
          .x_toc <- .x_toc |>
            full_join(temp, by = c(Sample_key = "old_Sample_key")) |>
            dplyr::rename(Old_Sample_key = Sample_key) |>
            dplyr::rename(Sample_key = ghost_sample_key) 
          if (any(is.na(.x_toc$Var))) print(.x_toc |> as.data.frame())
          ## Join and return the hydrocarbons and TOC data back together
          bind_rows(.x_hydrocarbons, .x_toc)
        } else {
          return(.x)
        }
      }
    )) |>
    dplyr::select(data1) |>
    unnest(data1) |>
    ungroup()
  df1
}


##' Rolling mean
##'
##' Rolling mean
##' @title Rolling mean 
##' @param x
##' @param window the rolling mean window
##' @return a vector 
##' @author Murray Logan
rolling_mean <- function(x, window) {
  sapply(seq_along(x), function(i) {
    start <- max(1, i - window + 1)  # Ensure we don't go below index 1
    mean(x[start:i])  # Compute mean on available values
  })
}

##' Standardise metals
##'
##' Standardise metals
##' @title Standardise metals 
##' @param df 
##' @return a tibble 
##' @author Murray Logan
standardise_metals <- function(df) {
  ## We need to take the lor flags out while we perform the standardisations
  ## These will be put back in via a join after the standardisations.
  df_old <- df |> dplyr::select(Var, lor_flag, Sample_key,
    Baseline_site, Baseline_acquire_date_time,
      Baseline, Replicate_flag, Duplicate_flag)
  df1 <- df |>
    ## dplyr::select(-lor_flag) |> 
    dplyr::select(-lor_flag, -Baseline_site, -Baseline_acquire_date_time,
      -Baseline, -Replicate_flag, -Duplicate_flag) |> 
    pivot_wider(
      id_cols = everything(),
      names_from = Var,
      values_from = values
    ) |>
    ## Start by calculating the Fe/Al ratio per sample
    ## start with Ag, Co, Cu, Hg, Ni, Pb and Zn
    ## - if Fe/Al < 1.3, val*50000/Fe
    ## - if Fe/Al > 1.3, val*20000/Al
    mutate(`Fe/Al` = `Fe (mg/kg)` / `Al (mg/kg)`,
      Fe_Al_normalisation = ifelse(`Fe/Al` < 1.3, 'Al', 'Fe')) |>
    ## Now calculate the average ratio over the last 5 years (as a
    ## rolling mean
    group_by(Site) |>
    mutate(
      ## `Original_Fe/Al` = `Fe/Al`,
      `Fe/Al` = rolling_mean(`Fe/Al`, window = 5),
      Fe_Al_normalisation = ifelse(`Fe/Al` < 1.3, 'Al', 'Fe'),
      Normalised_against = Fe_Al_normalisation
    ) |>
    ungroup() |>
    ## OLD:Determine the most recent Normalisation group - use this
    ## for normalising
    ## NEW:Determine the most common Normalisation group - use this
    ## for all (if there is a tie, Al wins)
    ## group_by(Site_ID) |>
    group_by(Site) |>
    ## mutate(Normalised_against = Fe_Al_normalisation[which.max(Acquire_date_time)]) |>
    mutate(Normalised_against = names(which.max(table(Fe_Al_normalisation)))) |>
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
    ## val*20000/Fe
    mutate(`n_V (mg/kg)` = `V (mg/kg)` * 20000 / `Fe (mg/kg)`) |>
    ## Arsenic
    ## - for outer harbour - use ratio of As to Mn
    ## - for inner (middle and upper) - use val*20000/Fe
    ## - also include a version that is always val*2000/Fe
    mutate(
      `n_As (mg/kg)` = ifelse(RegionName == "Outer",
        `As (mg/kg)` / `Mn (mg/kg)`,
        `As (mg/kg)` * 20000 / `Fe (mg/kg)`
      ),
      `n2_As (mg/kg)` = `As (mg/kg)` * 20000 / `Fe (mg/kg)`
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
      Normalised_against = ifelse(Var %in% "As (mg/kg)" & RegionName == "Outer" & Value_type == "Standardised",
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
    ) |>
    filter(!is.na(Values)) |>
    droplevels()

  df1 |> full_join(df_old, by = c("Sample_key", "Var")) |>
    filter(!(is.na(Latitude) & is.na(Longitude) & is.na(Values))) |>
    droplevels()
}

##' Standardise hydrocarbons
##'
##' Standardise hydrocarbons
##' @title Standardise hydrocarbons
##' @param df 
##' @return a tibble 
##' @author Murray Logan
standardise_hydrocarbons <- function(df) {
  df_hydrocarbons <- df |>
    filter(Var != "TOC (%)") |>
    mutate(temp_Sample_key = str_replace(Sample_key, "rep.", ""))

  df_toc <- df |> filter(Var == "TOC (%)")

  ## Join the hydrocarbons with the TOC
  df_join <-
    df_hydrocarbons |>
    left_join(df_toc |> dplyr::select(Sample_key, `TOC (%)` = values),
      by = c("Sample_key" = "Sample_key"))
  ## Extrat the ones whose Sample_keys matched
  df_match <- df_join |> 
    filter(!is.na(`TOC (%)`)) 
  ## Now for ones where the Sample_keys did not match,
  ## we will try to match them by a hydrocarbon Sample_key that does
  ## not have  "rep." 
  df_no_match <- df_join |>
    filter(is.na(`TOC (%)`)) |>
    dplyr::select(-`TOC (%)`) |>
    left_join(df_toc |> dplyr::select(Sample_key, `TOC (%)` = values),
      by = c("temp_Sample_key" = "Sample_key")) 
  ## Combine the two
  df1 <- bind_rows(df_match, df_no_match, df_toc) 

  df1 <- df1 |>
    mutate(Standardised = values / `TOC (%)`) |>
    dplyr::select(-`TOC (%)`) |>
    dplyr::rename("Unstandardised" = values) |>
    pivot_longer(
      cols = c("Unstandardised", "Standardised"),
      names_to = "Value_type",
      values_to = "Values"
    ) |>
    filter(!(is.na(Values) & Value_type == "Standardised"))
 df1 
}
## standardise_hydrocarbons_old <- function(df) {
##   df_old <- df |> dplyr::select(Var, lor_flag, Sample_key)
##   df1 <- df |>
##     dplyr::select(-lor_flag) |> 
##     pivot_wider(
##       id_cols = everything(),
##       names_from = Var,
##       values_from = values
##     ) |>
##     mutate(across(
##       any_of(
##         c(
##           ">C10 _C16 (mg/kg)",
##           ">C16 _C34 (mg/kg)",
##           ">C34 _C40 (mg/kg)",
##           ">C10_C40 (mg/kg)"
##         )
##       ),
##       list(n = ~ . / `TOC (%)`),
##       .names = "{.fn}_{.col}"
##     )) |>
##     ## Now pivot longer again, but put the standardised values in a separate field
##     pivot_longer(
##       cols = matches("^(n2?_)?>C[0-9].*|TOC\\s\\(%\\)"),
##       names_to = "Var",
##       values_to = "Values"
##     ) |>
##     mutate(
##             Value_type = case_when(
##                     str_detect(Var, "n2_") ~ "Alt_standardised",
##                     str_detect(Var, "n_") ~ "Standardised",
##                     .default = "Unstandardised"
##             ),
##             Var = str_replace(Var, "n2?_", "")
##     ) |>
##     filter(!is.na(Values))    #Remove these cases - they are caused by the hydrocarbons having replicates and the TOC not having replicates - so the Sample_key are different
##   df1 |> full_join(df_old, by = c("Sample_key", "Var")) 
## }

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
      ## summarise(
      reframe(
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
