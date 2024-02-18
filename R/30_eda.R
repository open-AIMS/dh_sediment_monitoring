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
  status::status_try_catch(
  {
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
  },
  stage_ = 4,
  name_ = "EDA temporal",
  item_ = "eda_temporal"
  )
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
  status::status_try_catch(
  {
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
  },
  stage_ = 4,
  name_ = "EDA type temporal",
  item_ = "eda_type_temporal"
  )
}

##' EDA temporal specific to each site and type
##'
##' EDA temporal specific to each site and type
##' @title EDA site temporal 
##' @param data 
##' @return nested tibble
##' @author Murray Logan
##' @export
eda_site_temporal <- function(data) {
  status::status_try_catch(
  {
    data |>
      ungroup() |>
      nest_by(RegionName, ZoneName, Site, Type, Var, .keep = TRUE) 
  },
  stage_ = 4,
  name_ = "EDA site temporal",
  item_ = "eda_site_temporal"
  )
}

expanded_date_range <- function(dts) {
  dts_f <- lubridate::floor_date(dts, unit = "years") |> unique()
  dts_c <- lubridate::ceiling_date(dts, unit = "years") |> unique()
  dts <- c(dts_f, dts_c) |>
          unique() |>
          sort()
  range(dts)
}

##' EDA temporal plot specific to each site and type
##'
##' EDA temporal plot specific to each site and type
##' @title EDA plot site temporal 
##' @param data 
##' @param x_range 
##' @return single ggplot object
##' @author Murray Logan
##' @export
eda_site_temporal_plot <- function(data, x_range = NULL) {
  #print(head(as.data.frame(data)))
  if (is.null(x_range)) x_range <- expanded_date_range(data$Acquire_date_time)
  nYrs <- lubridate::interval(x_range[1], x_range[2])/years(1) #length(unique(data$Year))
  break_str <- case_when(
    nYrs < 2 ~ "6 months",
    nYrs < 10 ~ "1 years",
    nYrs <= 20 ~ "5 years", nYrs > 20 ~ "10 years"
  )
  label_str <- case_when(
          nYrs <= 2 ~ "%b %Y",
          nYrs > 2 ~ "%Y"
  )
  # determine stripes every alternate year
  dts <- scales::breaks_width(width = "1 year")(x_range)
  ## dts_f <- lubridate::floor_date(x_range, unit = "years") |> unique()
  ## dts_c <- lubridate::ceiling_date(x_range, unit = "years") |> unique()
  ## dts <- c(dts_f, dts_c) |>
  ##         unique() |>
  ##         sort()
  odds <- seq(1, length(dts), by = 2)
  if (max(odds) == length(dts)) odds <- odds[-length(odds)]
  dts_strt <-  dts[odds]
  dts_end <- dts[odds + 1]
  ## dts_end <- replace_na(dts[odds + 1], dts[max(odds)] + years(1))
  y_range <- range(scales::breaks_pretty(n = 2)(data$Values))

  dd <- data.frame(dts_strt, dts_end)

  nSites <- length(unique(data$Site))
  g1 <- data |>
    ggplot(aes(
      y = Values, x = Acquire_date_time, colour = Baseline,
      text = paste0(
        "Site: ", Site,
        "\nReport year: ", Year,
        "\nFe/Al:", round(`Fe/Al`, 3),
        "\nNormalised against: ", Normalised_against,
        "\nNormalisation changed: ", Normalisation_flag,
        "\nRepicate: ", Replicate_flag,
        "\nDuplicate: ", Duplicate_flag
      )
    )) +
    geom_rect(data = dd, aes(y = NULL, x = NULL, text = NULL, colour=NULL, xmin = dts_strt, xmax = dts_end),
      ymin = y_range[1], ymax = y_range[2], fill = "grey", alpha = 0.35) +
    geom_point() +
    scale_y_continuous(unique(data$Var)) +
    scale_x_datetime("", date_breaks = break_str, date_labels = label_str, limits = x_range) +
    #          scale_x_datetime(date_breaks = "1 yr") +
    scale_colour_discrete(limits = c(FALSE, TRUE)) +
    #facet_wrap(~Value_type, scales = "free") +
    theme(strip.background = element_rect(color = "black"))

  if (nSites == 1) {
    g1 + ggtitle(paste0("Site:", unique(data$Site), ", Type: ", unique(data$Type), ", Var: ", unique(data$Var))) +
      facet_wrap(~Value_type, scales = "free") 
  } else {  ## for the multi-site version on the spatial trend EDA tab
    g1 + #ggtitle(paste0("Type: ", unique(data$Type), ", Var: ", unique(data$Var))) +
      theme_classic() +
      ggtitle("")+
      theme(
        #legend.position = "bottom", legend.direction = "horizontal",
        text = element_text(size = 8)
        ## strip.background = element_blank(),
        #title = element_blank()
        ## strip.text = element_blank()
      ) +
      geom_line(aes(group = Site, y = Values, x = Acquire_date_time),
        inherit.aes = FALSE, alpha = 0.05
      )
  }
}

##' EDA temporal plot specific to each site and type
##'
##' EDA temporal plot specific to each site and type
##' @title EDA plot site temporal 
##' @param data 
##' @return nested tibble containing plots
##' @author Murray Logan
##' @export
eda_site_temporal_plots <- function(data) {
  ## status::status_try_catch(
  ## {
    p <-
      data |>
      mutate(Plot = list({
        nYrs <- length(unique(data$Year))
        break_str <- case_when(
          nYrs < 2 ~ "6 months",
          nYrs < 10 ~ "1 years",
          nYrs <= 20 ~ "5 years", nYrs > 20 ~ "10 years"
        )
        label_str <- case_when(
          nYrs <= 2 ~ "%b %Y",
          nYrs > 2 ~ "%Y"
        )
        data |>
                ggplot(aes(y = Values, x = Acquire_date_time, colour = Baseline)) +
          geom_point() +
          scale_y_continuous(unique(Var)) +
          scale_x_datetime("", date_breaks = break_str, date_labels = label_str) +
          #          scale_x_datetime(date_breaks = "1 yr") +
          facet_wrap(~Value_type) +
          ggtitle(paste0("Site:", unique(Site), ", Type: ", unique(Type), ", Var: ", unique(Var)))
      }))
    p
  ## },
  ## stage_ = 4,
  ## name_ = "EDA site temporal plots",
  ## item_ = "eda_site_temporal_plots"
  ## )
}
