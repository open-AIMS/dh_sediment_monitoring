
##' Temporal analysis module
##'
##' Temporal analysis module
##' @title Temporal analysis module
##' @return NULL
##' @author Murray Logan
#' @export
module_temporal <- function() {
  ## Dont remove the following two lines. I dont know why, but without
  ## them, this future promise does not seem to be able to find the
  ## status or log file
  print(status_file)
  print(log_file)
  
  status::status_set_stage(stage = 5, title = "Temporal analysis")
  ## plan(multisession, workers = 4)

  ## Retrieve the data from primary data
  data <- retrieve_processed_data(file = paste0(data_path, "processed/data.RData"))

  spatial_lookup <-
    data |>
    dplyr::select(Area, RegionName, ZoneName, Site) |>
    distinct()
  saveRDS(spatial_lookup, file = paste0(data_path, "processed/spatial_lookup.RData"))

  ## Prepare data - add the replicate and duplicate stuff in to prepare_data()
  data <- data |> prepare_data()
  ## Prepare formula
  data <- data |> prepare_formula()
  ## Prepare priors
  data <- data |> prepare_priors()
  ## Prepare model template
  data <- data |> prepare_model_template()
  ## Fit models
  ## promise <- future_promise({
  ##   data <- data |> fit_models()
  ##   saveRDS(data, file = paste0(data_path, "modelled/data.RData"))
  ##   data
  ## })

  data <- data |> fit_models()
  saveRDS(data, file = paste0(data_path, "modelled/data.RData"))
  cat("====================================\n",
    file = paste0(data_path, "modelled/log_models.log"), append = TRUE
  )

  
  ##data <- readRDS(file = paste0(data_path, "modelled/data.RData"))
  ## data <- readRDS(file = paste0(data_path, "modelled/data.RData"))

  ## Validate models
  data <- validate_models(data)
  saveRDS(data, file = paste0(data_path, "modelled/data.RData"))
  cat("====================================\n",
    file = paste0(data_path, "modelled/log_models.log"), append = TRUE
  )

  ## Zone level posteriors and contrasts
  data_zone <- compile_posteriors(data, scale = "zone")
  data_zone <- collect_results(data_zone, scale = "zone")
  saveRDS(data_zone, file = paste0(data_path, "modelled/data_zone.RData"))
  data_zone <-
    data_zone |>
    mutate(scale = "zone") |>
    dplyr::select(-data, -effects) |>
    full_join(spatial_lookup |>
      dplyr::select(-Site) |>
      distinct())
  
  ## Site level effects
  data_site <- compile_posteriors(data, scale = "site")
  data_site <- collect_results(data_site, scale = "site")
  saveRDS(data_site, file = paste0(data_path, "modelled/data_site.RData"))
  data_site <-
    data_site |>
    mutate(scale = "site") |>
    dplyr::select(-data, -effects) |> 
    full_join(spatial_lookup)

  ## Put these all together into a single tibble
  data_all <-
    data_site |>
    mutate(scale = "site") |>
    dplyr::select(-fit) |>
    bind_rows(
      data_zone |>
        mutate(scale = "zone") ## |>
        ## dplyr::select(-fit)
    )
  saveRDS(data_all,
    file = paste0(data_path, "modelled/data_all.RData")
  )
  ## Pairwise tests
  ## Partial plots
  ## Caterpillar plots
  ##         get_prior(form, data = dat)
  ##         summary(mod)
  ##         conditional_effects(mod)
  ##         mod |>
  ##                 emmeans(~cYear) |>
  ##                 pairs() |>
  ##                 gather_emmeans_draws() |>
  ##                 dplyr::select(-.chain) |>
  ##                 summarise_draws(median, Pl = ~ mean(.x < 0), Pg = ~ mean(.x > 0))
  ## library(glmmTMB)
  ##         mod1 <- glmmTMB(Values ~ cYear + (1 | Site), data = dat, family = Gamma(link = "log"))
  ##         mod1 |>
  ##                 emmeans(~cYear) |>
  ##                 pairs() 
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
  stage_ = 5,
  name_ = "Retrieve data",
  item_ = "retrieve_data"
  )
}

prepare_data <- function(data) {
  status::status_try_catch(
  {
    data |>
      filter(!is.na(Values)) |>
      mutate(cYear = factor(Year)) |>
      mutate(cens_lor = ifelse(lor_flag, "left", "none")) |>
      nest(data = everything(), .by = c(ZoneName, Type, Var, Value_type)) |>
      mutate(data = map(
        .x = data,
        .f = ~ .x |> droplevels()
      ))
    #nest_by(ZoneName, Var, Value_type, .keep = TRUE) 
  },
  stage_ = 5,
  name_ = "Prepare data",
  item_ = "prepare_data"
  )
}

prepare_formula <- function(data) {
  status::status_try_catch(
  {
    data |>
      mutate(form = purrr::map(
        .x = data,
        .f = ~ {
          nYrs <- length(unique(.x$Year))
          if (nYrs > 1) {
            if (any(.x$lor_flag) & !all(.x$lor_flag)) {
              form <- bf(Values | cens(cens_lor) ~ cYear + (cYear | Site), family = Gamma(link = "log"))
            } else {
              form <- bf(Values ~ cYear + (cYear | Site), family = Gamma(link = "log"))
            }
          } else {
            if (any(.x$lor_flag) & !all(.x$lor_flag)) {
              form <- bf(Values | cens(cens_lor) ~ 1 + (1 | Site), family = Gamma(link = "log"))
            } else {
              form <- bf(Values ~ 1 + (1 | Site), family = Gamma(link = "log"))
            }
          }
          form
        }
      ))
  },
  stage_ = 5,
  name_ = "Prepare formula",
  item_ = "prepare_formula"
  )
}

prepare_priors <- function(data) {
  status::status_try_catch(
  {
    data |>
      mutate(priors = purrr::map(
        .x = data,
        .f = ~ {
          dat_sum <- .x |>
            mutate(lValues = log(Values + 0.1)) |>
            group_by(cYear) |>
            summarise(across(lValues, list(mean = mean, median = median, sd = sd, mad = mad)))
          int_mu <- round(dat_sum[1, "lValues_mean"], 2)[[1]]
          int_sd <- max(1, round(dat_sum[1, "lValues_sd"], 2)[[1]])
          sd_sd <- max(1, round(mean(dat_sum[, "lValues_sd"][[1]]), 2)[[1]])
          priors <- prior_string(paste0("student_t(3, ", int_mu, ", ", int_sd, ")"), class = "Intercept") +
            prior_string(paste0("student_t(3, 0, ", sd_sd, ")"), class = "sd", coef = "Intercept", group = "Site") +
            prior(gamma(0.01, 0.01), class = "shape")
          if (nrow(dat_sum) > 1) {
            b_sd <- max(1, round(abs(max(diff(dat_sum[, "lValues_mean"][[1]]))), 2))
            priors <- priors + prior_string(paste0("student_t(3, 0, ", b_sd, ")"), class = "b") 
          }
          priors
        }
      ))
  },
  stage_ = 5,
  name_ = "Prepare priors",
  item_ = "prepare_priors"
  )
}

prepare_model_template <- function(data) {
  status::status_try_catch(
  {
      data |>
      mutate(template = pmap(
        .l = list(data, form, priors),
        .f = ~ {
          nm <- paste0(data_path, "modelled/template.rds")
          if (!file.exists(nm)) {
            mod <- brm(..2,
              data = ..1,
              prior = ..3,
              sample_prior = "yes",
              iter = 5,
              chains = 3,
              warmup = 1,
              thin = 1,
              backend = "cmdstanr",
              refresh = 0,
              silent = 2,
              # seed =  123,
              control = list(adapt_delta = 0.95)
            ) |>
              suppressWarnings() |>
              suppressMessages()
            saveRDS(mod, file = nm)
          }
          nm
        }
      ))
  },
  stage_ = 5,
  name_ = "Prepare model template",
  item_ = "prepare_model_template"
  )
}

formula_same <- function(form1, form2) {
  f1 <- form1$formula |> as.character()
  f2 <- form2$formula |> as.character()
  fam1 <- form1$family
  fam2 <- form1$family
  ifelse(identical(f1, f2) & identical(fam1, fam2), TRUE, FALSE)
}


fit_models <- function(data) {
  status::status_try_catch(
  {
    nm_l <- paste0(data_path, "modelled/log_models.log")
    total_number_of_models <- nrow(data)
    data |>
      mutate(i = 1:n()) |> 
      mutate(fit = pmap(
        .l = list(data, form, priors, template, i),
        .f = ~ {
          l_d <- ..1
          l_f <- ..2
          l_p <- ..3
          l_t <- ..4
          i <- ..5
          nm <- paste0(
            data_path, "modelled/",
            sanitise_filename(paste0(
              "mod_", unique(l_d$ZoneName), "__",
              unique(l_d$Var), "___",
              unique(l_d$Value_type)
            ))
          )
          print(nm)
          cat(paste0(
            i, "/", total_number_of_models, " (",
            sprintf("% 3.1f%%", 100 * (i / total_number_of_models)), "): ",
            unique(l_d$ZoneName), " ", unique(l_d$Var), " (", unique(l_d$Value_type), ")"
          ), file = nm_l, append = TRUE)
          if (!file.exists(paste0(nm, ".rds"))) {
            ## Determine whether the model should be re-run (based on
            ## whether it already exists or not)
            mod_template <- readRDS(l_t)
            recom <- !formula_same(mod_template$form, l_f)
            ## print(paste("Recom: ", recom))
            ## print(l_f)
            ## print(l_p)
            ## print(l_d)

            ## utils::capture.output(
            ##   mod <- invisible(update(mod_template,
            ## mod <- update(mod_template,
            mod <- brm(form = l_f,
              ## newdata = l_d,
              data = l_d,
              prior = l_p,
              sample_prior = "yes",
              ## recompile = recom,
              ## recompile = FALSE,
              iter = 5000,
              chains = 3, cores = 3,
              warmup = 1000,
              thin = 5,
              backend = "cmdstanr",
              refresh = 0,
              silent = 2,
              file = nm,
              file_refit = "on_change",
              seed = 123,
              control = list(adapt_delta = 0.95)
              ##   ) |> suppressWarnings() |> suppressMessages()),
              ##   ## file = nullfile(),
              ##   file = nm_l,
              ##   append = TRUE
              ## )
            )
            cat("\t - model successfully fit\n", file = nm_l, append = TRUE)
          } else {
            cat("\t - loaded from previous run\n", file = nm_l, append = TRUE)
          }
          ## sink(
          ##   file = paste0(data_path, "temp.log"),
          ##   append = TRUE
          ## )
          ## cat(paste(nm, "\n"))
          ## sink()
          ## print(nm)
          ## file.copy(paste0(status_$settings$status_dir$item, "/", status_$settings$log_file$item),
          ##   paste0(data_path, "temp.log"),
          ##     overwrite = TRUE
          ##   )
          paste0(nm, ".rds")
        },
        .progress = TRUE
      ))
  },
  stage_ = 5,
  name_ = "Fit models",
  item_ = "fit_models"
  )
}



sanitise_filename <- function(nm) {
   str_replace_all(nm, "/", "\\.")
}


## Get the "data" field from the filtered nested data set
get_filtered_data <- function(data, scale, zone, var, type) {
  Scale <- scale
  if (Scale == "site") {
    data |>
      filter(
        scale == Scale,
        Site == zone,
        Var == var,
        Value_type == type
      ) |>
      droplevels()
  } else {
    data |>
      filter(
        scale == Scale,
        ZoneName == zone,
        Var == var,
        Value_type == type
      ) |>
      droplevels()##  |>
    ## pull(data) |>
    ## _[[1]]
  }
}

## Get the "fit" field from the filtered nested data set and use it to read in the model
get_filtered_model <- function(data, zone, var, type) {
  fit <- data |>
    filter(
      scale == "zone",
      ZoneName == zone,
      Var == var,
      Value_type == type
    ) |>
    droplevels() |>
    pull(fit) |>
    _[[1]]
  readRDS(file = fit)
}

make_contrasts_baseline_vs_years <- function(dat) {
  if (unique(dat$scale) == "zone") {   # zone level - these must fix the baseline to 2019-2020
    d <- dat |>
      dplyr::select(cYear, Baseline) |>
      distinct() |>
      filter(!Baseline | Baseline & cYear %in% c(2019, 2020))
  } else {  # site level - these contrasts can use the actual baselines
    d <- dat |>
      dplyr::select(cYear, Baseline) |>
      distinct() |>
      (function(df) {
        filter(df, !Baseline |
                     Baseline &
                     cYear == max(as.character(df[df$Baseline == TRUE, ]$cYear)))
      })() 
  }
  d <- d |> 
    mutate(BaselineYear = interaction(Baseline, cYear)) |>
    mutate(B = ifelse(Baseline, as.character(Baseline), as.character(cYear))) |>
    mutate(
      B = forcats::fct_relevel(B, "TRUE"),
      C = ifelse(Baseline, -1, 1)
    )
  ## Contrasts of baseline vs each other year
  m <- model.matrix(~ -1 + B, data = d)
  cmat <-
    d |>
    bind_cols(sweep(m, 2, colSums(m), "/")) |>
    mutate(across(matches("B[0-9]{4}"), function(x) x - BTRUE)) |>
    dplyr::select(matches("B[0-9]{4}")) |>
    dplyr::rename_with(function(x) str_replace(x, "B([0-9]{4})", "\\1 vs Baseline")) |> 
    as.matrix()
  ## Contrasts of most recent year and previous
  all_years <-
    d |>
    pull(cYear) |>
    as.character()
  last_years <- all_years[(length(all_years) - 1):length(all_years)]
  m1 <- data.frame(A = c(rep(0, length(all_years) - 2), -1, 1)) |>
    dplyr::rename_with(function(x) str_replace(x, "A", paste0(last_years[2], " vs ", last_years[1]))) |>
    as.matrix()
  
  cbind(cmat, m1)
}

compare_baseline_vs_years_posteriors <- function(dat, mod, nm) {
  nm_e <- str_replace(nm, "effects_", "effects_posteriors_")
  cmat <- make_contrasts_baseline_vs_years(dat)
  eff <-
    mod |>
    emmeans(~cYear) |>
    contrast(method = list(cYear = cmat)) |>
    ## pairs() |>
    gather_emmeans_draws() |>
    dplyr::select(-.chain) |>
    mutate(.value = exp(.value)) |> 
    mutate(contrast = str_replace(contrast, "cYear.", "")) 
  saveRDS(eff, file = nm_e)
  eff
}

compare_baseline_vs_years_summ <- function(dat, mod, nm) {
  compare_baseline_vs_years_posteriors(dat, mod, nm) |>
          posterior::summarise_draws(
                  median,
                  HDInterval::hdi,
                  Pl = ~ mean(.x < 1),
                  Pg = ~ mean(.x > 1)
          ) |>
          mutate(change = case_when(
            Pl > 0.95 ~ 7,
            Pl > 0.9 ~ 6,
            Pl > 0.85 ~ 5,
            Pg > 0.95 ~ 1,
            Pg > 0.9 ~ 2,
            Pg > 0.85 ~ 3,
            .default = 4
          )) |> 
    mutate(year = str_extract(contrast, "[0-9]{4}")) |> 
    dplyr::select(-variable)
}

make_contrasts_baseline_and_years <- function(dat) {
  if (unique(dat$scale) == "zone") {# zone level - these must fix the baseline to 2019-2020
    d <- dat |>
      dplyr::select(cYear, Baseline) |>
      distinct() |>
      filter(!Baseline | Baseline & cYear %in% c(2019, 2020)) |>
      mutate(BaselineYear = interaction(Baseline, cYear)) |>
      mutate(B = ifelse(Baseline, as.character(Baseline), as.character(cYear))) 
  } else {# site level - these contrasts can use the actual baselines
    d <- dat |>
      dplyr::select(cYear, Baseline) |>
      distinct() |>
      (function(df) {
        filter(df, !Baseline |
                     Baseline &
                     cYear == max(as.character(df[df$Baseline == TRUE, ]$cYear)))
      })() 
  }
  d <- d |> 
    mutate(BaselineYear = interaction(Baseline, cYear)) |>
    mutate(B = ifelse(Baseline, as.character(Baseline), as.character(cYear))) |>
    mutate(
      B = forcats::fct_relevel(B, "TRUE"),
      C = ifelse(Baseline, -1, 1)
    )
  m <- model.matrix(~ -1 + B, data = d)  # combine 2019/2020 into baseline
  cmat <-
    sweep(m, 2, colSums(m), "/") |>
    as.data.frame() |>
    dplyr::rename("Baseline" = BTRUE) |> 
    dplyr::rename_with(function(x) str_replace(x, "B([0-9]{4})", "\\1")) 
  m1 <- model.matrix(~ -1 + cYear, data = d)  # each year
  cmat1 <- m1 |>
    as.data.frame() |>
    dplyr::rename_with(function(x) str_replace(x, "cYear([0-9]{4})", "\\1"))

  wch <- colnames(cmat)[which(!colnames(cmat) %in% colnames(cmat1))]
  cmat2 <- cmat |> dplyr::select(all_of(wch)) |> cbind(cmat1)
  ## Remove any columns that sum to zero - since these years dont exist
  cmat2[, colSums(cmat2) != 0] |> as.matrix()
}

baseline_and_years_posteriors <- function(dat, mod) {
  cmat <- make_contrasts_baseline_and_years(dat)  
  
  mod |>
    emmeans(~cYear) |>
    contrast(method = list(cYear = cmat)) |>
    ## pairs() |>
    gather_emmeans_draws() |>
    dplyr::select(-.chain) |>
    mutate(.value = exp(.value)) |>
    mutate(contrast = str_replace(contrast, "cYear.", "")) |>
    ungroup() |>
    mutate(contrast = forcats::fct_relevel(contrast, "Baseline")) |>
    arrange(contrast) |>
    group_by(contrast)
}

baseline_and_years_summ <- function(dat, mod) {
  dd <-
          baseline_and_years_posteriors(dat, mod) |>
          posterior::summarise_draws(
                  median,
                  HDInterval::hdi,
                  Pl = ~ mean(.x < 1),
                  Pg = ~ mean(.x > 1)
          ) #|>
    ## ungroup() |> 
    ## mutate(contrast = forcats::fct_relevel(contrast, "Baseline")) |> 
    ## arrange(contrast)
  dd
}


validate_models <- function(data) {
  status::status_try_catch(
  {
    nm_l <- paste0(data_path, "modelled/log_models.log")
    total_number_of_models <- nrow(data)
    data |>
      mutate(i = 1:n()) |> 
      mutate(valid = pmap(
        .l = list(fit, data, i),
        .f = ~ {
          mod_s <- ..1
          l_d <- ..2
          i <- ..3
          nm <- str_replace(mod_s, "mod_", "resids_")
          ## print(nm)
          nm2 <- str_replace(mod_s, "mod_", "valid_")
          cat(paste0(
            i, "/", total_number_of_models, " (",
            sprintf("% 3.1f%%", 100 * (i / total_number_of_models)), "): ",
            unique(l_d$ZoneName), " ", unique(l_d$Var), " (", unique(l_d$Value_type), ")"
          ), file = nm_l, append = TRUE)
          if (!file.exists(nm)) {
            mod <- readRDS(mod_s)
            capture.output(
              resids <- make_brms_dharma_res(mod, integerResponse = FALSE) |>
                suppressWarnings() |>
                suppressMessages(),
              file = nullfile()
            )
            saveRDS(resids, file = nm)
            capture.output(
              v <- validate_model(resids) |>
                suppressWarnings() |>
                suppressMessages(),
              file = nullfile()
            )
            df <- data.frame(nm = nm) |> bind_cols(v)
            saveRDS(df, file = nm2)
            cat("\t - model successfully validated\n", file = nm_l, append = TRUE)
          } else {
            df <- readRDS(file = nm2)
            v <- df |> dplyr::select(-nm)
            cat("\t - model previously validated\n", file = nm_l, append = TRUE)
          }
          cbind(nm, v)
        },
        .progress = TRUE
      ))
  },
  stage_ = 5,
  name_ = "Validate models",
  item_ = "validate_models"
  )
}

validate_model <- function(resids) {
        ## resids <- make_brms_dharma_res(mod, integerResponse = FALSE)
        ## KS test
        ks <- validate_model_ks(resids)
        ## Dispersion
        ds <- validate_model_dispersion(resids)
        ## Quantiles
        q <- validate_model_quantiles(resids)
        ## Outliers
        o <- validate_model_outliers(resids)
        data.frame(ks = ks, ds = ds, q = q, o = o)
}

validate_model_ks <- function(resids) {
  DHARMa::testUniformity(resids, plot = FALSE) |> _[["p.value"]]
}
validate_model_dispersion <- function(resids) {
  p <- DHARMa::testDispersion(resids, plot = FALSE) |> _[["p.value"]]
  ## s <- DHARMa::testDispersion(resids, plot = FALSE) |> _[["statistic"]]
  ## data.frame(p, s)
  p
}

validate_model_quantiles <- function(resids) {
  DHARMa::testQuantiles(resids, plot = FALSE) |> _[["p.value"]]
}

validate_model_outliers <- function(resids) {
  DHARMa::testOutliers(resids, plot = FALSE) |> _[["p.value"]]
}


change_palette <- c('#d73027','#fc8d59','#fee08b','#ffffff','#d9ef8b','#91cf60','#1a9850')

compile_posteriors <- function(data, scale) {
    if (scale == "site") {
      data <- data |>
        dplyr::select(data, fit) |>
        unnest(c(data, fit)) |>
        dplyr::select(
          Type, Baseline, ZoneName, RegionName, Area,
          Site, Var, Value_type, cYear, fit
        ) |>
        nest(data = everything(), .by = c(ZoneName, Site, Type, Var, Value_type, fit))
      lst <- compile_posteriors_site(data)
    } else {
      lst <- compile_posteriors_zone(data)
    }
   lst
}
compile_posteriors_zone <- function(data, scale = "zone") {
  status::status_try_catch(
  {
    nm_l <- paste0(data_path, "modelled/log_models.log")
    total_number_of_models <- nrow(data)
    data |>
      dplyr::select(-any_of(c("form", "priors", "template"))) |>
      mutate(i = 1:n()) |> 
      mutate(effects = pmap(
        .l = list(data, fit, i),
        .f = ~ {
          l_d <- ..1
          fit <- ..2
          i <- ..3
          nm <- str_replace(fit, "mod_", "effects_")
          ## print(nm)
          ## print(i)
          ## print(unique(l_d$cYear))
          ## print(readRDS(file = fit))
          cat(paste0(
            i, "/", total_number_of_models, " (",
            sprintf("% 3.1f%%", 100 * (i / total_number_of_models)), "): ",
            unique(l_d$ZoneName), " ", unique(l_d$Var), " (", unique(l_d$Value_type), ")"
          ), file = nm_l, append = TRUE)
          lst <- get_all_posteriors(fit, l_d, nm, nm_l, scale)
          ## if (!file.exists(nm)) {
          ##   comp <- NULL
          ##   if (length(unique(l_d$Baseline)) > 1) {
          ##     l_d <- l_d |>
          ##       mutate(scale = scale) |>
          ##       distinct()
          ##     ## Calculate cellmeans posteriors
          ##     nm_cm <- str_replace(nm, "effects_", "cellmeans_posteriors_")
          ##     pstrs_cm <- get_cellmeans_posteriors(l_d, readRDS(file = fit))
          ##     saveRDS(pstrs_cm, file = nm_cm)
          ##     ## Calculate contrast posteriors
          ##     nm_e <- str_replace(nm, "effects_", "effects_posteriors_")
          ##     pstrs_e <- get_effects_posteriors(l_d, readRDS(file = fit))
          ##     saveRDS(pstrs_e, file = nm_e)
          ##     ## Summarise contrasts
          ##     comp <- get_effects_summ(pstrs_e)
          ##     saveRDS(comp, file = nm)
          ##   }
          ##   cat("\t - model successfully compared\n", file = nm_l, append = TRUE)
          ## } else {
          ##   comp <- readRDS(file = nm)
          ##   cat("\t - model previously compared\n", file = nm_l, append = TRUE)
          ## }
          ## comp
          lst
        },
        .progress = TRUE
      )) 
  },
  stage_ = 5,
  name_ = "Compile zone results",
  item_ = "compile_zone_results"
  )
}

compile_posteriors_site <- function(data, scale = "site") {
  status::status_try_catch(
  {
    nm_l <- paste0(data_path, "modelled/log_models.log")
    total_number_of_models <- nrow(data)
    data |>
      dplyr::select(-any_of(c("form", "priors", "template"))) |>
      mutate(i = 1:n()) |> 
      mutate(effects = pmap(
        .l = list(data, fit, i),
        .f = ~ {
          l_d <- ..1
          fit <- ..2
          i <- ..3
          ## print(i)
          ## print(total_number_of_models)
          nm <- str_replace(
            fit, "mod_",
            paste0("effects_", unique(l_d$Site), "_")
          )
          cat(paste0(
            i, "/", total_number_of_models, " (",
            sprintf("% 3.1f%%", 100 * (i / total_number_of_models)), "): ",
            unique(l_d$Site), " ", unique(l_d$Var), " (", unique(l_d$Value_type), ")"
          ), file = nm_l, append = TRUE)
          ## print("here")
          lst <- get_all_posteriors(fit, l_d, nm, nm_l, scale)
          ## if (!file.exists(nm)) {
          ##   comp <- NULL
          ##   if (length(unique(l_d$Baseline)) > 1) {
          ##     l_d <- l_d |>
          ##       mutate(scale = "site") |>
          ##       distinct()
          ##     ## Calculate cellmeans posteriors
          ##     nm_cm <- str_replace(nm, "effects_", "cellmeans_posteriors_")
          ##     pstrs_cm <- get_cellmeans_posteriors(l_d, readRDS(file = fit))
          ##     saveRDS(pstrs_cm, file = nm_cm)
          ##     ## Calculate contrast posteriors
          ##     nm_e <- str_replace(nm, "effects_", "effects_posteriors_")
          ##     pstrs_e <- get_effects_posteriors(l_d, readRDS(file = fit))
          ##     saveRDS(pstrs_e, file = nm_e)
          ##     ## Summarise contrasts
          ##     comp <- get_effects_summ(pstrs_e)
          ##     saveRDS(comp, file = nm)
          ##   }
          ##   cat("\t - model successfully compared\n", file = nm_l, append = TRUE)
          ## } else {
          ##   comp <- readRDS(file = nm)
          ##   cat("\t - model previously compared\n", file = nm_l, append = TRUE)
          ## }
          ## comp
          lst
        },
        .progress = TRUE
      )) 
  },
  stage_ = 5,
  name_ = "Compile site results",
  item_ = "compile_site_results"
  )
}

get_all_posteriors <- function(fit, l_d, nm, nm_l, scale) {
  nm_cm <- str_replace(nm, "effects_", "cellmeans_posteriors_")
  nm_e <- str_replace(nm, "effects_", "effects_posteriors_")
  if (!file.exists(nm)) {
    comp <- NULL
    if (length(unique(l_d$Baseline)) > 1) {
      l_d <- l_d |>
        mutate(scale = scale) |>
        distinct()
      ## Calculate cellmeans posteriors
      ## print("cellmeans")
      pstrs_cm <- get_cellmeans_posteriors(l_d, readRDS(file = fit))
      saveRDS(pstrs_cm, file = nm_cm)
      ## Calculate contrast posteriors
      ## print("effects")
      pstrs_e <- get_effects_posteriors(l_d, readRDS(file = fit))
      saveRDS(pstrs_e, file = nm_e)
      ## Summarise contrasts
      ## print("comp")
      comp <- get_effects_summ(pstrs_e)
      saveRDS(comp, file = nm)
    }
    cat("\t - model successfully compared\n", file = nm_l, append = TRUE)
  } else {
    comp <- readRDS(file = nm) |> ungroup()
    cat("\t - model previously compared\n", file = nm_l, append = TRUE)
  }
  list(nm_cm = nm_cm, nm_e = nm_e, comp = comp)
}


get_effects_summ <- function(pstrs_e) {
  pstrs_e |> 
    posterior::summarise_draws(
      median,
      HDInterval::hdi,
      Pl = ~ mean(.x < 1),
      Pg = ~ mean(.x > 1)
    ) |>
    mutate(change = case_when(
      Pl > 0.95 ~ 7,
      Pl > 0.9 ~ 6,
      Pl > 0.85 ~ 5,
      Pg > 0.95 ~ 1,
      Pg > 0.9 ~ 2,
      Pg > 0.85 ~ 3,
      .default = 4
    )) |> 
    mutate(year = str_extract(contrast, "[0-9]{4}")) |>
      dplyr::select(-variable) |>
      ungroup()
}


get_cellmeans_summ <- function(cm) {
  cm |> 
    posterior::summarise_draws(
      median,
      HDInterval::hdi
    ) |>
    filter(lower != upper) |>
    dplyr::select(-variable) |> 
    ungroup()
}


get_cellmeans_posteriors <- function(dat, mod) {
  ## print(unique(dat$cYear))
  cmat <- make_contrasts_baseline_and_years(dat)  
  ## print(cmat)
  if (unique(dat$scale) == "zone") {
    ## print("Zone emmeans")
    ## print(mod |>
    ##         emmeans(~cYear) 
    ## )
    ## print(mod |>
    ##         emmeans(~cYear) |>
    ##         contrast(method = list(cYear = cmat)) 
    ## )
    cm <- mod |>
      emmeans(~cYear) |>
      contrast(method = list(cYear = cmat)) |>
      gather_emmeans_draws() |>
      dplyr::select(-.chain) |>
      mutate(.value = exp(.value)) |>
      mutate(contrast = str_replace(contrast, "cYear.", "")) |>
      ungroup() |>
      mutate(contrast = forcats::fct_relevel(contrast, "Baseline")) |>
      arrange(contrast) |>
      ## group_by(contrast)
      as_draws_df() |>
      filter(contrast != "Baseline") |>
      mutate(Baseline = ifelse(contrast %in% c("2019", "2020"), TRUE, FALSE)) |> 
      ## left_join(dat |> dplyr::select(contrast = cYear, Baseline) |> distinct(),
      ##   by = "contrast") |> 
      group_by(contrast, Baseline) 
  } else {
    newdata <- dat |>
      dplyr::select(Site, cYear) |>
      droplevels()
    cm <-
      mod |>
      posterior_linpred(newdata = newdata) |>
      as.matrix() %*% cmat |>
      exp() |>
      as_tibble() |>
      pivot_longer(cols = c(everything()), names_to = "contrast", values_to = "value") |>
      as_draws_df() |>
      filter(contrast != "Baseline") |>
      left_join(dat |> dplyr::select(contrast = cYear, Baseline), by = "contrast") |> 
      group_by(contrast, Baseline) 
  }
  cm |>
    mutate(scale = unique(dat$scale)) |>
    group_by(scale, .add = TRUE)
}

get_effects_posteriors <- function(dat, mod) {
  cmat <- make_contrasts_baseline_vs_years(dat)
  if (unique(dat$scale) == "zone") {
    eff <- mod |>
      emmeans(~cYear) |>
      contrast(method = list(cYear = cmat)) |>
      gather_emmeans_draws() |>
      dplyr::select(-.chain) |>
      mutate(.value = exp(.value)) |>
      mutate(contrast = str_replace(contrast, "cYear.", "")) |>
      ungroup() |>
      group_by(contrast)
  } else {  ## site level
    newdata <- dat |>
      dplyr::select(Site, cYear) |>
      droplevels()
    eff <-
      mod |>
      posterior_linpred(newdata = newdata) |>
      as.matrix() %*% cmat |>
      exp() |>
      as_draws_df() |>
      pivot_longer(
        cols = contains("vs"),
        names_to = "contrast",
        values_to = ".value"
      ) |> 
      ungroup() |>
      group_by(contrast)
  }
  eff |> mutate(scale = unique(dat$scale)) |> 
    group_by(scale, add = TRUE)
}

## compile_baseline_vs_year_comparisons <- function(data) {
##   status::status_try_catch(
##   {
##     nm_l <- paste0(data_path, "modelled/log_models.log")
##     total_number_of_models <- nrow(data)
##     data |>
##       dplyr::select(-any_of(c("form", "priors", "template"))) |>
##       mutate(i = 1:n()) |> 
##       mutate(effects = pmap(
##         .l = list(data, fit, i),
##         .f = ~ {
##           l_d <- ..1
##           fit <- ..2
##           i <- ..3
##           nm <- str_replace(fit, "mod_", "effects_")
##           cat(paste0(
##             i, "/", total_number_of_models, " (",
##             sprintf("% 3.1f%%", 100 * (i / total_number_of_models)), "): ",
##             unique(l_d$ZoneName), " ", unique(l_d$Var), " (", unique(l_d$Value_type), ")"
##           ), file = nm_l, append = TRUE)
##           if (!file.exists(nm)) {
##             comp <- NULL
##             if (length(unique(l_d$Baseline)) > 1) {
##               l_d <- l_d |> mutate(scale = "zone")
##               comp <- compare_baseline_vs_years_summ(l_d, readRDS(file = fit), nm)
##               saveRDS(comp, file = nm)
##             }
##             cat("\t - model successfully compared\n", file = nm_l, append = TRUE)
##           } else {
##             comp <- readRDS(file = nm)
##             cat("\t - model previously compared\n", file = nm_l, append = TRUE)
##           }
##           comp
##         },
##         .progress = TRUE
##       )) 
##   },
##   stage_ = 5,
##   name_ = "Compile results",
##   item_ = "compile_results"
##   )
## }



site_compare_baseline_vs_years_posteriors <- function(dat, mod) {
  cmat <- make_contrasts_baseline_vs_years(dat)
  newdata <- dat |>
    dplyr::select(Site, cYear) |>
    droplevels()
  mod |>
    posterior_linpred(newdata = newdata) |>
    as.matrix() %*% cmat |> 
    exp() |> 
    as_draws_df() 
}

site_compare_baseline_vs_years_summ <- function(dat, mod) {
  site_compare_baseline_vs_years_posteriors(dat, mod) |>
          posterior::summarise_draws(
                  median,
                  HDInterval::hdi,
                  Pl = ~ mean(.x < 1),
                  Pg = ~ mean(.x > 1)
          ) |>
          mutate(change = case_when(
            Pl > 0.95 ~ 7,
            Pl > 0.9 ~ 6,
            Pl > 0.85 ~ 5,
            Pg > 0.95 ~ 1,
            Pg > 0.9 ~ 2,
            Pg > 0.85 ~ 3,
            .default = 4
          )) |> 
    dplyr::rename(contrast = variable) |> 
    mutate(year = str_extract(contrast, "[0-9]{4}")) 
}

derive_change <- function(summ) {
  summ |> 
    mutate(change = case_when(
      Pl > 0.95 ~ 7,
      Pl > 0.9 ~ 6,
      Pl > 0.85 ~ 5,
      Pg > 0.95 ~ 1,
      Pg > 0.9 ~ 2,
      Pg > 0.85 ~ 3,
      .default = 4
    )) 
}

## site_compile_baseline_vs_year_comparisons <- function(data) {
##   status::status_try_catch(
##   {
##     ## Re-nest with respect to site
##     data_site <- data |>
##       dplyr::select(data, fit) |>
##       unnest(c(data, fit)) |>
##       dplyr::select(
##         Type, Baseline, ZoneName, RegionName, Area,
##         Site, Var, Value_type, cYear, fit
##       ) |>
##       nest(data = everything(), .by = c(ZoneName, Site, Type, Var, Value_type, fit))
##     nm_l <- paste0(data_path, "modelled/log_models.log")
##     total_number_of_models <- nrow(data_site)
##     data_site |>
##       dplyr::select(-any_of(c("form", "priors", "template"))) |>
##       mutate(i = 1:n()) |> 
##       mutate(effects = pmap(
##         .l = list(data, fit, i),
##         .f = ~ {
##           l_d <- distinct(..1)
##           fit <- ..2
##           i <- ..3
##           nm <- str_replace(
##             fit, "mod_",
##             paste0("site_effects_", unique(l_d$Site), "_")
##           )
##           cat(paste0(
##             i, "/", total_number_of_models, " (",
##             sprintf("% 3.1f%%", 100 * (i / total_number_of_models)), "): ",
##             unique(l_d$Site), " ", unique(l_d$Var), " (", unique(l_d$Value_type), ")"
##           ), file = nm_l, append = TRUE)
##           if (!file.exists(nm)) {
##             comp <- NULL
##             if (length(unique(l_d$Baseline)) > 1) {
##               comp <- site_compare_baseline_vs_years_summ(l_d, readRDS(file = fit))
##               saveRDS(comp, file = nm)
##             }
##             cat("\t - model successfully compared\n", file = nm_l, append = TRUE)
##           } else {
##             comp <- readRDS(file = nm)
##             cat("\t - model previously compared\n", file = nm_l, append = TRUE)
##           }
##           comp
##         },
##         .progress = TRUE
##       )) 
##   },
##   stage_ = 5,
##   name_ = "Compile site results",
##   item_ = "compile_site_results"
##   )
## }

collect_results <- function(data, scale) {
  if (scale == "zone") {
    collect_results_zone(data)
  } else {
    collect_results_site(data)
  }
}

collect_results_zone <- function(data) {
  status::status_try_catch(
  {
    collect_results_all(data)
  },
  stage_ = 5,
  name_ = "Collect zone results",
  item_ = "Collect_zone_results"
  )
}

collect_results_site <- function(data) {
  status::status_try_catch(
  {
    collect_results_all(data)
  },
  stage_ = 5,
  name_ = "Collect site results",
  item_ = "Collect_site_results"
  )
}

collect_results_all <- function(data) {
  data |>
    ## Cellmeans posteriors
    ## mutate(pstr_cm = map(
    mutate(nm_cm = map(
      .x = effects,
      .f = ~ {
        if (file.exists(.x$nm_cm)) {
          ## pstr_cm <- readRDS(.x$nm_cm)
          nm_cm <- .x$nm_cm
        } else {
          pstr_cm <- NULL
        }
      }
    )) |> 
    ## Cellmeans posteriors
    ## mutate(pstr_e = map(
    mutate(nm_e = map(
      .x = effects,
      .f = ~ {
        if (file.exists(.x$nm_e)) {
          ## pstr_cm <- readRDS(.x$nm_e)
          nm_cm <- .x$nm_e
        } else {
          pstr_cm <- NULL
        }
      }
    )) |> 
    mutate(summ_e = map(
      .x = effects,
      .f = ~ {
        .x$comp
      }
    ))
}
