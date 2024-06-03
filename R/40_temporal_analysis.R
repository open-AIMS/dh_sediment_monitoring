
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
  
  ##data <- readRDS(file = paste0(data_path, "modelled/data.RData"))
  ## data <- readRDS(file = paste0(data_path, "modelled/data.RData"))

  ## Validate models
  data <- validate_models(data)
  saveRDS(data, file = paste0(data_path, "modelled/data.RData"))
  ## Compile all the effects
  data <- compile_baseline_vs_year_comparisons(data)
  saveRDS(data, file = paste0(data_path, "modelled/data_all.RData"))
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
      nest(data =  everything(), .by = c(ZoneName, Type, Var, Value_type))
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
              form <- bf(Values | cens(cens_lor) ~ cYear + (1 | Site), family = Gamma(link = "log"))
            } else {
              form <- bf(Values ~ cYear + (1 | Site), family = Gamma(link = "log"))
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
    data |>
      mutate(fit = pmap(
        .l = list(data, form, priors, template),
        .f = ~ {
          l_d <- ..1
          l_f <- ..2
          l_p <- ..3
          l_t <- ..4
          nm <- paste0(
            data_path, "modelled/",
            sanitise_filename(paste0(
              "mod_", unique(l_d$ZoneName), "__",
              unique(l_d$Var), "___",
              unique(l_d$Value_type)
            ))
          )
          ## print(nm)
          mod_template <- readRDS(l_t)
          recom <- !formula_same(mod_template$form, l_f)
          ## Determine whether the model should be re-run (based on
          ## whether it already exists or not)
          if (!file.exists(paste0(nm, ".rds"))) {
            ## ## Determine whether the model should be recompiled
            ## mod_template <- readRDS(l_t)
            ## recom <- !formula_same(mod_template$form, l_f)
            capture.output(
              mod <- invisible(update(mod_template,
                form = l_f,
                newdata = l_d,
                prior = l_p,
                sample_prior = "yes",
                recompile = recom,
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
              ) |> suppressWarnings() |> suppressMessages()),
              file = nullfile()
            )
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
get_filtered_data <- function(data, zone, var, type) {
  data |>
    filter(
      ZoneName == zone,
      Var == var,
      Value_type == type
    ) |>
    droplevels() |>
    pull(data) |>
    _[[1]]
}

## Get the "fit" field from the filtered nested data set and use it to read in the model
get_filtered_model <- function(data, zone, var, type) {
  fit <- data |>
    filter(
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
  d <- dat |>
    dplyr::select(cYear, Baseline) |>
    distinct() |>
    filter(!Baseline | Baseline & cYear %in% c(2019, 2020)) |>
    mutate(BaselineYear = interaction(Baseline, cYear)) |>
    mutate(B = ifelse(Baseline, as.character(Baseline), as.character(cYear))) |>
    mutate(
      B = forcats::fct_relevel(B, "TRUE"),
      C = ifelse(Baseline, -1, 1)
    )
  m <- model.matrix(~ -1 + B, data = d)
  cmat <-
    d |>
    bind_cols(sweep(m, 2, colSums(m), "/")) |>
    mutate(across(matches("B[0-9]{4}"), function(x) x - BTRUE)) |>
    dplyr::select(matches("B[0-9]{4}")) |>
    dplyr::rename_with(function(x) str_replace(x, "B([0-9]{4})", "\\1 vs Baseline")) |> 
    as.matrix()
  cmat
}

compare_baseline_vs_years_posteriors <- function(dat, mod) {
  cmat <- make_contrasts_baseline_vs_years(dat)
  mod |>
    emmeans(~cYear) |>
    contrast(method = list(cYear = cmat)) |>
    ## pairs() |>
    gather_emmeans_draws() |>
    dplyr::select(-.chain) |>
    mutate(.value = exp(.value)) |> 
    mutate(contrast = str_replace(contrast, "cYear.", "")) 
}

compare_baseline_vs_years_summ <- function(dat, mod) {
  compare_baseline_vs_years_posteriors(dat, mod) |>
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
  d <- dat |>
    dplyr::select(cYear, Baseline) |>
    distinct() |>
    filter(!Baseline | Baseline & cYear %in% c(2019, 2020)) |>
    mutate(BaselineYear = interaction(Baseline, cYear)) |>
    mutate(B = ifelse(Baseline, as.character(Baseline), as.character(cYear))) |>
    mutate(
      B = forcats::fct_relevel(B, "TRUE"),
      C = ifelse(Baseline, -1, 1)
    )
  m <- model.matrix(~ -1 + B, data = d)
  cmat <-
    sweep(m, 2, colSums(m), "/") |>
    as.data.frame() |>
    dplyr::rename("Baseline" = BTRUE) |> 
    dplyr::rename_with(function(x) str_replace(x, "B([0-9]{4})", "\\1")) |> 
    as.matrix()
  cmat
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
  data |>
    mutate(valid = map(
      .x = fit,
      .f = ~ {
        nm <- str_replace(.x, "mod_", "resids_")
        nm2 <- str_replace(.x, "mod_", "valid_")
        if (!file.exists(nm)) {
          mod <- readRDS(.x)
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
        } else {
          df <- readRDS(file = nm2)
          v <- df |> dplyr::select(-nm)
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

compile_baseline_vs_year_comparisons <- function(data) {
  status::status_try_catch(
  {
    data |>
      dplyr::select(-any_of(c("form", "priors", "template"))) |>
      mutate(effects = map2(
        .x = data,
        .y = fit,
        .f = ~ {
          nm <- str_replace(.y, "mod_", "effects_")
          if (!file.exists(nm)) {
            comp <- NULL
            if (length(unique(.x$Baseline)) > 1) {
              comp <- compare_baseline_vs_years_summ(.x, readRDS(file = .y))
              saveRDS(comp, file = nm)
            }
          } else {
            comp <- readRDS(file = nm)
          }
          comp
        },
        .progress = TRUE
      )) 
  },
  stage_ = 5,
  name_ = "Compile results",
  item_ = "compile_results"
  )
}
