setwd("R")
options(width = 80)
unloadNamespace("status")
#detach(package:status)
library(status)

## Stage 2
source("startup_functions.R")
source("05_stats_functions.R")

start_matter()

## Stage 2
source("10_load_data.R")
module_load_data()

## Stage 3
source("20_process_data.R")
module_process_data()

## Stage 4
source("30_eda.R")
module_eda()

data <- readRDS(file = paste0(data_path, "processed/data.RData"))

plots <- eda_temporal(data)
## lapply(
##         seq_along(plots$Initial_quarter),
##         function(i) {
##           ggsave(
##                   filename = paste0(output_path, "eda_temporal_plot__Initial_quarter_", i, ".png"),
##                   plot = plots[i, "Plot"][[1]][[1]],
##                   dpi = 300
##           )
##         }
## )

## plots_type <- eda_type_temporal(data)
## pmap(
##   .l = list(plots_type$ZoneName, plots_type$Type, plots_type$Plot),
##   .f = ~ ggsave(
##           filename = paste0(output_path, "eda_type_temporal_plot__Zone_", ..1, "__Type_", ..2, ".png"),
##           plot = ..3,
##           dpi = 300
##   )
## )

## Stage 5
source("40_temporal_analysis.R")
module_temporal()
