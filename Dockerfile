## Get R version 4.3.2
FROM rocker/r-ver:4.3.2

## Install packages
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    libudunits2-dev \
    libssl-dev \
    libgdal-dev \
    libproj-dev \
    libgeos-dev \
  && rm -rf /var/lib/apt/lists/*

## A selection of tidyverse packages
RUN R -e "options(repos = \
  list(CRAN = 'https://packagemanager.posit.co/cran/2023-12-01/')); \
  install.packages('dplyr'); \
  install.packages('lubridate'); \
  install.packages('ggplot2'); \
  install.packages('readr'); \
  install.packages('stringr'); \
  install.packages('tidyr'); \
"  

RUN R -e "options(repos = \
  list(CRAN = 'https://packagemanager.posit.co/cran/2023-12-01/')); \
  install.packages('crayon'); \
  install.packages('cli'); \
  install.packages('validate'); \
"  

RUN R -e "options(repos = \
  list(CRAN = 'https://packagemanager.posit.co/cran/2023-12-01/')); \
  install.packages('remotes'); \
"

RUN R -e "options(repos = \
  list(CRAN = 'https://packagemanager.posit.co/cran/2023-12-01/')); \
  remotes::install_github('open-AIMS/status@v0.0.1'); \
  remotes::install_github('open-AIMS/sedMon@v0.0.1'); \
"

RUN apt-get clean

CMD ["R"]
