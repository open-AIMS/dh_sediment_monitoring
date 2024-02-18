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
  install.packages('tidyverse'); \
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

## Project specific packages
RUN R -e "options(repos = \
  list(CRAN = 'https://packagemanager.posit.co/cran/2023-12-01/')); \
  remotes::install_github('open-AIMS/status@v0.0.1'); \
"

## Shiny packages
RUN R -e "options(repos = \
  list(CRAN = 'https://packagemanager.posit.co/cran/2023-12-01/')); \
  install.packages('shiny'); \
  install.packages('shinydashboard'); \
  install.packages('shinyWidgets'); \
  install.packages('shinyjs'); \
  install.packages('shinyBS'); \
  install.packages('shinyTree'); \
  install.packages('fansi'); \
  install.packages('DT'); \
  install.packages('reactable'); \
  install.packages('leaflet'); \
"  

## Other packages
RUN R -e "options(repos = \
  list(CRAN = 'https://packagemanager.posit.co/cran/2023-12-01/')); \
  install.packages('markdown'); \
"  

## Install extra packages required for tidyverse 
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
  && rm -rf /var/lib/apt/lists/*

RUN R -e "options(repos = \
  list(CRAN = 'https://packagemanager.posit.co/cran/2023-12-01/')); \
  install.packages('tidyverse'); \
  install.packages('testthat'); \
  install.packages('assertthat'); \
"  

RUN R -e "options(repos = \
  list(CRAN = 'https://packagemanager.posit.co/cran/2023-12-01/')); \
  install.packages('bookdown'); \
  install.packages('rmarkdown'); \
"  

RUN R -e "options(repos = \
  list(CRAN = 'https://packagemanager.posit.co/cran/2023-12-01/')); \
  install.packages('ggplotly'); \
"  

## Install extra packages required for quarto 
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    curl \
    gdebi-core \
  && rm -rf /var/lib/apt/lists/*

ARG QUARTO_VERSION="1.3.450"
RUN curl -o quarto-linux-amd64.deb -L https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-amd64.deb
RUN gdebi --non-interactive quarto-linux-amd64.deb

RUN R -e "options(repos = \
  list(CRAN = 'https://packagemanager.posit.co/cran/2023-12-01/')); \
  install.packages('quarto'); \
"  



RUN R -e "options(repos = \
  list(CRAN = 'https://packagemanager.posit.co/cran/2023-12-01/')); \
  install.packages('sf'); \
"  



RUN apt-get clean

# USER users

RUN mkdir /home/project
RUN mkdir -p /home/project/input

COPY R/ /home/project/R
COPY parameters/ /home/project/parameters
COPY md/ /home/project/md

WORKDIR /home/project

COPY run.sh /home/project/run.sh

# RUN cd ~/project

ENTRYPOINT ["/home/project/run.sh"]

EXPOSE 3838