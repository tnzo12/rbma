FROM rocker/shiny-verse:latest

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-openssl-dev \
    libssl-dev \
    libgit2-dev \
    libxml2-dev \
    libudunits2-dev \
    cmake \
    libgmp-dev \
    libmpc-dev

# install R packages required
# UI side libraries (change it dependeing on the packages you need)
RUN R -e "install.packages(c('devtools','remotes','ggplot2','tidyverse','knitr'), dependencies=TRUE)"
RUN R -e "install.packages(c('bs4Dash','networkD3','rhandsontable','formattable'), dependencies=TRUE)"
RUN R -e "install.packages(c('shinyWidgets','shinycssloaders','htmltools','plotly'), dependencies=TRUE)"

# Server side libraries
RUN R -e "install.packages(c('symengine','n1qn1','PreciseSums'), dependencies=TRUE)"
RUN R -e "install.packages('rxode2', dependencies=TRUE)"
RUN R -e "install.packages('nlmixr2', dependencies=TRUE)"
RUN R -e "install.packages('shinyjs', dependencies=TRUE)"
RUN R -e "install.packages('bslib', dependencies=TRUE)"
RUN R -e "install.packages('dplyr', dependencies=TRUE)"
RUN R -e "install.packages('sparkline', dependencies=TRUE)"
RUN R -e "install.packages('MetaStan', dependencies=TRUE)"
RUN R -e "install.packages('promises', dependencies=TRUE)"

# Data
RUN R -e "install.packages(c('reactable','kableExtra','data.table','furrr'), dependencies=TRUE)"

# copy the app to the image
# COPY shiny-server.sh /usr/bin/shiny-server.sh
COPY app.R /srv/shiny-server

# select port
EXPOSE 3838
