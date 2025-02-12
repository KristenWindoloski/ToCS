# Use the Rocker base image
FROM rocker/geospatial:latest AS build

# Set environment variables
ENV DEBIAN_FRONTEND=noninteractive
ENV LC_ALL=C.UTF-8
ENV LANG=C.UTF-8

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev libssl-dev libxml2-dev \
    libudunits2-dev libgdal-dev libgeos-dev libproj-dev \
    libnetcdf-dev libxt-dev libtiff-dev libjpeg-dev \
    git pandoc build-essential \
    libgit2-dev libharfbuzz-dev libfribidi-dev \
    libfontconfig1-dev libfreetype6-dev libpng-dev \
    nginx \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Install R dependencies
RUN R -e "install.packages('devtools', dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('httk', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c( \
    'bslib', 'cowplot', 'DescTools', 'dplyr', 'forcats', 'ggplot2', 'gridExtra', \
    'htmltools', 'magrittr', 'purrr', 'scales', 'shiny', 'shinyjs', 'stats', \
    'knitr', 'rmarkdown' \
  ), repos='https://cloud.r-project.org/')"

# Copy application source from the repository
WORKDIR /usr/local/src/ToCS
COPY ../src /usr/local/src/ToCS

# Copy Nginx configuration
COPY nginx.conf /etc/nginx/nginx.conf

# Start Shiny and Nginx
CMD ["/bin/bash", "-c", "\
    R -e \"options(shiny.host='0.0.0.0', shiny.port=3838); source('/usr/local/src/ToCS/start_app.R')\" & \
    nginx -g 'daemon off;'"]

