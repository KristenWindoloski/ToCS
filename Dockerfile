# Use the official R base image
FROM rocker/geospatial:latest

# Set environment variables
ENV DEBIAN_FRONTEND=noninteractive
ENV LC_ALL=C.UTF-8
ENV LANG=C.UTF-8

# Install required system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev libssl-dev libxml2-dev \
    libudunits2-dev libgdal-dev libgeos-dev libproj-dev \
    libnetcdf-dev libxt-dev libtiff-dev libjpeg-dev \
    git pandoc build-essential \
    libgit2-dev libharfbuzz-dev libfribidi-dev \
    libfontconfig1-dev libfreetype6-dev libpng-dev \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Install devtools separately and explicitly handle errors
RUN R -e "install.packages('devtools', dependencies=TRUE, repos='https://cloud.r-project.org/')"

# Install httk from CRAN
RUN R -e "install.packages('httk', repos='https://cloud.r-project.org/')"

# Install required CRAN dependencies
RUN R -e "install.packages(c( \
    'bslib', 'cowplot', 'DescTools', 'dplyr', 'forcats', 'ggplot2', 'gridExtra', \
    'htmltools', 'magrittr', 'purrr', 'scales', 'shiny', 'shinyjs', 'stats', \
    'knitr', 'rmarkdown' \
  ), repos='https://cloud.r-project.org/')"

# Clone the ToCS repository from GitHub
WORKDIR /usr/local/src
RUN git clone https://github.com/KristenWindoloski/ToCS.git

# Set working directory inside the container
WORKDIR /usr/local/src/ToCS

# Expose the Shiny port
EXPOSE 3838

# Ensure R uses the correct library path and run the application
CMD ["R", "-e", ".libPaths(c('/usr/local/lib/R/site-library', .libPaths())); library(devtools); devtools::load_all('/usr/local/src/ToCS'); options(shiny.host='0.0.0.0', shiny.port=3838); ToCS()"]

