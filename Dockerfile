# Use the official R base image
FROM rocker/r-ver:latest

# Set environment variables
ENV DEBIAN_FRONTEND=noninteractive
ENV LC_ALL=C.UTF-8
ENV LANG=C.UTF-8

# Install required system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libnetcdf-dev \
    libxt-dev \
    libtiff-dev \
    libjpeg-dev \
    git \
    pandoc \
    pandoc-citeproc \
    build-essential \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install remotes and devtools for package development
RUN R -e "install.packages(c('remotes', 'devtools', 'shiny', 'shinyjs'), repos='https://cloud.r-project.org/')"

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

# Set default command to ensure the GUI is externally accessible
CMD ["R", "-e", "devtools::load_all('.'); options(shiny.host='0.0.0.0', shiny.port=3838); ToCS()"]


# Set default command to launch R
CMD ["R"]

