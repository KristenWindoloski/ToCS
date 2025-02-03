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
    build-essential \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install remotes to facilitate package installation
RUN R -e "install.packages('remotes', repos='https://cloud.r-project.org/')"

# Install httk and dependencies
RUN R -e "remotes::install_github('USEPA/httk')"

# Set default command to launch R
CMD ["R"]

