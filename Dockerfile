ARG R_VERSION=4.4.1
FROM rocker/tidyverse:${R_VERSION}

# System libraries needed to compile the package's Stan/Rcpp code (rstan,
# RcppParallel, StanHeaders) and its R dependencies (e.g. markovchain -> igraph).
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    build-essential \
    ca-certificates \
    git \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libglpk-dev \
    libgmp-dev \
    libmpfr-dev \
    && rm -rf /var/lib/apt/lists/*

# Recommended compiler flags for building Stan models (see rstan docs).
# Avoid -march=native so the image stays portable across host machines.
RUN mkdir -p /home/rstudio/.R && \
    echo "CXX14FLAGS=-O3 -fPIC -Wno-ignored-attributes -Wno-unused-variable -Wno-unused-function" > /home/rstudio/.R/Makevars && \
    echo "CXX14=g++" >> /home/rstudio/.R/Makevars && \
    cp -r /home/rstudio/.R /root/.R

WORKDIR /tmp/deps

# Install the package's dependencies (Depends/Imports/LinkingTo/Suggests) as
# declared in DESCRIPTION, plus HuraultMisc which is only available on GitHub.
# `remotes::install_deps()` always installs the latest version available on
# CRAN, so versions here are not pinned.
COPY DESCRIPTION .
RUN Rscript -e "remotes::install_github('ghurault/HuraultMisc')" && \
    Rscript -e "remotes::install_deps(dependencies = TRUE)"

WORKDIR /home/rstudio/EczemaPred
