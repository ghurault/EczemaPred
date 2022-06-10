# Take inspiration from
# https://github.com/jrnold/docker-stan/blob/master/rstan/Dockerfile

FROM rocker/tidyverse

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
  clang \
  # Other depencies (for devtools, igraph, httr)
  libcurl4-openssl-dev \
  libssl-dev \
  libssh2-1-dev \
  libxml2-dev \
  libglpk-dev \
  libgmp3-dev \
  # makes the image smaller
  && rm -rf /var/lib/apt/lists/*

# Global site-wide config
RUN mkdir -p $HOME/.R/ \
  && echo "\nCXX=clang++ -ftemplate-depth-256\n" >> $HOME/.R/Makevars \
  && echo "CC=clang\n" >> $HOME/.R/Makevars

# Install rstan and other packages
RUN install2.r --error --deps TRUE \
    rstan \
    rstantools \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
# RUN R -e "install.packages(c('rstan', 'rstantools'), dependencies = TRUE)"

# Create a user variable
ENV USER=rstudio

# Create project directory and set it as working directory
WORKDIR /home/$USER/EczemaPred

# Install package from DESCRIPTION file
COPY ["DESCRIPTION", "./"]
RUN chown -R rstudio . \
 && sudo -u rstudio R -e 'devtools::install_dev_deps()'
RUN rm -rf DESCRIPTION

# To build docker image `docker build . -t docker-eczemapred`
# To run the container `docker run -d --rm -p 8787:8787 -e ROOT=TRUE -e DISABLE_AUTH=true -v $(pwd):/home/rstudio/EczemaPred docker-eczemapred`

