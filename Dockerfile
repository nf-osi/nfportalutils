FROM rocker/r-base:4.2.2

RUN apt-get update && \
    apt-get -y --no-install-recommends install software-properties-common \
    python3-dev \
    python3-pip \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev


RUN pip install pandas synapseclient==2.7.0

COPY . /nfportalutils

# R components
RUN R -e "install.packages(c('remotes'), repos='http://cran.rstudio.com/')"
RUN R -e "remotes::install_local('nfportalutils', dependencies = c('Depends', 'Imports'))"


