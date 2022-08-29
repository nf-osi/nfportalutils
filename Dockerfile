FROM rocker/r-base:4.1.2

RUN apt-get update && \
    apt-get install -y --no-install-recommends software-properties-common \
    python3.6 \
    python3-pip \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev

COPY . /nfportalutils

# R components
RUN R -e "install.packages(c('remotes'), repos='http://cran.rstudio.com/')"
RUN R -e "remotes::install_local('nfportalutils', dependencies = c('Depends', 'Imports'))"

# Python components    
RUN pip install virtualenv
RUN virtualenv /r-reticulate

# Same as running source /r-reticulate/bin/activate
ENV VIRTUAL_ENV /r-reticulate
ENV PATH /r-reticulate/bin:$PATH

RUN pip install pandas synapseclient 

