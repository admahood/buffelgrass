
FROM rocker/geospatial:latest
MAINTAINER "Adam Mahood" adam.mahood@colorado.edu

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    python3-pip

RUN pip3 install awscli

RUN install2.r --error \
    assertthat \
    doParallel \
    ggcorrplot \
    ggthemes \
    ggpubr \
    ggtext \
    httr \
    Hmsc \
    snow \
    rasterVis \
    RCurl \
    snowfall \
    tidyverse \
    viridis