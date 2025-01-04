FROM rocker/shiny:latest

RUN apt update && apt install -y \
  libmagick++-dev \
  libpq5 \
  libmysqlclient-dev \
  libudunits2-dev \
  libgdal-dev \
  libgeos-dev \
  libproj-dev

RUN install2.r --error \
  data.table \
  haven \
  httr \
  readxl \
  shinyBS \
  sjlabelled \
  DT \
  shinyjs \
  jsonlite \
  dplyr \
  stringr \
  sf

ENV R_LIBS_USER=/usr/local/lib/R/site-library
RUN R -e "install.packages('remotes')" && \
    chmod -R 755 /usr/local/lib/R/site-library

ARG GITHUB_PAT
RUN R -e "remotes::install_github('echeloninsights/EchelonSurveyTools', auth_token='${GITHUB_PAT}')"
RUN rm -r /srv/* && mkdir -p /srv/shiny-server/survey-monitor/Test-Data

COPY . /srv/shiny-server/survey-monitor/
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

RUN chmod 777 -R /srv/shiny-server

USER shiny

ENV SHINY_LOG_STDERR=1
ENV SHINY_LOG_LEVEL=INFO

ENTRYPOINT ["/usr/bin/shiny-server"]