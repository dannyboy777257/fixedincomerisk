FROM rocker/shiny-verse:latest 
RUN apt-get update && apt-get install -y git \
    libssl-dev \
    libcurl4-gnutls-dev \
    libxml2-dev

RUN git clone https://github.com/dannyboy777257/fixedincomerisk.git /srv/shiny-server/fixedincomerisk
RUN Rscript /srv/shiny-server/fixedincomerisk/requirements.R

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/fixedincomerisk', host = '0.0.0.0', port = 3838)"]