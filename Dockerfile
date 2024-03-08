FROM rocker/shiny-verse:latest 
RUN apt-get update && apt-get install -y git \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libmysqlclient-dev

RUN git clone https://github.com/dannyboy777257/abandonedwellliabilities.git /srv/shiny-server/wells
RUN Rscript /srv/shiny-server/wells/requirements.R

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/wells', host = '0.0.0.0', port = 3838)"]