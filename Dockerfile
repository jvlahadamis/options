FROM rocker/shiny-verse:latest 
RUN apt-get update && apt-get install -y git \
    libssl-dev \
    libcurl4-gnutls-dev 

RUN git clone https://github.com/jvlahadamis/options.git

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/options', host = '0.0.0.0', port = 3838)"]
