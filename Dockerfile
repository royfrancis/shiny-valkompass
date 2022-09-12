FROM rocker/shiny
LABEL authors="Roy Francis"
ARG REPO="royfrancis/shiny-valkompass"

RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean && \
    apt-get install -y git libxml2-dev libudunits2-dev && \
    rm -rf /var/lib/apt/lists/*

RUN Rscript -e 'install.packages(c("curl","dplyr","ggplot2","ggh4x","shinythemes","showtext"))'

RUN cd /srv/shiny-server/ && \
    git clone https://github.com/${REPO}.git app && \
    sudo chown -R shiny:shiny /srv/shiny-server/app

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/app/', host = '0.0.0.0', port = 8787)"]

# docker build -t royfrancis/shiny-valkompass:v1.0.0 .
# docker run --rm -p 8787:8787 royfrancis/shiny-valkompass:v1.0.0
