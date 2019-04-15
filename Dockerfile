FROM rocker/shiny:3.5.1
MAINTAINER Marine Institute
# install ssl
# and gdal
RUN sudo apt-get update && apt-get install -y libssl-dev libudunits2-0 libudunits2-dev libproj-dev libgdal-dev && apt-get clean && rm -rf /var/lib/apt/lists/ && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
# install additional packages
RUN Rscript -e "install.packages(c('htmlwidgets','dplyr','leaflet','mapview'), repos='https://cran.rstudio.com/')"

RUN sudo chown -R shiny:shiny /var/lib/shiny-server/
RUN Rscript -e "install.packages(c('shinythemes','shinydashboard','flexdashboard','ggridges','shinycssloaders','tidyverse'), repos='https://cran.rstudio.com/')" && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
RUN sudo apt-get update && apt-get install -y libprotobuf-dev protobuf-compiler libv8-3.14-dev libjq-dev && apt-get clean && rm -rf /var/lib/apt/lists/ && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
RUN Rscript -e "install.packages(c('tidyr','geojsonio'), repos='https://cran.rstudio.com/')" && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

RUN Rscript -e "install.packages(c('devtools'), repos='https://cran.rstudio.com/')"
RUN Rscript -e "install.packages(c('hexbin'), repos='https://cran.rstudio.com/')"
RUN Rscript -e "install.packages(c('data.table'), repos='https://cran.rstudio.com/')"
RUN wget https://cran.r-project.org/src/contrib/Archive/plotly/plotly_4.7.1.tar.gz
RUN R CMD INSTALL plotly_4.7.1.tar.gz

COPY Data /srv/shiny-server/Data
COPY Indices /srv/shiny-server/Indices
COPY adminLTE.css /srv/shiny-server/
COPY app.js /srv/shiny-server/
COPY google-analytics.js /srv/shiny-server/
COPY README.md /srv/shiny-server/
COPY server.R /srv/shiny-server/

#COPY igfs-data-explorer /srv/shiny-server/
COPY BOF /srv/shiny-server/apps/BOF
COPY COD /srv/shiny-server/apps/COD
COPY CUR /srv/shiny-server/apps/CUR
COPY DAB /srv/shiny-server/apps/DAB
COPY DGS /srv/shiny-server/apps/DGS
COPY ESB /srv/shiny-server/apps/ESB
COPY GUG /srv/shiny-server/apps/GUG
COPY HAD /srv/shiny-server/apps/HAD
COPY HER /srv/shiny-server/apps/HER
COPY HKE /srv/shiny-server/apps/HKE
COPY HOM /srv/shiny-server/apps/HOM
COPY JOD /srv/shiny-server/apps/JOD
COPY LSD /srv/shiny-server/apps/LSD
COPY MAC /srv/shiny-server/apps/MAC
COPY MEG /srv/shiny-server/apps/MEG
COPY MON /srv/shiny-server/apps/MON
COPY NOP /srv/shiny-server/apps/NEP
COPY NOP /srv/shiny-server/apps/NOP
COPY PLE /srv/shiny-server/apps/PLE
COPY POD /srv/shiny-server/apps/POD
COPY POK /srv/shiny-server/apps/POK
COPY POL /srv/shiny-server/apps/POL
COPY SDR /srv/shiny-server/apps/SDR
COPY SKT /srv/shiny-server/apps/SKT
COPY SOL /srv/shiny-server/apps/SOL
COPY SPR /srv/shiny-server/apps/SPR
COPY THR /srv/shiny-server/apps/THR
COPY WAF /srv/shiny-server/apps/WAF
COPY WHB /srv/shiny-server/apps/WHB
COPY WHG /srv/shiny-server/apps/WHG
COPY www /srv/shiny-server/www
#COPY shiny-server.conf /etc/shiny-server/
COPY ui.R /srv/shiny-server/
EXPOSE 3838
CMD ["/usr/bin/shiny-server.sh"]
