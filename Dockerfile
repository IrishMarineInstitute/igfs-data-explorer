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

COPY Data /srv/shiny-server/Data
COPY Indices /srv/shiny-server/Indices
COPY adminLTE.css /srv/shiny-server/
COPY app.js /srv/shiny-server/
COPY google-analytics.js /srv/shiny-server/
COPY README.md /srv/shiny-server/
COPY server.R /srv/shiny-server/

#COPY igfs-data-explorer /srv/shiny-server/
COPY BOF /srv/shiny-server/BOF
COPY COD /srv/shiny-server/COD
COPY CUR /srv/shiny-server/CUR
COPY DAB /srv/shiny-server/DAB
COPY DGS /srv/shiny-server/DGS
COPY ESB /srv/shiny-server/ESB
COPY GUG /srv/shiny-server/GUG
COPY HAD /srv/shiny-server/HAD
COPY HER /srv/shiny-server/HER
COPY HKE /srv/shiny-server/HKE
COPY HOM /srv/shiny-server/HOM
COPY JOD /srv/shiny-server/JOD
COPY LSD /srv/shiny-server/LSD
COPY MAC /srv/shiny-server/MAC
COPY MEG /srv/shiny-server/MEG
COPY MON /srv/shiny-server/MON
COPY NOP /srv/shiny-server/NEP
COPY NOP /srv/shiny-server/NOP
COPY PLE /srv/shiny-server/PLE
COPY POD /srv/shiny-server/POD
COPY POK /srv/shiny-server/POK
COPY POL /srv/shiny-server/POL
COPY SDR /srv/shiny-server/SDR
COPY SKT /srv/shiny-server/SKT
COPY SOL /srv/shiny-server/SOL
COPY SPR /srv/shiny-server/SPR
COPY THR /srv/shiny-server/THR
COPY WAF /srv/shiny-server/WAF
COPY WHB /srv/shiny-server/WHB
COPY WHG /srv/shiny-server/WHG
COPY www /srv/shiny-server/www

RUN Rscript -e "install.packages(c('devtools'), repos='https://cran.rstudio.com/')"
RUN Rscript -e "install.packages(c('hexbin'), repos='https://cran.rstudio.com/')"
RUN Rscript -e "install.packages(c('data.table'), repos='https://cran.rstudio.com/')"
RUN wget https://cran.r-project.org/src/contrib/Archive/plotly/plotly_4.7.1.tar.gz
RUN R CMD INSTALL plotly_4.7.1.tar.gz
COPY ui.R /srv/shiny-server/
EXPOSE 3838
CMD ["/usr/bin/shiny-server.sh"]
