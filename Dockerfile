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

COPY Data /srv/shiny-server/igfs/Data
COPY Indices /srv/shiny-server/igfs/Indices
COPY adminLTE.css /srv/shiny-server/igfs/
COPY app.js /srv/shiny-server/igfs/
COPY google-analytics.js /srv/shiny-server/igfs/
COPY README.md /srv/shiny-server/igfs/
COPY server.R /srv/shiny-server/igfs/

#COPY igfs-data-explorer /srv/shiny-server/
COPY BOF /srv/shiny-server/igfsbof/
COPY COD /srv/shiny-server/igfscod/
COPY CUR /srv/shiny-server/igfscur/
COPY DAB /srv/shiny-server/igfsdab/
COPY DGS /srv/shiny-server/igfsdgs/
COPY ESB /srv/shiny-server/igfsesb/
COPY GUG /srv/shiny-server/igfsgug/
COPY HAD /srv/shiny-server/igfshad/
COPY HER /srv/shiny-server/igfsher/
COPY HKE /srv/shiny-server/igfshke/
COPY HOM /srv/shiny-server/igfshom/
COPY JOD /srv/shiny-server/igfsjod/
COPY LSD /srv/shiny-server/igfslsd/
COPY MAC /srv/shiny-server/igfsmac/
COPY MEG /srv/shiny-server/igfsmeg/
COPY MON /srv/shiny-server/igfsmon/
COPY NOP /srv/shiny-server/igfsnep/
COPY NOP /srv/shiny-server/igfsnop/
COPY PLE /srv/shiny-server/igfsple/
COPY POD /srv/shiny-server/igfspod/
COPY POK /srv/shiny-server/igfspok/
COPY POL /srv/shiny-server/igfspol/
COPY SDR /srv/shiny-server/igfssdr/
COPY SKT /srv/shiny-server/igfsskt/
COPY SOL /srv/shiny-server/igfssol/
COPY SPR /srv/shiny-server/igfsspr/
COPY THR /srv/shiny-server/igfsthr/
COPY WAF /srv/shiny-server/igfswaf/
COPY WHB /srv/shiny-server/igfswhb/
COPY WHG /srv/shiny-server/igfswhg/
COPY www /srv/shiny-server/igfs/www
#COPY shiny-server.conf /etc/shiny-server/
COPY ui.R /srv/shiny-server/igfs/
EXPOSE 3838
ENTRYPOINT [/srv/shiny-server/igfs/ui.R]
CMD ["/usr/bin/shiny-server.sh"]
