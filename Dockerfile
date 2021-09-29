FROM rocker/shiny-verse:4.1.1
RUN apt-get update && apt-get install -y \
  libudunits2-dev \
  libgdal-dev \
  gdal-bin \
  libgeos-dev \
  libproj-dev
RUN R -e 'install.packages(c("sf", "lwgeom"),\
  repos = "https://packagemanager.rstudio.com/all/2021-09-27+Y3JhbiwyOjQ1MjYyMTU7NjZEOEIzRUI")'
COPY ./app/* /srv/shiny-server/
CMD ["/usr/bin/shiny-server"]