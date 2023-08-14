FROM rocker/geospatial:dev-osgeo

RUN R -e "install.packages('littler', dependencies=TRUE)"

RUN R -e "install.packages('aws.s3', dependencis=TRUE)"

RUN R -e "install.packages('future', dependencies=TRUE)"

RUN R -e "install.packages('furrr', dependencies=TRUE)"
RUN R -e "install.packages('devtools', dependencies=TRUE)"

RUN install2.r --error --deps TRUE methods

COPY ./rstudio-prefs.json /home/rstudio/.config/rstudio/rstudio-prefs.json



