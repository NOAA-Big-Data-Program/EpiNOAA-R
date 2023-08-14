#!/bin/bash

docker build -t arc_package .

docker run --rm \
    -ti \
    -e PASSWORD=esds_ncsu \
    -v "${PWD}"/../:/home/rstudio/workspace \
    -v $HOME/.aws/:/home/rstudio/.aws \
    -p 8080:8787 \
    arc_package
