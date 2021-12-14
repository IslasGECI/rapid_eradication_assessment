FROM islasgeci/base:0.7.0
COPY . /workdir
RUN apt-get update && \
    apt-get install --yes \
        gdal-bin \
        libgdal-dev \
        libproj-dev \
        proj-bin
RUN Rscript -e "install.packages(c('sp', 'r-cran-rgdal', 'rgdal', dependencies = TRUE), repos = 'http://cran.rstudio.com')"
RUN Rscript -e "install.packages(c('lubridate', 'mc2d', 'optparse', 'spatstat'), repos = 'http://cran.rstudio.com')"
RUN Rscript -e "install.packages(c('covr', 'devtools', 'lintr', 'roxygen2', 'styler', 'testthat', 'vdiffr'), repos='http://cran.rstudio.com')"