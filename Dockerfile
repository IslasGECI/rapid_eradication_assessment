FROM islasgeci/jupyter:3691
RUN pip install git+https://github.com/IslasGECI/descarga_datos.git@v0.1.0-beta
RUN add-apt-repository --yes ppa:ubuntugis/ubuntugis-unstable
RUN apt-get update && \
    apt-get install --yes \
        libgdal-dev \
        libproj-dev
RUN conda install --yes --channel conda-forge \
        libiconv \
        r-rgdal
RUN R -e "install.packages(c('lubridate', 'mc2d', 'optparse', 'spatstat'), repos = 'http://cran.rstudio.com')"