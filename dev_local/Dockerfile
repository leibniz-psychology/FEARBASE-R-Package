FROM opencpu/base:latest

# Install system headers before compiling R packages from source. ggiraph pulls in
# native graphics/text packages such as gdtools, systemfonts, and textshaping, so
# the image needs font, image, XML, SSL, curl, cairo, harfbuzz, and fribidi
# development libraries available during package installation. htmlwidgets also
# brings in rmarkdown through ggiraph, whose dependency chain compiles fs against
# libuv.
RUN apt-get update && apt-get install -y \
    cmake \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libfribidi-dev \
    libharfbuzz-dev \
    libjpeg-dev \
    libpng-dev \
    libssl-dev \
    libtiff5-dev \
    libuv1-dev \
    libxml2-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
    && rm -rf /var/www/html/index.html

# Enable the Apache modules used by OpenCPU for request routing and headers.
RUN a2enmod rewrite headers

# OpenCPU serves packages from its own library path, so install both package
# dependencies and the local package into that library.
ENV R_LIBS=/usr/lib/opencpu/library

RUN mkdir -p /usr/lib/opencpu/library

# Use a fixed CRAN mirror so non-interactive Docker builds never prompt for one.
RUN echo "options(repos = c(CRAN = 'https://cloud.r-project.org'))" \
    >> /usr/lib/R/etc/Rprofile.site

COPY . /home/opencpu/fearbase

# Install only the package runtime dependency set declared in DESCRIPTION. This
# includes ggiraph while avoiding unrelated local-analysis packages that make the
# compose build slow and fragile.
RUN R -e "install.packages('remotes', lib = '/usr/lib/opencpu/library'); \
          remotes::install_deps('/home/opencpu/fearbase', \
            lib = '/usr/lib/opencpu/library', \
            dependencies = c('Depends', 'Imports', 'LinkingTo'), \
            upgrade = 'never')"

RUN R CMD INSTALL --library=/usr/lib/opencpu/library /home/opencpu/fearbase
