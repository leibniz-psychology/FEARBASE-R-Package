FROM opencpu/base:latest

#install needed dependenciesp
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    && apt install -y cmake \
    ## clean up
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/ \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
    && rm -rf /var/www/html/index.html

RUN cp /etc/apache2/mods-available/rewrite.load /etc/apache2/mods-enabled/ && \
    cp /etc/apache2/mods-available/headers.load /etc/apache2/mods-enabled/

RUN R -e "install.packages('devtools')"
RUN R -e "install.packages('remotes')"
RUN R -e "install.packages('ggplot2')"
RUN R -e "install.packages('meta')"
RUN R -e "install.packages('rlang')"
RUN R -e 'remove.packages("rlang", "/usr/lib/opencpu/library")'
RUN R -e 'remove.packages("vctrs", "/usr/lib/opencpu/library")'
RUN R -e "install.packages('vctrs')"

COPY . /home/opencpu/fearbase

RUN cd /home/opencpu && \
    R -e "install.packages('remotes')" && \
    R CMD build fearbase && \
    mv fearbase_*.tar.gz fearbase_latest.tar.gz && \
    R CMD INSTALL fearbase_latest.tar.gz --library=/usr/local/lib/R/site-library








