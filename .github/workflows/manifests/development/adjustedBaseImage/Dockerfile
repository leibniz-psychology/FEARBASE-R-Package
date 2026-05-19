FROM opencpu/base:latest

ARG OCPU_PASS

#install needed dependenciesp
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev libcairo2-dev\
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
RUN R -e "install.packages('tidyverse')"
RUN R -e "install.packages('patchwork')"
RUN R -e "install.packages('lme4')"
RUN R -e "install.packages('lmerTest')"
RUN R -e "install.packages('psych')"
RUN R -e "install.packages('gsheet')"
RUN R -e "install.packages('plotly')"
RUN R -e "install.packages('ggridges')"
RUN R -e "install.packages('ggiraph')"
RUN R -e "install.packages('shadowtext')"
RUN R -e "install.packages('broom.mixed')"
RUN R -e "install.packages('ggrepel')"







