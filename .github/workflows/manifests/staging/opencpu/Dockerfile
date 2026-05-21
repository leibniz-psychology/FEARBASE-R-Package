FROM crzpiddev.azurecr.io/fearbase/ocpu-base-image:latest-dev

ARG OCPU_PASS
ARG SSL_KEY

RUN apt update -y &&\
    apt install -y vim

RUN cd /home/opencpu && \
    apt-get update && \
    apt-get install -y openssl && \
    openssl req -x509 -newkey rsa:4096 -sha256 -days 3650 -nodes \
      -keyout fearbase-ocpu_ssl.key -out fearbase-ocpu_ssl.crt -subj "/CN=fearbase-ocpu.stg.zpid.org" \
      -addext "subjectAltName=DNS:fearbase-ocpu.stg.zpid.org,DNS:www.fearbase-ocpu.stg.zpid.org,IP:127.0.0.1"

RUN cd /home/opencpu && \
    mv /home/opencpu/fearbase-ocpu_ssl.crt /etc/ssl/certs/ && \
    mv /home/opencpu/fearbase-ocpu_ssl.key /etc/ssl/private/

#copy some config files
COPY . /home/opencpu/fearbase
COPY .github/workflows/manifests/development/opencpu/index.html  /var/www/html/index.html

RUN cd /home/opencpu && \
    R -e "install.packages('remotes')" && \
    R CMD build fearbase && \
    mv fearbase_*.tar.gz fearbase_latest.tar.gz && \
    R CMD INSTALL fearbase_latest.tar.gz --library=/usr/local/lib/R/site-library && \
    chmod 755 /var/www/html/index.html

# Set opencpu password
RUN \
  echo "opencpu:"$OCPU_PASS | chpasswd






