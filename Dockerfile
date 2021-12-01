FROM openanalytics/r-base:4.0.4

RUN apt-get update && apt-get install -y \
sudo \
pandoc \
pandoc-citeproc \
libcurl4-gnutls-dev \
libcairo2-dev \
libxt-dev \
libssl-dev \
libssh2-1-dev \
libxml2 \
libxml2-dev \
gdebi-core 

# install shiny server
RUN wget --no-verbose https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/VERSION -O "version.txt" && \
VERSION=$(cat version.txt)  && \
wget --no-verbose "https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
gdebi -n ss-latest.deb && \
rm -f version.txt ss-latest.deb

# install R packages required 
RUN R -e "install.packages('remotes')"
RUN R -e "remotes::install_version('yaml', version='2.2.1')"
RUN R -e "remotes::install_version('readxl', version='1.3.1')"
RUN R -e "remotes::install_version('data.table', version='1.14.2')"
RUN R -e "remotes::install_version('purrr', version='0.3.4')"
RUN R -e "remotes::install_version('highcharter', version='0.8.2')"
RUN R -e "remotes::install_version('shiny', version='1.7.1')"
RUN R -e "remotes::install_version('shinydashboard', version='0.7.2')"
RUN R -e "remotes::install_version('argonR', version='0.2.0')"
RUN R -e "remotes::install_version('argonDash', version='0.2.0')"

# config and code
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY shiny-server.sh /usr/bin/shiny-server.sh
WORKDIR /srv/shiny-server/
  RUN rm -rf *
  COPY . .
RUN mkdir -p data 
RUN chmod -R 777 data
RUN mkdir -p admin 
RUN chmod a+rw admin 
  
  
# expose and show
EXPOSE 8080
CMD ["/usr/bin/shiny-server"]