FROM appsilon/rstudio:3.5.0_3

RUN apt-get update && apt-get install -y \
  libpq-dev \
  ssh

COPY install_packages.R /tmp/install_packages.R
RUN R -f /tmp/install_packages.R

RUN cp /usr/share/zoneinfo/Europe/Warsaw /etc/localtime

COPY .Rprofile /home/rstudio/.Rprofile

CMD ["/init"]
