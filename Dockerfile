FROM postgres:latest

#Copy Resources
ADD src/main/R/setup_library.R /root/setup_library.R
ADD src/main/R/paillier_functions.R /root/Rprofile.site
ADD src/main/sql/setup_udfs.sql /root/setup_udfs.sql
WORKDIR /root

#Install libraries
RUN apt-get update
RUN apt-get -y install r-base
RUN apt-get -y install libgmp-dev
RUN apt-get -y install libsodium-dev
RUN apt-get -y install git
RUN apt-get -y install make
RUN apt-get -y install libpq-dev
RUN apt-get -y install postgresql-server-dev-all
RUN apt-get -y install postgresql-common

#Setup R libraries and functions
RUN Rscript --verbose setup_library.R
RUN cp Rprofile.site /etc/R/Rprofile.site

#Setup plr
RUN cp setup_udfs.sql /docker-entrypoint-initdb.d
RUN git clone https://github.com/postgres-plr/plr.git
WORKDIR plr
RUN USE_PGXS=1 make
RUN USE_PGXS=1 make install




