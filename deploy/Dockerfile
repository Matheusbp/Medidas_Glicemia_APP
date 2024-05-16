FROM rocker/verse:4.2.3
RUN apt-get update -y && apt-get install -y  libicu-dev  make  libssl-dev libcurl4-openssl-dev git  libcurl4-openssl-dev libssl-dev  zlib1g-dev  libfreetype6-dev libfribidi-dev libharfbuzz-dev libfontconfig1-dev git pandoc libicu-dev libjpeg-dev libpng-dev libtiff-dev make zlib1g-dev libxml2-dev libssl-dev libgit2-dev libcurl4-openssl-dev  libicu-dev make zlib1g-dev pandoc  libssl-dev make libcurl4-openssl-dev  libssl-dev libgit2-dev libcurl4-openssl-dev git  git libssl-dev libcurl4-openssl-dev  git  make zlib1g-dev  libicu-dev libssl-dev libcurl4-openssl-dev  libicu-dev make pandoc  libssl-dev libcurl4-openssl-dev  libssl-dev  pandoc  libfreetype6-dev libfribidi-dev libharfbuzz-dev libfontconfig1-dev pandoc libicu-dev libjpeg-dev libpng-dev libtiff-dev libxml2-dev make libssl-dev libcurl4-openssl-dev  libfreetype6-dev libfribidi-dev libharfbuzz-dev libfontconfig1-dev libjpeg-dev libpng-dev libtiff-dev  pandoc libicu-dev make  libicu-dev libxml2-dev make pandoc  libxml2-dev libcurl4-openssl-dev libssl-dev  libicu-dev libxml2-dev libssl-dev libcurl4-openssl-dev  libfontconfig1-dev libfreetype6-dev  libfreetype6-dev libfribidi-dev libharfbuzz-dev libfontconfig1-dev  libfreetype6-dev libfribidi-dev libharfbuzz-dev libfontconfig1-dev pandoc libicu-dev libjpeg-dev libpng-dev libtiff-dev zlib1g-dev make libxml2-dev libssl-dev libcurl4-openssl-dev  git libssl-dev make libgit2-dev libcurl4-openssl-dev  libxml2-dev && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(renv.config.pak.enabled = F, repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'utils::install.packages(c("renv","remotes"))'
COPY renv.lock renv.lock
RUN R -e 'renv::restore()'

COPY MedidasGlicemia_*.tar.gz /app.tar.gz
RUN R -e 'remotes::install_local("/app.tar.gz",upgrade="never")'
RUN rm /app.tar.gz
EXPOSE 3839
CMD R -e "options('shiny.port'=3839,shiny.host='0.0.0.0');library(MedidasGlicemia);MedidasGlicemia::run_app()"
