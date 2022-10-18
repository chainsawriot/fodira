FROM rocker/tidyverse

RUN apt-get update -qq && apt-get -y --no-install-recommends install libcurl4-openssl-dev libssl-dev libxml2-dev libfontconfig1-dev libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev libssl-dev libsasl2-dev software-properties-common dirmngr git default-jre default-jdk libssh-dev && add-apt-repository ppa:mozillateam/ppa -y && apt install -y --no-install-recommends firefox

RUN R CMD javareconf && Rscript -e "install.packages(c('rJava', 'RSelenium', 'mongolite', 'remotes', 'docopt')); remotes::install_github('chainsawriot/fodira', dependencies = TRUE, upgrade = TRUE); z <- RSelenium::rsDriver(browser = 'firefox', port = sample(c(5683L), size = 1), check = TRUE, verbose = FALSE, extraCapabilities = list('moz:firefoxOptions' = list(args = list('--headless'))))"

ADD worker ./worker

CMD ["bash"]
