# Data collection scripts for FoDiRa

<!-- badges: start -->
[![R-CMD-check](https://github.com/chainsawriot/fodira/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/chainsawriot/fodira/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## envvars

The project depends on the following envvars. Please set these envvars in the *rc file

1. `TWITTER_BEARER`: Twitter Bearer Token
2. `TWEET_DB`: __Absolute path__ to the DuckDB file holding all tweets
3. `ARTICLE_DIR`: __Absolute path__ to the *temporary* directory holding html files
4. `FODIRA_HOST`: For workers: ssh server string

The project needs R (for most of the data collection) and node (for [readability](https://github.com/mozilla/readability)).

## Server/worker setup guide

It is better to use Ubuntu 20.04 LTS at the moment, due to the installation issues of MongoDB on 22.04 LTS.

1. System dependecies

```sh
sudo apt update -qq
sudo apt install libcurl4-openssl-dev libssl-dev libxml2-dev libfontconfig1-dev libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev libssl-dev libsasl2-dev software-properties-common dirmngr libssh-dev -y
```

2. R packages

**DON'T USE THE r-core provided by Ubuntu; it is currently version pre-4** (#14)

Install R according to [this guide](https://cran.r-project.org/bin/linux/ubuntu/)

From the guide: 

```sh
# update indices
wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
# add the R 4.0 repo from CRAN -- adjust 'focal' to 'groovy' or 'bionic' as needed
sudo add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/" -y
sudo apt install --no-install-recommends r-base-dev -y
```

And then

```r
install.packages(c("tidyverse", "rio", "remotes", "tidyRSS", "mongolite", "docopt"))
remotes::install_github("chainsawriot/fodira")
```

### Installation of MongoDB (Server only)

Install Mongodb according to [this guide](https://www.mongodb.com/docs/manual/tutorial/install-mongodb-on-ubuntu/)

```sh
sudo mkdir /data/mongodb
sudo chown -R mongodb:mongodb /data/mongodb
```

Edit the config file `/etc/mongod.conf` to point dbPath to `/data/mongodb`

Start the service

```sh
sudo systemctl enable mongod
```

### Page scraping

1. Install Firefox

**DON'T USE THE SNAP PACKAGE**

Install Firefox from the offical Mozilla PPA

```sh
sudo add-apt-repository ppa:mozillateam/ppa
sudo apt install firefox
firefox --version # testing
```

2. Install JRE, JDK, and rJava

```sh
sudo apt-get install -y default-jre
sudo apt-get install -y default-jdk
sudo R CMD javareconf
Rscript -e "install.packages(c('rJava', 'RSelenium'))"
```

3. Install the RSelenium binary

```R
ff_options <- list("moz:firefoxOptions" = list(args = list('--headless')))

rD <- RSelenium::rsDriver(browser = "firefox", port = sample(c(5678L, 5679L, 5680L, 5681L, 5682L), size = 1), check = TRUE, verbose = FALSE,
                          extraCapabilities = ff_options)
##rD <- RSelenium::rsDriver(browser = "firefox", port = sample(c(5678L, 5679L, 5680L, 5681L, 5682L), size = 1), check = TRUE, verbose = TRUE,
##                          extraCapabilities = ff_options)

## becareful of this issue
## https://github.com/ropensci/wdman/issues/31#issuecomment-1336651660

z <- rD$server$stop()
```
