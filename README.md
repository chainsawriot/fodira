# Data collection scripts

## envvars

The project depends on the following envvars. Please set these envvars in the *rc file

1. `TWITTER_BEARER`: Twitter Bearer Token
2. `TWEET_DB`: __Absolute path__ to the DuckDB file holding all tweets
3. `ARTICLE_DIR`: __Absolute path__ to the *temporary* directory holding html files

The project needs R (for most of the data collection) and node (for [readability](https://github.com/mozilla/readability)).

## Server setup guide (RSS Part)

It is better to use Ubuntu 20.04 LTS at the moment, due to the installation issues of MongoDB on 22.04 LTS.

1. System dependecies

```sh
sudo apt install libcurl4-openssl-dev libssl-dev libxml2-dev libfontconfig1-dev libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev libssl-dev libsasl2-dev -y
```

2. R packages

Install R according to [this guide](https://cran.r-project.org/bin/linux/ubuntu/)

And then

```r
install.packages(c("tidyverse", "rio", "devtools", "tidyRSS", "mongolite"))
```

3. Mongodb

Install Mongodb according to [this guide](https://www.mongodb.com/docs/manual/tutorial/install-mongodb-on-ubuntu/)

```sh
sudo mkdir /data/mongodb
sudo chown -R mongodb:mongodb /data/mongodb
```

Edit the config file `/etc/mongod.conf` to point dbPath to `/data/mongodb`

Start the service

```sh
sudo systemctl enable mongod`
```
