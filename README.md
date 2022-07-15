# Data collection scripts

## envvars

The project depends on the following envvars. Please set these envvars in the *rc file

1. `TWITTER_BEARER`: Twitter Bearer Token
2. `TWEET_DB`: __Absolute path__ to the DuckDB file holding all tweets
3. `ARTICLE_DB`: __Absolute path__ to the DuckDB file holding meta data of scraped articles
3. `ARTICLE_DIR`: __Absolute path__ to the directory holding html files

The project needs R (for most of the data collection) and node (for [readability](https://github.com/mozilla/readability)).

