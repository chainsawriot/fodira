# Usage

First use: Run this to create the DuckDB file at `TWEET_DB` (envvar).

```sh
Rscript ini.R
```

And then, to get data from `@zdfheute`

```sh
chmod+x twitter_grab
./twitter_grab zdfheute
```

Actually, you can put more than one handle

```sh
./twitter_grab zdfheute tagesschau
```

To loop through handles in a text file, e.g. poli.txt

```sh
./twitter_grab < poli.txt
```

Or as an input text stream

```sh
cat poli.txt | ./twitter_grab
```

# Storage logic

If there is no data in the DB for a given handle, `twitter_grab` will collect data from 2021-12-31 up to now. The next time it runs, it will only collect data from the last 30 days for that handle.

If we run it daily, we will at least have the like counts / comment counts etc. of a tweet in 30 days.
