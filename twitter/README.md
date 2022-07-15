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

If there is no data in the DB, `twitter_grab` will collect data from 2021-12-31 up to now. The next time it runs, it will only collect data from the last 30 days.

If we run it daily, we will at least have the like counts / comment counts etc. of a tweet in 30 days.

To loop through handles in a text file, e.g. poli.txt

```sh
while read p; do
  ./twitter_grab $p
done <poli.txt
```

