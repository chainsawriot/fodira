# Usage

```sh
chmod +x page_dl
./page_dl https://www.tagesschau.de/inland/lauterbach-vierte-impfung-corona-103.html
```


If the first argument is `db`, it will fetch data from the `ARTICLE_DB` and scrape from there.

```sh
./page_dl db
```

Safe mode only scrape articles from "safe" sources

```sh
./page_dl db --safe
```
