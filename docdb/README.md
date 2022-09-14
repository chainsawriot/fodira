# Input format

The output of the RSS aggregator or the archive scrapers should have the following format:

1. pub (e.g. freitag)
2. link
3. pubdate (optional, UTC)
4. title (optional)
5. description (optional)

# Setup

Please setup MongoDB Community Edition (`mongodb-org`) according to [this official guide](https://www.mongodb.com/docs/manual/tutorial/install-mongodb-on-ubuntu/).

After installation, please change `/etc/mongod.conf` and point the `dbPath` to somewhere else. For the main `fodira` server should be `/data/mongodb`.

Enable MongoDB as a service

```sh
sudo systemctl enable mongod
```

Content is stored in the collection `main.articles`, whereas html files are stored inside the `main.html` [GridFS](https://www.mongodb.com/docs/manual/core/gridfs/).

# Usage

To insert links to the DB (provided that the RDS file is in the abovementioned format)

```sh
chmod +x insert
../rss/rss_link_grab ../rss/rss_list.csv ./output.RDS
./insert output.RDS
rm output.RDS
```

# DuckDB to MongoDB: A migration guide

Previous version of the database was in DuckDB. Due to scalability concerns, the database has been migrated to MongoDB.

The steps to migrate:

1. Export the DuckDB to CSV

```sh
duckdb -c "copy articles to 'articles.csv' (header, delimiter ',');" articles.duckdb
```

2. Manipulate the csv with the following: We don't need `id`, `insert_timestamp`; also, replace the space in `htmlfile` with underscore

```sh
Rscript -e 'library(magrittr); rio::import("articles.csv") %>% dplyr::select(-id, -insert_timestamp) %>% dplyr::mutate(htmlfile = stringr::str_replace(htmlfile, " ", "_")) %>% rio::export("articles_clean.csv")'
```

2. Import the CSV to the database `articles` in MongoDB

```sh
mongoimport --type csv -d main -c articles --headerline --drop articles_clean.csv
```

3. We needs to make certain changes to the "schema"

```sh
mongosh
```

Thanks to the schemalessness of MongoDB, we need to unset `pubdate` if it is empty.

```js
use main
db.articles.updateMany({"pubdate": ""}, { $unset: {"pubdate": "" }} );
```

Convert `pubdate` to ISODate

```js
db.articles.updateMany({}, [{"$set": {"pubdate": { "$toDate": "$pubdate"}}}]);
```

Create the Indexes at `pubdate` and `pub`

```js
db.articles.createIndex({"pubdate": 1})
db.articles.createIndex({"pub": 1})
```

Remove all records with `link` being empty and create unique index

```js
db.articles.deleteMany({"link": ""})
db.articles.createIndex({"link": 1}, {"unique": true})
```

Now it is fine at the raw migration of the data.

```js
db.articles.find({"pub": "Heute"});
```

4. Renaming HTMLs

MongoDB's GridFS doesn't support spaces in filename.

```sh
cd $ARTICLE_DIR
for file in *; do mv "$file" `echo $file | tr ' ' '_'` ; done
```

5. Upload HTMLs to GridFS

```sh
Rscript push_html.R
```
