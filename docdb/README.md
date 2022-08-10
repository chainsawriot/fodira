# Input format

The output of the RSS aggregator or the archive scrapers should have the following format:

1. pub (e.g. freitag)
2. link
3. pubdate (optional, UTC)
4. title (optional)
5. description (optional)

```sql
CREATE TABLE articles(
id UBIGINT PRIMARY KEY DEFAULT nextval('serial'),
pub VARCHAR,
link VARCHAR,
pubdate TIMESTAMP,
title VARCHAR,
description VARCHAR,
htmlfile VARCHAR,
text VARCHAR,
insert_timestamp TIMESTAMP NOT NULL DEFAULT NOW()
);
```

AID is articles:id

```sql
create table paywall(
id UBIGINT PRIMARY KEY DEFAULT nextval('serial'),
aid UBIGINT,
paywall boolean,
insert_timestamp TIMESTAMP NOT NULL DEFAULT NOW()
)
```

# Usage

```sh
Rscript ini.R
```

