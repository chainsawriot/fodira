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

CREATE TABLE failedurls(
link VARCHAR,
insert_timestamp TIMESTAMP NOT NULL DEFAULT NOW()
);
