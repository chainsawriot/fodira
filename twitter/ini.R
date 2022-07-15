## debug
## Sys.setenv(TWEET_DB="~/dev/fodira/twitter/test.duckdb")
## Sys.setenv(TWEET_DB=":memory:")

if (Sys.getenv("TWEET_DB") == "") {
    stop("Please set up `TWEET_DB` envvar!")
}

if (file.exists(Sys.getenv('TWEET_DB'))) {
    stop("DB exists! If you want to rebuild the DB, please delete the old one first.")
}

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = Sys.getenv('TWEET_DB'), read_only = FALSE)
sql_statement <- paste(readLines(system.file("extdata", "seq.sql", package = "academicquacker")), collapse = "")
DBI::dbExecute(con, paste(readLines(system.file("extdata", "seq.sql", package = "academicquacker")), collapse = ""))
DBI::dbExecute(con, paste(readLines(system.file("extdata", "schema_timestamp.sql", package = "academicquacker")), collapse = ""))
DBI::dbExecute(con, "CREATE INDEX un_index ON tweets (user_username);")

DBI::dbDisconnect(con, shutdown = TRUE)
