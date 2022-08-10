## debug
Sys.setenv(ARTICLE_DB="~/dev/fodira/docdb/doc.duckdb")

if (Sys.getenv("ARTICLE_DB") == "") {
    stop("Please set up `ARTICLE_DB` envvar!")
}

if (file.exists(Sys.getenv('ARTICLE_DB'))) {
    stop("DB exists! If you want to rebuild the DB, please delete the old one first.")
}

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = Sys.getenv('ARTICLE_DB'), read_only = FALSE)
DBI::dbExecute(con, "CREATE SEQUENCE serial START 1;")
DBI::dbExecute(con, paste(readLines("create_table.sql"), collapse = ""))
DBI::dbExecute(con, "CREATE INDEX link_index ON articles (link);")
DBI::dbExecute(con, "CREATE INDEX pub_index ON articles (pub);")
DBI::dbDisconnect(con, shutdown = TRUE)

