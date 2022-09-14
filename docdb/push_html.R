require(mongolite)

##
## Sys.setenv(ARTICLE_DIR='~/dev/fodira/html')

fs <- mongolite::gridfs(db = "main", prefix = "html")

fs$upload(list.files(Sys.getenv("ARTICLE_DIR"), full.names = TRUE))

## x <- fs$find()

## out <- fs$read("000ebbd648f60b8da1064a94936d46be2821679f_2022-09-09_14:26:14.html", file("test.html"))
