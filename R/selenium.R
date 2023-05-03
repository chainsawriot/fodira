#' Generate an instance of self-contained Selenium instance
#'
#' @param headless whether to generate a headless instance (no GUI)
#' @return an instance of Selenium instance, a list containing both the Selenium Server and the Remote Driver
#' @export
gen_selen <- function(headless = TRUE) {
    ff_options <- list()
    if (headless) {
        ff_options <- list("moz:firefoxOptions" = list(args = list('--headless')))
    }
    rD <- RSelenium::rsDriver(browser = "firefox", port = sample(c(5678L, 5679L, 5680L, 5681L, 5682L), size = 1), check = FALSE, verbose = FALSE,
                          extraCapabilities = ff_options)
    remDr <- rD[["client"]]
# 20s
    remDr$setTimeout(type = "page load", milliseconds = 20000)
    return(list(rD = rD, remDr = remDr))
}

.scrape <- function(url, selen, output_dir = Sys.getenv("ARTICLE_DIR"), sleep = sample(seq(0, 1, .1), size = 1), write = TRUE, verbose = FALSE, push = FALSE, db = "main", collection = "articles", prefix = "html", delete = TRUE) {
    if (verbose) {
        message(url)
    }
    if (is.na(url)) {
        return(tibble::tibble(url = NA, fname = NA))
    }
    selen$remDr$navigate(url)
    src <- selen$remDr$getPageSource()
    url_hash <- digest::sha1(url, digits = 40)
    current_time <- gsub(" ", "_", Sys.time())
    fname <- paste0(url_hash, "_" ,current_time, ".html")
    if (write) {
        writeLines(src[[1]], file.path(output_dir, fname))
    } else {
        if (verbose) {
            print(file.path(output_dir, fname))
        }
    }
    Sys.sleep(sleep)
    if (push) {
        if (verbose) {
            cat(paste("Pushing:", fname, "\n"))
        }
        z <- push_html(url = url, fname = fname, output_dir = output_dir, db = db, collection = collection, prefix = prefix, delete = delete)
    }
    return(tibble::tibble(url = url, fname = fname))
}

#' Scrape urls and put it in the output directory
#'
#' @param urls a vector or URLs
#' @param selen an instance of Selenium from [gen_selen()]. If it is NULL, a new instance is generated and close automatically, i.e. `close_selen` is TRUE
#' @param output_dir a directory to hold HTML files
#' @param sleep sleep time between each collection
#' @param write whether to really write the HTML file
#' @param verbose whether to display debug information
#' @param close_selen whether to close the Selenium instance, to TRUE if `selen` is null
#' @param headless whether to generate a headless instance, if `selen` is null
#' @param push whether to update the url and push the html file to the DB
#' @return a dataframe with urls and filenames; if `write` is TRUE, HTML files are written to `output_dir`. All failed urls will be skipped.
#' @export
#' @inheritParams push_html
scrape <- function(urls, selen = NULL, output_dir = Sys.getenv("ARTICLE_DIR"), sleep = 1, write = TRUE, verbose = FALSE, close_selen = FALSE, headless = TRUE, push = FALSE, db = "main", collection = "articles", prefix = "html", delete = TRUE) {
    if (is.null(selen)) {
        selen <- gen_selen(headless = headless)
        close_selen <- TRUE
    }
    res <- purrr::map(urls, purrr::safely(.scrape), selen = selen, output_dir = output_dir, sleep = sleep, write = write, verbose = verbose, push = push, db = db, collection = collection, prefix = prefix, delete = delete) %>% purrr::discard(~!is.null(.$error)) %>% purrr::map("result") %>% dplyr::bind_rows()
    if (close_selen) {
        selen$remDr$close()
        z <- selen$rD$server$stop()
    }
    return(res)
}
