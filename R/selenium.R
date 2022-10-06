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

.scrape <- function(url, selen, output_dir = Sys.getenv("ARTICLE_DIR"), sleep = sample(seq(0, 1, .1), size = 1), write = TRUE, verbose = FALSE) {
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
    return(tibble::tibble(url = url, fname = fname))
}

#' @export
scrape <- function(urls, selen, output_dir = Sys.getenv("ARTICLE_DIR"), sleep = 1, write = TRUE, verbose = FALSE, close_selen = FALSE) {
    res <- purrr::map(urls, purrr::safely(.scrape), selen = selen, output_dir = output_dir, sleep = sleep, write = write, verbose = verbose) %>% purrr::discard(~!is.null(.$error)) %>% purrr::map("result") %>% dplyr::bind_rows()
    if (close_selen) {
        selen$remDr$close()
        z <- selen$rD$server$stop()
    }
    return(res)
}
