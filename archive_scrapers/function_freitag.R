require(RSelenium)
require(magrittr)

.process_html <- function(src, slug) {
    psrc <- rvest::read_html(src)
    titles <- rvest::html_elements(psrc, css = "a.js-article-card-url")
    title <- titles %>% rvest::html_text() %>% stringr::str_trim()
    link <- titles %>% rvest::html_attr("href")
    return(tibble::tibble(title = title, link = link, slug = slug))
}

## slug <- slugs[4]
## url <- paste0(base_url, slugs[4])
## remDr$navigate(url)

.scrape <- function(slug, max_pages = 30, debug = FALSE) {
    rD <- RSelenium::rsDriver(browser = "firefox", port = sample(c(#5678L, #5679L, 
                                                                   5680L, #5681L, 
                                                                   5682L), size = 1), check = FALSE, verbose = FALSE)
    remDr <- rD[["client"]]

    url <- paste0("https://www.freitag.de/", slug)
    remDr$navigate(url)
    x <- readline("Please click the cookies thing.")
    res <- tibble::tibble()
    counter <- 1
    cont <- TRUE
    while(cont) {
        Sys.sleep(2)
        if (debug) {
            message("Flip")
        }
        click_next <- tryCatch({
            webElem <- remDr$findElement(using = "css", "a[title='Next Page']")
            webElem$clickElement()
            TRUE
        }, error = function(e) {
            return(FALSE)
        })
        if (click_next) {
            counter <- counter + 1
        } else {
            cont <- FALSE
        }
        src <- remDr$getPageSource()[[1]]
        links <- .process_html(src, slug)
        res <- dplyr::bind_rows(res, links)
        if (counter > max_pages) {
            cont <- FALSE
        }
    }
    remDr$close()
    z <- rD$server$stop()
    return(res)
}


valid_links_1 <- purrr::map_dfr("politik", .scrape, max_pages = 35)
valid_links_2 <- purrr::map_dfr("wirtschaft", .scrape, max_pages = 35)
valid_links_3 <- purrr::map_dfr("kultur", .scrape, max_pages = 35)
valid_links_4 <- purrr::map_dfr("gruenes-wissen", .scrape, max_pages = 35)
valid_links_5 <- purrr::map_dfr("debatte", .scrape, max_pages = 35)

dplyr::distinct(rbind(valid_links_1, valid_links_2, valid_links_3, 
                      valid_links_4, valid_links_5)) -> valid_links



valid_links %>% dplyr::rename(title = title, link = link) %>% 
  dplyr::mutate(pub = "Freitag", description = NA, pubdate = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links


saveRDS(valid_links, "Freitag.RDS")



