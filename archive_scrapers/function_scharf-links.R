require(RSelenium)
require(magrittr)
rD <- RSelenium::rsDriver(browser = "firefox", port = sample(c(5678L, 5679L, 5680L, 5681L, 5682L
                                                               ), size = 1), check = FALSE, verbose = FALSE)
remDr <- rD[["client"]]

#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")



#function for geting links from page
sch_li_getlink <- function(html){
  #html <- remDr$getPageSource()[[1]]
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, article)]//h3//a[contains(@itemprop, url)]//span[contains(@itemprop, headline)]") %>% 
    rvest::html_text(trim = TRUE) -> item_title
  #item_title <- item_title[!stringr::str_detect(item_title, "Weiter lesen>>")]
  
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, article)]//h3//a[contains(@itemprop, url)]") %>% 
    rvest::html_attr("href") %>% paste0("http://www.scharf-links.de", .) -> item_link
  #item_link <- unique(item_link)
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, article)]//time[contains(@itemprob, datePublished)]") %>% 
    rvest::html_text(trim = TRUE) %>% 
    lubridate::mdy() -> item_pubdate
  
  df <- data.frame(item_title, item_link, item_pubdate)
    return(df)
}

sch_li_goget <- function(url){
  remDr$navigate(paste0("http://www.scharf-links.de", url))
  return(sch_li_getlink(remDr$getPageSource()[[1]]))
}


sch_li_go_thr_columns <- function(years = c(2022, 2023, 2024, 2025)){
  
  remDr$navigate("https://www.scharf-links.de/archiv")
  html <- remDr$getPageSource()[[1]]
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//ul/li/ul/li/a") %>% 
    rvest::html_attr("href") %>% 
    stringr::str_extract(pattern = paste0(paste0("^.*archiv.*", years, ".*$"),
                                          collapse = "|")) ->links
  
  links <- links[!is.na(links)]
  
  links2 <- lapply(links, function(first_url) {
    sapply(2:40, function(page_num) {
      # Replace "seite-1" with the appropriate page number using str_replace
      stringr::str_replace(first_url, "seite", paste0("seite-", page_num))
    })
  })
  
  links2 <- unlist(links2)
  
  # links3 <- c("/news",
  #             paste0("/news/seite-", 2:100))
  
  links3 <- c(links, links2)
  
  links3 %>%
    purrr::map_df(~sch_li_goget(.)) %>%
    dplyr::distinct() -> valid_links

  return(valid_links)
}

sch_li_go_thr_columns(years = c(2022, 2023, 2024, 2025)) -> valid_links

valid_links <- dplyr::distinct(valid_links)


valid_links %>% dplyr::rename(title = item_title, link = item_link, pubdate = item_pubdate) %>% 
  dplyr::mutate(pub = "scharf links", description = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links


saveRDS(valid_links, "scharf links.RDS")


remDr$close()
z <- rD$server$stop()

# 