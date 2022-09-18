require(RSelenium)
require(magrittr)

#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
jf_getlink <- function(html){
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'ee-grid__item ee-loop__item')]//a[contains(@class, 'ee-post__title')]") %>% 
    rvest::html_text(trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'ee-grid__item ee-loop__item')]//a[contains(@class, 'ee-post__title')]") %>% 
    rvest::html_attr("href") -> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'ee-grid__item ee-loop__item')]//li[contains(@class, 'ee-post__meta ee-post__meta--date ee-post__metas__date')]") %>% 
    rvest::html_text(., trim = TRUE) %>% stringr::str_extract("[0-9]+[.] [A-Za-zäöü]+ [0-9]+") %>%
    stringr::str_replace(., "März", "March") %>%
    #as.Date(., tryFormat = c("%d. %B %Y")) 
    lubridate::dmy() -> item_pubdate
  
  df <- data.frame(item_title, item_link, item_pubdate)
  return(df)
}

jf_go_thr_2022 <- function(startpage){
  rD <- RSelenium::rsDriver(browser = "firefox", port = sample(c(#5678L, 
    5679L, 5680L, 5681L, 5682L), size = 1), check = FALSE, verbose = FALSE)
  remDr <- rD[["client"]]
  Sys.sleep(5)
  remDr$navigate(paste0(startpage, "page/25/"))
  print(startpage)
  rvest::read_html(remDr$getPageSource()[[1]]) %>% 
    rvest::html_elements(xpath = "head/title") %>% 
    rvest::html_text(., trim = TRUE) %>% stringr::str_extract_all(pattern = "[0-9]+") -> end
   
  valid_links <- data.frame()
  for (i in 1:end[[1]][3]) {
    paste0(startpage, "page/", i, "/") %>% remDr$navigate()
    jf_getlink(remDr$getPageSource()[[1]]) -> subset_links
    valid_links <- rbind(valid_links, subset_links)
    print(remDr$getCurrentUrl())
    
  }
  
  remDr$close()
  z <- rD$server$stop()
  return(valid_links)
}

jf_go_thr_2022("https://jungefreiheit.de/2022/") -> valid_links

jf_getlink_url <- function(url){
  remDr$navigate(url)
  print(url)
  return(jf_getlink(remDr$getPageSource()[[1]]))
}

###which one is better?

jf_go_thr_archive <- function(startdate){
  seq(as.Date(startdate), Sys.Date(), by="days") %>% 
     stringr::str_replace_all(., "-", "/")-> V1
  
  rD <- RSelenium::rsDriver(browser = "firefox", port = sample(c(#5678L, 
    #5679L, #5680L, #5681L, 
    5682L), size = 1), check = FALSE, verbose = FALSE)
  remDr <- rD[["client"]]
  Sys.sleep(5)
  valid_links <- data.frame()
  
  for (i in 1:length(V1)) {
    remDr$navigate(paste0("https://jungefreiheit.de/", V1[i], "/"))
    print(paste0("https://jungefreiheit.de/", V1[i], "/"))
    jf_getlink(remDr$getPageSource()[[1]]) -> subset_links
    valid_links <- rbind(valid_links, subset_links)
  }
  remDr$close()
  z <- rD$server$stop()
  return(valid_links)
}


jf_go_thr_archive("2021-12-01") -> valid_links2

valid_links <- dplyr::distinct(rbind(valid_links2, valid_links))

valid_links %>% dplyr::rename(title = item_title, link = item_link, pubdate = item_pubdate) %>% 
  dplyr::mutate(pub = "Junge Freiheit", description = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links


saveRDS(valid_links, "Junge Freiheit.RDS")
