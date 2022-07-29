require(RSelenium)
require(magrittr)
rD <- RSelenium::rsDriver(browser = "firefox", port = sample(c(5678L, 5679L, 5680L, 5681L, 5682L), size = 1), check = FALSE, verbose = FALSE)
remDr <- rD[["client"]]

#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
dwn_getlink <- function(html){
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//dl/dd") %>% length() -> j
  
  item_title <- c()
  item_link <- c()
  item_pubdate <- c(as.Date(""))
  
  for (i in 1:j) {
    rvest::read_html(html) %>% 
      rvest::html_elements(xpath = paste0("//dl/dd[", i, "]/a[contains(@class, 'title')]")) %>% 
      rvest::html_text(., trim = TRUE) %>% c(item_title, .)-> item_title
    rvest::read_html(html) %>% 
      rvest::html_elements(xpath = paste0("//dl/dd[", i, "]/a[contains(@class, 'title')]")) %>% 
      rvest::html_attr("href") -> item_link_ 
    item_link <- c(item_link, item_link_)
    rvest::read_html(html) %>% 
      rvest::html_elements(xpath = paste0("//dl/dt[", i, "]")) %>% 
      rvest::html_text(., trim = TRUE) %>%
      as.Date(., tryFormat=c("%d.%m.%y")) %>% rep(. , length(item_link_)) -> item_pubdate_
    item_pubdate <- c(item_pubdate, item_pubdate_)
    ?rep
  }
    df <- data.frame(item_title, item_link, item_pubdate = item_pubdate[2:length(item_pubdate)])
    return(df)
}

dwn_getlink_url <- function(url){
  remDr$navigate(url)
  print(url)
  return(dwn_getlink(remDr$getPageSource()[[1]]))
}


dwm_go_thr_search <- function(url){
  valid_links <- data.frame()
  remDr$navigate(url)
  remDr$getPageSource()[[1]] %>% rvest::read_html() %>% 
      rvest::html_elements(xpath = paste0("//p[last()]/a[contains(@class, 'pager')][last()]")) %>% 
      rvest::html_text(., trim = TRUE) %>% as.numeric() -> nr_
  for (i in 1:nr_) {
    dwn_getlink_url(paste0(url, "&page=", i)) -> subset_links
    valid_links <- rbind(valid_links, subset_links)
  } 
  return(valid_links)
}


dwm_go_thr_search("https://deutsche-wirtschafts-nachrichten.de/search?lex=0&search=&match=0&author=&d1=1&m1=1&y1=2022&d2=28&m2=7&y2=2022&act=yes") -> valid_links


valid_links <- dplyr::distinct(valid_links)


remDr$close()
z <- rD$server$stop()
