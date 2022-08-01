
# require(RSelenium)
# require(magrittr)
# rD <- RSelenium::rsDriver(browser = "firefox", port = sample(c(5678L, 5679L, 5680L, 5681L, 5682L), size = 1), check = FALSE, verbose = FALSE)
# remDr <- rD[["client"]]

require(webdriver)
require(magrittr)
pjs_instance <- run_phantomjs()
pjs_session <- Session$new(port = pjs_instance$port)

#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
suedd_getlink <- function(html){
 # html <- pjs_session$getUrl()
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'entrylist__content')]//a/em") %>% 
    rvest::html_text(., trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'entrylist__content')]//a") %>% 
    rvest::html_attr("href") -> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//time") %>%
    rvest::html_text(., trim = TRUE) -> item_pubdate1
  
  ifelse(stringr::str_detect(item_pubdate1, "[0-9]+[.][0-9]+[.][0-9]+"), 
           stringr::str_extract(item_pubdate1, "[0-9]+[.][0-9]+[.][0-9]+"),
         format.Date(Sys.Date(), format="%d.%m.%Y")) %>% 
    as.Date(., format="%d.%m.%Y") -> item_pubdate

    df <- data.frame(item_title, item_link, item_pubdate)
    
    return(df)
}

pjs_session$go("https://www.sueddeutsche.de/archiv/m%C3%BCnchen/2022/01")

pjs_session$getUrl()

suedd_getlink_url <- function(url, startdate){
  # remDr$navigate(url)
  # print(url)
  # df <- suedd_getlink(remDr$getPageSource()[[1]])
  pjs_session$go(url)
  print(pjs_session$getUrl())
  df <- suedd_getlink(pjs_session$getSource())  %>%
    subset(., item_pubdate >= as.Date(startdate))
  print(nrow(df))
  # 
  # remDr$getPageSource()[[1]] %>% rvest::read_html(html) %>% 
  #   rvest::html_elements(xpath = "//li[contains(@class, 'navigation')]//li[last()]") %>%
  #   rvest::html_text(., trim = TRUE) -> n
  # 
  pjs_session$getSource() %>% rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//li[contains(@class, 'navigation')]//li[last()]") %>%
    rvest::html_text(., trim = TRUE) -> n
  
  if(length(n) > 0){
    for (i in 2:n) {
      # remDr$navigate(paste0(url, "page/", i))
      # print(paste0(url, "page/", i))
      # df <- rbind(df, suedd_getlink(remDr$getPageSource()[[1]]))
      pjs_session$go(paste0(url, "page/", i))
      print(pjs_session$getUrl())
      df <- suedd_getlink(pjs_session$getSource())  %>%
        subset(., item_pubdate >= as.Date(startdate)) %>% rbind(df, .)
      print(nrow(df))
    } 
  }
                      
  return(df)
}

## doesn't work headless - no idea why, individual pages work - sometimes not

suedd_go_thr_archive <- function(startdate){
  # remDr$navigate("https://www.sueddeutsche.de/archiv")
  pjs_session$go("https://www.sueddeutsche.de/archiv")
  print(pjs_session$getUrl())
  
  # remDr$getPageSource()[[1]] %>% rvest::read_html(html) %>% 
  #   rvest::html_elements(xpath = "//div[contains(@class, 'department-overview-title')]//a") %>% 
  #   rvest::html_attr("href") -> categories
  
  pjs_session$getSource() %>% rvest::read_html(html) %>%
    rvest::html_elements(xpath = "//div[contains(@class, 'department-overview-title')]//a") %>%
    rvest::html_attr("href") -> categories
  
  seq(as.Date(startdate), Sys.Date(), by="months") %>% 
    format.Date(format="/%Y/%m") -> V1
  
  purrr::cross2(categories, V1) %>% purrr::map_chr(paste, sep = "", collapse = "") -> V2
  
  
  V2 %>%
    paste0("https://www.sueddeutsche.de", ., "/") %>%
    purrr::map_df(~suedd_getlink_url(., startdate = startdate)) -> valid_links
  
  return(valid_links)
}

suedd_go_thr_archive(startdate = "2022-01-01") -> valid_links

# remDr$close()
# z <- rD$server$stop()
