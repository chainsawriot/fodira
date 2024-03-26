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

#remDr$navigate("https://www.n-tv.de/thema/")
### Click away the thing


#function for geting links from page
ntv_getlink <- function(html){
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//section[contains(@style, 'padding: 10px 0 30px 0;')]//span[contains(@class, 'teaser__kicker')]") %>% 
    rvest::html_text(trim = TRUE) %>% stringr::str_detect("Server-Fehler 404: Dokument nicht gefunden")-> error
  if(length(error) == 0) {
    
    rvest::read_html(html) %>% 
      rvest::html_elements(xpath = "//div[contains(@class, 'content')]//div[contains(@class, 'teaser__content')]/a[1]//span[contains(@class, 'teaser__headline')]") %>% 
      rvest::html_text(trim = TRUE) -> item_title
    
    rvest::read_html(html) %>% 
      rvest::html_elements(xpath = "//div[contains(@class, 'content')]//div[contains(@class, 'teaser__content')]/a[1]") %>% 
      rvest::html_attr("href")  -> item_link
    
    rvest::read_html(html) %>% 
      rvest::html_elements(xpath = "//div[contains(@class, 'content')]//div[contains(@class, 'teaser__content')]//span[contains(@class, 'teaser__date')]") %>% 
      rvest::html_text(trim = TRUE) %>% as.Date(., format= "%d.%m.%Y %H:%M") -> item_pubdate
    
    df <- data.frame(item_title, item_link, item_pubdate)
  } else{
    df <- data.frame()
  }
  #pjs_session$getSource() -> html
  
  
  
  
    return(df)
}


ntv_getlink_url <- function(url, startdate){
  print("uno")
  pjs_session$go(url)
  print("dos")
  print(pjs_session$getUrl())
  df <- ntv_getlink(pjs_session$getSource())  
  
  if(nrow(df) > 0){
    subset(df, item_pubdate >= as.Date(startdate)) -> df
  }
    
  
  # remDr$navigate(url)
  # print(url)
  # df <- ntv_getlink(remDr$getPageSource()[[1]]) %>% 
  #   subset(., item_pubdate >= as.Date(startdate))
  
  # remDr$getPageSource()[[1]] %>% rvest::read_html(html) %>% 
  #   rvest::html_elements(xpath = "//div[contains(@class, 'paging')]//li[last()]") %>%
  #   rvest::html_text(., trim = TRUE) %>% as.numeric()-> n
  print("tres")
  pjs_session$getSource() %>% rvest::read_html() %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'paging')]//li[last()]") %>%
    rvest::html_text(., trim = TRUE) %>% as.numeric()-> n
  print("quadro")
  print(nrow(df))
  
  if(and(length(n) > 0, nrow(df) > 0)){
    if(n > 1){
      i <- 1
      while(i <= n) {
        print("cinco")
        # pjs_session$getSource() %>% rvest::read_html(html) %>% 
        #   rvest::html_elements(xpath = "//a[contains(@class, 'paging__next1 icon icon__arrow')]") %>%
        #   rvest::html_attr("href")  -> link
        pjs_session$go(paste0(url, "/archiv-", i))
        print("ses")
        print(pjs_session$getUrl())
        
        # webElem <- remDr$findElement(using = "css", "a[class='paging__next1 icon icon__arrow']")
        # webElem$clickElement()
        # print(remDr$getCurrentUrl())
        
        ntv_getlink(pjs_session$getSource()) %>% 
          subset(., item_pubdate >= as.Date(startdate)) -> df2
        
        # ntv_getlink(remDr$getPageSource()[[1]]) %>% 
        #   subset(., item_pubdate >= as.Date(startdate)) -> df2
        
        print(nrow(df2))
        
        i <- i+1
        df <- rbind(df, df2)
        if(nrow(df2) == 0){
          i <- n+3
          #print("muh")
        }
        
      } 
    }

  }
  
  return(df)
}

#ntv_getlink_url("https://www.n-tv.de/thema/terroranschlaege-in-bruessel", "2022-01-01")


ntv_go_thr_topic <- function(url, startdate){
  # remDr$navigate(url)
  # print(url)
  # url <- "https://www.n-tv.de/thema/index-T"
  # startdate <- "2022-01-01"
  pjs_session$go(url)
  print(url)
  
  # rvest::read_html(remDr$getPageSource()[[1]]) %>% 
  #   rvest::html_elements(xpath = "//div[contains(@class, 'theme__list')]/a") %>% 
  #   rvest::html_attr("href")  -> theme_links
  
  rvest::read_html(pjs_session$getSource()) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'theme__list')]/a") %>% 
    rvest::html_attr("href")  -> theme_links
  
  
  theme_links %>% purrr::map_df(~ntv_getlink_url(., startdate)) -> df
  return(df)
}

ntv_go_thr_topics <- function(startdate, n){
  # remDr$navigate("https://www.n-tv.de/thema/")
  pjs_session$go("https://www.n-tv.de/thema/")
  print("https://www.n-tv.de/thema/")
  
  rvest::read_html(pjs_session$getSource()) %>% 
    rvest::html_elements(xpath = "//ul[contains(@class, 'theme__index')]/li/a") %>% 
    rvest::html_attr("href")  -> theme_links  
  
  # rvest::read_html(remDr$getPageSource()[[1]]) %>% 
  #   rvest::html_elements(xpath = "//ul[contains(@class, 'theme__index')]/li/a") %>% 
  #   rvest::html_attr("href")  -> theme_links
  
  theme_links[n] %>% purrr::map_df(~ntv_go_thr_topic(., startdate)) -> valid_links

  return(valid_links)
}


ntv_go_thr_topics("2023-01-01", 20:27)-> valid_links1

ntv_go_thr_topics("2023-01-01", 1:19)-> valid_links2

valid_links <- dplyr::distinct(rbind(valid_links1, valid_links2))
# 
# remDr$close()
# z <- rD$server$stop()

valid_links %>% dplyr::distinct() %>% 
  dplyr::rename(title = item_title, link = item_link, pubdate = item_pubdate) %>% 
  dplyr::mutate(pub = "NTV", description = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links


saveRDS(valid_links, "NTV_2.RDS")
