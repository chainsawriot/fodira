require(RSelenium)
require(magrittr)
rD <- RSelenium::rsDriver(browser = "firefox", port = sample(c(5678L, 5679L, 5680L, 5681L, 5682L), size = 1), check = FALSE, verbose = FALSE)
remDr <- rD[["client"]]

#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

remDr$navigate("https://www.n-tv.de/thema/")
### Click away the thing


#function for geting links from page
ntv_getlink <- function(html){
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'box no-border')]//div[contains(@class, 'teaser__content')]/a[1]//span[contains(@class, 'teaser__headline')]") %>% 
    rvest::html_text(trim = TRUE) -> item_title

  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'box no-border')]//div[contains(@class, 'teaser__content')]/a[1]") %>% 
    rvest::html_attr("href")  -> item_link
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'box no-border')]//span[contains(@class, 'teaser__date')]") %>% 
    rvest::html_text(trim = TRUE) %>% as.Date(., format= "%d.%m.%Y %H:%M") -> item_pubdate
  
  
  df <- data.frame(item_title, item_link, item_pubdate)
    return(df)
}


ntv_getlink_url <- function(url, startdate){
  remDr$navigate(url)
  print(url)
  df <- ntv_getlink(remDr$getPageSource()[[1]]) %>% 
    subset(., item_pubdate >= as.Date(startdate))
  
  remDr$getPageSource()[[1]] %>% rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'paging')]//li[last()]") %>%
    rvest::html_text(., trim = TRUE) %>% as.numeric()-> n
  
  if(and(length(n) > 0, nrow(df) > 0)){
    if(n > 1){
      i <- 1
      while(i <= n) {
        webElem <- remDr$findElement(using = "css", "a[class='paging__next1 icon icon__arrow']")
        webElem$clickElement()
        print(remDr$getCurrentUrl())
        ntv_getlink(remDr$getPageSource()[[1]]) %>% 
          subset(., item_pubdate >= as.Date(startdate)) -> df2
        i <- i+1
        df <- rbind(df, df2)
        if(nrow(df2) == 0){
          i <- n+1
        }
        
      } 
    }

  }
  
  return(df)
}

ntv_go_thr_topic <- function(url, startdate){
  remDr$navigate(url)
  print(url)
  rvest::read_html(remDr$getPageSource()[[1]]) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'theme__list')]/a") %>% 
    rvest::html_attr("href")  -> theme_links
  theme_links %>% purrr::map_df(~ntv_getlink_url(., startdate)) -> df
  return(df)
}

ntv_go_thr_topics <- function(startdate){
  remDr$navigate("https://www.n-tv.de/thema/")
  rvest::read_html(remDr$getPageSource()[[1]]) %>% 
    rvest::html_elements(xpath = "//ul[contains(@class, 'theme__index')]/li/a") %>% 
    rvest::html_attr("href")  -> theme_links
  
  theme_links %>% purrr::map_df(~ntv_go_thr_topic(., startdate)) -> valid_links

  return(valid_links)
}


ntv_go_thr_topics("2022-01-01")-> valid_links

valid_links <- dplyr::distinct(valid_links)

remDr$close()
z <- rD$server$stop()

# 