require(RSelenium)
require(magrittr)
rD <- RSelenium::rsDriver(browser = "firefox", port = sample(c(5678L, 5679L, 5680L, 5681L, 5682L), size = 1), check = FALSE, verbose = FALSE)
remDr <- rD[["client"]]

#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

remDr$navigate("https://www.zeit.de/thema/")
### Click away the thing


#function for geting links from page
zeit_getlink <- function(html){
  html <- remDr$getPageSource()[[1]]
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//article[contains(@class, 'newsteaser')]/a[contains(@class, 'newsteaser__link')]") %>% 
    rvest::html_attr("title") -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//article[contains(@class, 'newsteaser')]/a[contains(@class, 'newsteaser__link')]") %>% 
    rvest::html_attr("href")  -> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//article[contains(@class, 'newsteaser')]//time[contains(@class, 'newsteaser__time')]") %>% 
    rvest::html_text(trim = TRUE) %>% as.Date(., format= "%d. %m. %Y") -> item_pubdate
  
  
  df <- data.frame(item_title, item_link, item_pubdate)
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//article[contains(@class, 'zon-teaser-standard')]//a[contains(@class, 'zon-teaser-standard__faux-link')]") %>% 
    rvest::html_attr("title") -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//article[contains(@class, 'zon-teaser-standard')]//a[contains(@class, 'zon-teaser-standard__faux-link')]") %>% 
    rvest::html_attr("href")  -> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//article[contains(@class, 'zon-teaser-standard')]//time[contains(@class, 'zon-teaser-standard__datetime')]") %>% 
    rvest::html_text(trim = TRUE)
  as.Date(., format= "%d. %m. %Y") -> item_pubdate
  
    return(df)
}


zeit_getlink_url <- function(url, startdate){
  remDr$navigate(url)
  print(url)
  df <- zeit_getlink(remDr$getPageSource()[[1]]) 
    subset(., item_pubdate >= as.Date("2022-01-01"))
  
  remDr$getPageSource()[[1]] %>% rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//ul[contains(@class, 'pager__pages')]//li[last()]") %>%
    rvest::html_text(., trim = TRUE) %>% as.numeric()-> n
  
  if(and(length(n) > 0, nrow(df) > 0)){
    if(n > 1){
      i <- 1
      while(i <= n) {
        webElem <- remDr$findElement(using = "css", "a[class='pager__button pager__button--next']")
        webElem$clickElement()
        print(remDr$getCurrentUrl())
        zeit_getlink(remDr$getPageSource()[[1]]) %>% 
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

zeit_go_thr_topic <- function(url, startdate){
  remDr$navigate(url)
  print(url)
  rvest::read_html(remDr$getPageSource()[[1]]) %>% 
    rvest::html_elements(xpath = "//ul[contains(@class, ' zon-topicpage-list')]/li/a") %>% 
    rvest::html_attr("href")  -> theme_links
  theme_links %>% purrr::map_df(~zeit_getlink_url(., startdate)) -> df
  return(df)
}

zeit_go_thr_topics <- function(startdate){
  remDr$navigate("https://www.zeit.de/thema/index")
  rvest::read_html(remDr$getPageSource()[[1]]) %>% 
    rvest::html_elements(xpath = "//ol[contains(@class, 'alphabetpager')]/li/a") %>% 
    rvest::html_attr("href")  -> theme_links
  
  theme_links %>% purrr::map_df(~zeit_go_thr_topic(., startdate)) -> valid_links

  return(valid_links)
}


zeit_go_thr_topics("2022-01-01")-> valid_links

valid_links <- dplyr::distinct(valid_links)

remDr$close()
z <- rD$server$stop()

# 