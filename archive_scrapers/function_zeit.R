require(RSelenium)
require(magrittr)
rD <- RSelenium::rsDriver(browser = "chrome", chromever = "103.0.5060.134", port = sample(c(5678L, 5679L, 5680L, 5681L, 5682L), size = 1), check = FALSE, verbose = FALSE)
remDr <- rD[["client"]]


# require(webdriver)
# require(magrittr)
# pjs_instance <- run_phantomjs()
# pjs_session <- Session$new(port = pjs_instance$port)
# 
# pjs_session$go("https://www.zeit.de/")
# pjs_session$findElement(css="button.message_component")



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
    rvest::html_text(trim = TRUE) %>% stringr::str_replace(., "Heute, .+", format(Sys.Date(), "%d. %m. %Y")) %>% as.Date(., tryFormat= "%d. %m. %Y") -> item_pubdate
  
  df <- data.frame(item_title, item_link, item_pubdate)
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//article[contains(@class, 'zon-teaser-wide')]/a[contains(@class, 'zon-teaser-wide__faux-link')]") %>%
    rvest::html_attr("title") -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//article[contains(@class, 'zon-teaser-wide')]/a[contains(@class, 'zon-teaser-wide__faux-link')]") %>% 
    rvest::html_attr("href")  -> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//article[contains(@class, 'zon-teaser-wide')]//time[contains(@class, 'zon-teaser-wide__datetime')]") %>% 
    rvest::html_text(trim = TRUE) %>% 
    stringr::str_replace(., "Vor .+ Stunden", format(Sys.Date(), "%d. %B %Y")) %>%
    stringr::str_replace(., "Vor .+ Stunde", format(Sys.Date(), "%d. %B %Y")) %>%
    stringr::str_replace(., "Vor 1 Tag", format((Sys.Date()-1), "%d. %B %Y")) %>%
    stringr::str_replace(., "Vor 2 Tagen", format(Sys.Date()-2, "%d. %B %Y")) %>% 
    stringr::str_replace(., "Vor .+ Minuten", format(Sys.Date(), "%d. %B %Y")) %>%
    stringr::str_replace(., "Vor .+ Minute", format(Sys.Date(), "%d. %B %Y")) %>%
    as.Date(., tryFormat= c("%d. %m. %Y", "%d. %B %Y")) -> item_pubdate
  
  if(length(item_pubdate) < length(item_title)){
    l <- item_pubdate[1]
    item_pubdate <- l
  }
  
  df <- rbind(df, data.frame(item_title, item_link, item_pubdate))
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//article[contains(@class, 'zon-teaser-lead')]/a[contains(@class, 'zon-teaser-lead__faux-link')]") %>%
    rvest::html_attr("title") -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//article[contains(@class, 'zon-teaser-lead')]/a[contains(@class, 'zon-teaser-lead__faux-link')]") %>% 
    rvest::html_attr("href")  -> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//article[contains(@class, 'zon-teaser-lead')]//time[contains(@class, 'zon-teaser-lead__datetime')]") %>% 
    rvest::html_text(trim = TRUE) %>% 
    stringr::str_replace(., "Vor .+ Stunden", format(Sys.Date(), "%d. %B %Y")) %>%
    stringr::str_replace(., "Vor .+ Stunde", format(Sys.Date(), "%d. %B %Y")) %>%
    stringr::str_replace(., "Vor 1 Tag", format((Sys.Date()-1), "%d. %B %Y")) %>%
    stringr::str_replace(., "Vor 2 Tagen", format(Sys.Date()-2, "%d. %B %Y")) %>% 
    stringr::str_replace(., "Vor .+ Minuten", format(Sys.Date(), "%d. %B %Y")) %>%
    stringr::str_replace(., "Vor .+ Minute", format(Sys.Date(), "%d. %B %Y")) %>%
    as.Date(., tryFormat= c("%d. %m. %Y", "%d. %B %Y")) -> item_pubdate
  
  if(length(item_pubdate) < length(item_title)){
    l <- item_pubdate[1]
    item_pubdate <- l
  }
  
  df <- rbind(df, data.frame(item_title, item_link, item_pubdate))
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//article[contains(@class, 'zon-teaser-standard')]//a[contains(@class, 'zon-teaser-standard__faux-link')]") %>%
    rvest::html_attr("title") -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//article[contains(@class, 'zon-teaser-standard')]//a[contains(@class, 'zon-teaser-standard__faux-link')]") %>% 
    rvest::html_attr("href")  -> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//article[contains(@class, 'zon-teaser-standard')]//time[contains(@class, 'zon-teaser-standard__datetime')]") %>% 
    rvest::html_text(trim = TRUE) %>%
    stringr::str_replace(., "Vor .+ Stunden", format(Sys.Date(), "%d. %B %Y")) %>%
    stringr::str_replace(., "Vor .+ Stunde", format(Sys.Date(), "%d. %B %Y")) %>%
    stringr::str_replace(., "Vor 1 Tag", format((Sys.Date()-1), "%d. %B %Y")) %>%
    stringr::str_replace(., "Vor 2 Tagen", format(Sys.Date()-2, "%d. %B %Y")) %>% 
    stringr::str_replace(., "Vor .+ Minuten", format(Sys.Date(), "%d. %B %Y")) %>%
    stringr::str_replace(., "Vor .+ Minute", format(Sys.Date(), "%d. %B %Y")) %>%
    as.Date(., tryFormat= c("%d. %m. %Y", "%d. %B %Y")) -> item_pubdate
  
  if(length(item_pubdate) < length(item_title)){
    l <- item_pubdate[1]
    item_pubdate <- l
  }
  
  
  df <- rbind(df, data.frame(item_title, item_link, item_pubdate))
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//article[contains(@class, 'zon-teaser-poster')]//a[contains(@class, 'zon-teaser-poster__faux-link')]") %>%
    rvest::html_attr("title") -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//article[contains(@class, 'zon-teaser-poster')]//a[contains(@class, 'zon-teaser-poster__faux-link')]") %>% 
    rvest::html_attr("href")  -> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//article[contains(@class, 'zon-teaser-poster')]//time[contains(@class, 'zon-teaser-poster__datetime')]") %>% 
    rvest::html_text(trim = TRUE) %>%
    stringr::str_replace(., "Vor .+ Stunden", format(Sys.Date(), "%d. %B %Y")) %>%
    stringr::str_replace(., "Vor .+ Stunde", format(Sys.Date(), "%d. %B %Y")) %>%
    stringr::str_replace(., "Vor 1 Tag", format((Sys.Date()-1), "%d. %B %Y")) %>%
    stringr::str_replace(., "Vor 2 Tagen", format(Sys.Date()-2, "%d. %B %Y")) %>% 
    stringr::str_replace(., "Vor .+ Minuten", format(Sys.Date(), "%d. %B %Y")) %>%
    stringr::str_replace(., "Vor .+ Minute", format(Sys.Date(), "%d. %B %Y")) %>%
    as.Date(., tryFormat= c("%d. %m. %Y", "%d. %B %Y")) -> item_pubdate
  
  if(length(item_pubdate) < length(item_title)){
    l <- item_pubdate[1]
    item_pubdate <- l
  }
  
  df <- rbind(df, data.frame(item_title, item_link, item_pubdate))
  
  df <- dplyr::distinct(df)
  
  return(df)
}


zeit_getlink_url <- function(url, startdate){
  remDr$navigate(url)
  print(url)
  df <- zeit_getlink(remDr$getPageSource()[[1]]) %>%
    subset(., item_pubdate >= as.Date(startdate))
  
  # pjs_session$go(url)
  # print(pjs_session$getUrl())
  # df <- zeit_getlink(pjs_session$getSource())  %>%
  #   subset(., item_pubdate >= as.Date(startdate))
  
  
  print(nrow(df))
  
  remDr$getPageSource()[[1]] %>% rvest::read_html(html) %>%
    rvest::html_elements(xpath = "//ul[contains(@class, 'pager__pages')]//li[last()]") %>%
    rvest::html_text(., trim = TRUE) %>% as.numeric()-> n
  
  # pjs_session$getSource() %>% rvest::read_html(html) %>% 
  #   rvest::html_elements(xpath = "//ul[contains(@class, 'pager__pages')]//li[last()]") %>%
  #   rvest::html_text(., trim = TRUE) %>% as.numeric()-> n
  
  if(and(length(n) > 0, nrow(df) > 0)){
    if(n > 1){
      i <- 2
      while(i <= n+1) {
        webElem <- remDr$findElement(using = "css", "a[class='pager__button pager__button--next']")
        webElem$clickElement()
        print(remDr$getCurrentUrl())
        # pjs_session$go(paste0(url, "?p=", i))
        # print(pjs_session$getUrl())
        
        zeit_getlink(remDr$getPageSource()[[1]]) %>%
          subset(., item_pubdate >= as.Date(startdate)) -> df2
        # zeit_getlink(pjs_session$getSource()) %>% 
        #   subset(., item_pubdate >= as.Date("2022-01-01")) -> df2
        
        i <- i+1
        df <- rbind(df, df2)
        if(nrow(df2) == 0){
          i <- n+2
        }
        print(nrow(df2))
      } 
    }

  }
  
  return(df)
}

zeit_go_thr_topic <- function(url, startdate){
  remDr$navigate(url)
  print(url)
  
  # pjs_session$go(url)
  # print(url)
  
  rvest::read_html(remDr$getPageSource()[[1]]) %>%
    rvest::html_elements(xpath = "//ul[contains(@class, ' zon-topicpage-list')]/li/a") %>%
    rvest::html_attr("href")  -> theme_links
  
  # rvest::read_html(pjs_session$getSource()) %>% 
  #   rvest::html_elements(xpath = "//ul[contains(@class, ' zon-topicpage-list')]/li/a") %>% 
  #   rvest::html_attr("href")  -> theme_links
  
  theme_links %>% purrr::map_df(~zeit_getlink_url(., startdate)) -> df
  return(df)
}

# df <- zeit_getlink_url("https://www.zeit.de/thema/krieg-in-ukraine", "2022-01-01")
  
  


zeit_go_thr_topics <- function(startdate){
  remDr$navigate("https://www.zeit.de/thema/index")
  
  # pjs_session$go("https://www.zeit.de/thema/index")
  # print("https://www.zeit.de/thema/index")
  
  rvest::read_html(remDr$getPageSource()[[1]]) %>%
    rvest::html_elements(xpath = "//ol[contains(@class, 'alphabetpager')]/li/a") %>%
    rvest::html_attr("href")  -> theme_links
  
  
  # rvest::read_html(pjs_session$getSource()) %>% 
  #   rvest::html_elements(xpath = "//ol[contains(@class, 'alphabetpager')]/li/a") %>% 
  #   rvest::html_attr("href")  -> theme_links  
  
  theme_links %>% purrr::map_df(~zeit_go_thr_topic(., startdate)) -> valid_links

  return(valid_links)
}


zeit_go_thr_topics("2022-01-01")-> valid_links

valid_links <- dplyr::distinct(valid_links)

 remDr$close()
 z <- rD$server$stop()

# 