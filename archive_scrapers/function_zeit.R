#devtools::install_github("ropensci/RSelenium")
#install.packages("RSelenium")
require(RSelenium)
require(magrittr)
#eCap <- list(phantomjs.binary.path = "C:/phantomjs-2.1.1/bin.exe")
#fprof <- makeFirefoxProfile(list(permissions.default.image = 21))
rD <- RSelenium::rsDriver(browser = "firefox", 
                          #chromever = "103.0.5060.134", 
                          port = sample(c(5678L, 
                                          5679L, 
                            5680L, 
                            5681L, 
                                          5682L
                                          ), size = 1), 
                          #phantomver = "2.1.1",
                          #extraCapabilities = fprof,
                          check = FALSE, verbose = FALSE)

remDr <- rD[["client"]]


#binman::list_versions("phantomjs")
# 
# require(webdriver)
# require(magrittr)
# pjs_instance <- run_phantomjs()
# pjs_session <- Session$new(port = pjs_instance$port)
# 
# pjs_session$go("https://www.zeit.de/thema")
# 
# pjs_session$getSource() %>% writeLines("test.html")

# # el <- pjs_session$findElement(css = "button.message-component")
# el <- pjs_session$findElement(xpath = "//div[contains(@class, 'option__accbtn box__accbtn')]")
# el$click()
# pjs_session$getUrl()

#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

remDr$navigate("https://www.zeit.de/thema/")
webElem <- remDr$findElement(using = "xpath", "//div[contains(@class, 'option__accbtn box__accbtn')]")
webElem$clickElement()
# ### Click away the thing


#function for geting links from page
zeit_getlink <- function(html){
  
  #html <- remDr$getPageSource()[[1]]
  # html <- pjs_session$getSource()
  # 
  rvest::read_html(html) %>%  
  #html %>%
    rvest::html_elements(xpath = "//article[contains(@class, 'newsteaser')]/a[contains(@class, 'newsteaser__link')]") %>% 
    rvest::html_attr("title") -> item_title
  
  rvest::read_html(html) %>% 
  #html %>%
    rvest::html_elements(xpath = "//article[contains(@class, 'newsteaser')]/a[contains(@class, 'newsteaser__link')]") %>% 
    rvest::html_attr("href")  -> item_link
  
  rvest::read_html(html) %>% 
  #  html %>%
    rvest::html_elements(xpath = "//article[contains(@class, 'newsteaser')]//time[contains(@class, 'newsteaser__time')]") %>% 
    rvest::html_text(trim = TRUE) %>% stringr::str_replace(., "Heute, .+", format(Sys.Date(), "%d. %m. %Y")) %>% 
    lubridate::dmy() -> item_pubdate
  
  if(length(item_pubdate) < length(item_title)){
    l <- item_pubdate[order(item_pubdate, decreasing = TRUE)][1]
    item_pubdate <- l
  }
  

  
  df <- data.frame(item_title, item_link, item_pubdate)
  
  
  
  rvest::read_html(html) %>% 
 # html %>%
    rvest::html_elements(xpath = "//article[contains(@class, 'zon-teaser-square')]//a[contains(@class, 'zon-teaser-square__faux-link')]") %>%
    rvest::html_attr("title") -> item_title
  
  rvest::read_html(html) %>% 
  # html %>%
    rvest::html_elements(xpath = "//article[contains(@class, 'zon-teaser-square')]//a[contains(@class, 'zon-teaser-square__faux-link')]") %>% 
    rvest::html_attr("href")  -> item_link

  if(length(item_link)>0){
    item_pubdate <- lubridate::ymd("2022-01-01")
    
    df <- rbind(df, data.frame(item_title, item_link, item_pubdate))
  }

  
  rvest::read_html(html) %>% 
  # html %>%  
    rvest::html_elements(xpath = "//article[contains(@class, 'zon-teaser-wide')]//a[contains(@class, 'zon-teaser-wide__faux-link')]") %>%
    rvest::html_attr("title") -> item_title
  
  rvest::read_html(html) %>% 
  # html %>%
    rvest::html_elements(xpath = "//article[contains(@class, 'zon-teaser-wide')]//a[contains(@class, 'zon-teaser-wide__faux-link')]") %>% 
    rvest::html_attr("href")  -> item_link
  
  rvest::read_html(html) %>% 
  # html %>%  
    rvest::html_elements(xpath = "//article[contains(@class, 'zon-teaser-wide')]//time[contains(@class, 'zon-teaser-wide__datetime')]") %>% 
    rvest::html_text(trim = TRUE) %>% 
    stringr::str_replace(., "Vor .+ Stunden", format(Sys.Date(), "%d. %B %Y")) %>%
    stringr::str_replace(., "Vor .+ Stunde", format(Sys.Date(), "%d. %B %Y")) %>%
    stringr::str_replace(., "Vor .+ Tag", format((Sys.Date()-1), "%d. %B %Y")) %>%
    stringr::str_replace(., "Vor .+ Tagen", format(Sys.Date()-2, "%d. %B %Y")) %>% 
    stringr::str_replace(., "Vor .+ Minuten", format(Sys.Date(), "%d. %B %Y")) %>%
    stringr::str_replace(., "Vor .+ Minute", format(Sys.Date(), "%d. %B %Y")) %>%
    stringr::str_replace(., "M.rz", "March") %>%
    lubridate::dmy() -> item_pubdate
  
  if(length(item_pubdate) < length(item_title)){
    l <- item_pubdate[order(item_pubdate, decreasing = TRUE)][1]
    item_pubdate <- l
  }
  
  df <- rbind(df, data.frame(item_title, item_link, item_pubdate))
  

  
  rvest::read_html(html) %>% 
  # html %>%
    rvest::html_elements(xpath = "//article[contains(@class, 'zon-teaser-lead')]//a[contains(@class, 'zon-teaser-lead__faux-link')]") %>%
    rvest::html_attr("title") -> item_title
  
  rvest::read_html(html) %>%
  # html %>%
    rvest::html_elements(xpath = "//article[contains(@class, 'zon-teaser-lead')]//a[contains(@class, 'zon-teaser-lead__faux-link')]") %>% 
    rvest::html_attr("href")  -> item_link
  
  rvest::read_html(html) %>% 
  # html %>%  
    rvest::html_elements(xpath = "//article[contains(@class, 'zon-teaser-lead')]//time[contains(@class, 'zon-teaser-lead__datetime')]") %>% 
    rvest::html_text(trim = TRUE) %>% 
    stringr::str_replace(., "Vor .+ Stunden", format(Sys.Date(), "%d. %B %Y")) %>%
    stringr::str_replace(., "Vor .+ Stunde", format(Sys.Date(), "%d. %B %Y")) %>%
    stringr::str_replace(., "Vor .+ Tag", format((Sys.Date()-1), "%d. %B %Y")) %>%
    stringr::str_replace(., "Vor .+ Tagen", format(Sys.Date()-2, "%d. %B %Y")) %>% 
    stringr::str_replace(., "Vor .+ Minuten", format(Sys.Date(), "%d. %B %Y")) %>%
    stringr::str_replace(., "Vor .+ Minute", format(Sys.Date(), "%d. %B %Y")) %>%
    stringr::str_replace(., "M.rz", "March") %>%
    lubridate::dmy() -> item_pubdate
  
  if(length(item_pubdate) < length(item_title)){
    l <- item_pubdate[order(item_pubdate, decreasing = TRUE)][1]
    item_pubdate <- l
  }
  
  df <- rbind(df, data.frame(item_title, item_link, item_pubdate))
  

  
  rvest::read_html(html) %>% 
  # html %>%
    rvest::html_elements(xpath = "//article[contains(@class, 'zon-teaser-standard')]//a[contains(@class, 'zon-teaser-standard__faux-link')]") %>%
    rvest::html_attr("title") -> item_title
  
  rvest::read_html(html) %>% 
  # html %>%
    rvest::html_elements(xpath = "//article[contains(@class, 'zon-teaser-standard')]//a[contains(@class, 'zon-teaser-standard__faux-link')]") %>% 
    rvest::html_attr("href")  -> item_link
  
  rvest::read_html(html) %>% 
  # html %>%
    rvest::html_elements(xpath = "//article[contains(@class, 'zon-teaser-standard')]//time[contains(@class, 'zon-teaser-standard__datetime')]") %>% 
    rvest::html_text(trim = TRUE) %>%
    stringr::str_replace(., "Vor .+ Stunden", format(Sys.Date(), "%d. %B %Y")) %>%
    stringr::str_replace(., "Vor .+ Stunde", format(Sys.Date(), "%d. %B %Y")) %>%
    stringr::str_replace(., "Vor .+ Tag", format((Sys.Date()-1), "%d. %B %Y")) %>%
    stringr::str_replace(., "Vor .+ Tagen", format(Sys.Date()-2, "%d. %B %Y")) %>% 
    stringr::str_replace(., "Vor .+ Minuten", format(Sys.Date(), "%d. %B %Y")) %>%
    stringr::str_replace(., "Vor .+ Minute", format(Sys.Date(), "%d. %B %Y")) %>%
    stringr::str_replace(., "M.rz", "March") %>%
    lubridate::dmy() -> item_pubdate
  
  if(length(item_pubdate) < length(item_title)){
    l <- item_pubdate[order(item_pubdate, decreasing = TRUE)][1]
    item_pubdate <- l
  }
  
  
  df <- rbind(df, data.frame(item_title, item_link, item_pubdate))
  

  
  
  rvest::read_html(html) %>% 
  # html %>%
    rvest::html_elements(xpath = "//article[contains(@class, 'zon-teaser-poster')]//a[contains(@class, 'zon-teaser-poster__faux-link')]") %>%
    rvest::html_attr("title") -> item_title
  
  rvest::read_html(html) %>% 
  # html %>%
    rvest::html_elements(xpath = "//article[contains(@class, 'zon-teaser-poster')]//a[contains(@class, 'zon-teaser-poster__faux-link')]") %>% 
    rvest::html_attr("href")  -> item_link
  
  rvest::read_html(html) %>% 
  # html %>%
    rvest::html_elements(xpath = "//article[contains(@class, 'zon-teaser-poster')]//time[contains(@class, 'zon-teaser-poster__datetime')]") %>% 
    rvest::html_text(trim = TRUE) %>%
    stringr::str_replace(., "Vor .+ Stunden", format(Sys.Date(), "%d. %B %Y")) %>%
    stringr::str_replace(., "Vor .+ Stunde", format(Sys.Date(), "%d. %B %Y")) %>%
    stringr::str_replace(., "Vor .+ Tag", format((Sys.Date()-1), "%d. %B %Y")) %>%
    stringr::str_replace(., "Vor .+ Tagen", format(Sys.Date()-2, "%d. %B %Y")) %>% 
    stringr::str_replace(., "Vor .+ Minuten", format(Sys.Date(), "%d. %B %Y")) %>%
    stringr::str_replace(., "Vor .+ Minute", format(Sys.Date(), "%d. %B %Y")) %>%
    stringr::str_replace(., "M.rz", "March") %>%
    lubridate::dmy() -> item_pubdate
  
  if(length(item_pubdate) < length(item_title)){
    l <- item_pubdate[order(item_pubdate, decreasing = TRUE)][1]
    item_pubdate <- l
  }
  
  df <- rbind(df, data.frame(item_title, item_link, item_pubdate))
  
  df <- dplyr::distinct(df)
  
  return(df)
}


zeit_getlink_url <- function(url, startdate){
#  url <- "https://www.zeit.de/thema/angela-merkel"
  remDr$navigate(url)
  print(url)
  # a <- as.character(url)
  # html <- rvest::read_html(a)
  # print("a")
  # rvest::read_html(html)
  df <- zeit_getlink(remDr$getPageSource()[[1]]) %>%
  # df <- zeit_getlink(html) %>%
    subset(., item_pubdate >= as.Date(startdate))
  
  # pjs_session$go(url)
  # print(pjs_session$getUrl())
  # df <- zeit_getlink(pjs_session$getSource())  %>%
  #   subset(., item_pubdate >= as.Date(startdate))
  
  
  print(nrow(df))
  
  remDr$getPageSource()[[1]] %>% rvest::read_html() %>%
  #  html %>%
    rvest::html_elements(xpath = "//ul[contains(@class, 'pager__pages')]//li[last()]") %>%
    rvest::html_text(., trim = TRUE) %>% as.numeric()-> n
  
  # pjs_session$getSource() %>% rvest::read_html(html) %>% 
  #   rvest::html_elements(xpath = "//ul[contains(@class, 'pager__pages')]//li[last()]") %>%
  #   rvest::html_text(., trim = TRUE) %>% as.numeric()-> n
  
  if(and(length(n) > 0, nrow(df) > 0)){
    if(n > 1){
      i <- 2
      while(i <= n+1) {
        # webElem <- remDr$findElement(using = "css", "a[class='pager__button pager__button--next']")
        # webElem$clickElement()
        # Sys.sleep(.5)
        # print(remDr$getCurrentUrl())
        # print(2)
        # pjs_session$go(paste0(url, "?p=", i))
        # print(pjs_session$getUrl())
        
        remDr$navigate(paste0(url, "?p=", i))
        zeit_getlink(remDr$getPageSource()[[1]]) %>%
        # rvest::read_html(paste0(url, "?p=", i)) %>%
        #  zeit_getlink() %>%
          subset(., item_pubdate >= as.Date(startdate)) -> df2
        # zeit_getlink(pjs_session$getSource()) %>% 
        #   subset(., item_pubdate >= as.Date("2022-01-01")) -> df2
        print(remDr$getCurrentUrl())
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

#zeit_getlink_url("https://www.zeit.de/thema/angela-merkel", "2022-01-01")

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
  
  


zeit_go_thr_topics <- function(startdate, n){
  remDr$navigate("https://www.zeit.de/thema/index")
  
  # pjs_session$go("https://www.zeit.de/thema/index")
  # print("https://www.zeit.de/thema/index")
  
  rvest::read_html(remDr$getPageSource()[[1]]) %>%
    rvest::html_elements(xpath = "//ol[contains(@class, 'alphabetpager')]/li/a") %>%
    rvest::html_attr("href")  -> theme_links
  
  
  # rvest::read_html(pjs_session$getSource()) %>% 
  #   rvest::html_elements(xpath = "//ol[contains(@class, 'alphabetpager')]/li/a") %>% 
  #   rvest::html_attr("href")  -> theme_links  
  
  theme_links[n] %>% purrr::map_df(~zeit_go_thr_topic(., startdate)) -> valid_links

  return(valid_links)
}



zeit_go_thr_topics("2022-08-01", 1)-> valid_links_1
zeit_go_thr_topics("2022-08-01", 2)-> valid_links_2
zeit_go_thr_topics("2022-08-01", 3)-> valid_links_3
zeit_go_thr_topics("2022-08-01", 4)-> valid_links_4
zeit_go_thr_topics("2022-08-01", 5)-> valid_links_5
zeit_go_thr_topics("2022-08-01", 6)-> valid_links_6
zeit_go_thr_topics("2022-08-01", 7)-> valid_links_7
zeit_go_thr_topics("2022-08-01", 8)-> valid_links_8
zeit_go_thr_topics("2022-08-01", 9)-> valid_links_9
zeit_go_thr_topics("2022-08-01", 10)-> valid_links_10
zeit_go_thr_topics("2022-08-01", 11)-> valid_links_11
zeit_go_thr_topics("2022-08-01", 12)-> valid_links_12
zeit_go_thr_topics("2022-08-01", 13)-> valid_links_13
zeit_go_thr_topics("2022-08-01", 14)-> valid_links_14
zeit_go_thr_topics("2022-08-01", 15)-> valid_links_15
zeit_go_thr_topics("2022-08-01", 16)-> valid_links_16
zeit_go_thr_topics("2022-08-01", 17)-> valid_links_17
zeit_go_thr_topics("2022-08-01", 18)-> valid_links_18
zeit_go_thr_topics("2022-08-01", 19)-> valid_links_19
zeit_go_thr_topics("2022-08-01", 20)-> valid_links_20
zeit_go_thr_topics("2022-08-01", 21)-> valid_links_21
zeit_go_thr_topics("2022-08-01", 22)-> valid_links_22
zeit_go_thr_topics("2022-08-01", 23)-> valid_links_23
zeit_go_thr_topics("2022-08-01", 24)-> valid_links_24
zeit_go_thr_topics("2022-08-01", 25)-> valid_links_25
zeit_go_thr_topics("2022-08-01", 26)-> valid_links_26
zeit_go_thr_topics("2022-08-01", 27)-> valid_links_27


valid_links <- dplyr::distinct(rbind(valid_links_1, valid_links_10, 
                                     valid_links_11, valid_links_12, 
                                     valid_links_13, 
                                     valid_links_14, valid_links_15, 
                                     valid_links_16, 
                                     valid_links_17, 
                                     valid_links_18, valid_links_19, 
                                     valid_links_2, 
                                     valid_links_20, valid_links_21, 
                                     valid_links_22, valid_links_23, valid_links_24, 
                                     valid_links_25, valid_links_26, valid_links_27, 
                                     valid_links_3, valid_links_4, 
                                     valid_links_5, valid_links_6, valid_links_7, 
                                     valid_links_8, 
                                     valid_links_9), item_link,
                               .keep_all = TRUE)

valid_links %>% dplyr::rename(title = item_title, link = item_link, pubdate = item_pubdate) %>% 
  dplyr::mutate(pub = "Zeit", description = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links


saveRDS(valid_links, "Zeit.RDS")


 remDr$close()
 z <- rD$server$stop()

# rvest::read_html("https://www.zeit.de/thema/index")
# 