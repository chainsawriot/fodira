#devtools::install_github("ropensci/RSelenium")
#install.packages("RSelenium")
require(RSelenium)
require(magrittr)
# #eCap <- list(phantomjs.binary.path = "C:/phantomjs-2.1.1/bin.exe")
# fprof <- makeFirefoxProfile(list(permissions.default.image = 21))
rD <- RSelenium::rsDriver(browser = "firefox",
                          #chromever = "103.0.5060.134",
                          port = sample(c(5678L, 
                                          #5679L, 
                                          5680L, 
                                          5681L, 
                                          5682L), size = 1),
                          #phantomver = "2.1.1",
                          #extraCapabilities = fprof,
                          check = TRUE, verbose = FALSE)

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

# remDr$navigate("https://www.zeit.de/thema/")
# webElem <- remDr$findElement(using = "xpath", "//div[contains(@class, 'option__accbtn box__accbtn')]")
# webElem$clickElement()
# ### Click away the thing


#function for geting links from page
tag24_getlink <- function(html){
  
  # html <- remDr$getPageSource()[[1]]
  # html <- pjs_session$getSource()
  # 
  rvest::read_html(html) %>%  
  #html %>%
    rvest::html_elements(xpath = "//span[contains(@class, 'article-tile__inner')]/a[contains(@data-atype, 'news')]//span[contains(@class, 'article-tile__headline')]") %>% 
    rvest::html_text(trim = TRUE) -> item_title

  rvest::read_html(html) %>%  
    #html %>%
    rvest::html_elements(xpath = "//span[contains(@class, 'article-tile__inner')]/a[contains(@data-atype, 'news')]//h3[contains(@class, 'article-tile__headline')]") %>% 
    rvest::html_text(trim = TRUE) -> item_title2
  
  item_title <- c(item_title2, item_title)
    
  rvest::read_html(html) %>% 
  #html %>%
    rvest::html_elements(xpath = "//span[contains(@class, 'article-tile__inner')]/a[contains(@data-atype, 'news')]") %>% 
    rvest::html_attr("href")  -> item_link
  
 df <- data.frame(item_title, item_link)
  return(df)
}


tag24_getlink_url <- function(url, startdate){

  remDr$navigate(url)
  print(url)
  # pjs_session$go(url)
  # print(pjs_session$getUrl())

  df <- tag24_getlink(remDr$getPageSource()[[1]])
  
  # df <- tag24_getlink(pjs_session$getSource()) 
  
  
  print(nrow(df))
  
  remDr$getPageSource()[[1]] %>% rvest::read_html() %>%
  # pjs_session$getSource() %>% rvest::read_html() %>%
    rvest::html_elements(xpath = "//i[contains(@class, 'icon icon-chevron-right-solid')]") %>%
    rvest::html_text(., trim = TRUE) %>% length() -> n
  
  i <- 2
  while (n > 0) {
    
    remDr$navigate(paste0(url, "?page=", i))
    print(remDr$getCurrentUrl())
    # pjs_session$go(paste0(url, "?page=", i))
    # print(pjs_session$getUrl())

    df2 <- tag24_getlink(remDr$getPageSource()[[1]])
    # df2 <- tag24_getlink(pjs_session$getSource()) 
    print(nrow(df2))
    
    df <- rbind(df, df2)

    remDr$getPageSource()[[1]] %>% rvest::read_html() %>%
    # pjs_session$getSource() %>% rvest::read_html() %>%
      rvest::html_elements(xpath = "//i[contains(@class, 'icon icon-chevron-right-solid')]") %>%
      rvest::html_text(., trim = TRUE) %>% length() -> n
    
    remDr$navigate(df2$item_link[nrow(df2)])
    # pjs_session$go(df2$item_link[nrow(df2)])
    
    remDr$getPageSource()[[1]] %>% rvest::read_html() %>%
    # pjs_session$getSource() %>% rvest::read_html() %>%
      rvest::html_elements(xpath = "//div[contains(@class, 'article__info')]/time") %>%
      rvest::html_text(., trim = TRUE) %>% lubridate::dmy_hm()-> t
    if(t < as.Date(startdate)){
      n <- 0
    }
    i <- i+1
  }
  return(df)
}

#tag24_getlink_url("https://www.tag24.de/nachrichten/politik/deutschland/parteien/afd", "2022-01-01") -> df_


tag24_go_thr_topic <- function(url, startdate){
  remDr$navigate(url)
  print(url)
  
  # pjs_session$go(url)
  # print(url)
  
  rvest::read_html(remDr$getPageSource()[[1]]) %>%
  # rvest::read_html(pjs_session$getSource()) %>% 
    rvest::html_elements(xpath = "//a[contains(@class, 'article-tile__link')]") %>%
    rvest::html_attr("href") -> theme_links
  
  theme_links %>% purrr::map_df(~tag24_getlink_url(., startdate)) -> df
  return(df)
}

# df <- zeit_getlink_url("https://www.zeit.de/thema/krieg-in-ukraine", "2022-01-01")
  
  


tag24_go_thr_topics <- function(startdate, n){
  remDr$navigate("https://www.tag24.de/a-bis-z/")

  # pjs_session$go("https://www.tag24.de/a-bis-z/")

  
  rvest::read_html(remDr$getPageSource()[[1]]) %>%
  # rvest::read_html(pjs_session$getSource()) %>%
    rvest::html_elements(xpath = "//a[contains(@class, 'letter-menu__item')]") %>%
    rvest::html_attr("href")%>% paste0("https://www.tag24.de",.)  -> theme_links
  
  theme_links[n] %>% purrr::map_df(~tag24_go_thr_topic(., startdate)) -> valid_links

  return(valid_links)
}



tag24_go_thr_topics("2021-12-31", 1:5)-> valid_links_1
tag24_go_thr_topics("2022-12-31", 6:10)-> valid_links_2
tag24_go_thr_topics("2022-12-31", 11:15)-> valid_links_3
tag24_go_thr_topics("2022-12-31", 16:20)-> valid_links_4
tag24_go_thr_topics("2022-12-31", 21:25)-> valid_links_5
tag24_go_thr_topics("2022-12-31", 26:29)-> valid_links_6

valid_links <- dplyr::distinct(rbind(valid_links_1, valid_links_2, valid_links_3,
                                     valid_links_4, valid_links_5, valid_links_6))

saveRDS(valid_links, "tag24_1.RDS")

remDr$close()
z <- rD$server$stop()

# rvest::read_html("https://www.zeit.de/thema/index")
# 