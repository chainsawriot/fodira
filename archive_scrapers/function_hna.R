#devtools::install_github("ropensci/RSelenium")
#install.packages("RSelenium")
require(RSelenium)
require(magrittr)

#fprof <- makeFirefoxProfile(list(permissions.default.image = 21))
rD <- RSelenium::rsDriver(browser = "firefox", 
                          #chromever = "103.0.5060.134", 
                          port = sample(c(5678L, 5679L, 5680L, 5681L#, 5682L
                                          ), size = 1), 
                          #phantomver = "2.1.1",
                          #extraCapabilities = fprof,
                          check = FALSE, verbose = FALSE)

remDr <- rD[["client"]]

#binman::list_versions("phantomjs")

# require(webdriver)
# require(magrittr)
# pjs_instance <- run_phantomjs()
# pjs_session <- Session$new(port = pjs_instance$port)


#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")


#function for geting links from page
hna_get_links <- function(html){
  
  html <- remDr$getPageSource()[[1]]
  #html <- pjs_session$getSource()
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'id-LinkOverlay')]//a") %>% 
    rvest::html_attr("title") -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'id-LinkOverlay')]//a") %>% 
    rvest::html_attr("href") %>% paste0("https:",.)-> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'id-LinkOverlay')]//time") %>% 
    rvest::html_text(trim = TRUE) %>% stringr::str_extract("[0-9]+\\.[0-9]+\\.[0-9]+") %>%
    lubridate::dmy()-> item_pubdate
  
  if(item_title[1] == "Hier ist nichts"){
    df <- data.frame()
  } else {
    df <- data.frame(item_title, item_link, item_pubdate)
  }
  
  
  return(df)
}


hna_get_url <- function(url){
  remDr$navigate(url)
  print(remDr$getCurrentUrl())
  remDr$getPageSource()[[1]] %>% hna_get_links() -> df
  print(nrow(df))
  
  remDr$getPageSource()[[1]] %>% rvest::read_html() %>% 
    rvest::html_elements(xpath = "//a[contains(@class, 'id-Swiper-loadMore')]") %>% 
    rvest::html_attr("href") %>% length -> n
  if (nrow(df) == 0){
    remDr$navigate(url)
    print(remDr$getCurrentUrl())
    remDr$getPageSource()[[1]] %>% hna_get_links() -> df
    
    remDr$getPageSource()[[1]] %>% rvest::read_html() %>% 
      rvest::html_elements(xpath = "//a[contains(@class, 'id-Swiper-loadMore')]") %>% 
      rvest::html_attr("href") %>% length -> n
  }
  while(n>0){
    el <- remDr$findElement(using = "xpath", "//a[contains(@class, 'id-Swiper-loadMore')]")
    el$clickElement()
    df -> df2
    i <- 1
    while(nrow(df) >= nrow(df2)){
      remDr$getPageSource()[[1]] %>% hna_get_links() -> df2
      if (nrow(df2) == 0){
        remDr$navigate(url)
        print(remDr$getCurrentUrl())
        remDr$getPageSource()[[1]] %>% hna_get_links() -> df
        
        remDr$getPageSource()[[1]] %>% rvest::read_html() %>% 
          rvest::html_elements(xpath = "//a[contains(@class, 'id-Swiper-loadMore')]") %>% 
          rvest::html_attr("href") %>% length -> n
      }
      #print(i)
      i <- i + 1
      if(i > 50){
        remDr$navigate(remDr$getCurrentUrl())
        i <- 1
        Sys.sleep(.5)
        remDr$getPageSource()[[1]] %>% rvest::read_html() %>% 
          rvest::html_elements(xpath = "//a[contains(@class, 'id-Swiper-loadMore')]") %>% 
          rvest::html_attr("href") %>% length -> n
        if (n > 0){
          el <- remDr$findElement(using = "xpath", "//a[contains(@class, 'id-Swiper-loadMore')]")
          el$clickElement()
        }
        #print(i)
      }

    }
    remDr$getPageSource()[[1]] %>% rvest::read_html() %>% 
      rvest::html_elements(xpath = "//a[contains(@class, 'id-Swiper-loadMore')]") %>% 
      rvest::html_attr("href") %>% length -> n
    remDr$getPageSource()[[1]] %>% hna_get_links() -> df
    print(nrow(df))
    print(n)
  }
  
  print(remDr$getCurrentUrl())
  remDr$getPageSource()[[1]] %>% hna_get_links() -> df
  print(nrow(df))
  return(df)
}

hna_go_thr_archive <- function(startdate, enddate){
  #startdate <- "2022-01-01"
  #enddate <- "2022-01-31"
  seq(as.Date(startdate), as.Date(enddate), by="days") %>% 
    format.Date(format="%Y-%m-%d&fd=%Y-%m-%d") -> V1
  
  V1 %>% paste0("https://www.hna.de/suche/?tt=1&tx=&sb=&td=", ., "&qr=") %>%
    purrr::map_df(~hna_get_url(.)) -> df
  
  # pjs_session$go(url)
  # print(url)
  
  return(df)
}

# df <- zeit_getlink_url("https://www.zeit.de/thema/krieg-in-ukraine", "2022-01-01")
  
  

hna_go_thr_archive("2021-12-01", "2022-03-01")-> valid_links1

hna_go_thr_archive("2022-03-02", "2022-06-01")-> valid_links2

hna_go_thr_archive("2022-06-02", "2022-07-01")-> valid_links3

hna_go_thr_archive("2022-07-02", "2022-08-01")-> valid_links4

hna_go_thr_archive("2022-08-02", "2022-09-01")-> valid_links5

hna_go_thr_archive("2022-09-02", "2022-10-01")-> valid_links6

hna_go_thr_archive("2022-10-02", "2022-11-01")-> valid_links7

hna_go_thr_archive("2022-11-02", "2022-12-01")-> valid_links8

hna_go_thr_archive("2022-12-02", "2023-01-01")-> valid_links9

hna_go_thr_archive("2023-01-02", "2023-02-01")-> valid_links10

hna_go_thr_archive("2023-02-02", "2023-03-01")-> valid_links11

hna_go_thr_archive("2023-03-02", Sys.Date())-> valid_links12

dplyr::distinct(rbind(valid_links1, valid_links2, valid_links3,
                      valid_links4, valid_links5, valid_links6,
                      valid_links7, valid_links8, valid_links9,
                      valid_links10, valid_links11, valid_links12)) -> valid_links


valid_links %>% dplyr::rename(title = item_title, link = item_link, pubdate = item_pubdate) %>% 
  dplyr::mutate(pub = "HNA", description = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links


saveRDS(valid_links, "HNA.RDS")

 remDr$close()
 z <- rD$server$stop()

# 