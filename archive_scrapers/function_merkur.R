require(RSelenium)
require(magrittr)
rD <- RSelenium::rsDriver(browser = "firefox", port = sample(c(5678L, 5679L, 5680L, 5681L, 5682L), size = 1), check = FALSE, verbose = FALSE)
remDr <- rD[["client"]]

# require(webdriver)
# require(magrittr)
# pjs_instance <- run_phantomjs()
# pjs_session <- Session$new(port = pjs_instance$port)

#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#pjs_session$go("https://www.badische-zeitung.de/archiv/2022/01/03")

#function for geting links from page
merkur_getlink <- function(html){
  
  # html <- remDr$getPageSource()[[1]]
  # html <- pjs_session$getUrl()
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//a[contains(@class, 'id-LinkOverlay-link')]") %>% 
    rvest::html_attr("title") -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//a[contains(@class, 'id-LinkOverlay-link')]") %>% 
    rvest::html_attr("href") %>%
    paste0("https:", .)-> item_link

  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//span[contains(@class, 'id-Teaser-el-content-meta-item id-Teaser-el-content-meta-item-date')]") %>%
    rvest::html_text(., trim = TRUE) %>% 
    stringr::str_extract("[0-9]+\\.[0-9]+\\.[0-9]+") %>% 
    as.Date(. , tryFormat = "%d.%m.%y") -> item_pubdate
  
print(1)
  
    df <- data.frame(item_title, item_link, item_pubdate)
    
    return(df)
}

#pjs_session$go("https://www.sueddeutsche.de/archiv/m%C3%BCnchen/2022/01")

#pjs_session$getUrl()

#remDr$getPageSource()[[1]] %>% merkur_getlink()

merkur_getlink_url <- function(url){
  remDr$navigate(paste0("https://www.merkur.de/suche/index-vc-376496-", 1, url))
  remDr$getPageSource()[[1]] %>% 
  # pjs_session$go(paste0("https://www.merkur.de/suche/index-vc-376496-", 1, url))
  # pjs_session$getSource() %>% 
    rvest::read_html() %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'id-Swiper-navcount')]") %>%
    rvest::html_text(., trim = TRUE) %>% stringr::str_extract("[0-9]+$") %>%
    as.numeric() -> n
  Sys.sleep(.2)
  p <- 0
  while(length(n) == 0){
    p <- p+1
    if(p > 30){
      print("lzero")
      remDr$navigate(paste0("https://www.merkur.de/suche/index-vc-376496-", 1, url))
      p <- 0
    }

    Sys.sleep(.05)
    remDr$getPageSource()[[1]] %>% 
    # pjs_session$getSource() %>% 
      rvest::read_html() %>% 
      rvest::html_elements(xpath = "//div[contains(@class, 'id-Swiper-navcount')]") %>%
      rvest::html_text(., trim = TRUE) %>% stringr::str_extract("[0-9]+$") %>%
      as.numeric() -> n
  }
  
  if(is.na(n)){
    Sys.sleep(5)
    remDr$getPageSource()[[1]] %>% 
    # pjs_session$getSource() %>% 
      rvest::read_html() %>% 
      rvest::html_elements(xpath = "//div[contains(@class, 'id-Swiper-navcount')]") %>%
      rvest::html_text(., trim = TRUE) %>% stringr::str_extract("[0-9]+$") %>%
      as.numeric() -> n
  }
  

  print(remDr$getCurrentUrl())
  # print(pjs_session$getUrl())
  df <- merkur_getlink(remDr$getPageSource()[[1]])
  # df <- merkur_getlink(pjs_session$getSource())
  print(nrow(df))
  remDr$getPageSource()[[1]] %>% 
  # pjs_session$getSource() %>%
    rvest::read_html() %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'id-Swiper-navcount')]") %>%
    rvest::html_text(., trim = TRUE) %>% stringr::str_extract("[0-9]+$") %>%
    as.numeric() -> n
  
  if(is.na(n)){
    Sys.sleep(3)
    remDr$getPageSource()[[1]] %>% 
    # pjs_session$getSource() %>%
      rvest::read_html() %>% 
      rvest::html_elements(xpath = "//div[contains(@class, 'id-Swiper-navcount')]") %>%
      rvest::html_text(., trim = TRUE) %>% stringr::str_extract("[0-9]+$") %>%
      as.numeric() -> n
  }
  
  print(paste("n is ", n))
  i <- 2
  print(paste("i is ", i))

  while (i<=n) {
    
    remDr$navigate(paste0("https://www.merkur.de/suche/index-vc-376496-", i, url))
    # pjs_session$go(paste0("https://www.merkur.de/suche/index-vc-376496-", 1, url))
    # print(pjs_session$getUrl())

    Sys.sleep(.2)
    remDr$getPageSource()[[1]] %>% 
    # pjs_session$getSource() %>% 
      rvest::read_html() %>% 
      rvest::html_elements(xpath = "//div[contains(@class, 'id-Swiper-navcount')]") %>%
      rvest::html_text(., trim = TRUE) %>% stringr::str_extract("[0-9]+$") %>%
      as.numeric() -> m
    p <- 0
    while(length(m) == 0){
      p <- p+1
      if(p > 30){
        print("lzero")
        remDr$navigate(paste0("https://www.merkur.de/suche/index-vc-376496-", i, url))
        p <- 0
      }

      Sys.sleep(.05)
      remDr$getPageSource()[[1]] %>% 
      # pjs_session$getSource() %>% 
        rvest::read_html() %>% 
        rvest::html_elements(xpath = "//div[contains(@class, 'id-Swiper-navcount')]") %>%
        rvest::html_text(., trim = TRUE) %>% stringr::str_extract("[0-9]+$") %>%
        as.numeric() -> m
    }
    
    print(remDr$getCurrentUrl())
    # print(pjs_session$getUrl())
    df <- rbind(df, merkur_getlink(remDr$getPageSource()[[1]]))
    # df <- rbind(df, merkur_getlink(pjs_session$getSource()))
    print(nrow(df))
    i <- i + 1
  }
  
  return(df)
}

#bad_z_getlink_url("www.test.de")

## doesn't work headless - no idea why, individual pages work - sometimes not

merkur_go_thr_archive <- function(startdate, enddate, rubrik){

  seq(as.Date(startdate), as.Date(enddate), by="days") %>% 
    format.Date(format="%Y-%m-%d&fd=%Y-%m-%d") -> V1

  
  V1 %>%
    paste0(".html?tt=1&tx=&sb=0&td=", .,"&qr=", rubrik) %>%
    purrr::map_df(~merkur_getlink_url(.)) -> valid_links
  print(Sys.time())
  return(valid_links)
}

#merkur_go_thr_archive(startdate = "2021-12-31", enddate = "2022-01-31", "") -> valid_links

merkur_go_thr_archive(startdate = "2021-12-31", enddate = Sys.Date(), "") -> valid_links

valid_links <- dplyr::distinct(valid_links)

remDr$close()
z <- rD$server$stop()
