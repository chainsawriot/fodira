require(RSelenium)
require(magrittr)
rD <- RSelenium::rsDriver(browser = "firefox", port = sample(c(#5678L, 
  #5679L, 
  5680L, #5681L, 
  5682L), size = 1), check = FALSE, verbose = FALSE)
remDr <- rD[["client"]]

# require(webdriver)
# require(magrittr)
# pjs_instance <- run_phantomjs()
# pjs_session <- Session$new(port = pjs_instance$port)

#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#pjs_session$go("https://www.badische-zeitung.de/archiv/2023/01/03")

#function for geting links from page
merkur_getlink <- function(html, givedate){
  #Sys.sleep(5)
  html <- remDr$getPageSource()[[1]]
  # html <- pjs_session$getUrl()
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//a[contains(@class, 'id-LinkOverlay-link')]") %>% 
    rvest::html_attr("title") -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//a[contains(@class, 'id-LinkOverlay-link')]") %>% 
    rvest::html_attr("href") %>%
    paste0("https:", .)-> item_link
  
  #givedate <- Sys.Date()
  
  givedate -> item_pubdate
  
  print(1)
  
  df <- data.frame(item_title, item_link, item_pubdate)
  
  print("df")
  
  return(df)
}

#pjs_session$go("https://www.sueddeutsche.de/archiv/m%C3%BCnchen/2023/01")

#pjs_session$getUrl()

#remDr$getPageSource()[[1]] %>% merkur_getlink()

merkur_getlink_url <- function(date){
  date %>% 
    format.Date(format="%Y-%m-%d&fd=%Y-%m-%d") -> url
  
  remDr$navigate(paste0("https://www.merkur.de/suche?tt=1&tx=&sb=0&td=", url, "&qr="))

  df <- merkur_getlink(remDr$getPageSource()[[1]], date)
  
  Sys.sleep(3)
  
  html <- remDr$getPageSource()[[1]]
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'id-Swiper-navcount')]") %>% 
    rvest::html_text(., trim = TRUE) -> navcount
  
  check <- as.numeric(stringr::str_extract(navcount, "^[0123456789]*")) < as.numeric(stringr::str_extract(navcount, "[0123456789]*$"))
  
  print(check)

  
  if(length(navcount) == 0){
    return(df)
  } else {
    while(check){
      
      webElem <- remDr$findElement(using = "xpath", "//div[contains(@class, 'id-Swiper-navnew-nextWrap')]")
      webElem$clickElement()
      
      Sys.sleep(sample((100:300)/100, 1))
      
      df <- dplyr::distinct(rbind(df, merkur_getlink(remDr$getPageSource()[[1]], date)))
      # df <- rbind(df, merkur_getlink(pjs_session$getSource()))
      print(nrow(df))
      
      html <- remDr$getPageSource()[[1]]
      
      
      rvest::read_html(html) %>% 
        rvest::html_elements(xpath = "//div[contains(@class, 'id-Swiper-navcount')]") %>% 
        rvest::html_text(., trim = TRUE) -> navcount
      
      check <- as.numeric(stringr::str_extract(navcount, "^[0123456789]*")) < as.numeric(stringr::str_extract(navcount, "[0123456789]*$"))
      
      print(check)
    }
  }
  return(df)
}

#bad_z_getlink_url("www.test.de")

## doesn't work headless - no idea why, individual pages work - sometimes not

merkur_go_thr_archive <- function(startdate, enddate){
  
  seq(as.Date(startdate), as.Date(enddate), by="days") -> V1

  
  V1 %>%
    purrr::map_df(~merkur_getlink_url(.)) -> valid_links
  print(Sys.time())
  return(valid_links)
}

# merkur_go_thr_archive_2 <- function(startdate, enddate){
#   remDr$navigate("https://merkur.de")
#   
#   html <- remDr$getPageSource()[[1]]
# 
#   # rvest::read_html(html) %>% 
#   #   rvest::html_elements(xpath = "//a[contains(@class, 'id-MainNavV2-el-link')]") %>% 
#   #   rvest::html_text(., trim = TRUE) -> rubrics
#   
#   rubic %>% merkur_go_thr_archive(startdate, enddate, .)
# }

#merkur_go_thr_archive(startdate = "2021-12-31", enddate = "2023-01-31", "") -> valid_links

remDr$navigate("https://merkur.de")  ### click away thing

merkur_go_thr_archive(startdate = "2023-01-01", enddate = "2023-02-01") -> valid_links1

merkur_go_thr_archive(startdate = "2023-02-01", enddate = "2023-04-01") -> valid_links2

merkur_go_thr_archive(startdate = "2023-04-01", enddate = "2023-06-01") -> valid_links3

merkur_go_thr_archive(startdate = "2023-06-01", enddate = "2023-08-01") -> valid_links4

merkur_go_thr_archive(startdate = "2023-08-01", enddate = "2023-10-01") -> valid_links5

merkur_go_thr_archive(startdate = "2023-10-01", enddate = "2023-12-01") -> valid_links6

merkur_go_thr_archive(startdate = "2023-12-01", enddate = "2024-02-01") -> valid_links7

merkur_go_thr_archive(startdate = "2024-02-01", enddate = "2024-04-01") -> valid_links8

merkur_go_thr_archive(startdate = "2024-04-01", enddate = "2024-06-01") -> valid_links9

merkur_go_thr_archive(startdate = "2024-06-01", enddate = "2024-08-01") -> valid_links10

merkur_go_thr_archive(startdate = "2024-08-01", enddate = "2024-10-01") -> valid_links11

merkur_go_thr_archive(startdate = "2024-10-01", enddate = "2024-12-01") -> valid_links12

merkur_go_thr_archive(startdate = "2024-12-01", enddate = "2025-01-01") -> valid_links13

merkur_go_thr_archive(startdate = "2025-01-01", enddate = Sys.Date()) -> valid_links14


valid_links <- dplyr::distinct(rbind(valid_links1, valid_links2,
                                     valid_links3, valid_links4,
                                     valid_links5, valid_links6,
                                     valid_links7, valid_links8,
                                     valid_links9, valid_links10,
                                     valid_links11, valid_links12,
                                     valid_links13, valid_links14))


valid_links %>% dplyr::rename(title = item_title, link = item_link, pubdate = item_pubdate) %>% 
  dplyr::mutate(pub = "Merkur", description = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links


saveRDS(valid_links, "Merkur.RDS")


remDr$close()
z <- rD$server$stop()
