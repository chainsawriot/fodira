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
bad_z_getlink <- function(html){
  #html <- pjs_session$getUrl()
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//section[contains(@class, 'row grid-12')]//div[contains(@class, 'column large__12 medium__6 small__6')]//ul[1]/li[contains(@class, 'media-box__article__box__item')][1]/a[1]") %>% 
    rvest::html_attr("title") -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//section[contains(@class, 'row grid-12')]//div[contains(@class, 'column large__12 medium__6 small__6')]//ul[1]/li[contains(@class, 'media-box__article__box__item')][1]/a[1]") %>% 
    rvest::html_attr("href") %>% paste0("https://www.badische-zeitung.de",.)-> item_link

  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//h2[contains(@class, 'media-box__article media-box__article__titel')]") %>%
    rvest::html_text(., trim = TRUE) %>% 
    stringr::str_extract("[0-9]+\\. [A-Za-zäöü]+ [0-9]+") %>% 
    stringr::str_replace(., "März", "March") %>%
    stringr::str_replace(., "Dezember", "December") %>%
    stringr::str_replace(., "Oktober", "October") %>%
    stringr::str_replace(., "Januar", "January") %>%
    stringr::str_replace(., "Februar", "February") %>%
    stringr::str_replace(., "Mai", "May") %>%
    stringr::str_replace(., "Juni", "June") %>%
    stringr::str_replace(., "Juli", "July") %>%
    lubridate::dmy() -> item_pubdate
  

  
    df <- data.frame(item_title, item_link, item_pubdate)
    
    return(df)
}

#pjs_session$go("https://www.sueddeutsche.de/archiv/m%C3%BCnchen/2022/01")

#pjs_session$getUrl()

bad_z_getlink_url <- function(url){
  remDr$navigate(url)
  print(url)
  df <- bad_z_getlink(remDr$getPageSource()[[1]])
  # print(1)
  # pjs_session$go(url)
  # print(pjs_session$getUrl())
  # df <- bad_z_getlink(pjs_session$getSource())
  print(nrow(df))
  # 
  # remDr$getPageSource()[[1]] %>% rvest::read_html(html) %>% 
  #   rvest::html_elements(xpath = "//li[contains(@class, 'navigation')]//li[last()]") %>%
  #   rvest::html_text(., trim = TRUE) -> n
  # 
  
  return(df)
}

#bad_z_getlink_url("www.test.de")

## doesn't work headless - no idea why, individual pages work - sometimes not

bad_z_go_thr_archive <- function(startdate){

  seq(as.Date(startdate), Sys.Date(), by="days") %>% 
    format.Date(format="/%Y/%m/%d") -> V1

  
  V1 %>%
    paste0("https://www.badische-zeitung.de/archiv", .) %>%
    purrr::map_df(~bad_z_getlink_url(.)) -> valid_links
  
  return(valid_links)
}

bad_z_go_thr_archive(startdate = "2022-08-31") -> valid_links

unique(valid_links$item_pubdate)
 remDr$close()
 z <- rD$server$stop()

valid_links %>% dplyr::rename(title = item_title, link = item_link, pubdate = item_pubdate) %>% 
  dplyr::mutate(pub = "Badische Zeitung", description = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links

saveRDS(valid_links, "Badische Zeitung.RDS")
