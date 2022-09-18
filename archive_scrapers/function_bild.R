# 
# require(webdriver)
# require(magrittr)
# pjs_instance <- run_phantomjs()
# pjs_session <- Session$new(port = pjs_instance$port)

require(RSelenium)
require(magrittr)
rD <- RSelenium::rsDriver(browser = "firefox", port = sample(c(5678L, 5679L, 5680L, 5681L, 5682L), size = 1), check = FALSE, verbose = FALSE)
remDr <- rD[["client"]]

#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
bild_getlink <- function(html){

  #html <- remDr$getPageSource()[[1]]
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'txt')]//p/a[1]") %>% 
    rvest::html_text(., trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'txt')]//p/a[1]") %>% 
    rvest::html_attr("href")  %>% paste0("https://www.bild.de", .)-> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//span[contains(@class, 'headline')]") %>% 
    rvest::html_text(., trim = TRUE) %>% stringr::str_extract("[0-9]+[.][0-9]+[.][0-9]+") %>%
    as.Date(., tryFormat = c("%d.%m.%Y")) -> item_pubdate
  
  if (length(item_pubdate) == 0){
    df <- data.frame()
  } else {
    df <- data.frame(item_title, item_link, item_pubdate)
  }
    
    return(df[4:nrow(df),])
}

bild_getlink_url <- function(url){
  # pjs_session$go(url)
  # pjs_session$getUrl()
  remDr$navigate(url)
  print(url)
  df <- bild_getlink(remDr$getPageSource()[[1]])
  print(url)
  Sys.sleep(2)
  return(df)
}

bild_go_thr_archive <- function(startdate){
  seq(as.Date(startdate), Sys.Date(), by="days") %>% 
    as.character() %>% 
    stringr::str_replace_all("-0", "-") %>%
    stringr::str_replace_all("-", "/") -> V1
  
  V1 %>%
    paste0("https://www.bild.de/archive/", ., "/") %>%
    purrr::map_df(~bild_getlink_url(.)) -> valid_links
  
  return(valid_links)
}

#pjs_session$go("https://www.bild.de/archive/")
valid_links <- bild_go_thr_archive("2021-12-01")

valid_links %>% dplyr::distinct() %>% 
  dplyr::rename(title = item_title, link = item_link, pubdate = item_pubdate) %>% 
  dplyr::mutate(pub = "Bild", description = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links


saveRDS(valid_links, "Bild.RDS")


