
require(RSelenium)
require(magrittr)
rD <- RSelenium::rsDriver(browser = "firefox", port = sample(c(5678L, 5679L, 5680L, 5681L, 5682L), size = 1), check = FALSE, verbose = FALSE)
remDr <- rD[["client"]]

#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

wait_for_network_idle <- function(driver, timeout = 15) {
  start_time <- Sys.time()
  
  while (difftime(Sys.time(), start_time, units = "secs") < timeout) {
    active_requests <- driver$executeScript("return window.performance.getEntries().filter(e => e.responseEnd === 0).length;")[[1]]
    print("waitbuild")
    if (active_requests == 0) {
      print("no active requests")
      return(TRUE)  # No active network requests
    }
    print("sleep")
    Sys.sleep(1)  # Wait before checking again
  }
  print("timeout")
  return(FALSE)  # Timeout reached
}


#function for geting links from page
handelsblatt_getlink <- function(html){

  
  # Wait until the page is fully loaded
  while (!wait_for_network_idle(remDr)) {
    Sys.sleep(1)  # Wait for 1 second before checking again
  }
  
  
  for (i in 1:10) {
    remDr$executeScript("window.scrollBy(0, 1000);")
    Sys.sleep(sample(1:100/100, 1))
  }

  html <- remDr$getPageSource()[[1]]
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//app-dynamic-component-list//app-teaser-layout//h3") %>% 
    rvest::html_text(., trim = TRUE) -> item_title
  
  print(length(item_title))
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//app-dynamic-component-list//app-teaser-layout/a") %>% 
    rvest::html_attr("href") %>% paste0("https://www.handelsblatt.com", .) -> item_link
  
  print(length(item_link))
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//app-dynamic-component-list//app-teaser-layout//app-teaser-date") %>%
    rvest::html_text(., trim = TRUE) %>% stringr::str_replace("^[Gv].*", format(Sys.Date(), "%d.%m.%Y")) %>% lubridate::dmy() -> item_pubdate

  print(length(item_pubdate))
  
  
    df <- data.frame(item_title, item_link, item_pubdate)
    
    return(df)
}

handelsblatt_getlink_url <- function(url){
  remDr$navigate(url)
  print(url)
  df <- handelsblatt_getlink(remDr$getPageSource()[[1]])
  print(remDr$getCurrentUrl())
  
  # remDr$getPageSource()[[1]] %>% rvest::read_html(html) %>% 
  #   rvest::html_elements(xpath = "//div[contains(@class, 'vhb-teaser-pagination')]//li[last()]") %>%
  #   rvest::html_text(., trim = TRUE) -> n
  # 
  # if(length(n) > 0){
  #   for (i in 2:n) {
  #     webElem <- remDr$findElement(using = "css", "a[title='Weiter']")
  #     webElem$clickElement()
  #     df <- rbind(df, handelsblatt_getlink(remDr$getPageSource()[[1]]))
  #   } 
  # }
  
  return(df)
}

handelsblatt_go_thr_archive <- function(startdate){
  seq(as.Date(startdate), Sys.Date(), by="days") %>% 
    format.Date(format="%Y-%m-%d") -> V1
  
  V1 %>%
    paste0("https://www.handelsblatt.com/archiv/?date=", .) %>%
    purrr::map_df(~handelsblatt_getlink_url(.)) -> valid_links
  
  return(valid_links)
}


handelsblatt_go_thr_archive(startdate = "2023-01-01") -> valid_links

valid_links %>% dplyr::distinct() %>% 
  dplyr::rename(title = item_title, link = item_link, pubdate = item_pubdate) %>% 
  dplyr::mutate(pub = "Handelsblatt", description = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links


saveRDS(valid_links, "Handelsblatt.RDS")

remDr$close()
z <- rD$server$stop()
