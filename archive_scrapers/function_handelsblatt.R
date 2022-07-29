
require(RSelenium)
require(magrittr)
rD <- RSelenium::rsDriver(browser = "firefox", port = sample(c(5678L, 5679L, 5680L, 5681L, 5682L), size = 1), check = FALSE, verbose = FALSE)
remDr <- rD[["client"]]

#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
handelsblatt_getlink <- function(html){

  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//section[contains(@class, 'vhb-related')]//span[contains(@class, 'vhb-headline')]") %>% 
    rvest::html_text(., trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//section[contains(@class, 'vhb-related')]//ul[contains(@data-trigger-label, 'Treffer')]//a") %>% 
    rvest::html_attr("href") %>% paste0("https://www.rnd.de", .) -> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'vhb-archive-date')]") %>%
    rvest::html_text(., trim = TRUE) %>% 
    as.Date(tryFormat = c("%d. %B %Y")) -> item_pubdate

    df <- data.frame(item_title, item_link, item_pubdate)
    
    return(df)
}

handelsblatt_getlink_url <- function(url){
  remDr$navigate(url)
  print(url)
  df <- handelsblatt_getlink(remDr$getPageSource()[[1]])
  
  remDr$getPageSource()[[1]] %>% rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'vhb-teaser-pagination')]//li[last()]") %>%
    rvest::html_text(., trim = TRUE) -> n
  
  if(length(n) > 0){
    for (i in 2:n) {
      webElem <- remDr$findElement(using = "css", "a[title='Weiter']")
      webElem$clickElement()
      df <- rbind(df, handelsblatt_getlink(remDr$getPageSource()[[1]]))
    } 
  }
  
  return(df)
}

handelsblatt_go_thr_archive <- function(startdate){
  seq(as.Date(startdate), Sys.Date(), by="days") %>% 
    format.Date(format="%Y/%m/%d") -> V1
  
  V1 %>%
    paste0("https://www.handelsblatt.com/archiv/", ., "/") %>%
    purrr::map_df(~handelsblatt_getlink_url(.)) -> valid_links
  
  return(valid_links)
}


handelsblatt_go_thr_archive(startdate = "2022-01-01") -> valid_links

