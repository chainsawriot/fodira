
require(RSelenium)
require(magrittr)
rD <- RSelenium::rsDriver(browser = "firefox", port = sample(c(5678L, 5679L, 5680L, 5681L, 5682L), size = 1), check = FALSE, verbose = FALSE)
remDr <- rD[["client"]]

#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
taz_getlink <- function(html){

  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//ul[contains(@role, 'directory')]/li/a") %>% 
    length() -> j
  item_title <- c()
  for (i in 1:j) {
    
    rvest::read_html(html) %>% 
      rvest::html_elements(xpath = paste0("//ul[contains(@role, 'directory')]/li[",
                                          i,
                                          "]/a/h3")) %>% 
      rvest::html_text(., trim = TRUE) -> item_title_1
    if (length(item_title_1) == 0){
      rvest::read_html(html) %>% 
        rvest::html_elements(xpath = paste0("//ul[contains(@role, 'directory')]/li[",
                                            i,
                                            "]/a")) %>% 
        rvest::html_text(., trim = TRUE) -> item_title_1
    }
    item_title <- c(item_title, item_title_1)
  }

  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//ul[contains(@role, 'directory')]/li/a") %>% 
    rvest::html_attr("href") %>% paste0("https://www.taz.de", .) -> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//ul[contains(@role, 'directory')]//li[contains(@class, 'date')]") %>%
    rvest::html_text(., trim = TRUE) %>% 
    as.Date(tryFormat = c("%d. %m. %Y")) -> item_pubdate

    df <- data.frame(item_title, item_link, item_pubdate)
    
    return(df)
}

taz_getlink_url <- function(url){
  remDr$navigate(url)
  print(url)
  df <- taz_getlink(remDr$getPageSource()[[1]])
  
  remDr$getPageSource()[[1]] %>% rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'sectfoot')]//li[last()]") %>%
    rvest::html_text(., trim = TRUE) -> n
  remDr$getPageSource()[[1]] %>% rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'sectfoot')]//li[last()]/a") %>%
    rvest::html_attr("href") %>% paste0(url,.) -> url2
  
  while(n == "weitere >") {
    remDr$navigate(url2)
    print(url2)
    df <- rbind(df, taz_getlink(remDr$getPageSource()[[1]]))
    remDr$getPageSource()[[1]] %>% rvest::read_html(html) %>% 
      rvest::html_elements(xpath = "//div[contains(@class, 'sectfoot')]//li[last()]") %>%
      rvest::html_text(., trim = TRUE) -> n
    remDr$getPageSource()[[1]] %>% rvest::read_html(html) %>% 
      rvest::html_elements(xpath = "//div[contains(@class, 'sectfoot')]//li[last()]/a") %>%
      rvest::html_attr("href") %>% paste0(url,.) -> url2
  }
  
  return(df)
}

taz_go_thr_archive <- function(startdate){
  seq(as.Date(startdate)-1, Sys.Date()-2, by="days") %>% 
    format.Date(format="%Y-%m-%d") -> V1
  seq(as.Date(startdate)+1, Sys.Date(), by="days") %>% 
    format.Date(format="%Y-%m-%d") -> V2
  
  paste0("!s=&eTagAb=", V1, "&eTagBis=", V2, "/") -> V3
  
  V3 %>%
    paste0("https://www.taz.de/", .) %>%
    purrr::map_df(~taz_getlink_url(.)) -> valid_links
  
  return(valid_links)
}


taz_go_thr_archive(startdate = "2022-01-01") -> valid_links

remDr$close()
z <- rD$server$stop()
