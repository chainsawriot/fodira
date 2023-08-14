require(RSelenium)
require(magrittr)

rD <- RSelenium::rsDriver(browser = "firefox", port = sample(c(#5678L, #5679L, 
                                                               5680L, 5681L #, 5682L
                                                               ), size = 1), check = FALSE, verbose = FALSE)
remDr <- rD[["client"]]
remDr$navigate("https://www.freiewelt.net/")
webElem <- remDr$findElement(using = "css", "a[id='cmbutton-accept']")
webElem$clickElement()

# click to new page - click out google ad

#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
freiewelt_getlink <- function(html){
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//section[contains(@id, 'news')]//article//h1/a") %>% 
    rvest::html_text(., trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//section[contains(@id, 'news')]//article//h1/a") %>% 
    rvest::html_attr("href") %>% paste0("https://www.freiewelt.net/", .) -> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//section[contains(@id, 'news')]//article//time") %>% 
    rvest::html_text(., trim = TRUE) %>% 
    as.Date(tryFormat = c("%d.%m.%Y | %H:%M")) -> item_pubdate
  
  df <- data.frame(item_title, item_link, item_pubdate)
  return(df)
}

freiewelt_getlink_int_rep <- function(html){
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'c66l')]//article//h1/a") %>% 
    rvest::html_text(., trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'c66l')]//article//h1/a") %>% 
    rvest::html_attr("href") %>% paste0("https://www.freiewelt.net/", .) -> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'c66l')]//article//time") %>% 
    rvest::html_text(., trim = TRUE) %>% 
    as.Date(tryFormat = c("%d.%m.%Y | %H:%M")) -> item_pubdate
  
  df <- data.frame(item_title, item_link, item_pubdate)
  return(df)
}

freiewelt_getlink_focus <- function(html){
  html <- remDr$getPageSource()[[1]]
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//main[contains(@id, 'page-main')]//article[contains(@class, 'articlepreview mini related')]//h1/a") %>% 
    rvest::html_text(., trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//main[contains(@id, 'page-main')]//article[contains(@class, 'articlepreview mini related')]//h1/a") %>% 
    rvest::html_attr("href") %>% paste0("https://www.freiewelt.net/", .) -> item_link
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//main[contains(@id, 'page-main')]//article[contains(@class, 'articlepreview mini related')]//time") %>% 
    rvest::html_text(., trim = TRUE) %>% 
    as.Date(tryFormat = c("%d.%m.%Y | %H:%M")) -> item_pubdate
  
  df <- data.frame(item_title, item_link, item_pubdate)
  return(df)
}


freiewelt_getlink_blog <- function(html){
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//section[contains(@id, 'ourblogs')]//article//h1/a") %>% 
    rvest::html_text(., trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//section[contains(@id, 'ourblogs')]//article//h1/a") %>% 
    rvest::html_attr("href") %>% paste0("https://www.freiewelt.net/", .) -> item_link
  
  NA -> item_pubdate
  
  df <- data.frame(item_title, item_link, item_pubdate)
  return(df)
}

freiewelt_go_thr_archive <- function(url, startdate){
  i <- 1
  j <- 1
  valid_links <- data.frame()
  remDr$navigate(url)
  
  while (i >0) {
    if (url == "https://www.freiewelt.net/reportagen/") {
      freiewelt_getlink_int_rep(remDr$getPageSource()[[1]]) %>% subset(., item_pubdate >= as.Date(startdate))-> subset_links
      i <- nrow(subset_links)
    } else if(url == "https://www.freiewelt.net/interviews/") {
      freiewelt_getlink_int_rep(remDr$getPageSource()[[1]]) %>% subset(., item_pubdate >= as.Date(startdate))-> subset_links
      i <- nrow(subset_links)
    } else if(url == "https://www.freiewelt.net/im-fokus/") {
      freiewelt_getlink_focus(remDr$getPageSource()[[1]]) %>% subset(., item_pubdate >= as.Date(startdate))-> subset_links
      i <- nrow(subset_links)
    } else if (url == "https://www.freiewelt.net/blogs/"){
      freiewelt_getlink_blog(remDr$getPageSource()[[1]]) -> subset_links
      i <- 15-j
      j <- j+1
      print(nrow(subset_links))
    } else {
      freiewelt_getlink(remDr$getPageSource()[[1]]) %>% subset(., item_pubdate >= as.Date(startdate))-> subset_links
      i <- nrow(subset_links)
    }
    valid_links <- rbind(valid_links, subset_links)
    print(remDr$getCurrentUrl())
    print(i)
    rvest::read_html(remDr$getPageSource()[[1]]) %>% 
      rvest::html_elements(xpath = "//ul[contains(@class, 'paginator')]/li[last()]/a") %>% 
      rvest::html_attr("href") %>% paste0("https://www.freiewelt.net/", .) %>% remDr$navigate()
    
  }
  return(valid_links)
}

c("https://www.freiewelt.net/blogs/", "https://www.freiewelt.net/interviews/", 
  "https://www.freiewelt.net/reportagen/",   "https://www.freiewelt.net/im-fokus/",
  "https://www.freiewelt.net/", "https://www.freiewelt.net/corona/",
  "https://www.freiewelt.net/politik/", "https://www.freiewelt.net/wirtschaft/",
  "https://www.freiewelt.net/familie/", "https://www.freiewelt.net/christliches/", "https://www.freiewelt.net/nachrichten/",
   "https://www.freiewelt.net/lebensart/") %>% 
  purrr::map_df(~freiewelt_go_thr_archive(., startdate="2022-08-01"))  -> valid_links

valid_links <- dplyr::distinct(valid_links)

remDr$close()
z <- rD$server$stop()


valid_links %>% dplyr::rename(title = item_title, link = item_link, pubdate = item_pubdate) %>% 
  dplyr::mutate(pub = "Freie Welt", description = NA) %>%
  dplyr::select(pub, link, pubdate, title, description) -> valid_links


saveRDS(valid_links, "Freie Welt.RDS")
