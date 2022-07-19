
require(webdriver)
require(magrittr)
pjs_instance <- run_phantomjs()
pjs_session <- Session$new(port = pjs_instance$port)


#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")

#function for geting links from page
epoch_getlink <- function(html){

  rvest::read_html(pjs_session$getSource()) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'col-12 col-md-8 border-r')]//div[contains(@class, 'card-body')]/a") %>% 
    rvest::html_attr("title") -> item_title
  
  rvest::read_html(pjs_session$getSource()) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'col-12 col-md-8 border-r')]//div[contains(@class, 'card-body')]/a") %>% 
    rvest::html_attr("href")  -> item_link
  
  
  df <- data.frame(item_title, item_link)
  
  rvest::read_html(pjs_session$getSource()) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'col-12 col-md-8 border-r')]//div[contains(@class, 'd-flex flex-row align-items-center')]/a") %>% 
    rvest::html_attr("title") -> item_title
  
  rvest::read_html(pjs_session$getSource()) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'col-12 col-md-8 border-r')]//div[contains(@class, 'd-flex flex-row align-items-center')]/a") %>% 
    rvest::html_attr("href")  -> item_link
  
  df <- rbind(data.frame(item_title, item_link), df)
    return(df)
}

epoch_getdate <- function(html){

  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//span[contains(@class, 'publish-date')]") %>% 
    rvest::html_text(., trim = TRUE) %>% 
    as.Date(., tryFormat = c("%d. %B %Y")) -> item_pubdate
  
  return(item_pubdate)
}

epoch_getdate_url <- function(url){
  pjs_session$go(url)
  print(url)
  Sys.sleep(3)
  return(epoch_getdate(pjs_session$getSource()))
}

epoch_getlink_url <- function(url){
  pjs_session$go(url)
  print(url)
  df <- epoch_getlink(pjs_session$getSource())
  df$item_pubdate <- df$item_link %>% purrr::map_chr(~epoch_getdate_url(.))
  return(df)
}

pjs_session$go("https://www.epochtimes.de/politik/page/3")

df <- epoch_getlink(pjs_session$getSource())
df$item_pubdate <- df$item_link %>% purrr::map_chr(~epoch_getdate_url(.))

x <- epoch_getlink_url("https://www.epochtimes.de/politik/page/3")

lcm_go_thr_columns <- function(rubrik, startdate){
  i <- 1
  j <- 1
  valid_links <- data.frame()
  while (i > 0) {
    lcm_getlink_url(paste0("https://lowerclassmag.com/category/", rubrik, "/page/", j, "/")) %>% 
      subset(item_pubdate>=as.Date(startdate)) -> subset_links
    i <- nrow(subset_links)
    j <- j + 1
    valid_links <- rbind(valid_links, subset_links)
  }
  return(valid_links)
}


c("themen/inland", "themen/ausland", "themen/wirtschaft", "themen/kultur", 
  "lcm-columns/hinter-feindlichen-linien", "lcm-columns/anger-management",
  "lcm-columns/schabers-hass", "lcm-columns/bafi-im-bann-der-daemonen") %>% 
  purrr::map_dfr(~lcm_go_thr_columns(., startdate = "2022-01-01")) -> valid_links




