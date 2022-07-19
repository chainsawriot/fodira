
require(webdriver)
require(magrittr)
pjs_instance <- run_phantomjs()
pjs_session <- Session$new(port = pjs_instance$port)

#function for geting links from page
#Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "de_DE")


rubikon_getlink <- function(html){

  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'article-content')]//h2/a") %>% 
    rvest::html_text(., trim = TRUE) -> item_title
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//div[contains(@class, 'article-content')]//h2/a") %>% 
    rvest::html_attr("href") %>% paste0("https://www.rubikon.news", .)-> item_link

  ## Date is in 2 different formats - didn't find better way to get dates
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//article[contains(@class, 'article-horizontal article-big')]//div[contains(@class, 'article-meta')]") %>% 
    rvest::html_text(., trim = TRUE) %>% 
    stringr::str_extract(., pattern = "[0-9]+[. ]+[0-9a-zA-Z]+[. ][0-9]+") %>%
    as.Date(tryFormat = c("%d.%m.%Y", "%d. %B %Y")) -> item_pubdate
  
  rvest::read_html(html) %>% 
    rvest::html_elements(xpath = "//article[contains(@class, 'article-horizontal article-small')]//div[contains(@class, 'article-meta')]") %>% 
    rvest::html_text(., trim = TRUE) %>% 
    stringr::str_extract(., pattern = "[0-9]+[. ]+[0-9]+[. ][0-9]+") %>%
    as.Date(tryFormat = c("%d.%m.%Y", "%d. %B %Y"))  %>% c(item_pubdate, .)-> item_pubdate
    
    df <- data.frame(item_title, item_link, item_pubdate)
    return(df)
}

rubikon_getlink_url <- function(url){
  pjs_session$go(url)
  print(url)
  return(rubikon_getlink(pjs_session$getSource()))
}

rubikon_go_thr_archive <- function(startdate){
  i <- 1
  j <- 1
  valid_links <- data.frame()
  while (i > 0) {
    rubikon_getlink_url(paste0("https://www.rubikon.news/artikel/page/", j)) %>% 
      subset(item_pubdate>=as.Date(startdate)) -> subset_links
    i <- nrow(subset_links)
    j <- j + 1
    valid_links <- rbind(valid_links, subset_links)
  }
  return(valid_links)
}

valid_links <- rubikon_go_thr_archive("2022-01-01")


